# Economic Innovation Group
# Telework Analysis
# Last updated 07-NOV-2023 by Eric C.

# CODE OVERVIEW
# This code computes occupation level HHI measures
# based on commuting zone employment
# NOTE: 2021 and 2019 5-year hsamples use 2010 PUMA codes
# 1. Apply PUMA --> county crosswalk
# 2. Compute county-level employment for each occupation
# 3. Aggregate to czone level
# 4. Compute occupation hhi

# Preliminaries
rm(list = ls())           # Clears workspace, be careful about hidden dependencies

# Load libraries
library(tidyverse)
library(tidycensus)
library(fixest)
library(ipumsr)
library(janitor)



theme_set(theme_bw())

# Define user-specific root folders
root_folders <- c("bglasner" = "C:/Users/bglasner/Dropbox/GitHub",
                  "bngla" = "C:/Users/bngla/Dropbox/GitHub",
                  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/Dropbox/GitHub",
                  "ericcarlson" = "/Users/ericcarlson/Dropbox/github")


# Get current user
current_user <- Sys.info()[["user"]]

# Check if current user's root folder is defined
if (current_user %in% names(root_folders)) {
  path_project <- root_folders[[current_user]]
} else {
  stop("Root folder for current user is not defined.")
}

# Define paths
path_data <- file.path(path_project, "telework-ASEC-analysis", "Data")
path_output <- file.path(path_project, "telework-ASEC-analysis", "Output")

# Set working directory
setwd(path_data)



ddi = read_ipums_ddi("usa_00054.xml")
df = read_ipums_micro(ddi) %>% 
  clean_names() %>%
  mutate(puma = paste0(str_pad(statefip, 2, "left", "0"), str_pad(puma, 5, "left", "0")))



# Open Geographic Crosswalks
county_xwalk_colnames = names(read_csv("puma10_county10_xwalk_geocorr.csv", n_max = 1))
county_xwalk = read_csv(
  "puma10_county10_xwalk_geocorr.csv",
  skip = 2,
  col_names = county_xwalk_colnames) %>%
  mutate(puma12 = as.character(paste0(state, puma12)))

cz_xwalk = readxl::read_xls("cz00_eqv_v1.xls") %>%
  select(county = 1, 
         czone = 2)


df = left_join(df, county_xwalk, by = c("puma" = "puma12"), relationship = "many-to-many")

# By occupation
# Collapse to occupation and county level
df_occ_hhi = df %>%
  group_by(occ2010, county, year) %>%
  summarise(annual_labor_hours = sum(wkswork1*uhrswork*perwt*afact),
            total_employment = sum(perwt*afact))


# Collapse to commuting zone level
df_occ_hhi = left_join(df_occ_hhi, cz_xwalk, by = "county") %>%
  filter(!is.na(czone)) %>%
  group_by(occ2010, czone, year) %>%
  summarise(annual_labor_hours = sum(annual_labor_hours),
            total_employment = sum(total_employment)) %>%
  group_by(occ2010, year) %>%
  mutate(share_labor_hours = 100*annual_labor_hours/sum(annual_labor_hours),
         share_employment = 100*total_employment/sum(total_employment)) %>%
  summarise(occ_hhi_hours = sum(share_labor_hours^2),
            occ_hhi_employment = sum(share_employment^2)) 


# By industry
# Collapse to industry and county level
df_ind_hhi = df %>%
  group_by(ind1990, county, year) %>%
  summarise(annual_labor_hours = sum(wkswork1*uhrswork*perwt*afact),
            total_employment = sum(perwt*afact))


# Collapse to commuting zone level
df_ind_hhi = left_join(df_ind_hhi, cz_xwalk, by = "county") %>%
  filter(!is.na(czone)) %>%
  group_by(ind1990, czone, year) %>%
  summarise(annual_labor_hours = sum(annual_labor_hours),
            total_employment = sum(total_employment)) %>%
  group_by(ind1990, year) %>%
  mutate(share_labor_hours = 100*annual_labor_hours/sum(annual_labor_hours),
         share_employment = 100*total_employment/sum(total_employment)) %>%
  summarise(ind_hhi_hours = sum(share_labor_hours^2),
            ind_hhi_employment = sum(share_employment^2)) 



# Add remote info
load("CPS_telework.RData")
table(merged_data$year, merged_data$month) # confirm that everything is March, 2023

# Create fulltime and hybrid remote variables
merged_data = merged_data %>%
  mutate(
    telework_factor = if_else(telework_any==1 & ahrsworkt>telework_hours,"Some Telework",
                              if_else(telework_any==1 & ahrsworkt==telework_hours,"All Telework", "No Telework")),
    fully_remote = as.integer(telework_factor == "All Telework"),
    hybrid_remote = as.integer(telework_factor == "Some Telework"),
    any_remote = fully_remote + hybrid_remote
  ) %>%
  mutate(college = as.numeric(educ >= 111))





# Restrict to people between 22 and 64
df = merged_data %>%
  filter(age > 22 & age < 65)


df_occ_remote = df %>%
  group_by(occ2010) %>%
  summarise(n_fully_remote = sum(fully_remote*wtfinl),
            n_hybrid_remote = sum(hybrid_remote*wtfinl),
            n_any_remote = sum(any_remote*wtfinl),
            n_college = sum(college*wtfinl),
            n_employees = sum(wtfinl)) %>%
  mutate(fully_remote_share = 100*n_fully_remote/n_employees,
         hybrid_remote_share = 100*n_hybrid_remote/n_employees,
         any_remote_share = 100*n_any_remote/n_employees,
         hybrid_remote_share_condit = 100*n_hybrid_remote/(n_fully_remote + n_hybrid_remote),
         fully_remote_share_condit = 100*n_fully_remote/(n_fully_remote + n_hybrid_remote),
         college_share = n_college/n_employees,
         aweight = n_employees/sum(n_employees)) 


df_ind_remote = df %>%
  group_by(ind1990) %>%
  summarise(n_fully_remote = sum(fully_remote*wtfinl),
            n_hybrid_remote = sum(hybrid_remote*wtfinl),
            n_any_remote = sum(any_remote*wtfinl),
            n_college = sum(college*wtfinl),
            n_employees = sum(wtfinl)) %>%
  mutate(fully_remote_share = 100*n_fully_remote/n_employees,
         hybrid_remote_share = 100*n_hybrid_remote/n_employees,
         any_remote_share = 100*n_any_remote/n_employees,
         hybrid_remote_share_condit = 100*n_hybrid_remote/(n_fully_remote + n_hybrid_remote),
         fully_remote_share_condit = 100*n_fully_remote/(n_fully_remote + n_hybrid_remote),
         college_share = n_college/n_employees,
         aweight = n_employees/sum(n_employees)) 



df_occ_hhi = left_join(df_occ_hhi, df_occ_remote, by = "occ2010", relationship = "many-to-many")
df_ind_hhi = left_join(df_ind_hhi, df_ind_remote, by = "ind1990", relationship = "many-to-many")



# Figures
# Occupation HHI
df_occ_hhi %>%
  filter(year == 2019) %>%
  ggplot(aes(x = occ_hhi_hours, y = hybrid_remote_share_condit)) +
  geom_point(aes(size = aweight),
             color = "#000050") +
  geom_smooth(method = "lm",
              se = F,
              color = "firebrick",
              lty = "dashed",
              aes(weight = aweight)) +
  guides(size = "none") +
  theme(aspect.ratio = 0.618)

hybrid_condit_reg_2019 = lm(hybrid_remote_share_condit ~ occ_hhi_hours, data = df_occ_hhi %>% filter(year == 2019), weights = df_occ_hhi[df_occ_hhi$year == 2019, ]$aweight)
hybrid_condit_reg_2021 = lm(hybrid_remote_share_condit ~ occ_hhi_hours, data = df_occ_hhi %>% filter(year == 2021), weights = df_occ_hhi[df_occ_hhi$year == 2021, ]$aweight)
summary(hybrid_condit_reg_2019)  
summary(hybrid_condit_reg_2021)  


# Industry HHI
df_ind_hhi %>%
  filter(year == 2021) %>%
  ggplot(aes(x = ind_hhi_hours, y = hybrid_remote_share_condit)) +
  geom_point(aes(size = aweight),
             color = "#000050") +
  geom_smooth(method = "lm",
              se = F,
              color = "firebrick",
              lty = "dashed",
              aes(weight = aweight)) +
  guides(size = "none") +
  theme(aspect.ratio = 0.618)


hybrid_condit_reg_2019 = lm(hybrid_remote_share_condit ~ ind_hhi_hours, data = df_ind_hhi %>% filter(year == 2019), weights = df_ind_hhi[df_ind_hhi$year == 2019, ]$aweight)
hybrid_condit_reg_2021 = lm(hybrid_remote_share_condit ~ ind_hhi_hours, data = df_ind_hhi %>% filter(year == 2021), weights = df_ind_hhi[df_ind_hhi$year == 2021, ]$aweight)
summary(hybrid_condit_reg_2019)  
summary(hybrid_condit_reg_2021)  


