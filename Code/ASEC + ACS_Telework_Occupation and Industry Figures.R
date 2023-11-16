# Economic Innovation Group
# Telework Analysis
# Last updated 07-NOV-2023 by Eric C.


# OVERVIEW 
# Constructs occupation level measure of telework
# 1. Full time remote
# 2. Hybrid remote
# 3. Total remote
# 4. Total workers

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
# C:/Users/...
root_folders <- c()



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



# Load merged data file
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
  mutate(
    college = as.numeric(educ >= 111),
    male = as.numeric(sex == 1),
    black = as.numeric(race == 200),
    white = as.numeric(race == 100),
    asian = as.numeric(race == 651),
    disability = as.numeric(diffany == 2),
    race = as.factor(race),
    educ = as.factor(educ),
    ind1990 = as.factor(ind1990),
    occ2010 = as.factor(occ2010)
  )



# Restrict to people between 22 and 64
df = merged_data %>%
  filter(age >= 22 & age <= 64)


# Collapse to occupation level
df_occ = df %>%
  group_by(occ2010) %>%
  summarise(n_fully_remote = sum(fully_remote*wtfinl),
            n_hybrid_remote = sum(hybrid_remote*wtfinl),
            n_any_remote = sum(any_remote*wtfinl),
            n_workers = sum(wtfinl)) %>%
  ungroup() %>%
  mutate(fully_remote_share = 100*n_fully_remote/n_workers,
         hybrid_remote_share = 100*n_hybrid_remote/n_workers,
         any_remote_share = 100*n_any_remote/n_workers,
         employment_share = 100*n_workers/sum(n_workers)) 


# Collapse to industry level
df_ind = df %>%
  group_by(ind1990) %>%
  summarise(n_fully_remote = sum(fully_remote*wtfinl),
            n_hybrid_remote = sum(hybrid_remote*wtfinl),
            n_any_remote = sum(any_remote*wtfinl),
            n_workers = sum(wtfinl)) %>%
  ungroup() %>%
  mutate(fully_remote_share = 100*n_fully_remote/n_workers,
         hybrid_remote_share = 100*n_hybrid_remote/n_workers,
         any_remote_share = 100*n_any_remote/n_workers,
         employment_share = 100*n_workers/sum(n_workers)) 



# Scatter plots
df_occ %>%
  ggplot(aes(x = fully_remote_share, y = hybrid_remote_share)) +
  geom_point(aes(size = employment_share)) +
  geom_abline(intercept = 0, slope = 1.0, 
              color = "firebrick",
              lty = "dashed",
              size = 1) +
  xlim(c(0, 50)) +
  ylim(c(0, 50)) +
  guides(size = FALSE) +
  theme(aspect.ratio = 1.0)


df_occ %>%
  ggplot(aes(y = hybrid_remote_share, x = any_remote_share)) +
  geom_point(aes(size = employment_share)) +
  geom_abline(intercept = 0, slope = 0.5, 
              color = "firebrick",
              lty = "dashed",
              size = 1) +
  guides(size = FALSE) +
  theme(aspect.ratio = 1.0)





# Occupation HHI for MSAs
df_occ_hhi = df %>%
  group_by(metfips, occ2010) %>%
  summarise(employment = sum(wtfinl)) %>%
  group_by(metfips) %>%
  mutate(employment_share = 100*employment/sum(employment)) %>%
  summarise(occupation_hhi = sum(employment_share^2)) 

df_ind_hhi = df %>%
  group_by(metfips, ind1990) %>%
  summarise(employment = sum(wtfinl)) %>%
  group_by(metfips) %>%
  mutate(employment_share = 100*employment/sum(employment)) %>%
  summarise(industry_hhi = sum(employment_share^2)) 


df_hhi = left_join(df_occ_hhi, df_ind_hhi, by = "metfips")

df_hhi %>%
  ggplot() +
  geom_histogram(aes(x = occupation_hhi, after_stat(density)),
                 bins = 30,
                 fill = "lightgrey",
                 color = "black")



df_hhi %>%
  ggplot() +
  geom_histogram(aes(x = industry_hhi, after_stat(density)),
                 bins = 30,
                 fill = "lightgrey",
                 color = "black")





# Mapping
cbsa_sf = tigris::core_based_statistical_areas(year = 2013, cb = FALSE, resolution = "500k") %>%
  tigris::shift_geometry() %>%
  filter(!grepl(", PR", NAME),
         !grepl("Micro Area", NAMELSAD))

states_sf = tigris::states(year = 2021, cb = FALSE, resolution = "500k") %>%
  tigris::shift_geometry() %>%
  filter(as.numeric(STATEFP) < 57)


cbsa_sf = left_join(cbsa_sf %>% mutate(GEOID = as.numeric(GEOID)), df_hhi, by = c("GEOID" = "metfips"))


occ_hhi_breaks = quantile(df_hhi$occupation_hhi, probs = seq(0, 1, by = 1/5), na.rm = T)
occ_hhi_labs = c(
  paste(round(occ_hhi_breaks[1],2)," - ", round(occ_hhi_breaks[2],2), sep = ""),
  paste(round(occ_hhi_breaks[2],2)," - ", round(occ_hhi_breaks[3],2), sep = ""),
  paste(round(occ_hhi_breaks[3],2)," - ", round(occ_hhi_breaks[4],2), sep = ""),
  paste(round(occ_hhi_breaks[4],2)," - ", round(occ_hhi_breaks[5],2), sep = ""),
  paste(round(occ_hhi_breaks[5],2)," - ", round(occ_hhi_breaks[6],2), sep = "")
)


color_gradient = colorRampPalette(c("#FFFFFF", "#000050"))
color_gradient(5)

blue5 = c("#FFFFFF", "#D4D4E1", "#AAAAC4", "#55558A", "#000050")



ggplot() +
  geom_sf(aes(fill = cut(occupation_hhi, occ_hhi_breaks, include.lowest = TRUE)),
          data = cbsa_sf,
          color = "black") +
  geom_sf(data = states_sf,
          color = "black",
          fill = NA) +
  scale_fill_manual(name = "Occupation HHI",
                    values = blue5,
                    labels = occ_hhi_labs) +
  theme_void()


