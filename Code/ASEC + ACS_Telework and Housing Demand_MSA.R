# Economic Innovation Group
# Telework Analysis
# Last updated 02-NOV-2023 by Eric C.

# CODE OVERVIEW
# This file conains four main parts
# 1. Compute expected working from home shares for fulltime and hyrid remote 
#    workers using CPS telework data 
# 2. Assign predicted telework shares to MSAs based on their 2019 industry 
#    and occupation mix
# 3. Use this to compute the difference between predicted and actual telework
#    shares at the MSA level
# 4. Merge ACS median home value data for 2019 and 2022 with telework 
#    predictions
# 5. Run regression looking at effect of telework on home values 



# Preliminaries
rm(list = ls())           # Clears workspace, be careful about hidden dependencies

# Load libraries
library(tidyverse)
library(tidycensus)
library(fixest)
library(ipumsr)
library(tigris)
library(janitor)

options(tigris_use_cache = TRUE)
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




# 1. Predict telework shares from CPS data----
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
    age_sq = age^2,
    black = as.numeric(race == 200),
    white = as.numeric(race == 100),
    asian = as.numeric(race == 651),
    disability = as.numeric(diffany == 2)
  )



# Restrict to people between 22 and 64
df = merged_data %>%
  filter(age > 22 & age < 65)

fully_remote = feols(fully_remote ~ college + male + white + black + asian + age + age_sq | occ2010 + ind1990, subset(df,wtfinl>0), weights = df$wtfinl[df$wtfinl>0])
hybrid_remote = feols(hybrid_remote ~ college + male + white + black + asian + age + age_sq | occ2010 + ind1990, subset(df,wtfinl>0), weights = df$wtfinl[df$wtfinl>0])





# 2. Applying model to data from 2019---- 

# Open ipums data 
ddi_2019 = read_ipums_ddi("cps_2019_ddi.xml")
df_cps_2019 = read_ipums_micro(ddi_2019) %>%
  clean_names()

df_cps_2019 = df_cps_2019 %>%
  filter(empstat == 10,
         age > 15 & age < 64) %>%
  mutate(
    college = as.numeric(educ >= 111),
    male = as.numeric(sex == 1),
    age_sq = age^2,
    black = as.numeric(race == 200),
    white = as.numeric(race == 100),
    asian = as.numeric(race == 651)
  ) 

fully_remote_pred = predict(fully_remote, newdata = df_cps_2019)
hybrid_remote_pred = predict(hybrid_remote, newdata = df_cps_2019)


df_cps_2019$fully_remote_pred = fully_remote_pred
df_cps_2019$hybrid_remote_pred = hybrid_remote_pred






# 3. MSA differences 2019 and 2022----
df_metro_2019 = df_cps_2019 %>%
  filter(!is.na(fully_remote_pred)) %>%
  mutate(fully_remote_pred = ifelse(fully_remote_pred < 0, 0, fully_remote_pred),
         hybrid_remote_pred = ifelse(hybrid_remote_pred < 0, 0, hybrid_remote_pred)) %>%
  group_by(metfips) %>%
  summarise(statefip = first(statefip),
            fully_remote_pred = sum(fully_remote_pred*wtfinl),
            hybrid_remote_pred = sum(hybrid_remote_pred*wtfinl),
            metro_pop = sum(wtfinl)) %>%
  mutate(fully_remote_share_pred = 100*fully_remote_pred/metro_pop,
         hybrid_remote_share_pred = 100*hybrid_remote_pred/metro_pop) 



df_metro_2022 = df %>%
  group_by(metfips) %>%
  summarise(fully_remote_2022 = sum(fully_remote*wtfinl),
            hybrid_remote_2022 = sum(hybrid_remote*wtfinl),
            metro_pop_2022 = sum(wtfinl)) %>%
  mutate(fully_remote_share_2022 = 100*fully_remote_2022/metro_pop_2022,
         hybrid_remote_share_2022 = 100*hybrid_remote_2022/metro_pop_2022)




df_metro = left_join(df_metro_2019, df_metro_2022, by = "metfips") %>%
  mutate(fully_remote_share_diff = fully_remote_share_2022 - fully_remote_share_pred,
         hybrid_remote_share_diff = hybrid_remote_share_2022 - hybrid_remote_share_pred) %>%
  filter(metfips < 99998)





# 4. Adding MSA home values----

acs_varlist = load_variables(year = 2022, dataset = "acs1")

df_acs_2022 = get_acs(
  variables = c(median_home_value_2022 = "B25077_001"),
  geography = "cbsa",
  year = 2022,
  survey = "acs1"
)  %>%
  filter(!grepl("Micro Area", NAME),
         !grepl(", PR", NAME)) %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    values_from = estimate,
    names_from = variable
  )

df_acs_2019 = get_acs(
  variables = c(median_home_value_2019 = "B25077_001"),
  geography = "cbsa",
  year = 2019,
  survey = "acs1"
)  %>%
  filter(!grepl("Micro Area", NAME),
         !grepl(", PR", NAME)) %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    values_from = estimate,
    names_from = variable
  ) %>%
  rename(NAME_2019 = NAME)


df_acs = left_join(df_acs_2019, df_acs_2022, by = "GEOID") %>%
  select(GEOID, NAME, median_home_value_2019, median_home_value_2022)



# Crosswalks 
xwalk_colnames1 = names(read_csv("cbsa20_puma10_xwalk_geocorr.csv", n_max = 1))
xwalk_colnames2 = names(read_csv("puma10_cbsa13_xwalk_geocorr.csv", n_max = 1))

xwalk_cbsa_puma = read_csv("cbsa20_puma10_xwalk_geocorr.csv", 
                           skip = 2,
                           col_names = xwalk_colnames1) %>%
  filter(as.numeric(state) < 57) %>%
  mutate(puma = paste0(state, puma12)) %>%
  select(cbsa20, puma, afact1 = afact)


xwalk_puma_cbsa = read_csv("puma10_cbsa13_xwalk_geocorr.csv", 
                           skip = 2,
                           col_names = xwalk_colnames2) %>%
  filter(as.numeric(state) < 57) %>%
  mutate(puma = paste0(state, puma12)) %>%
  select(puma, metfips = cbsa13, afact2 = afact)


big_xwalk = full_join(xwalk_cbsa_puma, xwalk_puma_cbsa, by = "puma", relationship = "many-to-many") %>%
  mutate(afact = afact1*afact2) %>%
  filter(!is.na(metfips),
         cbsa20 != 99999) %>%
  group_by(cbsa20, metfips) %>%
  summarise(afact = sum(afact))


xwalk_matches = big_xwalk %>%
  mutate(exact_match = as.numeric(cbsa20 == metfips)) %>%
  filter(exact_match == 1) %>%
  select(cbsa20, exact_match)

cbsa20_cbsa2013_xwalk = left_join(big_xwalk, xwalk_matches, by = "cbsa20") %>%
  mutate(exact_match = ifelse(is.na(exact_match), 0, exact_match)) %>%
  filter(exact_match == 1 & cbsa20 == metfips | exact_match == 0) %>%
  group_by(cbsa20) %>%
  mutate(afact = afact/sum(afact)) %>%
  select(-exact_match)




df_metro_housing = left_join(df_acs %>% mutate(GEOID = as.numeric(GEOID)), cbsa20_cbsa2013_xwalk, by = c("GEOID" = "cbsa20")) %>%
  group_by(metfips) %>%
  summarise(NAME = first(NAME),
            median_home_value_2019 = sum(median_home_value_2019*afact),
            median_home_value_2022 = sum(median_home_value_2022*afact),
            denom = sum(afact)) %>%
  mutate(median_home_value_2019 = round(median_home_value_2019/denom, 0),
         median_home_value_2022 = round(median_home_value_2022/denom, 0)) %>%
  mutate(d_median_home_value = median_home_value_2022 - median_home_value_2019,
         d_median_home_value_pct = 100*(median_home_value_2022 - median_home_value_2019)/(median_home_value_2019)) %>%
  select(-denom) 




df_metro_housing = left_join(df_metro, df_metro_housing, by = "metfips") %>%
  relocate(NAME, .after = metfips) 
  
write_csv(df_metro_housing, "metro_housing_and_telework.csv")


# Plots and figures
plot1 = df_metro_housing %>%
  ggplot(aes(x = fully_remote_share_2022, y = d_median_home_value/1000)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = F,
              color = "firebrick",
              lty = "dashed") +
  xlab("Share Fully Remote (2022)") +
  ylab("Change in Median Home Value (Thousands USD)") +
  ggtitle("(a) Fully Remote") +
  theme(aspect.ratio = 0.618,
        plot.title = element_text(hjust = 0.5))


plot2 = df_metro_housing %>%
  ggplot(aes(x = hybrid_remote_share_2022, y = d_median_home_value/1000)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = F,
              color = "firebrick",
              lty = "dashed") +
  xlab("Share Hybrid Remote (2022)") +
  ylab("Change in Median Home Value (Thousands USD)") +
  ggtitle("(b) Hybrid Remote") +
  theme(aspect.ratio = 0.618,
        plot.title = element_text(hjust = 0.5))


plot3 = df_metro_housing %>%
  ggplot(aes(x = fully_remote_share_diff, y = d_median_home_value/1000)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = F,
              color = "firebrick",
              lty = "dashed") +
  xlab("Residual Fully Remote") +
  ylab("Change in Median Home Value (Thousands USD)") +
  ggtitle("(c) Fully Remote Resid") +
  theme(aspect.ratio = 0.618,
        plot.title = element_text(hjust = 0.5))

plot4 = df_metro_housing %>%
  ggplot(aes(x = hybrid_remote_share_diff, y = d_median_home_value/1000)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = F,
              color = "firebrick",
              lty = "dashed") +
  xlab("Residual Hybrid Remote") +
  ylab("Change in Median Home Value (Thousands USD)") +
  ggtitle("(d) Hybrid Remote Resid") +
  theme(aspect.ratio = 0.618,
        plot.title = element_text(hjust = 0.5))


gridExtra::grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)


# Mapping
cbsa_sf = tigris::core_based_statistical_areas(year = 2013, cb = FALSE, resolution = "500k") %>%
  tigris::shift_geometry() %>%
  filter(!grepl(", PR", NAME),
         !grepl("Micro Area", NAMELSAD))

states_sf = tigris::states(year = 2021, cb = FALSE, resolution = "500k") %>%
  tigris::shift_geometry() %>%
  filter(as.numeric(STATEFP) < 57)



msa_data = left_join(cbsa_sf %>% mutate(GEOID = as.numeric(GEOID)), df_metro, by = c("GEOID" = "metfips")) %>%
  separate(NAME, into = c("NAME", "STATE_ABBRV"), sep = ", ") %>%
  separate(STATE_ABBRV, into = c("STATE_ABBRV"), sep = "-")

# Zoom in on NY (26), PA (42), NJ (34), MD (24)

# Residuals (fully remote)
ggplot() +
  geom_sf(aes(fill = fully_remote_share_diff),
          data = msa_data %>%
            filter(STATE_ABBRV == "NY" | STATE_ABBRV == "PA" | STATE_ABBRV == "NJ" | STATE_ABBRV == "MD"),
          color = "black") +
  geom_sf(data = states_sf %>% 
            filter(STATEFP == "26" | STATEFP == "42" | STATEFP == "34" | STATEFP == "24"),
          color = "black",
          fill = NA) +
  scale_fill_gradient2(
    low = "firebrick",
    mid = "white",
    high = "navyblue",
    midpoint = 0
  ) +
  theme_void()


# Fully remote share in 2022
ggplot() +
  geom_sf(aes(fill = fully_remote_share_2022),
          data = msa_data,
          color = "black") +
  geom_sf(data = states_sf,
          color = "black",
          fill = NA) +
  scale_fill_gradient2(
    low = "firebrick",
    mid = "white",
    high = "navyblue",
    midpoint = 0
  ) +
  theme_void()

