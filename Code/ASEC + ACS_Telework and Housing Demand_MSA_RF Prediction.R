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
library(ranger) # random forest package

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
    black = as.numeric(race == 200),
    white = as.numeric(race == 100),
    asian = as.numeric(race == 651),
    disability = as.numeric(diffany == 2),
    race = as.factor(race),
    educ = as.factor(educ),
    ind1990 = as.factor(ind1990),
    occ2010 = as.factor(occ2010),
    age_sq = age^2
  )



# Restrict to people between 22 and 64
df = merged_data %>%
  filter(age >= 22 & age <= 64)


# Compare out of sample fit between RF and FEOLS models using 80/20 split
# RF parameters
n_trees = 201
mtry_para = 3

train_size = floor(nrow(df)*0.80)
idx = sample(1:nrow(df), train_size)
df_train = df[idx,]
df_test = df[-idx,]


# Fully remote
# RF model on training data
fully_remote_rf = ranger(
  fully_remote ~ educ + male + race + age + disability + ind1990 + occ2010, 
  data = df_train, 
  case.weights = "wtfinl",
  num.trees = n_trees,
  classification = FALSE,
  importance = 'permutation',
  scale.permutation.importance = TRUE,
  respect.unordered.factors=TRUE,
  write.forest = TRUE,
  verbose = TRUE,
  mtry = mtry_para)

# FEOLS model on training data 
fully_remote_ols = feols(fully_remote ~ college + male + white + black + asian + age + age_sq | occ2010 + ind1990, subset(df_test,wtfinl>0), weights = df_test$wtfinl[df_test$wtfinl>0])




# Predict using testing set
fully_remote_pred_rf = predict(fully_remote_rf, data = df_test)
fully_remote_pred_ols = predict(fully_remote_ols, newdata = df_test)


df_check = data.frame(
  serial = df_test$serial, 
  pernum = df_test$pernum, 
  month = df_test$month, 
  year = df_test$year, 
  fully_remote = df_test$fully_remote, 
  fully_remote_pred_rf,
  fully_remote_pred_ols)




df_check_rf %>%
  mutate(
    fully_remote_guess_rf = as.numeric(prediction > 0.50),
    fully_remote_guess_ols = as.numeric(fully_remote_pred_ols > 0.50),
    correct_rf = as.numeric(fully_remote == fully_remote_guess_rf),
    correct_ols = as.numeric(fully_remote == fully_remote_guess_ols)
  ) %>%
  summarise(n_correct_rf = sum(correct_rf),
            n_correct_ols = sum(correct_ols),
            n_total = n()) %>%
  mutate(error_rf = 100*(1 - n_correct_rf/n_total),
         error_ols = 100*(1 - n_correct_ols/n_total))
  



# Hybrid remote
# RF model on training data
hybrid_remote_rf = ranger(
  hybrid_remote ~ educ + male + race + age + disability + ind1990 + occ2010, 
  data = df_train, 
  case.weights = "wtfinl",
  num.trees = n_trees,
  classification = FALSE,
  importance = 'permutation',
  scale.permutation.importance = TRUE,
  respect.unordered.factors=TRUE,
  write.forest = TRUE,
  verbose = TRUE,
  mtry = mtry_para)

# FEOLS model on training data 
hybrid_remote_ols = feols(hybrid_remote ~ college + male + white + black + asian + age + age_sq | occ2010 + ind1990, subset(df_test,wtfinl>0), weights = df_test$wtfinl[df_test$wtfinl>0])




# Predict using testing set
hybrid_remote_pred_rf = predict(hybrid_remote_rf, data = df_test)
hybrid_remote_pred_ols = predict(hybrid_remote_ols, newdata = df_test)


df_check = data.frame(
  serial = df_test$serial, 
  pernum = df_test$pernum, 
  month = df_test$month, 
  year = df_test$year, 
  hybrid_remote = df_test$hybrid_remote, 
  hybrid_remote_pred_rf,
  hybrid_remote_pred_ols)




df_check %>%
  mutate(
    hybrid_remote_guess_rf = as.numeric(prediction > 0.50),
    hybrid_remote_guess_ols = as.numeric(hybrid_remote_pred_ols > 0.50),
    correct_rf = as.numeric(hybrid_remote == hybrid_remote_guess_rf),
    correct_ols = as.numeric(hybrid_remote == hybrid_remote_guess_ols)) %>%
  summarise(n_correct_rf = sum(correct_rf),
            n_correct_ols = sum(correct_ols),
            n_total = n()) %>%
  mutate(error_rf = 100*(1 - n_correct_rf/n_total),
         error_ols = 100*(1 - n_correct_ols/n_total))










# Full model
# Fit to training data
# Predict remote work probabilities
fully_remote <- ranger(
  fully_remote ~ educ + male + race + age + disability + ind1990 + occ2010, 
  data = df, 
  case.weights = "wtfinl",
  num.trees = 201,
  classification = FALSE,
  importance = 'permutation',
  scale.permutation.importance = TRUE,
  respect.unordered.factors=TRUE,
  write.forest = TRUE,
  verbose = TRUE,
  mtry = 3)

hybrid_remote <- ranger(
  hybrid_remote ~ educ + male + race + age + disability + ind1990 + occ2010, 
  data = df, 
  case.weights = "wtfinl",
  num.trees = 201,
  classification = FALSE,
  importance = 'permutation',
  scale.permutation.importance = TRUE,
  respect.unordered.factors=TRUE,
  write.forest = TRUE,
  verbose = TRUE,
  mtry = 3)


any_remote <- ranger(
  any_remote ~ educ + male + race + age + disability + ind1990 + occ2010, 
  data = df, 
  case.weights = "wtfinl",
  num.trees = 201,
  classification = FALSE,
  importance = 'permutation',
  scale.permutation.importance = TRUE,
  respect.unordered.factors=TRUE,
  write.forest = TRUE,
  verbose = TRUE,
  mtry = 3)



# 2. Applying model to data from 2019---- 

# Open ipums data 
ddi_2019 = read_ipums_ddi("cps_2019_ddi.xml")
df_cps_2019 = read_ipums_micro(ddi_2019) %>%
  clean_names()

df_cps_2019 = df_cps_2019 %>%
  filter(empstat == 10,
         age > 22 & age < 64) %>%
  mutate(
    college = as.numeric(educ >= 111),
    male = as.numeric(sex == 1),
    age_sq = age^2,
    black = as.numeric(race == 200),
    white = as.numeric(race == 100),
    asian = as.numeric(race == 651),
    disability = as.numeric(diffany == 2),
    race = as.factor(race),
    educ = as.factor(educ),
    ind1990 = as.factor(ind1990),
    occ2010 = as.factor(occ2010)
  )


fully_remote_pred_rf = predict(fully_remote, data = df_cps_2019)
hybrid_remote_pred_rf = predict(hybrid_remote, data = df_cps_2019)
any_remote_pred_rf = predict(any_remote, data = df_cps_2019)


df_cps_2019$fully_remote_pred = fully_remote_pred_rf$predictions
df_cps_2019$hybrid_remote_pred = hybrid_remote_pred_rf$predictions
df_cps_2019$any_remote_pred = any_remote_pred_rf$predictions



# 3. MSA differences 2019 and 2022----
df_metro_2019 = df_cps_2019 %>%
  filter(!is.na(fully_remote_pred)) %>%
  group_by(metfips) %>%
  summarise(statefip = first(statefip),
            fully_remote_pred = sum(fully_remote_pred*wtfinl),
            hybrid_remote_pred = sum(hybrid_remote_pred*wtfinl),
            any_remote_pred = sum(any_remote_pred*wtfinl),
            metro_pop_2019 = sum(wtfinl)) %>%
  mutate(fully_remote_share_pred = 100*fully_remote_pred/metro_pop_2019,
         hybrid_remote_share_pred = 100*hybrid_remote_pred/metro_pop_2019,
         any_remote_share_pred = 100*any_remote_pred/metro_pop_2019) 



df_metro_2022 = df %>%
  group_by(metfips) %>%
  summarise(fully_remote_2022 = sum(fully_remote*wtfinl),
            hybrid_remote_2022 = sum(hybrid_remote*wtfinl),
            any_remote_2022 = sum(any_remote*wtfinl),
            metro_pop_2022 = sum(wtfinl)) %>%
  mutate(fully_remote_share_2022 = 100*fully_remote_2022/metro_pop_2022,
         hybrid_remote_share_2022 = 100*hybrid_remote_2022/metro_pop_2022,
         any_remote_share_2022 = 100*any_remote_2022/metro_pop_2022)




df_metro = left_join(df_metro_2019, df_metro_2022, by = "metfips") %>%
  mutate(fully_remote_share_diff = fully_remote_share_2022 - fully_remote_share_pred,
         hybrid_remote_share_diff = hybrid_remote_share_2022 - hybrid_remote_share_pred,
         any_remote_share_diff = any_remote_share_2022 - any_remote_share_pred) %>%
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
  select(cbsa20, puma, afact1 = afact) %>%
  filter(cbsa20 %in% unique(df_acs$GEOID))


xwalk_puma_cbsa = read_csv("puma10_cbsa13_xwalk_geocorr.csv", 
                           skip = 2,
                           col_names = xwalk_colnames2) %>%
  filter(as.numeric(state) < 57) %>%
  mutate(puma = paste0(state, puma12)) %>%
  select(puma, metfips = cbsa13, afact2 = afact) %>%
  filter(metfips %in% unique(df_cps_2019$metfips))


big_xwalk = full_join(xwalk_cbsa_puma, xwalk_puma_cbsa, by = "puma", relationship = "many-to-many") %>%
  mutate(afact = afact1*afact2) %>%
  filter(!is.na(metfips),
         cbsa20 != 99999) %>%
  group_by(cbsa20, metfips) %>%
  summarise(afact = sum(afact))



df_metro_housing = left_join(df_acs %>% mutate(GEOID = as.numeric(GEOID)), big_xwalk, by = c("GEOID" = "cbsa20")) %>%
  arrange(desc(afact), GEOID) %>%
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
  relocate(NAME, .after = metfips) %>%
  mutate(pop_share_2019 = metro_pop_2019/sum(metro_pop_2019),
         pop_share_2022 = metro_pop_2019/sum(metro_pop_2022))



# Write data set as .csv
write_csv(df_metro_housing, "metro_housing_and_telework_rf.csv")




# Plots and figures
