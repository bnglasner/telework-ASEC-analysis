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
library(tidycensus)
library(tidyverse)
library(fixest)
library(ipumsr)
library(tigris)
library(janitor)
library(randomForest)

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



# Restrict to people between 16 and 64
df = merged_data %>%
  filter(age > 15 & age < 65)


df$telework_factor = as.factor(df$telework_factor)
df$any_remote = as.factor(df$any_remote)

training_set_size = floor(nrow(df)*0.80)
idx = sample(1:nrow(df), size = training_set_size)
df_train = df[idx,] %>% 
  select(telework_factor, occ2010, ind1990, college, male, white, black, asian, age, age_sq)
df_test = df[-idx,] %>% 
  select(telework_factor, occ2010, ind1990, college, male, white, black, asian, age, age_sq)


rf = randomForest(telework_factor ~ ., data = df_train, mtry = 4, ntree = 2001, importance = TRUE)
rf

plot(rf)


result = data.frame(df_test$telework_factor, predict(rf, df_test, type = "response"))
result %>%
  head(15)


plot(result)



