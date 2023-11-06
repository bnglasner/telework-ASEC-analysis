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
    disability = as.numeric(diffany == 2),
    race = as.factor(race),
    educ = as.factor(educ),
    ind1990 = as.factor(ind1990),
    occ2010 = as.factor(occ2010)
  )



# Restrict to people between 16 and 64
df = merged_data %>%
  filter(age >= 22 & age <= 64)

# In the previous version, we use an feols model for the prediction, commented out below. Now we will try a random forest instead
# fully_remote = feols(fully_remote ~ college + male + white + black + asian + age + age_sq | occ2010 + ind1990, subset(df,wtfinl>0), weights = df$wtfinl[df$wtfinl>0])
# hybrid_remote = feols(hybrid_remote ~ college + male + white + black + asian + age + age_sq | occ2010 + ind1990, subset(df,wtfinl>0), weights = df$wtfinl[df$wtfinl>0])

fully_remote <- ranger(fully_remote ~ educ + male + race + age + disability + ind1990 + occ2010, 
                       data = df, 
                       case.weights = "wtfinl",
                       # classification = TRUE,
                       importance = 'permutation',
                       scale.permutation.importance = TRUE,
                       respect.unordered.factors=TRUE,
                       write.forest = TRUE,
                       verbose = TRUE,
                       mtry = 3)
print(fully_remote)
p.remote <- predict(fully_remote, data=df)
str(p.remote)
summary(df$fully_remote - p.remote$predictions) 

df$fully_remote_prediction_noedit <- p.remote$predictions

# If you use the classification =TRUE, then test accuracy across bins of prediction 
df <- df %>% mutate(
  fully_remote_prediction = case_when(
    fully_remote_prediction_noedit<0.5 ~ 0,
    fully_remote_prediction_noedit>=0.5 ~1,
    TRUE ~ NA
  ),
  fully_remote_prediction_type = case_when(
  fully_remote == 0 & fully_remote_prediction == 0 ~ "Correctly Predict Non-Remote",
  fully_remote == 1 & fully_remote_prediction == 1 ~ "Correctly Predict Remote",
  fully_remote == 1 & fully_remote_prediction == 0 ~ "Failed to Predict Remote",
  fully_remote == 0 & fully_remote_prediction == 1 ~ "Failed to Predict Non-Remote",
  TRUE ~ "Missing"
))
table(df$fully_remote_prediction_type)
ranger::importance(fully_remote)
#####

hybrid_remote <- ranger(hybrid_remote ~ educ + male + race + age + disability + ind1990 + occ2010, 
                       data = df, 
                       case.weights = "wtfinl",
                       # classification = TRUE,
                       importance = 'permutation',
                       scale.permutation.importance = TRUE,
                       respect.unordered.factors=TRUE,
                       write.forest = TRUE,
                       verbose = TRUE,
                       mtry = 3)
print(hybrid_remote)
p.remote <- predict(hybrid_remote, data=df)
str(p.remote)
summary(df$hybrid_remote - p.remote$predictions) 

df$hybrid_remote_prediction_noedit <- p.remote$predictions

# If you use the classification =TRUE, then test accuracy across bins of prediction 
df <- df %>% mutate(
  hybrid_remote_prediction = case_when(
    hybrid_remote_prediction_noedit<0.5 ~ 0,
    hybrid_remote_prediction_noedit>=0.5 ~1,
    TRUE ~ NA
  ),
  hybrid_remote_prediction_type = case_when(
    hybrid_remote == 0 & hybrid_remote_prediction == 0 ~ "Correctly Predict Non-Hybrid",
    hybrid_remote == 1 & hybrid_remote_prediction == 1 ~ "Correctly Predict Hybrid",
    hybrid_remote == 1 & hybrid_remote_prediction == 0 ~ "Failed to Predict Hybrid",
    hybrid_remote == 0 & hybrid_remote_prediction == 1 ~ "Failed to Predict Non-Hybrid",
    TRUE ~ "Missing"
  ))
table(df$hybrid_remote_prediction_type)
ranger::importance(hybrid_remote)

