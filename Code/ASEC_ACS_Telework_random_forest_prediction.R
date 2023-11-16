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

# Function to train a ranger model
train_ranger_model <- function(target, df) {
  formula_str <- paste0(target, " ~ educ + male + race + age + disability + ind1990 + occ2010")
  model <- ranger(
    formula = as.formula(formula_str),
    data = df,
    # You can select either probability, classification, or neither.
    probability = TRUE, # This might be a better approach than just the proportion within each bin
    # classification = TRUE, # This will likely underestimate the number of remote workers - since they are still the minority share across most category bins
    case.weights = df$wtfinl,
    num.trees = 500,
    importance = 'permutation',
    scale.permutation.importance = TRUE,
    respect.unordered.factors = TRUE,
    write.forest = TRUE,
    verbose = TRUE,
    mtry = 3
  )
  return(model)
}

# Function to make predictions using a trained ranger model
make_predictions <- function(model, df) {
  predictions <- predict(model, data = df)$predictions
  return(predictions)
}

# Function to categorize predictions
categorize_predictions <- function(predictions, actual, prefix) {
  prediction_type <- case_when(
    actual == 0 & predictions < 0.5 ~ paste("Correctly Predict Non-", prefix, sep=""),
    actual == 1 & predictions >= 0.5 ~ paste("Correctly Predict ", prefix, sep=""),
    actual == 1 & predictions < 0.5 ~ paste("Failed to Predict ", prefix, sep=""),
    actual == 0 & predictions >= 0.5 ~ paste("Failed to Predict Non-", prefix, sep=""),
    TRUE ~ "Missing"
  )
  return(prediction_type)
}

# Need to run the following functions within each outcome of interest:

# Define a list of outcome-label pairs
# First element is the outcome variable and second is the label to use
outcomes_and_labels <- list(
  c('fully_remote', "Remote"),
  c('hybrid_remote', "Hybrid")
)

# Loop through each pair to train the model, make predictions, categorize them, and view the tables
for (pair in outcomes_and_labels) {
  outcome_variable <- pair[1]
  label <- pair[2]
  
  # Train the model
  model <- train_ranger_model(outcome_variable, df)
  
  # Make predictions
  predictions <- make_predictions(model, df)
  
  # Categorize predictions
  prediction_type_column <- paste0(outcome_variable, "_prediction_type")
  df[[prediction_type_column]] <- categorize_predictions(predictions, df[[outcome_variable]], label)
  
  # View the tables of categorized predictions
  print(table(df[[prediction_type_column]]))
}
