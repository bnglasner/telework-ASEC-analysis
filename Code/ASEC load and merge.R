# Economic Innovation Group
# Telework Analysis
# 11/2/2023

##########################
#         Set Up         #  
##########################

library(dplyr)
library(readxl)
library(readr)
library(ipumsr)
library(janitor)

#################
### Set paths ###
#################

# Define user-specific root folders
root_folders <- c("bglasner" = "C:/Users/bglasner/Dropbox/GitHub",
                  "bngla" = "C:/Users/bngla/Dropbox/GitHub",
                  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/Dropbox/GitHub")

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


#################
### Data Load ###
#################

telework_microdata_CPS <- read_csv("telework_microdata_CPS.csv")
telework_microdata_CPS <- telework_microdata_CPS %>% clean_names()

cps_monthly_files <- read_csv("cps_monthly_files.csv")
cps_monthly_files <- cps_monthly_files %>% 
  clean_names()  %>%
  select(serial, pernum, year, month, hrhhid ,hrhhid2, pulineno )

ddi <- read_ipums_ddi("cps_00025.xml")
data <- read_ipums_micro(ddi)
data <- data %>% 
  clean_names() %>% 
  # filter(year >=2023) %>%
  select(-serial, -pernum) %>%
  mutate(hrhhid = as.character(hrhhid)) %>%
  rename(pulineno = lineno)

##################
### Data merge ###
##################

merged_data <- inner_join(cps_monthly_files, data) # merge ipums with census using hhrid hrhhid2 and pulineno (lineno)
merged_data <- inner_join(merged_data, telework_microdata_CPS) # merge census to telework using year month serial pernum

#########################
### Describe the data ###
#########################

table(merged_data$year, merged_data$month) # confirm that everything is March, 2023

merged_data %>%
  group_by(year, month) %>%
  summarise(workers = sum(earnwt),
            workers_diffany = sum(earnwt[diffany == 2])/workers,
            workers_diffcare = sum(earnwt[diffcare == 2])/workers,
            workers_diffhear = sum(earnwt[diffhear == 2])/workers,
            workers_diffeye = sum(earnwt[diffeye == 2])/workers,
            workers_diffrem = sum(earnwt[diffrem == 2])/workers,
            workers_diffphys = sum(earnwt[diffphys == 2])/workers,
            workers_diffmob = sum(earnwt[diffmob == 2])/workers
  )


