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
library(ggplot2)

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
  select(serial, pernum, year, month, hrhhid ,hrhhid2, pulineno ) %>%
  na.omit() %>%
  mutate(hrhhid = as.character(as.numeric(hrhhid)))

ddi <- read_ipums_ddi("cps_00027.xml")
data <- read_ipums_micro(ddi)

data <- data %>% 
  clean_names() %>%
  filter(asecflag == 2 | is.na(asecflag)) %>% 
  select(-serial, -pernum) %>%
  mutate(hrhhid = as.character(hrhhid)) %>%
  rename(pulineno = lineno) 

##################
### Data merge ###
##################
# 
# eeptools::isid(data, vars = c("year", "month", "hrhhid", "hrhhid2", "pulineno"))
# eeptools::isid(cps_monthly_files, vars = c("year", "month", "hrhhid", "hrhhid2", "pulineno"))
# eeptools::isid(data, vars = c("serial", "pernum", "year", "month"))

merged_data <- inner_join(cps_monthly_files, data) # merge ipums with census using hhrid hrhhid2 and pulineno (lineno)
merged_data <- inner_join(merged_data, telework_microdata_CPS) # merge ipums with census using hhrid hrhhid2 and pulineno (lineno)

##################
### Save Data  ###
##################
save(merged_data, file = "CPS_telework.RData")
write.csv(merged_data, file = "CPS_telework.csv", row.names = FALSE)
