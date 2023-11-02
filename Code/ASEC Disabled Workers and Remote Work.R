# Economic Innovation Group
# Telework Analysis
# 11/2/2023

##########################
#         Set Up         #  
##########################

library(dplyr)
library(readxl)
library(ggplot2)
library(fixest)
# https://cran.r-project.org/web/packages/fixest/vignettes/fixest_walkthrough.html#13_Other_standard-errors

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
load("CPS_telework.RData")

#########################
### Describe the data ###
#########################

table(merged_data$year, merged_data$month) # confirm that everything is March, 2023


##########################
### Trends in the data ###
##########################

monthly_respondent_worforce <- merged_data %>%
  group_by(year, month) %>%
  summarise(workers = sum(earnwt))

merged_data %>%
  mutate(telework_factor = if_else(telework_any==1 & ahrsworkt>telework_hours,"Some Telework",
                                   if_else(telework_any==1 & ahrsworkt==telework_hours,"All Telework", "No Telework"))) %>%
  group_by(year, month, telework_factor) %>%
  summarise(workers_diffany = sum(earnwt[diffany == 2]),
            workers_diffcare = sum(earnwt[diffcare == 2]),
            workers_diffhear = sum(earnwt[diffhear == 2]),
            workers_diffeye = sum(earnwt[diffeye == 2]),
            workers_diffrem = sum(earnwt[diffrem == 2]),
            workers_diffphys = sum(earnwt[diffphys == 2]),
            workers_diffmob = sum(earnwt[diffmob == 2])
  ) %>%
  left_join(monthly_respondent_worforce) %>%
  mutate(date = as.Date(paste0("1-",month,"-",year),tryFormats = c("%d-%m-%Y")),
         workers_diffany = workers_diffany/workers) %>% 
  ggplot(aes(group = telework_factor,
             color = telework_factor,
             x = date,
             y = workers_diffany)) +
  geom_line() + 
  geom_point() + 
  ylab("Percent of the Respondent Workforce") +
  xlab("Date") + 
  ggtitle("Workers with Disabilities") + 
  scale_y_continuous(limits = c(0,0.045),
                     labels = scales::percent_format()) + 
  theme_bw() +
  guides(color = guide_legend(title = ""))


##########################
### Regression         ###
##########################
merged_data <- merged_data %>%
  mutate(date = as.Date(paste0("1-",month,"-",year),tryFormats = c("%d-%m-%Y")),
         telework_factor = if_else(telework_any==1 & ahrsworkt>telework_hours,"Some Telework",
                                   if_else(telework_any==1 & ahrsworkt==telework_hours,"All Telework", "No Telework")),
         fully_remote = as.integer(telework_factor == "All Telework"),
         hybrid_remote = as.integer(telework_factor == "Some Telework"),
         any_remote = as.integer(telework_any == 1),
         diffany_bin = as.integer(diffany==2))


fully_remote = feols(fully_remote ~ diffany_bin | occ + ind, subset(merged_data,earnwt>0))
hybrid_remote = feols(hybrid_remote ~ diffany_bin | occ + ind, subset(merged_data,earnwt>0))
any_remote = feols(any_remote ~ diffany_bin | occ + ind, subset(merged_data,earnwt>0))


etable(fully_remote, hybrid_remote, any_remote,
       vcov = "twoway", headers = c("Fully Remote", "Hybrid Remote", "Any Remote"))
