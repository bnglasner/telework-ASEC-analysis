# Economic Innovation Group
# Telework Analysis
# 11/2/2023

##########################
#         Set Up         #  
##########################

library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)
library(ipumsr)
library(fixest)
# https://cran.r-project.org/web/packages/fixest/vignettes/fixest_walkthrough.html#13_Other_standard-errors

#################
### Set paths ###
#################

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


#################
### Data Load ###
#################
load("CPS_telework_ASEC.RData")

#########################
### Describe the data ###
#########################

merged_data <- merged_data %>%
  mutate(date = as.Date(paste0("1-",month,"-",year),tryFormats = c("%d-%m-%Y")),
         moved = as.integer(migrate1 %in% c(2:6)),
         moved_in_county = as.integer(migrate1 == 3),
         moved_out_county = as.integer(migrate1 %in% c(4:6)))

merged_data %>%
  group_by(date) %>%
  summarise(count = n())
##########################
### Trends in the data ###
##########################
telework_hh <- merged_data %>%
  mutate(full_remote = as.integer(telework_any==1 & ahrsworkt==telework_hours),
         hybrid_remote = as.integer(telework_any==1 & ahrsworkt>telework_hours),
         any_remote = as.integer(telework_any==1),
         remote_hours = if_else(telework_hours>0, telework_hours, 0)) %>%
  group_by(serial) %>%
  summarize(household_work_hours = sum(ahrsworkt),
            household_remote_hours = sum(remote_hours),
            num_full_remote = sum(full_remote),
            num_hybrid = sum(hybrid_remote),
            full_remote = mean(full_remote),
            any_remote = mean(any_remote),
            num_workers = n(),
            share_hybrid = num_hybrid/num_workers,
            share_full = num_full_remote/num_workers,
            share_remote_hours = household_remote_hours/household_work_hours) %>%
  mutate(household_remote_type = case_when(
    full_remote == 1  ~ "Fully Remote Household",
    any_remote >0 & full_remote < 1 ~ "Partially Remote Household",
    any_remote ==0  ~ "Not a Remote Household",
    TRUE ~ "Missing"
  )) %>% 
  select(serial,household_remote_type,
         num_workers,num_hybrid,num_full_remote,
         share_hybrid,share_full,share_remote_hours)

merged_data %>%
  left_join(telework_hh) %>%
  group_by(household_remote_type) %>%
  summarise(moved_count = sum(moved),
    moved = weighted.mean(moved,w = asecwt, na.rm = TRUE))  
 
###########################
### Regression Analysis ###
###########################
analysis_data <- merged_data %>%
  left_join(telework_hh) %>%
  filter(asecwt > 0) %>%
  filter(age >= 22 & age <= 64) %>%
  mutate(telework_factor = if_else(telework_any==1 & ahrsworkt>telework_hours,"Some Telework",
                                   if_else(telework_any==1 & ahrsworkt==telework_hours,"All Telework", "No Telework")))

analysis_data %>%
  group_by(telework_factor) %>%
  summarise(count = n())

analysis_data %>%
  group_by(age) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = age,
             y = count,
             size = count)) + 
  geom_point() + 
  theme_bw()

analysis_data %>%
  # group_by(moved) %>%
  # group_by(moved_in_county) %>%
  # group_by(moved_out_county) %>%
  summarise(
    # count = n(),
    avg = weighted.mean(moved_out_county, asecwt))

# analysis_data <- merged_data %>%
#   left_join(telework_hh) %>%
#   filter(asecwth > 0) %>%
# mutate(telework_factor = if_else(telework_any==1 & ahrsworkt>telework_hours,"Some Telework",
# if_else(telework_any==1 & ahrsworkt==telework_hours,"All Telework", "No Telework"))) %>%
#   arrange(serial,pernum) %>%
#   group_by(serial) %>%
#   slice(1)

analysis_data$household_remote_type <- factor(analysis_data$household_remote_type, levels = c("Not a Remote Household","Partially Remote Household","Fully Remote Household"))
analysis_data$telework_factor <- factor(analysis_data$telework_factor, levels = c("No Telework","Some Telework","All Telework"))

# Convert moving variables to binary format within the analysis_data dataset

# binary_moving_vars <- c("household_remote_type + num_workers")
binary_moving_vars <- c("telework_factor + num_workers")

# Define the weights object for the feols
# asecwth <- analysis_data$asecwth
asecwth <- analysis_data$asecwt

# Define a custom function to run feols with given dependent variable
baseline_feols <- function(dependent_variable, independent_variable_bin) {
  feols(as.formula(paste(dependent_variable, "~", independent_variable_bin, "| occ2010 + ind1990")), 
        data = analysis_data,
        se = "cluster",
        cluster = c("serial"),
        weights = asecwth[asecwth > 0])
}

ind_info_feols <- function(dependent_variable, independent_variable_bin) {
  feols(as.formula(paste(dependent_variable, "~", independent_variable_bin, "| occ2010 + ind1990 + race + sex + marst + educ + nchild + age")), 
        data = analysis_data,
        se = "cluster",
        cluster = c("serial"),
        weights = asecwth[asecwth > 0])
}

# Initialize lists to store models
any_baseline_models <- list()
any_indinfo_models <- list()
in_county_baseline_models <- list()
in_county_indinfo_models <- list()
out_county_baseline_models <- list()
out_county_indinfo_models <- list()

# Loop through each binary moving variable and run regressions
for (diff_var in binary_moving_vars) {
  independent_var_bin <- paste0(diff_var)
  any_baseline_models[[diff_var]] <- baseline_feols("moved", independent_var_bin)
  any_indinfo_models[[diff_var]] <- ind_info_feols("moved", independent_var_bin)
  
  in_county_baseline_models[[diff_var]] <- baseline_feols("moved_in_county", independent_var_bin)
  in_county_indinfo_models[[diff_var]] <- ind_info_feols("moved_in_county", independent_var_bin)
  
  out_county_baseline_models[[diff_var]] <- baseline_feols("moved_out_county", independent_var_bin)
  out_county_indinfo_models[[diff_var]] <- ind_info_feols("moved_out_county", independent_var_bin)
}

# Use etable to display results from the first variable as an example
result_table <- etable(any_baseline_models[[binary_moving_vars[1]]], any_indinfo_models[[binary_moving_vars[1]]],
       in_county_baseline_models[[binary_moving_vars[1]]], in_county_indinfo_models[[binary_moving_vars[1]]],
       out_county_baseline_models[[binary_moving_vars[1]]], out_county_indinfo_models[[binary_moving_vars[1]]],
       # se = "cluster",
       # cluster = c("serial"),
       headers = c("Moved - Baseline","Moved - Extended",
                   "Within County - Baseline","Within County - Extended",
                   "Out of County - Baseline","Out of County - Extended" 
                   ),
       tex = FALSE
       )

###########################
### Visual     Analysis ###
###########################
setwd(path_output)

# Step 1: Extract Coefficients and Confidence Intervals
all_results <- bind_rows(as.data.frame(any_baseline_models[[1]]$coeftable[1:2,]), as.data.frame(any_indinfo_models[[1]]$coeftable[1:2,]),
                         as.data.frame(in_county_baseline_models[[1]]$coeftable[1:2,]), as.data.frame(in_county_indinfo_models[[1]]$coeftable[1:2,]),
                         as.data.frame(out_county_baseline_models[[1]]$coeftable[1:2,]), as.data.frame(out_county_indinfo_models[[1]]$coeftable[1:2,]))




all_results$household_type <- c("Partially Remote","Fully Remote",
                                "Partially Remote","Fully Remote",
                                "Partially Remote","Fully Remote",
                                "Partially Remote","Fully Remote",
                                "Partially Remote","Fully Remote",
                                "Partially Remote","Fully Remote"
                                )
  
all_results$model <- c("Moved - Baseline","Moved - Baseline",
                       "Moved - Extended","Moved - Extended",
                       "Within County - Baseline","Within County - Baseline",
                       "Within County - Extended","Within County - Extended",
                       "Out of County - Baseline","Out of County - Baseline",
                       "Out of County - Extended","Out of County - Extended"
                       )
all_results$reg <- c("Baseline","Baseline",
                     "Extended","Extended",
                     "Baseline","Baseline",
                     "Extended","Extended",
                     "Baseline","Baseline",
                     "Extended","Extended"
  )
all_results$type <- c("Any Move","Any Move",
                      "Any Move","Any Move",
                      "Within County","Within County",
                      "Within County","Within County",
                      "Out of County","Out of County",
                      "Out of County","Out of County"  
  )

all_results <- all_results %>%
  # filter(type != "Any") %>%
  arrange(type,household_type) %>%
  mutate(row_num = row_number(),
         conf.low = Estimate - 1.96*`Std. Error`,
         conf.high = Estimate + 1.96*`Std. Error`) 

# startpoints <- c(1,5,9)
midpoints <- tapply(all_results$row_num, all_results$type, 
                    function(x) mean(range(x)))
ticktext <- unique(all_results$type)

P <- plot_ly(all_results, 
             x = ~row_num, 
             y = ~Estimate, 
             text = ~paste("Model Type:", model, 
                           "<br>Household Type:", household_type,
                           "<br>Effect Size:", round(Estimate,3),
                           "<br>t-value:", round(`t value`,3)),  # Custom hover text
             hoverinfo = 'text',  # Display only the custom text
             error_y = ~list(array = conf.high - Estimate,
                             arrayminus = Estimate - conf.low),
             type = 'scatter',
             mode = 'markers',
             symbol = ~reg,
             color = ~household_type,
             marker = list(size = 10)) %>%
  layout(title = "The effect of Remote Work on Propensity to Move",
         xaxis = list(title = "",
                      tickmode = "array",
                      tickvals = midpoints,
                      ticktext = ticktext),
         yaxis = list(title = "Coefficient Estimate"),
         legend = list(title = list(text = 'Dependent Variable'))
  ) 

P <- P %>%
  layout(shapes = list(
    list(
      type = 'line',
      line = list(
        color = 'grey',
        width = 2,
        dash = 'dash'
      ),
      x0 = 4.5,
      x1 = 4.5,
      y0 = min(all_results$conf.low),  # Replace with the minimum y-value you want the line to start at
      y1 = max(all_results$conf.high)  # Replace with the maximum y-value you want the line to end at
    ),
    list(
      type = 'line',
      line = list(
        color = 'grey',
        width = 2,
        dash = 'dash'
      ),
      x0 = 8.5,
      x1 = 8.5,
      y0 = min(all_results$conf.low),  # Replace with the minimum y-value you want the line to start at
      y1 = max(all_results$conf.high)  # Replace with the maximum y-value you want the line to end at
    )
  )) 
P

htmlwidgets::saveWidget(
  widget = P, #the plotly object
  file = "Remote Work Effect by Moves.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)

