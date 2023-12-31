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
load("CPS_telework.RData")

#########################
### Describe the data ###
#########################

merged_data <- merged_data %>%
  mutate(date = as.Date(paste0("1-",month,"-",year),tryFormats = c("%d-%m-%Y")))

merged_data %>%
  group_by(date) %>%
  summarise(count = n())
##########################
### Trends in the data ###
##########################

monthly_respondent_worforce <- merged_data %>%
  group_by(date) %>%
  summarise(workers = sum(wtfinl))

 P <- merged_data %>%
  mutate(telework_factor = if_else(telework_any==1 & ahrsworkt>telework_hours,"Some Telework",
                                   if_else(telework_any==1 & ahrsworkt==telework_hours,"All Telework", "No Telework"))) %>%
  group_by(date, telework_factor) %>%
  summarise(workers_diffany = sum(wtfinl[diffany == 2]),
            workers_diffcare = sum(wtfinl[diffcare == 2]),
            workers_diffhear = sum(wtfinl[diffhear == 2]),
            workers_diffeye = sum(wtfinl[diffeye == 2]),
            workers_diffrem = sum(wtfinl[diffrem == 2]),
            workers_diffphys = sum(wtfinl[diffphys == 2]),
            workers_diffmob = sum(wtfinl[diffmob == 2])
  ) %>%
  left_join(monthly_respondent_worforce) %>%
  mutate(workers_diffany = workers_diffany/workers) %>% 
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

 setwd(path_output)
png(filename = "Share of disabled workforce and telework.png",width =  850, height = 600)
plot(P)
dev.off()
 
###########################
### Regression Analysis ###
###########################

merged_data <- merged_data %>%
  mutate(telework_factor = if_else(telework_any==1 & ahrsworkt>telework_hours,"Some Telework",
                                   if_else(telework_any==1 & ahrsworkt==telework_hours,"All Telework", "No Telework")),
         fully_remote = as.integer(telework_factor == "All Telework"),
         hybrid_remote = as.integer(telework_factor == "Some Telework"),
         any_remote = as.integer(telework_any == 1),
         diffany_bin = as.integer(diffany==2)) %>%
  filter(wtfinl > 0) %>%
  filter(age>=22 & age<=64)


merged_data %>%
  group_by(date,diffany) %>%
  summarise(fully_remote_share = weighted.mean(fully_remote, w = wtfinl, rm.na = TRUE))

# Convert difficulty variables to binary format within the merged_data dataset
binary_difficulty_vars <- c("diffhear", "diffeye", "diffrem", "diffphys", "diffmob", "diffcare", "diffany")
for (var in binary_difficulty_vars) {
  merged_data[[paste0(var, "_bin")]] <- as.integer(merged_data[[var]] == 2)
}

# Define the weights object for the feols
wtfinl <- merged_data$wtfinl

# Define a custom function to run feols with given dependent variable
baseline_feols <- function(dependent_variable, independent_variable_bin) {
  feols(as.formula(paste(dependent_variable, "~", independent_variable_bin, "| occ2010 + ind1990")), 
        data = merged_data,
        se = "cluster",
        cluster = c("occ2010","ind1990"),
        weights = wtfinl)
}

ind_info_feols <- function(dependent_variable, independent_variable_bin) {
  feols(as.formula(paste(dependent_variable, "~", independent_variable_bin, "| occ2010 + ind1990 + race + sex + marst + educ + nchild + age")), 
        data = merged_data,
        se = "cluster",
        cluster = c("occ2010","ind1990"),
        weights = wtfinl)
}

# Initialize lists to store models
any_baseline_models <- list()
any_indinfo_models <- list()
fully_baseline_models <- list()
fully_indinfo_models <- list()
hybrid_baseline_models <- list()
hybrid_indinfo_models <- list()

# Loop through each binary difficulty variable and run regressions
for (diff_var in binary_difficulty_vars) {
  independent_var_bin <- paste0(diff_var, "_bin")
  any_baseline_models[[diff_var]] <- baseline_feols("any_remote", independent_var_bin)
  any_indinfo_models[[diff_var]] <- ind_info_feols("any_remote", independent_var_bin)
  
  fully_baseline_models[[diff_var]] <- baseline_feols("fully_remote", independent_var_bin)
  fully_indinfo_models[[diff_var]] <- ind_info_feols("fully_remote", independent_var_bin)
  
  hybrid_baseline_models[[diff_var]] <- baseline_feols("hybrid_remote", independent_var_bin)
  hybrid_indinfo_models[[diff_var]] <- ind_info_feols("hybrid_remote", independent_var_bin)
}

# Use etable to display results from the first variable as an example
result_table <- etable(any_baseline_models[[binary_difficulty_vars[7]]], any_indinfo_models[[binary_difficulty_vars[7]]],
       fully_baseline_models[[binary_difficulty_vars[7]]], fully_indinfo_models[[binary_difficulty_vars[7]]],
       hybrid_baseline_models[[binary_difficulty_vars[7]]], hybrid_indinfo_models[[binary_difficulty_vars[7]]],
       cluster = c("occ2010","ind1990"), 
       headers = c("Any - Baseline","Any - Extended",
                   "Fully - Baseline","Fully - Extended",
                   "Hybrid - Baseline","Hybrid - Extended" 
                   ))


###########################
### Visual     Analysis ###
###########################
setwd(path_output)

# Step 1: Extract Coefficients and Confidence Intervals
outcome_list <- c("Deaf/Diff. Hearing", "Blind /Diff. Seeing", 
                  "Cognitive Diff.", "Diff. Walking/Stairs", 
                  "Diff. Activities Outside the Home", "Diff. Personal Needs", 
                  "Any Difficulty")


plot_data <- list()

for(i in seq_along(any_baseline_models)){
  plot_data[[i]] <- bind_rows(as.data.frame(any_baseline_models[[i]]$coeftable), as.data.frame(any_indinfo_models[[i]]$coeftable),
                              as.data.frame(fully_baseline_models[[i]]$coeftable), as.data.frame(fully_indinfo_models[[i]]$coeftable),
                              as.data.frame(hybrid_baseline_models[[i]]$coeftable), as.data.frame(hybrid_indinfo_models[[i]]$coeftable))
  plot_data[[i]]$model <- c("Any - Baseline","Any - Extended",
                            "Fully - Baseline","Fully - Extended",
                            "Hybrid - Baseline","Hybrid - Extended" 
  )
  plot_data[[i]]$reg <- c("Baseline","Extended",
                            "Baseline","Extended",
                            "Baseline","Extended" 
  )
  plot_data[[i]]$type <- c("Any Remote","Any Remote",
                            "Fully Remote","Fully Remote",
                            "Hybrid Remote","Hybrid Remote" 
  )
  plot_data[[i]]$dependent <- c(outcome_list[[i]],outcome_list[[i]],outcome_list[[i]],
                                outcome_list[[i]],outcome_list[[i]],outcome_list[[i]])
  
}

all_results <- bind_rows(plot_data)

all_results$dependent <- factor(all_results$dependent,
                                levels = c("Deaf/Diff. Hearing", "Blind /Diff. Seeing", 
                                           "Cognitive Diff.", "Diff. Walking/Stairs", 
                                           "Diff. Activities Outside the Home", "Diff. Personal Needs", 
                                           "Any Difficulty"))

all_results <- all_results %>%
  filter(type != "Any Remote") %>%
  arrange(dependent,model) %>%
  mutate(row_num = row_number(),
         conf.low = Estimate - 1.96*`Std. Error`,
         conf.high = Estimate + 1.96*`Std. Error`) 

# startpoints <- c(1,7,14,21,28,35,42)
midpoints <- tapply(all_results$row_num, all_results$dependent, 
                    function(x) mean(range(x)))
ticktext <- unique(all_results$dependent)

P <- plot_ly(all_results, 
             x = ~row_num, 
             y = ~Estimate, 
             text = ~paste("Model Type:", model, 
                           "<br>Dependent Variable Type:", dependent,
                           "<br>Effect Size:", round(Estimate,3),
                           "<br>t-value:", round(`t value`,3)),  # Custom hover text
             hoverinfo = 'text',  # Display only the custom text
             error_y = ~list(array = conf.high - Estimate,
                             arrayminus = Estimate - conf.low),
             type = 'scatter',
             mode = 'markers',
             symbol = ~reg,
             color = ~type,
             marker = list(size = 10)
             ) %>%
  layout(title = "The effect of Disabilities on Propensity to Telework",
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
    ),
    list(
      type = 'line',
      line = list(
        color = 'grey',
        width = 2,
        dash = 'dash'
      ),
      x0 = 12.5,
      x1 = 12.5,
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
      x0 = 16.5,
      x1 = 16.5,
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
      x0 = 20.5,
      x1 = 20.5,
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
      x0 = 24.5,
      x1 = 24.5,
      y0 = min(all_results$conf.low),  # Replace with the minimum y-value you want the line to start at
      y1 = max(all_results$conf.high)  # Replace with the maximum y-value you want the line to end at
    )
  )) 
P

htmlwidgets::saveWidget(
  widget = P, #the plotly object
  file = "Dis. Effect by Type.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)
