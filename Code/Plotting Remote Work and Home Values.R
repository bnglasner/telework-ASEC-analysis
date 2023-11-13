rm(list = ls())

library(tidyverse)
library(plotly)


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



df = read_csv("metro_housing_and_telework_rf.csv")


df$fully_remote_condit = round(100*df$fully_remote_2022/df$any_remote_2022, 2)
df$pop_share_2022 = round(df$pop_share_2022, 2)

  
df = df %>%
  mutate(`Median Home Value (2019)` = paste("$", median_home_value_2019, sep = ""),
         `Fully Remote Share of All Remote Workers` = fully_remote_condit,
         `Population Share` = pop_share_2022) 






# Best fit line
fit = lm(fully_remote_condit ~ median_home_value_2019, data = df, weights = pop_share_2022)
fit_df = data.frame(median_home_value_2019 = df$median_home_value_2019, 
                    fitted_values = predict(fit, newdata = df), 
                    NAME = df$NAME, 
                    fully_remote_condit = df$fully_remote_condit,
                    pop_share_2022 = df$pop_share_2022
                    )%>%
  mutate(`Median Home Value (2019)` = paste("$", median_home_value_2019, sep = ""),
         `Fully Remote Share of All Remote Workers` = fully_remote_condit,
         `Population Share` = pop_share_2022) 


fit_df = fit_df %>% arrange(median_home_value_2019)



p = plot_ly(
  data = df,
  x = ~median_home_value_2019/1000, 
  y = ~fully_remote_condit,
  type = "scatter",
  mode = "markers",
  marker = list(
    color = "white", 
    size = ~1500*pop_share_2022),
  text = ~paste(
    "MSA:", NAME,
    "<br>Fully Remote Share of All Remote Workers (%):", fully_remote_condit,
    "<br>Median Home Value (2019):", `Median Home Value (2019)`,
    sep = " "
  ),
  hoverinfo = "text"
) %>%
  add_lines(
  x = ~median_home_value_2019/1000, 
  y = ~fitted_values, 
  line = list(color = "firebrick", dash = "dash"), 
  hoverinfo = "none",
  data = fit_df,
  inherit = F) %>%
  layout(xaxis = list(title = "Median Home Value 2019 (Thousands)"),
         yaxis = list(title = "Fully Remote Share of All Remote Workers (%)"),
         title = "Median Home Value vs Remote Share",
         showlegend = F) 

p

# Save the graph
setwd(path_output)

htmlwidgets::saveWidget(
  widget = p, #the plotly object
  file = "Median Home Values vs Remote Work.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)


