rm(list = ls())

library(tidyverse)
library(janitor)
library(haven)
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




# Scatter of fully vs hybrid by occupation
setwd(path_data)

load("CPS_telework.RData")
table(merged_data$year, merged_data$month)



# Create fulltime and hybrid remote variables
merged_data = merged_data %>%
  mutate(
    telework_factor = if_else(telework_any==1 & ahrsworkt>telework_hours,"Some Telework",
                              if_else(telework_any==1 & ahrsworkt==telework_hours,"All Telework", "No Telework")),
    fully_remote = as.integer(telework_factor == "All Telework"),
    hybrid_remote = as.integer(telework_factor == "Some Telework"),
    any_remote = fully_remote + hybrid_remote
  ) 



# Restrict to people between 22 and 64
df = merged_data %>%
  filter(age >= 22 & age <= 64)


df_occ = df %>%
  group_by(occ2010) %>%
  summarise(n_fully_remote = sum(fully_remote*wtfinl),
            n_hybrid_remote = sum(hybrid_remote*wtfinl),
            n_workers = sum(wtfinl)) %>%
  mutate(fully_remote_share = 100*n_fully_remote/n_workers,
         hybrid_remote_share = 100*n_hybrid_remote/n_workers,
         emp_share = n_workers/sum(n_workers)) 

occ2010_labs = read_csv("occ2010_labels.csv") %>%
  select(
    occ2010 = Code,
    occ2010_label = Label) %>%
  filter(!is.na(occ2010),
         occ2010 != "Code") %>%
  mutate(occ2010 = as.integer(occ2010))


df_occ = left_join(df_occ, occ2010_labs, by = "occ2010") %>%
  select(occ2010,
         occ2010_label,
         fully_remote_share,
         hybrid_remote_share,
         emp_share)

# Look at high fully remote and low hybrid
df_occ %>%
  arrange(desc(fully_remote_share),
          hybrid_remote_share) %>%
  View()

# Look at high hybrid remote and low fully
df_occ %>%
  mutate(hybrid_diff = fully_remote_share - hybrid_remote_share) %>%
  arrange(desc(hybrid_diff)) %>%
  View()



p2 = plot_ly(
  data = df_occ,
  x = ~fully_remote_share, 
  y = ~hybrid_remote_share,
  type = "scatter",
  mode = "markers",
  marker = list(
    color = "white", 
    size = ~1500*emp_share),
  text = ~paste(
    "Occupation:", occ2010_label,
    "<br>Fully Remote Share (%):", round(fully_remote_share, 1),
    "<br>Hybrid Remote Share (%):", round(hybrid_remote_share, 1),
    sep = " "
  ),
  hoverinfo = "text"
) %>%
  add_lines(
    data = df_occ,
    x = ~fully_remote_share,
    y = ~fully_remote_share,
    line = list(color = "firebrick", dash = "dash"),
    hoverinfo = "none",
    inherit = F
    ) %>%
  layout(xaxis = list(title = "Fully Remote Share (%)"),
         yaxis = list(title = "Hybrid Remote Share (%)"),
         title = "Fully vs Hybrid Remote Shares",
         showlegend = F) 



p2



# Save the graph
setwd(path_output)

htmlwidgets::saveWidget(
  widget = p2, #the plotly object
  file = "Fully vs Hybrid Remote Scatter.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)

