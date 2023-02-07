# Tom Harris
# Event study analysis 1

# n.b. Before any new scripts, run psv_feedback_master.R

# Installing packages
# Install and load packages ---------------
packages <- c(
  "tidyverse",
  "haven",
  "tidyr",
  "dplyr",
  "plyr",
  "ggpmisc",
  "wesanderson",
  "plotly",
  "ggplot2",
  "data.table"
)

# Change to install = TRUE to install the required packages
pacman::p_load(packages, character.only = TRUE, install = FALSE)


# Loading data
sensor_data <-
  readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))


# Creating data for analysis

# Creating data for plot
sensor_data_clean <- sensor_data %>%
  
  # Only consider vehicles with sensor installed
  dplyr::filter(sticker_installed %in% T) %>%
  
  # Days since installation
  dplyr::mutate(days_since_sticker = as.numeric(date - sticker_install_date)) %>%
  
  # Only look 30 days before/after installed
  dplyr::filter(abs(days_since_sticker) <= 50)


# Need a variable which measures number of speed violations per km, otherwise
# biased by buses which drive longer routes.
sensor_data_clean$over_80_by_km <-
  sensor_data_clean$N_speed_over_80 / sensor_data_clean$distance_km

# Generating variable to capture all g-force related violations 
# should this also be 'per km'?
sensor_data_clean$total_g_violations <-
  sensor_data_clean$N_violation_acceleration +
  sensor_data_clean$N_violation_brake +
  sensor_data_clean$N_violation_turn

















