# Tom Harris
# Practice script

# n.b. Before any new scripts, run psv_feedback_master.R

# Installing packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyr,
               dplyr, plyr,
               wesanderson,
               plotly,
               ggplot2,
               data.table)

# Loading data
sensor_data <-
  readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

sensor_data$sacco <- sensor_data$sacco.x

feedback_data <-
  readRDS(file.path(rider_feedback_dir, "FinalData", "rider_feedback.Rds"))

feedback_trans_data <- readRDS(file.path(
  rider_feedback_dir,
  "FinalData",
  "rider_feedback_translated.Rds"
))


### Grouping the feedback data

#### Simple Average method (I'm not sure about this...) ####

# Load sample data

# First, converting the safety and speed factor variables into numeric variables
# which can be gruoped and averaged by regno_clean

feedback_data$safety_numeric <- feedback_data$safety_label_en

feedback_data$safety_numeric <-
  mapvalues(
    feedback_data$safety_numeric,
    from = c("Not Very Safe", "Not Safe", "Safe", "Very Safe"),
    to = c(4, 3, 2, 1)
  )

# converting speed feedback to numeric

feedback_data$speed_numeric <- feedback_data$speed_label_en

feedback_data$speed_numeric <-
  mapvalues(
    feedback_data$speed_numeric,
    from = c(
      "Very slow [0 - 10 km/h]",
      "Slow [10 - 30]",
      "Average [30 - 50]",
      "Fast [50 - 80]",
      "Very fast [80k]"
    ),
    to = c(1, 2, 3, 4, 5)
  )


# Group the data by the "regno_clean" id variable
grouped_data <- sensor_data %>%
  group_by(regno_clean)

# Need a variable which measures number of violations per km, otherwise
# biased by buses which drive longer routes.
grouped_data$over_80_by_km <-
  grouped_data$N_speed_over_80 / grouped_data$distance_km

# ### Specify which columns we want to take weighted average of:
variablesToAvg <-
  names(grouped_data)[c(1, 2, 10:72, 75, 77, 78, 80)] ## you may want to tweak which variables you average

### set as data table
grouped_data <-
  data.table(grouped_data)

# getting rid of rows where N_speed_over_0 is 'na' or zero as this doesn't make sense..
grouped_data <- grouped_data %>%
  filter(!is.na(N_speed_over_0) & N_speed_over_0 != 0)


### collapse to FB postcode level using weighted means
final_data <-
  grouped_data[,
               lapply(.SD, mean), ## applies weighted mean function with weights w over specified columns
               by = list(regno_clean),
               .SDcols = variablesToAvg] ## specifies columns to average

final_data$sacco <-
  grouped_data$sacco[match(final_data$regno_clean, grouped_data$regno_clean)]
