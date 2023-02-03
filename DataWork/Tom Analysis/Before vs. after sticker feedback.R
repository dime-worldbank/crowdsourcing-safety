# Tom Harris
# Before and after analysis

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

## Looks like the translated data isn't there anymore...?
# feedback_trans_data <- readRDS(file.path(
#   rider_feedback_dir,
#   "FinalData",
#   "rider_feedback_translated.Rds"
# ))


### Grouping the feedback data

#### Simple Average method (I'm not sure about this...) ####

# Load sample data

# First, converting the safety and speed factor variables into numeric variables
# which can be gruoped and averaged by regno_clean

feedback_data$safety_numeric <- feedback_data$safety_label_en

feedback_data$safety_numeric <-
  mapvalues(
    feedback_data$safety_numeric,
    from = c("Not Very Safe",
             "Not Safe",
             "Safe",
             "Very Safe"),
    #I'm not sure here if 'not very safe' is better than 'not safe'?
    to = c(4, 3, 2, 1)
  )

feedback_data$safety_numeric <-
  as.numeric(feedback_data$safety_numeric)

# converting speed feedback to numeric

feedback_data$speed_numeric <- feedback_data$speed_label_en

feedback_data$speed_numeric <-
  mapvalues(
    feedback_data$speed_numeric,
    from = c(
      "Very slow [0-10 km/h]",
      "Slow [10-30]",
      "Average [30-50]",
      "Fast [50-80]",
      "Very fast [80+]"
    ),
    to = c(1, 2, 3, 4, 5)
  )

feedback_data$speed_numeric <-
  as.numeric(feedback_data$speed_numeric)


# Generating variable for 'number of days since sticker installed'...

feedback_data$days_since_sticker <-
  difftime(feedback_data$completion_date,
           feedback_data$sticker_install_date,
           units = "days")
