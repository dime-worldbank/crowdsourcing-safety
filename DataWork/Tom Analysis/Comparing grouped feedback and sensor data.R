# Tom Harris
# Comparing grouped feedback and sensor data

# n.b. Before any new scripts, run psv_feedback_master.R

# Installing packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyr,
  dplyr, plyr,
  wesanderson,
  plotly,
  ggplot2,
  data.table
)

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
    from = c(
      "Not Very Safe",
      "Not Safe",
      "Safe",
      "Very Safe"
    ),
    # I'm not sure here if 'not very safe' is better than 'not safe'?
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


# Group the data by the "regno_clean" id variable
grouped_data <- feedback_data %>%
  group_by(regno_clean)

# # Need a variable which measures number of violations per km, otherwise
# # biased by buses which drive longer routes.
# grouped_data$over_80_by_km <-
#   grouped_data$N_speed_over_80 / grouped_data$distance_km

# ### Specify which columns we want to take weighted average of:
variablesToAvg <-
  names(grouped_data)[c(25, 26, 27, 29, 30, 32, 33)] ## you may want to tweak which variables you average

### set as data table
grouped_data <-
  data.table(grouped_data)

# getting rid of rows where N_speed_over_0 is 'na' or zero as this doesn't make sense..
grouped_data <- grouped_data %>%
  filter(!is.na(safety_numeric) & !is.na(speed_numeric))


### collapse to bus using weighted means
final_data <-
  grouped_data[,
    lapply(.SD, mean), ## applies weighted mean function with weights w over specified columns
    by = list(regno_clean),
    .SDcols = variablesToAvg
  ] ## specifies columns to average

final_data$sacco <-
  grouped_data$matatu_sacco_stk_inst_srvy[match(final_data$regno_clean, grouped_data$regno_clean)]

feedback_grouped <- final_data


# Q. does safety rating correlate with speed rating?

# plot of number of 80kmh speed violations per km by Sacco
plot_1 <- ggplot(feedback_grouped) +
  aes(
    x = feedback_grouped$safety_numeric,
    y = feedback_grouped$speed_numeric,
    color = sacco
  ) +
  geom_point(
    shape = "circle",
    size = 3,
    alpha = 0.6
  ) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Safety Rating (1 = Very safe, 4 = Very unsafe)",
    y = "Speed Rating (1 = Very slow, 5 = Very fast",
    title = "Comparing Rider Feedback on Speed vs. Safety",
    subtitle = " "
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = "Sacco Company") +
  xlim(2.5, 4) +
  ylim(1.5, 5)

plot_1

# interactive version
ggplotly(plot_1)



#### Now bringing in sensor data ####



#### Simple Average method (I'm not sure about this...) ####

# Load sample data

# Group the data by the "regno_clean" id variable
grouped_data <- sensor_data %>%
  group_by(regno_clean)

#### note to self
# readings not taken per x seconds - based on change in speed --- so divide by journey time
# Need a variable which measures number of violations per km, otherwise
# biased by buses which drive longer routes.
grouped_data$over_80_by_km <-
  grouped_data$N_speed_over_80 / grouped_data$distance_km

# Specify which columns we want to take weighted average of:
variablesToAvg <-
  names(grouped_data)[c(1, 2, 9:64, 73)] ## you may want to tweak which variables you average

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
    .SDcols = variablesToAvg
  ] ## specifies columns to average

# won't work as sacco variable no longer there...
# final_data$sacco <-
#   grouped_data$sacco[match(final_data$regno_clean, grouped_data$regno_clean)]

sensor_grouped <- final_data


## joining grouped data sets by regno_clean

joined_data <- left_join(sensor_grouped, feedback_grouped)


# plot of sensor vs. feedback data
# note to self
# focus on proportion of responses that are fast or very fast instead of average of 1 to 5


plot_1 <- ggplot(joined_data) +
  aes(
    x = joined_data$over_80_by_km,
    y = joined_data$speed_numeric,
    color = sacco
  ) +
  geom_point(
    shape = "circle",
    size = 3,
    alpha = 0.6
  ) +
  scale_color_hue(direction = 1) +
  labs(
    x = ">80km/h violations per km",
    y = "Speed Rating (1 = Very slow, 5 = Very fast",
    title = "Comparing Rider Feedback on Speed vs. Safety",
    subtitle = " "
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = "Sacco Company") +
  xlim(2.5, 4) +
  ylim(1.5, 5)

plot_1

# interactive version
ggplotly(plot_1)
