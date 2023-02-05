# Tom Harris
# Practice script
# Need to re-do this because underlying data has changed and column index is diff...!

# n.b. Before any new scripts, run psv_feedback_master.R

# Installing packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyr,
               dplyr,
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

### Notes to self...
## common id is probs regno_clean

## first step, aggregate everything to the vehicle level
#how safe does sensor data say vehicle, how safe does feedback data say it is.... do these corr?
# how do we define 'safe' . e,g, avg speed, make other indicators .... proportion of time spent over 80 kmh etc.
# eventually some kidn of index of dangerous driving ....

#### Weighted Average method (I'm not sure about this...) ####
#
# # Group the data by the "gear" variable
# grouped_data <- sensor_data %>%
#   group_by(regno_clean)
#
#
# ### Specify which columns we want to take weighted average of:
# variablesToAvg <-
#   names(grouped_data)[c(1, 2, 10:50)] ## you may want to tweak which variables you average
#
# ### set as data table
# grouped_data <-
#   data.table(grouped_data)
#
# grouped_data <- grouped_data %>%
#   filter(!is.na(N_speed_over_0) & N_speed_over_0 != 0)
#
# ### collapse to FB postcode level using weighted means
#
# final_data <-
#   grouped_data[,
#                lapply(.SD, weighted.mean, w = distance_km), ## applies weighted mean function with weights w over specified columns
#                by = list(regno_clean),
#                .SDcols = variablesToAvg] ## specifies columns to average
#
# final_data$sacco <-
#   grouped_data$sacco[match(final_data$regno_clean, grouped_data$regno_clean)]
#
# final_data$over_80_by_km <-
#   final_data$N_speed_over_80 / final_data$distance_km
#
# # plot
# plot_1 <- ggplot(final_data) +
#   aes(x = distance_km, y = over_80_by_km, colour = sacco) +
#   geom_point(shape = "circle",
#              size = 3,
#              alpha = 0.6) +
#   scale_color_hue(direction = 1) +
#   labs(
#     x = "Average Journey Distance (km)",
#     y = "Count of >80km/h violations per km",
#     title = ">80km/h speed violations per km",
#     subtitle = " "
#   ) +
#   theme_minimal() +
#   theme(legend.position = "top") +
#   labs(fill = "Sacco Company") +
#   xlim(0, 1000)
#
# plot_1
#
# # interactive version
# ggplotly(plot_1)

#### Simple Average method (I'm not sure about this...) ####

# Load sample data

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

# plot of number of 80kmh speed violations per km by Sacco
plot_1 <- ggplot(final_data) +
  aes(x = distance_km,
      y = final_data$over_80_by_km,
      colour = sacco) +
  geom_point(shape = "circle",
             size = 3,
             alpha = 0.6) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Average Journey Distance (km)",
    y = "Count of >80km/h violations per km",
    title = ">80km/h speed violations per km",
    subtitle = " "
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = "Sacco Company") +
  xlim(0, 1000)

plot_1

# interactive version
ggplotly(plot_1)

# plot of number of 80kmh speed violations per km by sticker treatment
plot_2 <- ggplot(final_data) +
  aes(
    x = distance_km,
    y = final_data$over_80_by_km,
    colour = as.factor(final_data$drvr_feedback_treat_sticker)
  ) +
  geom_point(shape = "circle",
             size = 3,
             alpha = 0.6) +
  labs(
    x = "Average Journey Distance (km)",
    y = "Count of >80km/h violations per km",
    title = ">80km/h speed violations per km",
    color = 'Sticker Treatment'
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  xlim(0, 1000) +
  scale_color_manual(values = c("#9E0142", "#5E4FA2", "#999999"))

plot_2

# interactive version
ggplotly(plot_2)


# plot of number of 80kmh speed violations per km by feedback treatment
plot_3 <- ggplot(final_data) +
  aes(
    x = distance_km,
    y = final_data$over_80_by_km,
    colour = as.factor(final_data$drvr_feedback_treat_feedback)
  ) +
  geom_point(shape = "circle",
             size = 3,
             alpha = 0.6) +
  labs(
    x = "Average Journey Distance (km)",
    y = "Count of >80km/h violations per km",
    title = ">80km/h speed violations per km",
    color = 'Feedback Treatment'
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  xlim(0, 1000) +
  scale_color_manual(values = c("#9E0142", "#5E4FA2", "#999999"))

plot_3

# interactive version
ggplotly(plot_3)


# G-force violations

# plot of number of 80kmh speed violations per km by feedback treatment
plot_4 <- ggplot(final_data) +
  aes(
    x = distance_km,
    y = (final_data$N_violation / final_data$distance_km),
    colour = sacco
  ) +
  geom_point(shape = "circle",
             size = 3,
             alpha = 0.6) +
  labs(
    x = "Average Journey Distance (km)",
    y = "Count of G violations per km",
    title = "G violations per km",
    color = 'Sacco'
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  xlim(0, 1000)
plot_4

# interactive version
ggplotly(plot_4)

# plot of number of 80kmh speed violations per km by feedback treatment
plot_5 <- ggplot(final_data) +
  aes(
    x = distance_km,
    y = (final_data$N_violation / final_data$distance_km),
    colour = as.factor(final_data$drvr_feedback_treat_feedback)
  ) +
  geom_point(shape = "circle",
             size = 3,
             alpha = 0.6) +
  labs(
    x = "Average Journey Distance (km)",
    y = "Count of G violations per km",
    title = "G violations per km",
    color = 'Sticker Treatment'
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  xlim(0, 1000) +
  scale_color_manual(values = c("#9E0142", "#5E4FA2", "#999999"))

plot_5

# interactive version
ggplotly(plot_5)


# Notes to self:
##  sticker treatment adsfasdf need to make sure it's after teh sticker has actually been installed
# event study type plot before and after...
# more analysis of has treatment worked
###### compare sensor data to feedback data --- now has common id
