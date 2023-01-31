# Tom Harris
# Practice script

# n.b. Before any new scripts, run psv_feedback_master.R

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyr,
               dplyr,
               plotly,
               ggplot2,
               data.table)


sensor_data <-
  readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))


feedback_data <-
  readRDS(file.path(rider_feedback_dir, "FinalData", "rider_feedback.Rds"))

feedback_trans_data <- readRDS(file.path(
  rider_feedback_dir,
  "FinalData",
  "rider_feedback_translated.Rds"
))


## common id is probs regno_clean

## first step, aggregate everything to the vehicle level
#how safe does sensor data say vehicle, how safe does feedback data say it is.... do these corr?
# how do we define 'safe' . e,g, avg speed, make other indicators .... proportion of time spent over 80 kmh etc.
# eventually some kidn of index of dangerous driving ....




# Load sample data

# Group the data by the "gear" variable
grouped_data <- sensor_data %>%
  group_by(regno_clean)




### Specify which columns we want to take weighted average of:
variablesToAvg <-
  names(grouped_data)[c(1, 2, 10:50)] ## you may want to tweak which variables you average

### set as data table
grouped_data <-
  data.table(grouped_data)

grouped_data <- grouped_data %>%
  filter(!is.na(N_speed_over_0) & N_speed_over_0 != 0)


### collapse to FB postcode level using weighted means

final_data <-
  grouped_data[,
               lapply(.SD, weighted.mean, w = distance_km), ## applies weighted mean function with weights w over specified columns
               by = list(regno_clean),
               .SDcols = variablesToAvg] ## specifies columns to average

final_data$sacco <-
  grouped_data$sacco[match(final_data$regno_clean, grouped_data$regno_clean)]


# plot
plot_1 <- ggplot(final_data) +
  aes(x = distance_km, y = N_speed_over_80, colour = sacco) +
  geom_point(shape = "circle",
             size = 1.5,
             alpha = 0.6) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Average Journey Distance (km)",
    y = "Count of >80km/h observations",
    title = ">80km/h speed violations by mutatus",
    subtitle = " "
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = "Sacco Company") +
  xlim(0, 1000)

plot_1

# interactive version
ggplotly(plot_1)
