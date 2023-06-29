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

#### Loading and joining data ####

#### Loading and cleaning sensor data ####

# Loading sensor data
sensor_data <-
  readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

# Convert the 'reg_no' column to lower case
sensor_data <- sensor_data %>% mutate(reg_no = tolower(reg_no))

# subsetting to only those journeys over 100 km distance
# sensor_data <- subset(sensor_data, sensor_data$distance_km > 1)

# Loading sensor data with time spent over xkm/h
time_data <-
  read_parquet(file.path(sensors_dir, "FinalData", "time_moving_day.gz.parquet"))

# Remove double spaces from 'reg_no' column
time_data <- time_data %>% mutate(reg_no = gsub("\\s+", " ", reg_no))

# subset to journeys longer than 60 minutes
time_data <- subset(time_data, time_data$time_mov_s > (60 * 60))

# Join data by reg_no and date
joined_data <- left_join(time_data, sensor_data, by = c("reg_no", "date"))

# Generating variable for proportion of time spent over 80km/h
joined_data$prop_80 <- joined_data$time_spd80_s / joined_data$time_mov_s

# Generating variable for proportion of time spent over 100km/h
joined_data$prop_100 <- joined_data$time_spd100_s / joined_data$time_mov_s

# Generating variable to capture all g-force related violations
joined_data$total_g_violations <-
  (joined_data$N_violation_acceleration +
    joined_data$N_violation_brake +
    joined_data$N_violation_turn)

# this is now per hour moving
joined_data$total_g_violations_per_hour <-
  joined_data$total_g_violations / (joined_data$time_mov_s / 3600)

joined_data$days_since_sticker <- as.numeric(joined_data$date - joined_data$sticker_install_date)

# Subset the data frame by column index numbers
clean_data <- joined_data[, c(1:5, 7, 9:12, 74:81)]

# Collapse the data frame by reg_no and calculate the average of x, y, and z
sensor_grouped <- aggregate(clean_data[, 12:17], by = list(reg_no = clean_data$reg_no), FUN = mean)

# Appending regno_clean name
sensor_grouped$regno_clean <- clean_data$regno_clean[match(
  sensor_grouped$reg_no,
  clean_data$reg_no
)]


#### Loading and cleaning feedback data ####

feedback_data <-
  readRDS(file.path(rider_feedback_dir, "FinalData", "rider_feedback.Rds"))

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

# Specify which columns we want to take weighted average of:
variablesToAvg <-
  names(grouped_data)[c(25, 26, 27, 29, 30, 32, 33)] ##

### set as data table
grouped_data <-
  data.table(grouped_data)

# getting rid of rows where N_speed_over_0 is 'na' or zero as this doesn't make sense..
grouped_data <- grouped_data %>%
  filter(!is.na(safety_numeric) & !is.na(speed_numeric))

### collapse to bus using means
final_data <-
  grouped_data[,
    lapply(.SD, mean), ## applies weighted mean function with weights w over specified columns
    by = list(regno_clean),
    .SDcols = variablesToAvg
  ] ## specifies columns to average

final_data$sacco <-
  grouped_data$matatu_sacco_stk_inst_srvy[match(final_data$regno_clean, grouped_data$regno_clean)]

feedback_grouped <- final_data

#### Joining sensor and feedback data ####

joined_data <- left_join(sensor_grouped, feedback_grouped)

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
  labs(fill = "Sacco Company")

plot_1

# interactive version
ggplotly(plot_1)


#### Comparing sensor to feedback data ####
# focus on proportion of responses that are fast or very fast instead of average of 1 to 5

plot_1 <- ggplot(joined_data) +
  aes(
    x = joined_data$prop_100,
    y = joined_data$speed_numeric
  ) +
  geom_point(
    shape = "circle",
    size = 3,
    alpha = 0.6,
    aes(color = sacco)
  ) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Proportion of time >100 km/h",
    y = "Speed Rating (1 = Very slow, 5 = Very fast",
    title = "Comparing Rider Feedback on Speed vs. Safety",
    subtitle = " "
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_smooth(method = "lm", se = TRUE) +
  xlim(0,0.02)

# labs(fill = "Sacco Company") +
# xlim(2.5, 4) +
# ylim(1.5, 5)

plot_1

# interactive version
ggplotly(plot_1)



#### Comparing sensor to feedback data ####
# focus on proportion of responses that are fast or very fast instead of average of 1 to 5

plot_1 <- ggplot(joined_data) +
  aes(
    x = joined_data$total_g_violations_per_hour,
    y = joined_data$safety_numeric
  ) +
  geom_point(
    shape = "circle",
    size = 3,
    alpha = 0.6,
    aes(color = sacco)
  ) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Total G force violations per hour",
    y = "Safety Rating (1 = Very safe, 4 = Not very safe)",
    title = "Comparing Rider Feedback on Speed vs. Safety",
    subtitle = " "
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_smooth(method = "lm", se = TRUE)
# labs(fill = "Sacco Company") +
# xlim(2.5, 4) +
# ylim(1.5, 5)

plot_1
