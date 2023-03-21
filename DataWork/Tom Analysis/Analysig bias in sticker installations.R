# Tom Harris
# Analysig bias in sticker installations
# ...

# n.b. Before any new scripts, run psv_feedback_master.R

# Installing further required packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyr,
  dplyr,
  plyr,
  ggpmisc,
  wesanderson,
  plotly,
  ggplot2,
  stargazer,
  tidymodels,
  data.table,
  tidyverse,
  haven,
  lfe
)

#### Loading and joining data ####

# Loading sensor data
sensor_data <-
  readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

# Convert the 'reg_no' column to lower case
sensor_data <- sensor_data %>% mutate(reg_no = tolower(reg_no))

# Loading sensor data with time spent over xkm/h
time_data <-
  read_parquet(file.path(sensors_dir, "FinalData", "time_moving_day.gz.parquet"))

# Remove double spaces from 'reg_no' column
time_data <- time_data %>% mutate(reg_no = gsub("\\s+", " ", reg_no))

# subset to journeys longer than 10 minutes
time_data <- subset(time_data, time_data$time_mov_s > 600)

# Join data by reg_no and date
joined_data <- left_join(time_data, sensor_data, by = c("reg_no", "date"))

# Generating variable for proportion of time spent over 80km/h
joined_data$prop_80 <- joined_data$time_spd80_s / joined_data$time_mov_s

# Generating variable for proportion of time spent over 100km/h
joined_data$prop_100 <- joined_data$time_spd100_s / joined_data$time_mov_s

# Generating variable to capture all g-force related violations
# Q. should this also be 'per km'?
joined_data$total_g_violations <-
  (joined_data$N_violation_acceleration +
    joined_data$N_violation_brake +
    joined_data$N_violation_turn)

# this is now per hour moving
joined_data$total_g_violations_per_hour <-
  joined_data$total_g_violations / (joined_data$time_mov_s / 3600)



joined_data$days_since_sticker <- as.numeric(joined_data$date - joined_data$sticker_install_date)

clean_data <- subset(joined_data, joined_data$days_since_sticker < 0 | is.na(joined_data$days_since_sticker))

clean_data$n_stickers_installed <- if_else(is.na(clean_data$n_stickers_installed), 0, clean_data$n_stickers_installed)

clean_data <- subset(clean_data, clean_data$sticker_installed == "TRUE")

# Subset the data frame by column index numbers
clean_data <- clean_data[, c(1:5, 7, 9:12, 74:81)]

# Collapse the data frame by reg_no and calculate the average of x, y, and z
agg_df <- aggregate(clean_data[, 12:17], by = list(reg_no = clean_data$reg_no), FUN = mean)


## Box plots by number of stickers

# Proportion of time over 80km/h
plot_1 <- ggplot(clean_data, aes(
  x = as.factor(clean_data$n_stickers_installed), y = clean_data$prop_80,
  colour = as.factor(n_stickers_installed), alpha = 0.1
)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  coord_flip() +
  xlab("Number of Stickers Installed") +
  ylab("Proportion of time spent >80km/h") +
  ggtitle("Testing bias in number of stickers") +
  theme_bw() +
  scale_color_discrete(
    name = "Number of Stickers",
    labels = c("None", "Two", "Three", "Four", "Five", "Six")
  )

plot_1
ggplotly(plot_1)

# Proportion of time over 100km/h

ggplot(clean_data, aes(
  x = as.factor(clean_data$n_stickers_installed), y = clean_data$prop_100,
  colour = as.factor(n_stickers_installed), alpha = 0.1
)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  coord_flip() +
  xlab("Number of Stickers Installed") +
  ylab("Proportion of time spent >100 km/h") +
  ggtitle("Testing bias in number of stickers") +
  theme_bw() +
  scale_color_discrete(
    name = "Number of Stickers",
    labels = c("None", "Two", "Three", "Four", "Five", "Six")
  )


# G force violations per hour

ggplot(clean_data, aes(
  x = as.factor(clean_data$n_stickers_installed), y = clean_data$total_g_violations_per_hour,
  colour = as.factor(n_stickers_installed), alpha = 0.1
)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  coord_flip() +
  xlab("Number of Stickers Installed") +
  ylab("Total G Force Violations Per Hour") +
  ggtitle("Testing bias in number of stickers") +
  theme_bw() +
  scale_color_discrete(
    name = "Number of Stickers",
    labels = c("None", "Two", "Three", "Four", "Five", "Six")
  )



#### Repeat analysis by regno id ####

## Box plots by number of stickers

# Proportion of time over 80km/h
plot_1 <- ggplot(agg_df, aes(
  x = as.factor(agg_df$n_stickers_installed), y = agg_df$prop_80,
  colour = as.factor(n_stickers_installed), alpha = 0.1
)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  coord_flip() +
  xlab("Number of Stickers Installed") +
  ylab("Proportion of time spent >80km/h") +
  ggtitle("Testing bias in number of stickers") +
  theme_bw() +
  scale_color_discrete(
    name = "Number of Stickers",
    labels = c("None", "Two", "Three", "Four", "Five", "Six")
  )

plot_1

# Proportion of time over 100km/h

ggplot(agg_df, aes(
  x = as.factor(agg_df$n_stickers_installed), y = agg_df$prop_100,
  colour = as.factor(n_stickers_installed), alpha = 0.1
)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  coord_flip() +
  xlab("Number of Stickers Installed") +
  ylab("Proportion of time spent >100 km/h") +
  ggtitle("Testing bias in number of stickers") +
  theme_bw() +
  scale_color_discrete(
    name = "Number of Stickers",
    labels = c("None", "Two", "Three", "Four", "Five", "Six")
  )


# G force violations per hour

ggplot(agg_df, aes(
  x = as.factor(agg_df$n_stickers_installed), y = agg_df$total_g_violations_per_hour,
  colour = as.factor(n_stickers_installed), alpha = 0.1
)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  coord_flip() +
  xlab("Number of Stickers Installed") +
  ylab("Total G Force Violations Per Hour") +
  ggtitle("Testing bias in number of stickers") +
  theme_bw() +
  scale_color_discrete(
    name = "Number of Stickers",
    labels = c("None", "Two", "Three", "Four", "Five", "Six")
  )

