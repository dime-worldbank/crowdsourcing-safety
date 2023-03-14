# Tom Harris
# Analysig bias in sticker installations
# asdf

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



## plotting 

# install.packages("ggplot2")
library(ggplot2)

# Data
set.seed(8)
y <- rnorm(200)
group <- sample(LETTERS[1:3],
                size = 200,
                replace = TRUE)
df <- data.frame(y, group)

# Box plot by group with jitter
ggplot(df, aes(x = group, y = y,
               colour = group,
               shape = group)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() 





# Creating data for analysis
sensor_data_sticker <- joined_data %>%
  # Only consider vehicles with sensor installed
  dplyr::filter(sticker_installed %in% T) %>%
  # Days since installation
  dplyr::mutate(days_since_sticker = as.numeric(date - sticker_install_date)) %>%
  # Only look before sticker installed to see if there are pre diffs
  dplyr::filter(days_since_sticker < 0)

# Creating data for analysis
sensor_data_sticker <- joined_data %>%
  # Only consider vehicles with sensor installed
  dplyr::filter(sticker_installed %in% T) %>%
  # Days since installation
  dplyr::mutate(days_since_sticker = as.numeric(date - sticker_install_date)) %>%
  # Only look before sticker installed to see if there are pre diffs
  dplyr::filter(days_since_sticker < 0)


# Generating variable to capture all g-force related violations
# Q. should this also be 'per km'?
sensor_data_clean$total_g_violations <-
  (sensor_data_clean$N_violation_acceleration +
    sensor_data_clean$N_violation_brake +
    sensor_data_clean$N_violation_turn)

# this is now per hour moving
sensor_data_clean$total_g_violations_per_hour <-
  sensor_data_clean$total_g_violations / (sensor_data_clean$time_mov_s / 3600)

# renaming to this for ease ... temporary
sensor_data_clean$total_g_violations <- sensor_data_clean$total_g_violations_per_hour

# Preparing data for event study analysis
analysis_data <- sensor_data_clean %>%
  group_by(regno_clean) %>%
  mutate(
    days_since_sticker = as.numeric(date - sticker_install_date),
    after = ifelse(days_since_sticker > 0, TRUE, FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    days_since_sticker = relevel(as.factor(days_since_sticker), ref = "-1"),
    regno_clean = as.factor(regno_clean),
    date = as.factor(date)
  )