# Check Data Issues

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

sensor_latest_date <- sensor_df %>%
  dplyr::filter(date %in% max(date))

sensor_df$N_obs_speed %>% summary()
sensor_latest_date$speed_p05

sensor_df$spee

sensor_df$date


