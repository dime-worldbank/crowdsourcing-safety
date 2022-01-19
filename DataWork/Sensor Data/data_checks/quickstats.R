# Check Data

# Load data --------------------------------------------------------------------
sensor_dayhr_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_dayhr.Rds"))
sensor_day_df   <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

sensor_day_df %>%
  group_by(date) %>%
  dplyr::filter(date < ymd("2022-01-18")) %>%
  dplyr::summarise(speed_p90 = mean(speed_p90, na.rm = T)) %>%
  ggplot() +
  geom_line(aes(x = date,
                y = speed_p90))

sensor_dayhr_df$speed %>% is.na %>% table()
