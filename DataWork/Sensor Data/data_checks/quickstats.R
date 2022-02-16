# Check Data

# Load data --------------------------------------------------------------------
sensor_dayhr_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_dayhr.Rds"))
sensor_day_df   <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

# No speed data but have violation data: HOURLY
weird_cases <- sensor_dayhr_df[is.na(sensor_dayhr_df$speed_max) & sensor_dayhr_df$N_violation > 0,]
weird_cases %>%
  group_by(reg_no) %>%
  dplyr::summarise(N = n()) %>%
  arrange(-N)

# No speed data but have violation data: HOURLY
weird_cases <- sensor_dayhr_df[is.na(sensor_dayhr_df$speed_max) & sensor_dayhr_df$N_violation > 0,]
weird_cases %>%
  group_by(reg_no) %>%
  dplyr::summarise(N = n()) %>%
  arrange(-N)

weird_cases$datetime_eat[weird_cases$reg_no %in% "hiace kcu 283c"] %>% sort()

load_st_raw(dates = "2022-01-09",
            vehicles = "kcu283c")

sensor_dayhr_df$speed %>% is.na %>% table()


plot(sensor_day_df$distance_daily_km, sensor_day_df$distance_km)

sensor_day_df %>%
  group_by(date) %>%
  dplyr::filter(date < ymd("2022-01-18")) %>%
  dplyr::summarise(speed_p90 = mean(speed_p90, na.rm = T)) %>%
  ggplot() +
  geom_line(aes(x = date,
                y = speed_p90))

sensor_dayhr_df$speed %>% is.na %>% table()
