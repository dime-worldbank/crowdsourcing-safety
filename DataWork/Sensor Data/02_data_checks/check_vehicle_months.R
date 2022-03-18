
sensor_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

day1 <- sensor_df %>%
  group_by(reg_no_id) %>%
  dplyr::filter(!is.na(distance_daily_km)) %>%
  dplyr::summarise(day1 = min(date[distance_daily_km > 0])) %>%
  mutate(month1 = day1 %>% floor_date(unit = "month")) %>%
  group_by(month1) %>%
  dplyr::summarise(N = n())

day1$N %>% sum()
150-84

