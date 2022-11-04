# Quick Stats

# Daily ------------------------------------------------------------------------
daily_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))
#daily_sf <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day_polyline.Rds"))

a <- daily_df %>%
  dplyr::filter(N_obs_speed > 0) %>%
  group_by(date) %>%
  dplyr::summarise(n = n())

a %>%
  ggplot() +
  geom_col(aes(x = date, y = n))


daily_sub_df <- daily_df %>%
  dplyr::filter(distance_km >= 100) %>%
  dplyr::mutate(prop_speed_over_110 = N_speed_over_110 / N_obs_speed,
                prop_speed_over_100 = N_speed_over_100 / N_obs_speed,
                prop_speed_over_95 = N_speed_over_95 / N_obs_speed,
                prop_speed_over_90 = N_speed_over_90 / N_obs_speed)

daily_sub_df$prop_speed_over_110 %>% summary()
daily_sub_df$prop_speed_over_100 %>% summary()
daily_sub_df$prop_speed_over_95 %>% summary()
daily_sub_df$prop_speed_over_90 %>% summary()

daily_sub_df$N_speed_over_110 %>%
  mean(na.rm=T)


issue_df <- daily_sf %>%
  dplyr::filter(distance_km >= 100,
                speed_max %in% 0)

a <- daily_sub_df[daily_sub_df$speed_max %in% 0,]
daily_sub_df$speed_max %>% hist()

# Echo Driving -----------------------------------------------------------------
# Vehicle and acceleration types
echo_df <- read_parquet(file.path(sensors_dir, "FinalData", "echodriving.gz.parquet"))

echo_sum_df <- echo_df %>%
  dplyr::filter(violation %>% tolower %>% str_detect("accel")) %>%
  dplyr::mutate(violation = violation %>% as.character())

df_out <- table(echo_sum_df$reg_no, echo_sum_df$violation) %>% 
  as.data.frame.matrix 

write.csv(df_out, file.path("~/Desktop/echo_driving_acc_violation_count.csv"))


# Echo Driving -----------------------------------------------------------------
echo_df <- read_parquet(file.path(sensors_dir, "FinalData", "echodriving.gz.parquet"))

echo_df <- echo_df %>%
  dplyr::mutate(date = begin_datetime %>% as.Date)

echo_df$violation %>% table()

## All
echo_df %>%
  distinct(begin_datetime, .keep_all = T) %>%
  dplyr::group_by(date, reg_no) %>%
  dplyr::summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = date, y = N)) +
  facet_wrap(~reg_no)

## By Type
echo_df %>%
  dplyr::filter(violation %>% str_detect("Brak")) %>%
  distinct(begin_datetime, .keep_all = T) %>%
  dplyr::group_by(date, reg_no) %>%
  dplyr::summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = date, y = N)) +
  facet_wrap(~reg_no)

echo_df %>%
  dplyr::filter(violation %>% str_detect("Acceler")) %>%
  distinct(begin_datetime, .keep_all = T) %>%
  dplyr::group_by(date, reg_no) %>%
  dplyr::summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = date, y = N)) +
  facet_wrap(~reg_no)

echo_df %>%
  dplyr::filter(violation %>% str_detect("Turn")) %>%
  distinct(begin_datetime, .keep_all = T) %>%
  dplyr::group_by(date, reg_no) %>%
  dplyr::summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = date, y = N)) +
  facet_wrap(~reg_no)

echo_df %>%
  dplyr::filter(violation %>% str_detect("Speed")) %>%
  distinct(begin_datetime, .keep_all = T) %>%
  dplyr::group_by(date, reg_no) %>%
  dplyr::summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = date, y = N)) +
  facet_wrap(~reg_no)

# Sensor Tracing ---------------------------------------------------------------
sensor_df <- read_parquet(file.path(sensors_dir, "FinalData", "sensortracing.gz.parquet"))

sensor_df <- sensor_df %>%
  dplyr::mutate(time_str = time_str %>% ymd_hms(tz = "UTC") %>% with_tz(tzone = "Africa/Nairobi") %>% round_date(unit = "hour")) %>%
  dplyr::filter(!is.na(time_str))

sensor_sum_df <- sensor_df %>%
  dplyr::group_by(time_str, reg_no) %>%
  dplyr::summarise(speed = max(speed_kmhr))

sensor_sum_df %>%
  ggplot() +
  geom_col(aes(x = time_str,
               y = speed)) + 
  facet_wrap(~reg_no)


