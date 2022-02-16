# Quick Stats

# Echo Driving -----------------------------------------------------------------
echo_df <- read_parquet(file.path(sensors_dir, "FinalData", "echodriving_clean.gz.parquet"))
sensor_df <- read_parquet(file.path(sensors_dir, "FinalData", "sensortracing_clean.gz.parquet"))

sensor_df1 <- file.path(sensors_dir, "RawData", "sensor_tracing_individual_data", "2021-09-17") %>%
  list.files(full.names = T, pattern = "*.parquet") %>%
  map_df(read_parquet)
sensor_df2 <- file.path(sensors_dir, "RawData", "sensor_tracing_individual_data", "2021-09-18") %>%
  list.files(full.names = T, pattern = "*.parquet") %>%
  map_df(read_parquet)

sensor_df <- bind_rows(sensor_df1,
                       sensor_df2)

sensor1_df <- sensor_df[sensor_df$reg_no %>% str_detect("kdb 218m"),]

sensor1_dfr <- sensor1_df[sample(nrow(sensor1_df)),]

sensor1_df$speed %>% hist()

leaflet() %>%
  addTiles() %>%
  addCircles(data = sensor1_dfr[1:5000,],
             radius = 1)





sensor_veh <- read_parquet()

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


