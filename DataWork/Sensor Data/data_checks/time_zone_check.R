# Time Zone Checks

# Load Data --------------------------------------------------------------------
sensor_df <- read_parquet(file.path(sensors_dir, "FinalData", "sensortracing_clean.gz.parquet"))

# Randomly sort
sensor_df <- sensor_df[sample(nrow(sensor_df)),]

# Make UTC a time object
sensor_df <- sensor_df %>%
  dplyr::mutate(#time_str_raw_utc = ymd_hms(time_str_raw_utc),
    date_eat = datetime_eat %>% as.Date(),
    hour = datetime_eat %>% hour())

if(F){
  sensor_df$regno_clean %>% table %>% View()
}

# N Non-Zero Obs ---------------------------------------------------------------
sensor_dt <- sensor_df %>% data.table()

hour_nobs_speed_df <- sensor_dt %>%
  filter(speed_kmhr > 20) %>%
  group_by(hour) %>%
  summarise(N = n()) %>%
  as.data.frame()

p <- hour_nobs_speed_df %>%
  filter(!is.na(hour)) %>%
  mutate(hour = hour %>% as.factor()) %>%
  ggplot() +
  geom_col(aes(x = hour,
               y = N)) +
  labs(title = "N Obs Over 20 km/hr")

ggsave(p,
       filename = file.path(sensors_dir, "Outputs", 
                            "n_obs_by_hour.png"),
       height = 4,
       width = 5)

# Example 1 --------------------------------------------------------------------
sensor_df_sub <- sensor_df %>%
  dplyr::filter(regno_clean %in% "KCZ 255G",
                datetime_eat >= ymd_hms("2021-10-06 00:00:00"),
                datetime_eat <= ymd_hms("2021-10-10 00:00:00")) 

sensor_df_sub %>% 
  ggplot() +
  geom_line(aes(x = datetime_eat,
                y = speed_kmhr)) +
  facet_wrap(~date_eat,
             scales = "free")

leaflet() %>%
  addTiles() %>%
  addCircles(data = sensor_df_sub[1:5000,],
             radius = 1)

# Example 2 --------------------------------------------------------------------
sensor_df_sub <- sensor_df %>%
  dplyr::filter(regno_clean %in% "KCW 543M",
                datetime_eat >= ymd_hms("2021-10-06 00:00:00"),
                datetime_eat <= ymd_hms("2021-10-10 00:00:00")) %>%
  dplyr::mutate(date_eat = datetime_eat %>% as.Date())

sensor_df_sub %>% 
  ggplot() +
  geom_line(aes(x = datetime_eat,
                y = speed_kmhr)) +
  facet_wrap(~date_eat,
             scales = "free")

leaflet() %>%
  addTiles() %>%
  addCircles(data = sensor_df_sub[1:5000,],
             radius = 1)

# Example 3 --------------------------------------------------------------------
sensor_df_sub <- sensor_df %>%
  dplyr::filter(regno_clean %in% "KCR 120F",
                datetime_eat >= ymd_hms("2021-10-06 00:00:00"),
                datetime_eat <= ymd_hms("2021-10-10 00:00:00")) %>%
  dplyr::mutate(date_eat = datetime_eat %>% as.Date())

sensor_df_sub %>% 
  ggplot() +
  geom_line(aes(x = datetime_eat,
                y = speed_kmhr)) +
  facet_wrap(~date_eat,
             scales = "free")

leaflet() %>%
  addTiles() %>%
  addCircles(data = sensor_df_sub[1:5000,],
             radius = 1)

# Example 4 --------------------------------------------------------------------
sensor_df_sub <- sensor_df %>%
  dplyr::filter(regno_clean %in% "KDB 820M",
                datetime_eat >= ymd_hms("2021-10-06 00:00:00"),
                datetime_eat <= ymd_hms("2021-10-10 00:00:00")) %>%
  dplyr::mutate(date_eat = datetime_eat %>% as.Date())

sensor_df_sub %>% 
  ggplot() +
  geom_line(aes(x = datetime_eat,
                y = speed_kmhr)) +
  facet_wrap(~date_eat,
             scales = "free")

leaflet() %>%
  addTiles() %>%
  addCircles(data = sensor_df_sub[1:5000,],
             radius = 1)
