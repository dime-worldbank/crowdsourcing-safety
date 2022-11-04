# Compare NTSA Data with Our Sensor Data

# Load data --------------------------------------------------------------------
ntsa_df <- readRDS(file.path(ntsa_speed_dir, "FinalData", "speed_limiter_transmission_data_20221004.Rds"))
ntsa_df <- ntsa_df %>%
  mutate(date_time = date_time %>% ymd_hms(tz = "Africa/Nairobi"))

sensor_kct481v_df <- load_st_raw(dates = c("2022-10-04"),
                                 vehicles = c("kct481v")) %>%
  mutate(time_str = time_str %>% ymd_hms() %>% with_tz(tzone = "Africa/Nairobi"))

# sensor_kdb218m_df <- load_st_raw(dates = c("2022-10-03","2022-10-04","2022-10-05"),
#                                  vehicles = c("kdb218m")) %>%
#   mutate(time_str = time_str %>% ymd_hms())

sensor_kcq580g_df <- load_st_raw(dates = c("2022-10-04"),
                                 vehicles = c("kcq580g")) %>%
  mutate(time_str = time_str %>% ymd_hms() %>% with_tz(tzone = "Africa/Nairobi"))

# Compare 1 --------------------------------------------------------------------
ntsa_df %>%
  dplyr::filter(regno == "kct481v") %>%
  ggplot() +
  geom_point(aes(x = date_time,
                 y = speed))

ggplot() +
  geom_point(data = sensor_kct481v_df,
             aes(x = time_str,
                 y = speed)) +
  geom_point(data = ntsa_df %>%
               dplyr::filter(regno == "kct481v"),
             aes(x = date_time,
                 y = speed),
             color = "red")


leaflet() %>%
  addTiles() %>%
  addCircles(data = ntsa_df[ntsa_df$regno %in% "kct481v",], color = "red") %>%
  addCircles(data = sensor_kct481v_df %>%
               dplyr::filter(time_str >= ntsa_df$date_time %>% min(),
                             time_str <= ntsa_df$date_time %>% max()),
             color = "black")

# Compare 2 --------------------------------------------------------------------
ntsa_df %>%
  dplyr::filter(regno == "kcq580g") %>%
  ggplot() +
  geom_point(aes(x = date_time,
                 y = speed))

ggplot() +
  geom_point(data = sensor_kcq580g_df,
             aes(x = time_str,
                 y = speed)) +
  geom_point(data = ntsa_df %>%
               dplyr::filter(regno == "kcq580g"),
             aes(x = date_time,
                 y = speed),
             color = "red")

ggplot() +
  geom_point(data = sensor_kcq580g_df %>%
               dplyr::filter(time_str >= ntsa_df$date_time %>% min(),
                             time_str <= ntsa_df$date_time %>% max()),
             aes(x = time_str,
                 y = speed)) +
  geom_point(data = ntsa_df %>%
               dplyr::filter(regno == "kcq580g"),
             aes(x = date_time,
                 y = speed),
             color = "red")

leaflet() %>%
  addTiles() %>%
  addCircles(data = ntsa_df[ntsa_df$regno %in% "kcq580g",], color = "red") %>%
  addCircles(data = sensor_kcq580g_df %>%
               dplyr::filter(time_str >= ntsa_df$date_time %>% min(),
                             time_str <= ntsa_df$date_time %>% max()),
             color = "black")

# Compare 3 --------------------------------------------------------------------
ntsa_df %>%
  dplyr::filter(regno == "kdb218m") %>%
  ggplot() +
  geom_point(aes(x = date_time,
                 y = speed), color = "red")

leaflet() %>%
  addTiles() %>%
  addCircles(data = ntsa_df[ntsa_df$regno %in% "kdb218m",], color = "red") 
