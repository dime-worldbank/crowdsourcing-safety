# Quick figures to visualize data

library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

# Echo Driving =================================================================
sensor_df <- read_parquet(file.path(sensors_dir, "FinalData", "sensortracing_clean.gz.parquet")) %>%
  as.data.table()

sensor_df$dist_nairobi_km <- sqrt((sensor_df$latitude - -1.286389)^2+
                                    (sensor_df$longitude - 36.817222)^2)*111.12

# Prep Data - Hour --------------------------------------------------------------------
sensor_hour_df <- sensor_df %>%
  mutate(datetime_eat_hour = datetime_eat %>% round_date(unit = "hour")) %>%
  group_by(datetime_eat_hour, reg_no, reg_no_id) %>%
  summarise(speed_kmhr_max = max(speed_kmhr, na.rm = T),
            latitude = median(latitude),
            longitude = median(longitude)) %>%
  as.data.frame() %>%
  dplyr::filter(!is.na(datetime_eat_hour)) 

sensor_hour_df$speed_kmhr_max[sensor_hour_df$speed_kmhr_max >= 150] <- 150

# Figure - Speed ---------------------------------------------------------------
sensor_sum_df <- sensor_df %>%
  group_by(regno_clean, route) %>%
  dplyr::summarise(speed_kmhr_max = quantile(speed_kmhr, 0.95)) %>%
  as.data.frame()

p <- sensor_sum_df %>%
  filter(!is.na(route)) %>%
  mutate(regno_clean = regno_clean %>% as.factor(),
         regno_clean = fct_reorder(regno_clean, speed_kmhr_max)) %>%
  ggplot() +
  geom_col(aes(x = speed_kmhr_max,
               y = fct_reorder(regno_clean, speed_kmhr_max),
               fill = route),
           color = "black") +
  labs(title = "95% of the time, the vehicle drives this speed or faster",
       x = "Speed (km/hr)",
       fill = "Route",
       y = NULL) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold",
                                  color = "black")) 

ggsave(p, filename = file.path(dropbox_dir, 
                               "Presentations", 
                               "MOA Meeting 2021-10-09",
                               "figures",
                               "allveh_speed.png"),
       height = 8, width = 7)

# Figure - Example Day ---------------------------------------------------------
sensor_ex_veh_df <- sensor_df %>%
  filter(reg_no %in% "hiace kcz 255g",
         datetime_eat >= ymd_hms("2021-09-28 00:00:00", tz = "Africa/Nairobi"),
         datetime_eat <= ymd_hms("2021-09-28 23:59:59", tz = "Africa/Nairobi")) %>%
  as.data.frame() %>%
  mutate(datetime_eat_hour = datetime_eat %>% round_date(unit = "5 mins")) %>%
  group_by(datetime_eat_hour, reg_no, reg_no_id) %>%
  dplyr::summarise(speed_kmhr_max = max(speed_kmhr, na.rm = T),
                   latitude = median(latitude),
                   longitude = median(longitude)) 

p <- sensor_ex_veh_df %>%
  ggplot() +
  geom_col(aes(x = datetime_eat_hour,
                y = speed_kmhr_max),
           fill = "dodgerblue4",
           color = "dodgerblue3") +
  labs(x = NULL,
       y = "Speed (km/hr)") +
  theme_minimal()
ggsave(p, filename = file.path(dropbox_dir, 
                               "Presentations", 
                               "MOA Meeting 2021-10-09",
                               "figures",
                               "ex_veh_day_speed.png"),
       height = 5, width = 7)

pal <- colorNumeric(
  palette = "RdYlGn",
  domain = sensor_ex_veh_df$speed_kmhr_max)

sensor_ex_veh_df %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.35)) %>%
  addProviderTiles(providers$Stamen.TonerLabels) %>%
  addCircles(color = ~pal(speed_kmhr_max)) %>%
  addLegend("topright", 
            pal = pal,
            values = sensor_ex_veh_df$speed_kmhr_max,
            title = "Speed (km/hr)",
            opacity = 1
  )

# Map - Kenya ------------------------------------------------------------------
pal <- colorNumeric(
  palette = "RdYlGn",
  domain = sensor_hour_df$speed_kmhr_max)

sensor_hour_df %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.35)) %>%
  addProviderTiles(providers$Stamen.TonerLabels) %>%
  addCircles(color = ~pal(speed_kmhr_max)) %>%
  addLegend("topright", 
            pal = pal,
            values = sensor_hour_df$speed_kmhr_max,
            title = "Speed (km/hr)",
            opacity = 1
  )

# Map - Nairobi ----------------------------------------------------------------
sensor_nbo_df <- sensor_df %>%
  as.data.table() %>%
  filter(dist_nairobi_km <= 20) %>%
  mutate(datetime_eat_hour = datetime_eat %>% round_date(unit = "5 mins")) %>%
  group_by(datetime_eat_hour, reg_no, reg_no_id) %>%
  summarise(speed_kmhr_max = max(speed_kmhr, na.rm = T),
            latitude = median(latitude),
            longitude = median(longitude)) %>%
  as.data.frame() %>%
  dplyr::filter(!is.na(datetime_eat_hour)) %>%
  arrange(speed_kmhr_max) 

sensor_nbo_df0 <- sensor_nbo_df[sensor_nbo_df$speed_kmhr_max > 0,]

pal <- colorNumeric(
  palette = "RdYlGn",
  domain = sensor_nbo_df0$speed_kmhr_max)

sensor_nbo_df0 %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.35)) %>%
  addProviderTiles(providers$Stamen.TonerLabels) %>%
  addCircles(color = ~pal(speed_kmhr_max)) %>%
  addLegend("topright", 
            pal = pal,
            values = sensor_nbo_df0$speed_kmhr_max,
            title = "Speed (km/hr)",
            opacity = 1
  )
