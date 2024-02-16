# Telematis Over Time: Persistence

library(leaflet)

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(data_dir, "FinalData", "sensor_day_days_when_traveled.Rds"))

# Prep data --------------------------------------------------------------------
sensor_clean_df <- sensor_df %>%
  filter(rate_N_valueg_above0_5_base_10kph < 10) %>%
  # mutate(date_week = floor_date(date, unit = "weeks")) %>%
  # group_by(regno, date_week) %>%
  # dplyr::summarise(prop_time_over_100kph_base_10kph = mean(prop_time_over_100kph_base_10kph),
  #                  N_valueg_above0_5 = mean(N_valueg_above0_5)) %>%
  # ungroup() %>%
  # 
  group_by(regno) %>%
  mutate(speed_mean = median(prop_time_over_100kph_base_10kph, na.rm = T),
         harsh_mean = median(rate_N_valueg_above0_5_base_10kph, na.rm = T),
         n_day = n()) %>%
  ungroup() %>%
  
  filter(n_day >= 160) %>%
  mutate(speed_rank = base::rank(speed_mean) %>% as.factor() %>% as.numeric(),
         harsh_rank = base::rank(harsh_mean) %>% as.factor() %>% as.numeric())

#### Speed
rank_max <- sensor_clean_df$speed_rank %>% max()
rank_lim <- rank_max - 23

sensor_clean_df %>%
  filter(speed_rank >= rank_lim) %>%
  mutate(regno = reorder(regno, -speed_rank)) %>%
  ggplot(aes(x = date, 
             y = prop_time_over_100kph_base_10kph)) +
  geom_col(fill = "firebrick") +
  facet_wrap(~regno, nrow = 3, ncol = 8) +
  labs(x = NULL,
       y = "Proportion") +
  theme_classic2() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 6, angle = 30, vjust = 0.8),
        plot.title = element_text(face = "bold", size = 10),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "gray90")) 

ggsave(filename = "~/Desktop/speed_trends.png",
       height = 5, width = 10)

#### Speed Example
sensor_clean_df %>%
  filter(regno %in% "KDD 084K",
         date >= ymd("2022-10-01"),
         date <= ymd("2022-11-30")) %>%
  ggplot() +
  geom_col(aes(x = date, y = prop_time_over_100kph_base_10kph),
           fill = "firebrick") +
  labs(x = NULL,
       y = "Proportion") +
  theme_classic2() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 10),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "gray90")) 

ggsave(filename = "~/Desktop/speed_trends_ex.png",
       height = 5, width = 7)

#### Harsh Driving
rank_max <- sensor_clean_df$harsh_rank %>% max()
rank_lim <- rank_max - 23

sensor_clean_df %>%
  filter(harsh_rank >= rank_lim) %>%
  mutate(regno = reorder(regno, -harsh_rank)) %>%
  ggplot(aes(x = date, 
             y = rate_N_valueg_above0_5_base_10kph)) +
  geom_col(fill = "firebrick") +
  facet_wrap(~regno, nrow = 3, ncol = 8) +
  labs(x = NULL,
       y = "Rate") +
  theme_classic2() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 6, angle = 30, vjust = 0.8),
        plot.title = element_text(face = "bold", size = 10),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "gray90")) 

ggsave(filename = "~/Desktop/harsh_trends.png",
       height = 5, width = 10)

#### Location Example
sensor_sf <- readRDS(file.path(db_dir, "Data", "Sensor Data", 
                               "FinalData", "sensor_day_polyline.Rds"))

sensor_veh_sf <- sensor_sf %>%
  filter(regno %in% "KDD 084K") 

leaflet() %>% 
  addTiles() %>%
  addPolylines(data = sensor_veh_sf %>%
                 filter(date == ymd("2022-10-01")),
               color = "red",
               opacity = 1)

leaflet() %>% 
  addTiles() %>%
  addPolylines(data = sensor_veh_sf %>%
                 filter(date == ymd("2022-10-02")),
               color = "red",
               opacity = 1)

#### Location Example: Speed
day1_sf <- read_parquet(file.path(db_dir, "Data", "Sensor Data", 
                                  "RawData", "sensor_tracing_individual_data",
                                  "2022-10-02",
                                  "sensortracing_24492122_2022-10-02.gz.parquet"))

day2_sf <- read_parquet(file.path(db_dir, "Data", "Sensor Data", 
                                  "RawData", "sensor_tracing_individual_data",
                                  "2022-10-03",
                                  "sensortracing_24492122_2022-10-03.gz.parquet"))

spectral_palette <- colorNumeric(palette = "Spectral", domain = day1_sf$speed, reverse = T)

leaflet() %>%
  addTiles() %>%
  addCircles(data = day1_sf,
             color = "black",
             fillOpacity = 1,
             weight = 8) %>%
  addCircles(data = day1_sf, 
             color = ~spectral_palette(speed),
             fillOpacity = 1,
             popup = ~paste("Speed:", speed)) %>%
  addLegend(data = day1_sf,
            position = "bottomright",
            pal = spectral_palette,
            values = ~speed,
            title = "Speed\n(km/h)",
            opacity = 1)


spectral_palette <- colorNumeric(palette = "Spectral", domain = day2_sf$speed, reverse = T)

leaflet() %>%
  addTiles() %>%
  addCircles(data = day2_sf,
             color = "black",
             fillOpacity = 1,
             weight = 8) %>%
  addCircles(data = day2_sf, 
             color = ~spectral_palette(speed),
             fillOpacity = 1,
             popup = ~paste("Speed:", speed)) %>%
  addLegend(data = day2_sf,
            position = "bottomright",
            pal = spectral_palette,
            values = ~speed,
            title = "Speed\n(km/h)",
            opacity = 1)


