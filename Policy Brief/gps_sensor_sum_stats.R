# GPS Figures

library(geodata)
library(h3jsr)

# Load data --------------------------------------------------------------------
veh_df    <- readRDS(file.path(ap_data_dir, "FinalData", "vehicle_level.Rds"))
sensor_df <- readRDS(file.path(ap_data_dir, "RawData", "sensor_day.Rds"))

# Prop above 80 km/h -----------------------------------------------------------
veh_df %>%
  dplyr::mutate(prop_time_over_80kph_base_10kph = floor(prop_time_over_80kph_base_10kph * 10) / 10) %>%
  group_by(prop_time_over_80kph_base_10kph) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  
  ggplot() + 
  geom_col(aes(x = prop_time_over_80kph_base_10kph,
               y = n),
           width = 0.075,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = prop_time_over_80kph_base_10kph,
                y = n + 5,
                label = n)) +
  scale_x_continuous(#limits = c(-0.05, 0.55),
    breaks = seq(0, 0.5 , 0.1),
    labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%")) +
  labs(x = "Percent of driving time",
       y = "N Vehicles",
       title = "Percent of time vehicle traveling above 80 km/h",
       caption = "Percent calculated as: (time traveling above 80 km/h) / (time traveling above 10 km/h) * 100") +
  theme_classic2() +
  theme(plot.title = element_text(size = 9),
        plot.subtitle = element_text(size = 8, face = "bold"),
        plot.caption = element_text(size = 6),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8, color = "black")) 

ggsave(filename = file.path(brief_figures_dir, "prop_over_80kmh.png"),
       height = 2.25, width = 4)

# Harsh driving ----------------------------------------------------------------
veh_df %>%
  dplyr::mutate(rate_N_valueg_above0_5_base_10kph = rate_N_valueg_above0_5_base_10kph * 60 * 60) %>%
  dplyr::mutate(rate_N_valueg_above0_5_base_10kph = case_when(
    rate_N_valueg_above0_5_base_10kph >= 3 ~ 3,
    TRUE ~ rate_N_valueg_above0_5_base_10kph
  )) %>%
  dplyr::mutate(rate_N_valueg_above0_5_base_10kph = round(rate_N_valueg_above0_5_base_10kph * 5) / 5) %>%
  group_by(rate_N_valueg_above0_5_base_10kph) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  
  ggplot() + 
  geom_col(aes(x = rate_N_valueg_above0_5_base_10kph,
               y = n),
           width = 0.15,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = rate_N_valueg_above0_5_base_10kph,
                y = n + 5,
                label = n)) +
  scale_x_continuous(#limits = c(-0.1, 3.1),
    breaks = c(0, 1, 2, 3),
    labels = c("0", "1", "2", ">3")) +
  labs(x = "N Harsh Driving Events per Driving Hour",
       y = "N Vehicles",
       title = "Number of harsh driving events per driving hour, on average",
       subtitle = "Harsh breaking events are instances where acceleration is >0.5g",
       caption = "Calculating as: (N harsh breaking events) / (time traveling above 10 km/h)") +
  theme_classic2() +
  theme(plot.title = element_text(size = 9),
        plot.subtitle = element_text(size = 8, face = "bold"),
        plot.caption = element_text(size = 6),
        axis.title = element_text(size = 8)) 

ggsave(filename = file.path(brief_figures_dir, "harsh_breaking_distribution.png"),
       height = 2.25, width = 4)

# Speed and acceleration comparison --------------------------------------------
# cor.test(veh_df$speed_p90, veh_df$rate_N_valueg_above0_4_base_50kph)
# cor.test(veh_df$time_over_130kph_secs, veh_df$rate_N_valueg_above0_4_base_50kph)
# 
# veh_df %>%
#   ggplot() +
#   geom_point(aes(x = time_over_100kph_secs,
#                  y = N_valueg_above1_0))


# Speed map --------------------------------------------------------------------
# getData function to load a polygon of Ghana
roi_sf <- gadm(country = "KEN", level=0, path = tempdir()) |> st_as_sf()
nbo_sf <- gadm(country = "KEN", level=1, path = tempdir()) |> st_as_sf()
nbo_sf <- nbo_sf[nbo_sf$NAME_1 %in% "Nairobi",]

#### Grab speeds
speed_df <- map_df(seq.Date(from = ymd("2022-06-01"),
                            to = ymd("2022-06-07"),
                            by = 1), 
                   function(date_i){
                     file.path(data_dir, "Sensor Data", "RawData", "sensor_tracing_individual_data",
                               date_i) %>%
                       list.files(pattern = "*.parquet",
                                  full.names = T) %>%
                       map_df(read_parquet)
                   })

#### Cleanup
speed_df <- speed_df %>%
  dplyr::filter(speed > 0) %>%
  arrange(speed) %>%
  distinct(latitude, longitude, .keep_all = T) %>%
  dplyr::mutate(id = 1:n())

#### Aggregate to h3
speed_sf <- st_as_sf(speed_df, coords = c("longitude", "latitude"),
                     crs = 4326)

speed_sf$h3 <- point_to_cell(speed_sf, res = 9)
speed_sf <- cbind(speed_sf, st_coordinates(speed_sf))

speed_h3_df <- speed_sf %>%
  st_drop_geometry() %>%
  group_by(h3) %>%
  dplyr::summarise(speed = mean(speed),
                   longitude = median(X),
                   latitude = median(Y)) %>%
  ungroup() %>%
  dplyr::mutate(id = 1:n())

#### Restrict to Kenya
speed_h3_sf <- st_as_sf(speed_h3_df, coords = c("longitude", "latitude"),
                     crs = 4326) %>%
  st_intersection(roi_sf) 
speed_h3_df <- speed_h3_df[speed_h3_df$id %in% speed_h3_sf$id,]

#### Restrict to Nairobi
speed_h3_nbo_sf <- st_as_sf(speed_h3_df, coords = c("longitude", "latitude"),
                     crs = 4326) %>%
  st_intersection(nbo_sf) 
speed_h3_nbo_df <- speed_h3_df[speed_h3_df$id %in% speed_h3_nbo_sf$id,]

#### Speed limit
speed_h3_df$speed_adj <- speed_h3_df$speed
speed_h3_df$speed_adj[speed_h3_df$speed_adj >= 150] <- 150

#### Map, Kenya
p <- speed_h3_df %>%
  arrange(speed) %>%
  ggplot() +
  geom_sf(data = roi_sf,
          color = "black",
          fill = "gray95") +
  geom_point(aes(x = longitude, y = latitude),
             color = "black",
             size = 1) +
  geom_point(aes(x = longitude, y = latitude,
                 color = speed_adj),
             size = 0.8) +
  scale_color_distiller(palette = "Spectral",
                        limits = c(0, 150),
                        breaks = c(0, 30, 60, 90, 120, 150),
                        labels = c("0", "30", "60", "90", "120", ">150")) +
  labs(color = "Speed\n(km/h)") +
  theme_void()

ggsave(p,
       filename = file.path(brief_figures_dir, "speed_map.png"),
       height = 5, width = 5)

#### Map, Nairobi
speed_h3_nbo_df$speed_adj <- speed_h3_nbo_df$speed
speed_h3_nbo_df$speed_adj[speed_h3_nbo_df$speed_adj >= 100] <- 100
p <- speed_h3_nbo_df %>%
  dplyr::filter(speed <= 150) %>%
  ggplot() +
  geom_sf(data = nbo_sf,
          color = "black",
          fill = "gray95") +
  geom_point(aes(x = longitude, y = latitude),
             color = "black",
             size = 0.8) +
  geom_point(aes(x = longitude, y = latitude,
                 color = speed_adj),
             size = 0.5) +
  scale_color_distiller(palette = "Spectral",
                        limits = c(0, 150),
                        breaks = c(0, 30, 60, 90, 120, 150),
                        labels = c("0", "30", "60", "90", "120", ">150")) +
  labs(color = "Speed\n(km/h)") +
  theme_void()

ggsave(p,
       filename = file.path(brief_figures_dir, "speed_nbo_map.png"),
       height = 5, width = 5)



# hour_sf <- file.path(data_dir, "Sensor Data", "FinalData",
#                      "sensortracing_hourly_individual_files",
#                      "data_and_polyline") %>%
#   list.files(full.names = T) %>%
#   str_subset("2022-06-01|2022-06-02") %>%
#   lapply(readRDS) 
# 
# filtered_list <- Filter(function(df) nrow(df) >= 24, hour_sf)
# 
# hr_sf <- filtered_list %>%
#   bind_rows() %>%
#   arrange(speed_p90) 
# 
# dist <- st_length(hr_sf) %>% as.numeric()
# hr_sf <- hr_sf[dist > 0,]
# hr_sf$id <- 1:nrow(hr_sf)
# 
# hr_sf <- st_intersection(hr_sf, roi_sf)
# 
# ggplot() +
#   geom_sf(data = roi_sf,
#           color = "black",
#           fill = "white") +
#   geom_sf(data = hr_sf %>%
#             dplyr::filter(speed_p90 > 30) %>%
#             dplyr::filter(!(id %in% c(1982,
#                                       1343,
#                                       1913,
#                                       2292,
#                                       2162,
#                                       3092))),
#           aes(color = speed_p90),
#           size = 2) +
#   scale_color_distiller(palette = "Spectral") +
#   theme_void()
# 
# # leaflet() %>%
# #   addTiles() %>%
# #   addPolylines(data = hr_sf,
# #                popup = ~as.character(id))
# 
