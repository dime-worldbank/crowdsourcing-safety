# GPS Figures

library(geodata)
library(h3jsr)
library(h3)

# Load data --------------------------------------------------------------------
veh_df    <- readRDS(file.path(ap_data_dir, "FinalData", "vehicle_level_all202.Rds"))
sensor_df <- readRDS(file.path(ap_data_dir, "RawData", "sensor_day.Rds"))

# Speed map --------------------------------------------------------------------
# getData function to load a polygon of Ghana
roi_sf <- gadm(country = "KEN", level=0, path = tempdir()) |> st_as_sf()
nbo_sf <- gadm(country = "KEN", level=1, path = tempdir()) |> st_as_sf()
nbo_sf <- nbo_sf[nbo_sf$NAME_1 %in% "Nairobi",]

#### Grab speeds
if(F){
  speed_sf <- map_df(seq.Date(from = ymd("2022-01-01"),
                              to = ymd("2022-12-31"),
                              by = 1),
                     function(date_i){
                       
                       print(date_i)
                       
                       #date_i <- "2022-06-01"
                       
                       speed_sf <- file.path(data_dir, "Sensor Data", "RawData", "sensor_tracing_individual_data",
                                             date_i) %>%
                         list.files(pattern = "*.parquet",
                                    full.names = T) %>%
                         map_df(read_parquet) %>%
                         
                         dplyr::filter(speed > 0) %>%
                         arrange(speed) %>%
                         dplyr::mutate(id = 1:n()) %>%
                         
                         st_as_sf(coords = c("longitude", "latitude"),
                                  crs = 4326) 
                       
                       speed_sf$h3 <- point_to_cell(speed_sf, res = 11)
                       speed_sf <- cbind(speed_sf, st_coordinates(speed_sf))
                       
                       speed_h3_df <- speed_sf %>%
                         st_drop_geometry() %>%
                         group_by(h3) %>%
                         dplyr::summarise(speed = mean(speed),
                                          longitude = median(X),
                                          latitude = median(Y)) %>%
                         ungroup() %>%
                         dplyr::mutate(id = 1:n())
                       
                       speed_h3_df$date <- date_i
                       
                       return(speed_h3_df)
                       
                     })
  
  saveRDS(speed_sf, file.path(dropbox_dir, "Policy Brief", "temp_data", "speeds_h3.Rds"))
} else{
  speed_sf <- readRDS(file.path(dropbox_dir, "Policy Brief", "temp_data", "speeds_h3.Rds"))
}

speed_sf$h3_large <- get_parent(speed_sf$h3, res = 8)

# Kenya ------------------------------------------------------------------------
speed_h3_df <- speed_sf %>%
  group_by(h3_large) %>%
  dplyr::summarise(speed = mean(speed), 
                   latitude = median(latitude), 
                   longitude = median(longitude)) %>%
  ungroup() %>%
  dplyr::rename(id = h3_large)

speed_h3_sf <- st_as_sf(speed_h3_df, coords = c("longitude", "latitude"),
                        crs = 4326) %>%
  st_intersection(roi_sf) 
speed_h3_df <- speed_h3_df[speed_h3_df$id %in% speed_h3_sf$id,]


# Nairobi ----------------------------------------------------------------------
speed_h3_nbo_df <- speed_sf %>%
  group_by(h3) %>%
  dplyr::summarise(speed = mean(speed), 
                   latitude = median(latitude), 
                   longitude = median(longitude)) %>%
  ungroup() %>%
  dplyr::rename(id = h3)

speed_h3_nbo_sf <- st_as_sf(speed_h3_nbo_df, coords = c("longitude", "latitude"),
                        crs = 4326) %>%
  st_intersection(nbo_sf) 
speed_h3_nbo_df <- speed_h3_nbo_df[speed_h3_nbo_df$id %in% speed_h3_nbo_sf$id,]

# Map --------------------------------------------------------------------------
#### Speed limit
speed_h3_df$speed_adj <- speed_h3_df$speed
speed_h3_df$speed_adj[speed_h3_df$speed_adj >= 150] <- 150

cities_df <- bind_rows(
  data.frame(name = "Nairobi", lat = -1.286389, lon = 36.817222),
  data.frame(name = "Eldoret", lat = 0.516667, lon = 35.283333),
  data.frame(name = "Marsabit", lat = 2.333333, lon = 37.983333),
  data.frame(name = "Meru", lat = 0.05, lon = 37.65),
  data.frame(name = "Mombasa", lat = -4.05, lon = 39.666667),
  data.frame(name = "Nakuru", lat = -0.3, lon = 36.066667),
  data.frame(name = "Namanga", lat = -2.55, lon = 36.783333),
  data.frame(name = "Narok", lat = -1.083333, lon = 35.866667)
)

source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")
speed_h3_sf <- h3_to_geo_boundary_sf(speed_h3_df$id)
speed_h3_sf <- bind_cols(speed_h3_sf, speed_h3_df)
speed_h3_buff_sf <- st_buffer_chunks(speed_h3_sf, dist = 150, chunk_size = 100)

#### Map, Kenya
p <- ggplot() +
  geom_sf(data = roi_sf,
          color = "black",
          fill = "gray30") +
  geom_sf(data = speed_h3_buff_sf,
          fill = "black",
          color = "black") +
  geom_sf(data = speed_h3_sf,
          aes(fill = speed_adj,
              color = speed_adj)) +
  # geom_point(aes(x = longitude, y = latitude),
  #            color = "black",
  #            size = 0.5) +
  # geom_point(aes(x = longitude, y = latitude,
  #                color = speed_adj),
  #            size = 0.3) +
  geom_point(data = cities_df,
             aes(x = lon,
                 y = lat),
             size = 0.5) +
  geom_text(data = cities_df,
            aes(x = lon,
                y = lat,
                label = name),
            size = 3,
            color = c("white","white","white","white","black","white","black","white"),
            nudge_x = c(-0.4, 0,    0.6,  0.4, 0.65,-0.35,-0.3,-0.3),
            nudge_y = c(-0.2, 0.2, 0,    0,   0,   0.15,-0.15,-0.12)) +
  scale_color_distiller(palette = "Spectral",
                        limits = c(0, 150),
                        breaks = c(0, 30, 60, 90, 120, 150),
                        labels = c("0", "30", "60", "90", "120", ">150")) +
  scale_fill_distiller(palette = "Spectral",
                        limits = c(0, 150),
                        breaks = c(0, 30, 60, 90, 120, 150),
                        labels = c("0", "30", "60", "90", "120", ">150")) +
  labs(color = "Speed\n(km/h)",
       fill = "Speed\n(km/h)") + 
  theme_void()

ggsave(p,
       filename = file.path(brief_figures_dir, "speed_map.png"),
       height = 5, width = 5)

# Map Nairobi ------------------------------------------------------------------

#### Speed limit
speed_h3_nbo_sf$speed_adj <- speed_h3_nbo_sf$speed
speed_h3_nbo_sf$speed_adj[speed_h3_nbo_sf$speed_adj >= 150] <- 150

source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")
speed_h3_nbo_sf <- h3_to_geo_boundary_sf(speed_h3_nbo_df$id)
speed_h3_nbo_sf <- bind_cols(speed_h3_nbo_sf, speed_h3_nbo_df)
speed_h3_nbo_buff_sf <- st_buffer_chunks(speed_h3_nbo_sf, dist = 20, chunk_size = 100)

#### Map, Kenya
p <- ggplot() +
  geom_sf(data = nbo_sf,
          color = "black",
          fill = "gray30") +
  geom_sf(data = speed_h3_nbo_buff_sf,
          fill = "black",
          color = "black") +
  geom_sf(data = speed_h3_nbo_sf,
          aes(fill = speed_adj,
              color = speed_adj)) +
  # geom_point(aes(x = longitude, y = latitude),
  #            color = "black",
  #            size = 0.5) +
  # geom_point(aes(x = longitude, y = latitude,
  #                color = speed_adj),
  #            size = 0.3) +
  scale_color_distiller(palette = "Spectral",
                        limits = c(0, 150),
                        breaks = c(0, 30, 60, 90, 120, 150),
                        labels = c("0", "30", "60", "90", "120", ">150")) +
  scale_fill_distiller(palette = "Spectral",
                       limits = c(0, 150),
                       breaks = c(0, 30, 60, 90, 120, 150),
                       labels = c("0", "30", "60", "90", "120", ">150")) +
  labs(color = "Speed\n(km/h)",
       fill = "Speed\n(km/h)") + 
  theme_void()

ggsave(p,
       filename = file.path(brief_figures_dir, "speed_nbo_map.png"),
       height = 5, width = 5, dpi = 1000)


#### Map, Nairobi
# speed_h3_nbo_df$speed_adj <- speed_h3_nbo_df$speed
# speed_h3_nbo_df$speed_adj[speed_h3_nbo_df$speed_adj >= 150] <- 150
# 
# p <- speed_h3_nbo_df %>%
#   dplyr::filter(speed <= 150) %>%
#   ggplot() +
#   geom_sf(data = nbo_sf,
#           color = "black",
#           fill = "gray95") +
#   geom_point(aes(x = longitude, y = latitude),
#              color = "black",
#              size = 0.8) +
#   geom_point(aes(x = longitude, y = latitude,
#                  color = speed_adj),
#              size = 0.5) +
#   scale_color_distiller(palette = "Spectral",
#                         limits = c(0, 150),
#                         breaks = c(0, 30, 60, 90, 120, 150),
#                         labels = c("0", "30", "60", "90", "120", ">150")) +
#   labs(color = "Speed\n(km/h)") +
#   theme_void()
# 
# ggsave(p,
#        filename = file.path(brief_figures_dir, "speed_nbo_map.png"),
#        height = 5, width = 5)
# 
# 
# 
# 
