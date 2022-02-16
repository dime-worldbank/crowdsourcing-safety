# Maps of Vehicles

load_raw_sensor_data <- function(date){
  # Load raw sensor tracing data for a specific date
  
  file.path(sensors_dir, "RawData",
            "sensor_tracing_individual_data",
            date) %>%
    list.files(full.names = T) %>%
    map_df(read_parquet)
}

# Load data --------------------------------------------------------------------
dates_to_load <- seq(from = ymd("2021-12-11"),
                     to = ymd("2021-12-17"),
                     by = 1)

sensor_df <- map_df(dates_to_load, load_sensor_data)

nrow(sensor_df)

# Grab maximum latitude and longitude at a location per vehicle
sensor_df <- sensor_df %>%
  dplyr::filter(!is.na(latitude)) %>%
  group_by(latitude, longitude, reg_no_id) %>%
  slice_max(order_by = speed, n = 1) %>%
  ungroup() 

# Map --------------------------------------------------------------------------
# Truncate speed
sensor_df <- sensor_df %>%
  dplyr::mutate(speed_trunc = case_when(
    speed >= 120 ~ 120,
    TRUE ~ speed
  ))

pal <- colorNumeric(
  palette = "RdYlGn",
  domain = sensor_df$speed_trunc,
  reverse = T)

sensor_df %>%
  slice_sample(n = 100000) %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.35)) %>%
  #addProviderTiles(providers$Stamen.TonerLabels) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addCircles(color = ~pal(speed_trunc),
             weight = 1,
             opacity = 1) %>%
  addLegend("topright", 
            pal = pal,
            values = sensor_df$speed_trunc,
            title = "Speed (km/hr)",
            opacity = 1
  )


