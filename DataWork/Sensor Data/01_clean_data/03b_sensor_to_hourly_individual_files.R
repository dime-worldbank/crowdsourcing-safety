# Aggregate Sensor Tracing

# Aggregate individual sensor tracing files; save aggregate file for each
# sensor and date

distHaversine_checkna <- function(p1, p2){
  
  if(Inf %in% p1){
    out <- NA
  } else{
    out <- distHaversine(p1, p2)
  }
  
  return(out)
}

# Raw Files --------------------------------------------------------------------
raw_sensor_files <- file.path(sensors_dir, "RawData", "sensor_tracing_individual_data") %>%
  list.files(pattern = "*.parquet",
             recursive = T,
             full.names = T)

# If overwrite, delete ---------------------------------------------------------
if(OVERWRITE_EXTRACTED_DATA){
  tmp <- file.path(sensors_dir, "FinalData", "sensortracing_hourly_individual_files",
                   "data_only") %>%
    list.files(pattern = "*.Rds",
               full.names = T) %>%
    lapply(file.remove)
  
  tmp <- file.path(sensors_dir, "FinalData", "sensortracing_hourly_individual_files",
                   "data_and_polyline") %>%
    list.files(pattern = "*.Rds",
               full.names = T) %>%
    lapply(file.remove)
}

for(raw_sensor_filepath_i in raw_sensor_files){
  
  ## Path for output
  raw_sensor_file_i <- raw_sensor_filepath_i %>% 
    str_replace_all(".*/", "") %>%
    str_replace_all(".gz.parquet", ".Rds")
  
  OUT_PATH_GEO <- file.path(sensors_dir, "FinalData", "sensortracing_hourly_individual_files",
                            "data_and_polyline", raw_sensor_file_i)
  
  OUT_PATH_DATA <- file.path(sensors_dir, "FinalData", "sensortracing_hourly_individual_files",
                             "data_only", raw_sensor_file_i)
  
  ## Check if already extracted
  if(!file.exists(OUT_PATH_GEO) | !file.exists(OUT_PATH_DATA) | OVERWRITE_EXTRACTED_DATA){
    print(raw_sensor_file_i)
    
    ## Load and aggregate
    sensor_df <- read_parquet(raw_sensor_filepath_i)
    
    if(nrow(sensor_df) %in% 0){
      polyline_sf <- data.frame(NULL)
      only_data_df <- data.frame(NULL)
    } else{
      #### Prep Variables
      sensor_df <- sensor_df %>%
        dplyr::mutate(datetime_eat = time_str %>% 
                        ymd_hms() %>%
                        with_tz(tzone = "Africa/Nairobi")) %>%
        dplyr::filter(!is.na(datetime_eat)) %>%
        dplyr::mutate(datetime_eat = floor_date(datetime_eat, unit = "hour"),
                      hour = datetime_eat %>% hour())
      
      #### Aggregate
      sensor_sum_df <- sensor_df %>%
        dplyr::filter(!is.na(datetime_eat),
                      !is.na(speed)) %>%
        group_by(reg_no, reg_no_id, datetime_eat, hour) %>%
        dplyr::summarise(speed_min = min(speed),
                         speed_p05 = quantile(speed, probs = 0.05) %>% as.numeric(),
                         speed_p10 = quantile(speed, probs = 0.10) %>% as.numeric(),
                         speed_p15 = quantile(speed, probs = 0.15) %>% as.numeric(),
                         speed_p25 = quantile(speed, probs = 0.25) %>% as.numeric(),
                         speed_p50 = quantile(speed, probs = 0.50) %>% as.numeric(),
                         speed_p75 = quantile(speed, probs = 0.75) %>% as.numeric(),
                         speed_p85 = quantile(speed, probs = 0.85) %>% as.numeric(),
                         speed_p90 = quantile(speed, probs = 0.90) %>% as.numeric(),
                         speed_p95 = quantile(speed, probs = 0.95) %>% as.numeric(),
                         speed_max = max(speed),
                         speed_mean = mean(speed),
                         N_speed_over_0   = sum(speed > 0),
                         N_speed_over_25   = sum(speed > 25),
                         N_speed_over_50  = sum(speed > 50),
                         N_speed_over_80  = sum(speed > 80),
                         N_speed_over_85  = sum(speed > 85),
                         N_speed_over_90  = sum(speed > 90),
                         N_speed_over_95  = sum(speed > 95),
                         N_speed_over_100 = sum(speed > 100),
                         N_speed_over_110 = sum(speed > 110),
                         N_speed_over_120 = sum(speed > 120),
                         N_speed_over_130 = sum(speed > 130),
                         N_speed_over_150 = sum(speed > 150),
                         distance_minmax_latlon_km = distHaversine_checkna(p1 = c(min(longitude, na.rm=T),
                                                                                  min(latitude, na.rm=T)),
                                                                           p2 = c(max(longitude, na.rm=T),
                                                                                  max(latitude, na.rm=T)))/1000,
                         latitude = median(latitude, na.rm=T),
                         longitude = median(longitude, na.rm=T),
                         N_obs_speed = n()) %>%
        ungroup()
      
      #### Polyline
      sensor_latlon_df <- sensor_df %>%
        dplyr::filter(!is.na(latitude),
                      !is.na(longitude))
      
      if(nrow(sensor_latlon_df) %in% 0){
        polyline_sf <- data.frame(NULL)
        only_data_df <- sensor_sum_df
      } else{
        
        polyline_sf <- sf_linestring(sensor_latlon_df, 
                                     x = "longitude", 
                                     y = "latitude",
                                     linestring_id = "hour")
        st_crs(polyline_sf) <- 4326 # CRS("+init=epsg:4326") 
        
        #### Merge
        polyline_sf <- merge(polyline_sf, sensor_sum_df, by = "hour")
        
        #### Add distance
        polyline_sf$distance_km <- as.numeric(st_length(polyline_sf) / 1000)
        
        #### Cleanup
        only_data_df <- polyline_sf
        only_data_df$geometry <- NULL
        only_data_df <- only_data_df %>% as.data.frame()
      }
    }
    
    ## Export
    saveRDS(polyline_sf,  OUT_PATH_GEO)
    saveRDS(only_data_df, OUT_PATH_DATA)
    
    rm(polyline_sf)
    rm(only_data_df)
  }
}


