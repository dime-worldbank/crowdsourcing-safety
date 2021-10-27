# Append Hourly Sensor Data

# Data only --------------------------------------------------------------------
sensor_df <- file.path(sensors_dir, "FinalData", "sensortracing_hourly_individual_files",
                       "data_only") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS) 
saveRDS(sensor_df, file.path(sensors_dir, "FinalData", "sensortracing_dayhr_dataonly.Rds"))
write_parquet(sensor_df, file.path(sensors_dir, "FinalData", 
                                   "sensortracing_dayhr_dataonly.gz.parquet"), 
              compression = "gzip", compression_level = 5)

# Data and polyline ------------------------------------------------------------
sensor_sf <- file.path(sensors_dir, "FinalData", "sensortracing_hourly_individual_files",
                       "data_and_polyline") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  lapply(function(file_i){
    
    # Issue appending sf file with data.frame(NULL); here check if zero rows and
    # convert to NULL
    data <- readRDS(file_i)
    if(nrow(data) %in% 0){
      out <- NULL
    } else{
      out <- data
    }
    
    return(out)
  }) %>%
  bind_rows()

saveRDS(sensor_sf, file.path(sensors_dir, "FinalData", "sensortracing_dayhr_datapolyline.Rds"))


