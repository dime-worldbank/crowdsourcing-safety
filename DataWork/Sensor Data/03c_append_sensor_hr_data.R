# Append Hourly Sensor Data

read_clean_sensor <- function(path){
  
  ## Load Data
  df <- readRDS(path) 
  
  ## Fix variables (make consistent types for appending)
  if(!is.null(df$speed[1])){
    df <- df %>%
      dplyr::mutate(speed = speed %>% as.character %>% as.numeric)
  }
  
  if(!is.null(df$latitude[1])){
    df <- df %>%
      dplyr::mutate(latitude = latitude %>% as.character %>% as.numeric)
  }
  
  if(!is.null(df$longitude[1])){
    df <- df %>%
      dplyr::mutate(longitude = longitude %>% as.character %>% as.numeric)
  }
  
  if(!is.null(df$reg_no_id[1])){
    df <- df %>%
      dplyr::mutate(reg_no_id = reg_no_id %>% as.character)
  }
  
  return(df)
}

# Data only --------------------------------------------------------------------
sensor_df <- file.path(sensors_dir, "FinalData", "sensortracing_hourly_individual_files",
                       "data_only") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(read_clean_sensor) 

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
    data <- read_clean_sensor(file_i)
    if(nrow(data) %in% 0){
      out <- NULL
    } else{
      out <- data
    }
    
    return(out)
  }) %>%
  bind_rows()

saveRDS(sensor_sf, file.path(sensors_dir, "FinalData", "sensortracing_dayhr_datapolyline.Rds"))


