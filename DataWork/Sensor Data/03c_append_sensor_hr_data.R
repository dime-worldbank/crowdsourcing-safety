# Append Hourly Sensor Data

# Functions --------------------------------------------------------------------
read_clean_sensor <- function(path, show_progress = T){
  if(show_progress){
    pb$tick() 
  }
  
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
sensor_files_dataonly <- file.path(sensors_dir, "FinalData", "sensortracing_hourly_individual_files",
                                   "data_only") %>%
  list.files(pattern = "*.Rds",
             full.names = T)

pb <- progress_bar$new(total = length(sensor_files_dataonly))

sensor_df <- sensor_files_dataonly %>%
  map_df(~read_clean_sensor(.)) 

saveRDS(sensor_df, file.path(sensors_dir, "FinalData", "sensortracing_dayhr_dataonly.Rds"))
write_parquet(sensor_df, file.path(sensors_dir, "FinalData", 
                                   "sensortracing_dayhr_dataonly.gz.parquet"), 
              compression = "gzip", compression_level = 5)

# Data and polyline ------------------------------------------------------------
sensor_files_data_polyline <- file.path(sensors_dir, "FinalData", "sensortracing_hourly_individual_files",
                                        "data_and_polyline") %>%
  list.files(pattern = "*.Rds",
             full.names = T)

pb <- progress_bar$new(total = length(sensor_files_data_polyline))

sensor_sf <- sensor_files_data_polyline %>%
  map(function(file_i){
    pb$tick() 

    # Issue appending sf file with data.frame(NULL); here check if zero rows and
    # convert to NULL
    data <- read_clean_sensor(file_i, show_progress = F)
    if(nrow(data) %in% 0){
      out <- NULL
    } else{
      out <- data
    }
    
    return(out)
  }) %>%
  bind_rows()

saveRDS(sensor_sf, file.path(sensors_dir, "FinalData", "sensortracing_dayhr_datapolyline.Rds"))


