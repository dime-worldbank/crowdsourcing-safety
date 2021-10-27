# Append Data Downloaded from Wialon System

# Data originally downloaded into multiple files (across dates and units). Append
# everything into one dataset

clean_dates <- function(x){
  # Wialon dates are originally in a [day][month][year]_[hour][minute][second]
  # format and originally in UTC. Convert to standard date/time format and 
  # convert to EAT.
  x %>% dmy_hms(tz = "UTC") %>% with_tz(tzone = "Africa/Nairobi")
}

# Echo Driving -----------------------------------------------------------------
#### Load Data
echodriving <- file.path(sensors_dir, "RawData", "echo_driving_individual_data") %>%
  list.files(pattern = "*.gz.parquet",
             full.names = T) %>%
  map_df(function(path){
    df <- read_parquet(path)
    
    if(nrow(df) > 0){
      df <- df %>%
        dplyr::mutate_at(vars(latitude_begin, longitude_begin,
                              latitude_end, longitude_end), . %>% as.character %>% as.numeric)
    }
    
    return(df)
  })


#### Clean Data
echodriving_clean <- echodriving %>%
  dplyr::mutate(begin_datetime_str = begin_datetime_str %>% clean_dates(),
                end_datetime_str = end_datetime_str %>% clean_dates()) %>%
  dplyr::mutate_at(vars(value, max_speed, distance), ~.x %>% 
                     str_replace_all("[[:alpha:]]|[[/]]", "") %>%
                     str_squish() %>%
                     as.numeric) %>%
  dplyr::rename(max_speed_kmhr = max_speed,
                distance_km = distance,
                value_g = value,
                begin_datetime_eat = begin_datetime_str,
                end_datetime_eat = end_datetime_str)

#### Save Data
write_parquet(echodriving_clean, file.path(sensors_dir, "FinalData", "echodriving.gz.parquet"), 
              compression = "gzip", compression_level = 5)

# Sensor Tracing ---------------------------------------------------------------
#### Load Data
sensortracing <- file_dir <- file.path(sensors_dir, "RawData", "sensor_tracing_individual_data") %>%
  list.files(pattern = "*.gz.parquet",
             full.names = T,
             recursive = T) %>%
  map_df(function(path){
    
    ## Load Data
    df <- read_parquet(path) 
    
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
    
  })

#### Clean Data
sensortracing_clean <- sensortracing %>%
  dplyr::mutate_at(vars(speed), ~.x %>% 
                     str_replace_all("[[:alpha:]]|[[/]]", "") %>%
                     str_squish() %>%
                     as.numeric) %>%
  dplyr::rename(speed_kmhr = speed) %>%
  dplyr::select(-diff_num_vars_rawdata) %>%
  dplyr::mutate(datetime_eat = time_str %>% 
                  ymd_hms() %>%
                  with_tz(tzone = "Africa/Nairobi")) %>%
  dplyr::rename(time_str_raw_utc = time_str)

#### Save Data
write_parquet(sensortracing_clean, file.path(sensors_dir, "FinalData", "sensortracing.gz.parquet"), 
              compression = "gzip", compression_level = 5)

