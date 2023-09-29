# Append and clean Echo Driving Data

clean_dates <- function(x){
  # Wialon dates are originally in a [day][month][year]_[hour][minute][second]
  # format and originally in UTC. Convert to standard date/time format and 
  # convert to EAT.
  x %>% ymd_hms(tz = "UTC") %>% with_tz(tzone = "Africa/Nairobi")
}

# Load/append data -------------------------------------------------------------
speedings_df <- file.path(sensors_dir, "RawData", "speedings_individual_data") %>%
  list.files(pattern = "*.gz.parquet",
             full.names = T) %>%
  map_df(function(path){
    df <- read_parquet(path)
    
    #if(nrow(df) > 0){
    #  df <- df %>%
    #    dplyr::mutate_at(vars(latitude_begin, longitude_begin,
    #                          latitude_end, longitude_end), . %>% as.character %>% as.numeric)
    #}
    
    return(df)
  })

# Clean data -------------------------------------------------------------------
speedings_clean_df <- speedings_df %>%
  dplyr::mutate(datetime_eat = time_str %>% clean_dates()) %>%
  mutate_at(vars(max_speed, avg_speed, speed_limit, distance), . %>% 
              str_replace_all("km/h|km", "") %>%
              str_squish() %>%
              as.numeric()) %>%
  dplyr::rename(max_speed_kmh = max_speed,
                avg_speed_kmh = avg_speed,
                speed_limit_kmh = speed_limit,
                distance_km = distance) %>%
  separate(duration, c("duration_tmp_hr", "duration_tmp_min", "duration_tmp_sec")) %>%
  mutate_at(vars(contains("duration_tmp_")), as.numeric) %>%
  dplyr::mutate(duration_seconds = duration_tmp_hr*60*60 + duration_tmp_min*60 + duration_tmp_sec) %>%
  dplyr::select(reg_no_id, datetime_eat, latitude, longitude, location, 
                duration_seconds,
                distance_km,
                max_speed_kmh, avg_speed_kmh, speed_limit_kmh)

# Export data ------------------------------------------------------------------
write_parquet(speedings_clean_df, file.path(sensors_dir, "FinalData", "speedings.gz.parquet"), 
              compression = "gzip", compression_level = 5)
saveRDS(speedings_clean_df, file.path(sensors_dir, "FinalData", "speedings.Rds"))


