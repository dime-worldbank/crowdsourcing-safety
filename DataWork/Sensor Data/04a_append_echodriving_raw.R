# Append and clean Echo Driving Data

clean_dates <- function(x){
  # Wialon dates are originally in a [day][month][year]_[hour][minute][second]
  # format and originally in UTC. Convert to standard date/time format and 
  # convert to EAT.
  x %>% dmy_hms(tz = "UTC") %>% with_tz(tzone = "Africa/Nairobi")
}

# Load/append data -------------------------------------------------------------
survey_df <- readRDS(file.path(sensor_install_survey_dir, "FinalData", 
                               "psv_sensor_installation_clean.Rds"))

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

# Clean data -------------------------------------------------------------------
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

#### Violation and unique
# Sometime see two "turn" violation at the same time with different values;
# we take the higher value
echodriving_clean <- echodriving_clean %>%
  dplyr::mutate(violation = violation %>% tolower()) %>%
  dplyr::mutate(violation = case_when(
    violation %>% str_detect("brake") ~ "brake",
    violation %>% str_detect("acceleration") ~ "acceleration",
    violation %>% str_detect("turn") ~ "turn"
  )) %>%
  arrange(desc(value_g)) %>%
  distinct(reg_no_id, begin_datetime_eat, .keep_all = T)

#### Merge in Survey Data
clean_reg_no <- function(x){
  x %>%
    as.character() %>%
    str_replace_all(" ", "") %>%
    tolower() %>%
    str_sub(-7,-1)
}

echodriving_clean <- echodriving_clean %>%
  mutate(regno_clean = reg_no %>% clean_reg_no())

echodriving_clean <- echodriving_clean %>%
  left_join(survey_df, by = "regno_clean")

# Export data ------------------------------------------------------------------
write_parquet(echodriving_clean, file.path(sensors_dir, "FinalData", "echodriving.gz.parquet"), 
              compression = "gzip", compression_level = 5)
saveRDS(echodriving_clean, file.path(sensors_dir, "FinalData", "echodriving.Rds"))


