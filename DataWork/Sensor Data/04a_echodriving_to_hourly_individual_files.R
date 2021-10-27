# Aggregate Echo Tracing

# Aggregate individual echo driving files; save aggregate file for each date

clean_dates <- function(x){
  # Wialon dates are originally in a [day][month][year]_[hour][minute][second]
  # format and originally in UTC. Convert to standard date/time format and 
  # convert to EAT.
  x %>% dmy_hms(tz = "UTC") %>% with_tz(tzone = "Africa/Nairobi")
}


# Load data --------------------------------------------------------------------
raw_echo_files <- file.path(sensors_dir, "RawData", "echo_driving_individual_data") %>%
  list.files(pattern = "*.parquet",
             recursive = T,
             full.names = T)

# If overwrite, delete ---------------------------------------------------------
if(OVERWRITE_EXTRACTED_DATA){
  tmp <- file.path(sensors_dir, "FinalData", "echodriving_hourly_individual_files") %>%
    list.files(pattern = "*.Rds",
               full.names = T) %>%
    lapply(file.remove)
}

# Loop through and clean, export -----------------------------------------------
raw_echo_filepath_i = raw_echo_files[100]
for(raw_echo_filepath_i in raw_echo_files){
  
  ## Path for output
  raw_echo_file_i <- raw_echo_filepath_i %>% 
    str_replace_all(".*/", "") %>%
    str_replace_all(".gz.parquet", ".Rds")
  
  OUT_PATH <- file.path(sensors_dir, "FinalData", "echodriving_hourly_individual_files",
                        raw_echo_file_i)
  
  ## Check if already extracted
  if(!file.exists(OUT_PATH) | OVERWRITE_EXTRACTED_DATA){
    print(raw_echo_file_i)
    
    ## Load and aggregate
    echo_df <- read_parquet(raw_echo_filepath_i)
    
    if(nrow(echo_df) %in% 0){
      echo_clean_df <- data.frame(NULL)
    } else{
      
      #### To numeric
      echo_clean_df <- echo_df %>%
        dplyr::mutate_at(vars(latitude_begin, longitude_begin,
                              latitude_end, longitude_end), . %>% as.character %>% as.numeric)
      
      #### Cleanup variables and rename
      echo_clean_df <- echo_clean_df %>%
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
      # Sometime see two "turn" violation (eg) at the same time with different values;
      # we take the higher value
      echo_clean_df <- echo_clean_df %>%
        dplyr::mutate(violation = violation %>% tolower()) %>%
        dplyr::mutate(violation = case_when(
          violation %>% str_detect("brake") ~ "brake",
          violation %>% str_detect("acceleration") ~ "acceleration",
          violation %>% str_detect("turn") ~ "turn"
        )) %>%
        arrange(desc(value_g)) %>%
        distinct(reg_no_id, begin_datetime_eat, .keep_all = T)
      
      #### Aggregate
      echo_clean_df <- echo_clean_df %>%
        dplyr::mutate(datetime_eat = begin_datetime_eat %>% floor_date(unit = "hour")) %>%
        dplyr::group_by(reg_no_id, reg_no, violation, datetime_eat) %>%
        dplyr::summarise(N_violation = n(),
                         N_valueg_above0_1 = sum(value_g > 0.1),
                         N_valueg_above0_2 = sum(value_g > 0.2),
                         N_valueg_above0_3 = sum(value_g > 0.3),
                         N_valueg_above0_4 = sum(value_g > 0.4),
                         N_valueg_above0_5 = sum(value_g > 0.5),
                         N_valueg_above1_0 = sum(value_g > 1.0)) %>%
        ungroup() %>%
        pivot_wider(id_cols = c(reg_no_id, reg_no, datetime_eat),
                    values_from = c(N_violation,
                                    N_valueg_above0_1,
                                    N_valueg_above0_2,
                                    N_valueg_above0_3,
                                    N_valueg_above0_4,
                                    N_valueg_above0_5,
                                    N_valueg_above1_0),
                    names_from = violation) %>%
        dplyr::mutate_at(vars(contains("N_")), ~ replace_na(., 0)) %>%
        dplyr::mutate(N_violation = N_violation_acceleration +
                        N_violation_brake +
                        N_violation_turn,
                      
                      N_valueg_above0_1 = N_valueg_above0_1_acceleration + 
                        N_valueg_above0_1_brake + 
                        N_valueg_above0_1_turn,
                      
                      N_valueg_above0_2 = N_valueg_above0_2_acceleration + 
                        N_valueg_above0_2_brake + 
                        N_valueg_above0_2_turn,
                      
                      N_valueg_above0_3 = N_valueg_above0_3_acceleration + 
                        N_valueg_above0_3_brake + 
                        N_valueg_above0_3_turn,
                      
                      N_valueg_above0_4 = N_valueg_above0_4_acceleration + 
                        N_valueg_above0_4_brake + 
                        N_valueg_above0_4_turn,
                      
                      N_valueg_above0_5 = N_valueg_above0_5_acceleration + 
                        N_valueg_above0_5_brake + 
                        N_valueg_above0_5_turn,
                      
                      N_valueg_above1_0 = N_valueg_above1_0_acceleration + 
                        N_valueg_above1_0_brake + 
                        N_valueg_above1_0_turn
        )
      
      
    }
    
    saveRDS(echo_clean_df, OUT_PATH)
    
  }
}