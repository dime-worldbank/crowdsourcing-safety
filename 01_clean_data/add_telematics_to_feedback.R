# Add Telematics to Survey

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "RawData", "passenger_feedback.Rds"))
sensor_day_df <- readRDS(file.path(data_dir, "RawData", "sensor_day.Rds"))

fb_df <- fb_df[fb_df$date %in% sensor_day_df$date,]

regno_df <- sensor_day_df %>%
  distinct(reg_no_id, regno)

fb_df <- fb_df %>%
  left_join(regno_df, by = "regno")

# Add telematics ---------------------------------------------------------------
speed_all_df <- map_df(1:nrow(fb_df), function(i){
  if( (i %% 100) == 0 )  message(i)
  
  #### Grab survey i
  fb_df_i <- fb_df[i,]
  
  #### Load sensor dataframe
  survey_datetime <- fb_df_i$datetime
  
  sensor_df <- file.path(data_all_dir, "Sensor Data", "RawData", "sensor_tracing_individual_data",
                         fb_df_i$date, 
                         paste0("sensortracing_",fb_df_i$reg_no_id,"_",fb_df_i$date,".gz.parquet")) %>%
    read_parquet()
  
  if(nrow(sensor_df) > 0){
    
    #### Distance since survey
    sensor_df <- sensor_df %>%
      dplyr::mutate(datetime = time_str %>% 
                      ymd_hms() %>%
                      #ymd_hms(tz = "UTC") %>%
                      with_tz(tzone = "Africa/Nairobi"))
    
    sensor_df$seconds_since_survey <- difftime(sensor_df$datetime, 
                                               survey_datetime, 
                                               units = "secs")
    sensor_df <- sensor_df %>%
      dplyr::mutate(seconds_since_survey_abs = abs(seconds_since_survey))
    
    #### Average speeds
    #mins_before <- 1
    
    speed_df <- map_dfc(c(1, 5, 10, 15, 30, 60), function(mins_before){
      
      speed_avg_df <- sensor_df %>%
        dplyr::filter(seconds_since_survey <= 0,
                      seconds_since_survey >= -60*mins_before) %>%
        dplyr::summarise(n = n(),
                         speed_avg = mean(speed)) %>%
        ungroup()
      
      names(speed_avg_df) <- paste0(names(speed_avg_df), "_", mins_before, "min_before")
      
      return(speed_avg_df)
    })
    
    speed_df$uid <- fb_df_i$uid 
    
  } else{
    # If sensor dataframe has 0 observations
    speed_df <- data.frame(NULL)
  }
  
  return(speed_df)
})


fb_data_df <- speed_all_df %>%
  left_join(fb_df, by = "uid")

fb_data_df %>%
  dplyr::filter(speed_avg_1min_before > 0) %>%
  ggplot() +
  geom_boxplot(aes(x = speed_avg_1min_before,
                   y = q_speed_rating_v2))


fb_data_df %>%
  dplyr::filter(speed_avg_5min_before > 0) %>%
  ggplot() +
  geom_boxplot(aes(x = speed_avg_5min_before,
                   y = q_speed_rating_v2))

fb_data_df %>%
  dplyr::filter(speed_avg_10min_before > 0) %>%
  ggplot() +
  geom_boxplot(aes(x = speed_avg_10min_before,
                   y = q_speed_rating_v2))


fb_data_df %>%
  dplyr::filter(!is.na(speed_avg_5min_before)) %>%
  group_by(q_speed_rating_v2) %>%
  dplyr::summarise(speed_avg_5min_before = mean(speed_avg_5min_before),
                   n = n())
