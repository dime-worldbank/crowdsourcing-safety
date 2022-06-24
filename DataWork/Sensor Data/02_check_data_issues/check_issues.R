# Check Data Issues

# drvr_feedback_treat_sticker

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))
#wialon_df <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "user_ids.csv"))

# Cleanup ----------------------------------------------------------------------
# Add variables / clean data in way relevant for all analysis
sensor_df <- sensor_df %>%
  mutate(has_speed_data = as.numeric(N_obs_speed > 0))

# Continuous days with no data -------------------------------------------------
# TODO: Add relevant variables (eg, treat status, install date, etc) -- old install date has more issues?
# drvr_feedback_treat_sticker

# regno_clean_i <- "KCZ 750E"
cont_nodata_df <- map_df(unique(sensor_df$regno_clean), function(regno_clean_i){

  sensor_df_i <- sensor_df[sensor_df$regno_clean %in% regno_clean_i,]
  
  sensor_df_i <- sensor_df_i %>%
    arrange(date)
  
  lastest_date_has_speed_data <- sensor_df_i %>%
    tail(1) %>%
    pull(has_speed_data)
  
  if(lastest_date_has_speed_data){
    n_dats_no_speed <- 0
  } else{
    n_dats_no_speed <- sensor_df_i %>%
      ungroup() %>%
      dplyr::mutate(date_max_data = max(date[has_speed_data %in% 1])) %>%
      dplyr::filter(date > date_max_data) %>%
      nrow()
  }
  
  out_df <- data.frame(
    regno_clean = regno_clean_i,
    n_days_no_speed = n_dats_no_speed
  )
  
  out_df$install_date <- sensor_df_i$install_date[1]
  
  return(out_df)
  
})

#View(cont_nodata_df)

cont_nodata_df$n_days_no_speed %>% table()

table(cont_nodata_df$n_days_no_speed > 7)



