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
  out_df$route        <- sensor_df_i$route[1]
  out_df$sacco        <- sensor_df_i$sacco[1]
  
  return(out_df)
  
})

#View(cont_nodata_df)

# good_df <- cont_nodata_df[cont_nodata_df$n_days_no_speed %in% 0,]
# good_df <- good_df %>%
#   dplyr::select(regno_clean)
# 
# write.csv(good_df, "~/Desktop/regnos_with_data.csv", row.names = F)

cont_nodata_df$n_days_no_speed %>% table()

table(cont_nodata_df$n_days_no_speed >= 7)
table(cont_nodata_df$n_days_no_speed >= 14)
table(cont_nodata_df$n_days_no_speed >= 21)
table(cont_nodata_df$n_days_no_speed >= 31)
table(cont_nodata_df$n_days_no_speed >= 50)

issues_to_check <- cont_nodata_df %>%
  filter(n_days_no_speed >= 31) %>%
  arrange(-n_days_no_speed) %>%
  dplyr::select(regno_clean, sacco, route, n_days_no_speed)

write_csv(issues_to_check, "~/Desktop/sensor_issues.csv")

