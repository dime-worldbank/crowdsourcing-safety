# Move Data to Paper Data Directory

sum_na <- function(x){
  if(length(x) == length(x[is.na(x)])){
    out <- NA
  } else{
    out <- sum(x, na.rm = T)
  }
  return(out)
}

# Load data --------------------------------------------------------------------
#### GPS Sensor Data: Daily
sensor_df <- readRDS(file.path(data_dir, "RawData", "sensor_day.Rds"))

#### Passenger Feedback
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

#### Vehicle information
veh_data_df <- readRDS(file.path(data_dir, "RawData", "gps_vehicle_info.Rds"))

#### Sticker Installation Survey
st_insll_df <- readRDS(file.path(data_dir, "RawData", "gps_vehicle_sticker_install_survey.Rds"))

#### GPS Installation Survey
gps_survey_df <- readRDS(file.path(data_dir, "RawData", "gps_install_survey.Rds"))

#### Vehicle Award Info
award_info_df <- readRDS(file.path(data_dir, "RawData", "vehicle_award_info.Rds"))

# Aggregate Passenger Feedback -------------------------------------------------
#### Date sticker installed (proxy using first survey date)
fb_df <- fb_df %>%
  dplyr::group_by(regno) %>%
  dplyr::mutate(sticker_install_date = min(date)) %>%
  ungroup() %>%
  dplyr::mutate(days_since_install = as.numeric(difftime(date, sticker_install_date,
                                                         units = "days")))

fb_sum_df <- fb_df %>%
  dplyr::filter(regno != "UNKNOWN") %>%
  dplyr::mutate(q_safety_prop_unsafe = (q_safety_rating == "Not safe") | (q_safety_rating == "Not very safe"),
                q_speed_rating_v1_fast = (q_speed_rating_v1 == "Fast") | (q_speed_rating_v1 == "Dangerously fast"),
                q_speed_rating_v1_dfast = (q_speed_rating_v1 == "Dangerously fast"),
                q_speed_rating_v2_fast = q_speed_rating_v2 == "Very fast [80+]") %>%
  group_by(regno) %>%
  dplyr::summarise(n_feedback = n(),
                   n_feedback_1wk = sum(days_since_install <= 7*1),
                   n_feedback_2wk = sum(days_since_install <= 7*2),
                   n_feedback_3wk = sum(days_since_install <= 7*3),
                   n_feedback_4wk = sum(days_since_install <= 7*4),
                   n_feedback_5wk = sum(days_since_install <= 7*5),
                   n_feedback_6wk = sum(days_since_install <= 7*6),
                   n_feedback_7wk = sum(days_since_install <= 7*7),
                   n_feedback_8wk = sum(days_since_install <= 7*8),
                   
                   n_feedback_qr  = sum(response_method == "qr code"),
                   n_feedback_sms = sum(response_method == "shortcode"),
                   
                   comment_driver_sntmt_code_compl_sum = sum_na(comment_driver_sntmt_code_compl),
                   comment_driver_sntmt_code_neg_sum = sum_na(comment_driver_sntmt_code_neg),
                   
                   sentiment_snmtr_driving = mean(sentiment_snmtr[q_comment %>% tolower() %>% str_detect(DRIVING_WORDS)],na.rm = T),
                   
                   across(c(q_safety_rating_num,
                            q_covid_measures_num,
                            q_speed_rating_v1_num,
                            q_speed_rating_v2_num,
                            q_occupancy_num,
                            q_safety_prop_unsafe,
                            q_speed_rating_v1_fast,
                            q_speed_rating_v1_dfast,
                            q_speed_rating_v2_fast,
                            sentiment_snmtr,
                            comment_driver_sntmt_code_compl,
                            comment_driver_sntmt_code_neg), 
                          mean,
                          na.rm=T)) %>%
  ungroup()

# Aggregate Sensor Data --------------------------------------------------------
set_na <- function(x, y){
  x[y == 0] <- NA
  x
}

vars_for_base <- names(sensor_df) %>%
  str_subset("time_over|N_valueg_abov")

for(var in vars_for_base){
  for(base in c("time_over_10kph_secs", "time_over_50kph_secs", "time_over_80kph_secs")){
    
    if(str_detect(var, "time_over"))     prefix <- "prop_"
    if(str_detect(var, "N_valueg_abov")) prefix <- "rate_"
    
    base_short <- base %>% 
      str_replace_all("time_over_", "") %>%
      str_replace_all("_secs", "")
    
    var_short <- var %>% str_replace_all("_secs", "")
    
    sensor_df[[paste0(prefix, var_short, "_base_", base_short)]] <- set_na(sensor_df[[var]] / sensor_df[[base]], sensor_df[[base]])
    
    # N per hour (originally N per second)
    if(str_detect(var, "N_valueg_abov")){
      sensor_df[[paste0(prefix, var_short, "_base_", base_short)]] <-
        sensor_df[[paste0(prefix, var_short, "_base_", base_short)]] * 60 * 60 
    }
    
  }
}

sensor_sum_df <- sensor_df %>%
  # dplyr::mutate(prop_time_over_80kph_base_10kph = set_na(time_over_80kph_secs / time_over_10kph_secs, time_over_10kph_secs),
  #               prop_time_over_80kph_base_50kph = set_na(time_over_80kph_secs / time_over_50kph_secs, time_over_50kph_secs),
  #               
  #               rate_N_valueg_above0_4_base_10kph = set_na(N_valueg_above0_4 / time_over_10kph_secs, time_over_10kph_secs),
  #               rate_N_valueg_above0_4_base_50kph = set_na(N_valueg_above0_4 / time_over_50kph_secs, time_over_50kph_secs),
  #               
  #               rate_N_valueg_above0_5_base_10kph = set_na(N_valueg_above0_5 / time_over_10kph_secs, time_over_10kph_secs),
  #               rate_N_valueg_above0_5_base_50kph = set_na(N_valueg_above0_5 / time_over_50kph_secs, time_over_50kph_secs),
  #               
  #               rate_N_valueg_above1_0_base_10kph = set_na(N_valueg_above1_0 / time_over_10kph_secs, time_over_10kph_secs),
  #               rate_N_valueg_above1_0_base_50kph = set_na(N_valueg_above1_0 / time_over_50kph_secs, time_over_50kph_secs)) %>%
group_by(regno) %>%
  dplyr::summarise(across(where(is.numeric), mean, na.rm = T)) %>%
  ungroup() 

# Merge data -------------------------------------------------------------------
award_info_df <- award_info_df %>%
  distinct(regno, .keep_all = T)

veh_df <- fb_sum_df %>%
  full_join(sensor_sum_df, by = "regno") %>%
  full_join(award_info_df, by = "regno") %>%
  left_join(gps_survey_df, by = "regno")

veh_df$n_feedback[is.na(veh_df$n_feedback) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_1wk[is.na(veh_df$n_feedback_1wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_2wk[is.na(veh_df$n_feedback_2wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_3wk[is.na(veh_df$n_feedback_3wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_4wk[is.na(veh_df$n_feedback_4wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_5wk[is.na(veh_df$n_feedback_5wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_6wk[is.na(veh_df$n_feedback_6wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_7wk[is.na(veh_df$n_feedback_7wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_8wk[is.na(veh_df$n_feedback_8wk) & !is.na(veh_df$award_type)] <- 0

# Make subsets of datasets -----------------------------------------------------
veh_all_df <- veh_df

veh_sticker_df <- veh_df %>%
  dplyr::filter(!is.na(shortcode_on_sticker))

veh_sticker_fdback_df <- veh_df %>%
  dplyr::filter(!is.na(shortcode_on_sticker)) %>%
  dplyr::filter(n_feedback >= 10)

veh_tele_sticker_df <- veh_df %>%
  dplyr::filter(!is.na(speed_mean)) %>%
  dplyr::filter(!is.na(shortcode_on_sticker))

veh_tele_sticker_fdback_df <- veh_tele_sticker_df %>%
  dplyr::filter(n_feedback >= 10)

# Export -----------------------------------------------------------------------
saveRDS(veh_df,
        file.path(data_dir, "FinalData", "vehicle_level_all202.Rds"))

saveRDS(veh_sticker_df,
        file.path(data_dir, "FinalData", "vehicle_level_stickers.Rds"))

saveRDS(veh_sticker_fdback_df,
        file.path(data_dir, "FinalData", "vehicle_level_stickers_suff_feedback.Rds"))

saveRDS(veh_tele_sticker_df,
        file.path(data_dir, "FinalData", "vehicle_level_stickers_telematics.Rds"))

saveRDS(veh_tele_sticker_fdback_df,
        file.path(data_dir, "FinalData", "vehicle_level_stickers_telematics_suff_feedback.Rds"))


