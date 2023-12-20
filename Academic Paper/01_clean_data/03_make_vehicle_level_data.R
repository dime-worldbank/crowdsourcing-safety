# Move Data to Paper Data Directory

# Load data --------------------------------------------------------------------
#### GPS Sensor Data: Daily
sensor_df <- readRDS(file.path(data_dir, "RawData", "sensor_day.Rds"))

#### Passenger Feedback
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

#### Vehicle information
veh_data_df <- readRDS(file.path(data_dir, "RawData", "gps_vehicle_info.Rds"))

#### Sticker Installation Survey
st_insll_df <- readRDS(file.path(data_dir, "RawData", "gps_vehicle_sticker_install_survey.Rds"))

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
  dplyr::filter(regno != "UNKNOWN",
                ptn_cheating_fill %in% 0) %>%
  dplyr::mutate(unsafe = (q_safety_rating == "Not safe") | (q_safety_rating == "Not very safe"),
                fast = q_speed_rating_v2 == "Very fast [80+]") %>%
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
                   
                   q_safety_prop_unsafe = mean(unsafe, na.rm = T),
                   q_speed_rating_v2_fast = mean(fast, na.rm = T),
 
                   across(c(q_safety_rating_num,
                            q_covid_measures_num,
                            q_speed_rating_v1_num,
                            q_speed_rating_v2_num,
                            q_occupancy_num,
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

sensor_sum_df <- sensor_df %>%
  dplyr::mutate(prop_time_over_80kph_base_10kph = set_na(time_over_80kph_secs / time_over_10kph_secs, time_over_10kph_secs),
                prop_time_over_80kph_base_50kph = set_na(time_over_80kph_secs / time_over_50kph_secs, time_over_50kph_secs),
                
                rate_N_valueg_above0_4_base_10kph = set_na(N_valueg_above0_4 / time_over_10kph_secs, time_over_10kph_secs),
                rate_N_valueg_above0_4_base_50kph = set_na(N_valueg_above0_4 / time_over_50kph_secs, time_over_50kph_secs),
                
                rate_N_valueg_above0_5_base_10kph = set_na(N_valueg_above0_5 / time_over_10kph_secs, time_over_10kph_secs),
                rate_N_valueg_above0_5_base_50kph = set_na(N_valueg_above0_5 / time_over_50kph_secs, time_over_50kph_secs),
                
                rate_N_valueg_above1_0_base_10kph = set_na(N_valueg_above1_0 / time_over_10kph_secs, time_over_10kph_secs),
                rate_N_valueg_above1_0_base_50kph = set_na(N_valueg_above1_0 / time_over_50kph_secs, time_over_50kph_secs)) %>%
  group_by(regno_clean) %>%
  dplyr::summarise(across(where(is.numeric), mean, na.rm = T)) %>%
  ungroup() %>%
  dplyr::rename(regno = regno_clean)

# Merge data -------------------------------------------------------------------
award_info_df <- award_info_df %>%
  distinct(regno, .keep_all = T)

veh_df <- fb_sum_df %>%
  full_join(sensor_sum_df, by = "regno") %>%
  full_join(award_info_df, by = "regno")

veh_df$n_feedback[is.na(veh_df$n_feedback) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_1wk[is.na(veh_df$n_feedback_1wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_2wk[is.na(veh_df$n_feedback_2wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_3wk[is.na(veh_df$n_feedback_3wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_4wk[is.na(veh_df$n_feedback_4wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_5wk[is.na(veh_df$n_feedback_5wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_6wk[is.na(veh_df$n_feedback_6wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_7wk[is.na(veh_df$n_feedback_7wk) & !is.na(veh_df$award_type)] <- 0
veh_df$n_feedback_8wk[is.na(veh_df$n_feedback_8wk) & !is.na(veh_df$award_type)] <- 0

# Export -----------------------------------------------------------------------
saveRDS(veh_df,
        file.path(data_dir, "FinalData", "vehicle_level.Rds"))
