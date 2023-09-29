# Rider Feedback - All Data

# TODO
# 1. Sacco
# 2. Route
# 3. Bring in translated comments
# 4. Save (1) separately and (2) appended

# Load data --------------------------------------------------------------------
#### Pilot
fb_pilot_df <- readRDS(file.path(dropbox_dir, "Data", "Rider Feedback - Pilot", 
                                 "All Data", "FinalData", 
                                 "rider_feedback.Rds"))

#### Recent
fb_recent_df <- readRDS(file.path(data_dir, "Rider Feedback", "FinalData",
                                  "rider_feedback_clean.Rds"))

# Cleanup ----------------------------------------------------------------------
# (1) Driver rating and (2) Speed rating different

#### Initial Pilots
fb_pilot_df <- fb_pilot_df %>%
  dplyr::select(phone_hash, reg_no, start_datetime, complete_datetime,
                response_method, pilot_number,
                matatu_number,
                #award_type, award_amount_posted, award_offer_end,
                
                driver_rating, driver_rating_asked,
                covid_measures, covid_measures_asked,
                speed_rating, speed_rating_asked,
                occupancy, occupancy_asked,
                feedback, feedback_asked) %>%
  dplyr::rename(q_safety_rating = driver_rating,
                q_safety_rating_asked = driver_rating_asked,
                
                q_covid_measures = covid_measures,
                q_covid_measures_asked = covid_measures_asked,
                
                q_speed_rating_v1 = speed_rating,
                q_speed_rating_v1_asked = speed_rating_asked,
                
                q_occupancy = occupancy,
                q_occupancy_asked = occupancy_asked,
                
                q_comment = feedback,
                q_comment_asked = feedback_asked,
                
                regno = reg_no,
                psv_num = matatu_number) %>%
  dplyr::mutate(start_datetime = start_datetime %>% 
                  as.character() %>% 
                  paste("EAT") %>% 
                  ymd_hms(tz = "Africa/Nairobi")) %>%
  dplyr::mutate(q_safety_rating = case_when(
    q_safety_rating == "unsafe" ~ "not safe",
    q_safety_rating == "very unsafe" ~ "not very safe",
    TRUE ~ q_safety_rating
  )) %>%
  dplyr::mutate(regno = regno %>% str_squish() %>% toupper(),
                psv_num = psv_num %>% as.character())

fb_pilot_df$pilot_number[is.na(fb_pilot_df$pilot_number)] <- "1 to 4"

fb_pilot_df$start_datetime[is.na(fb_pilot_df$start_datetime)] <- 
  fb_pilot_df$complete_datetime[is.na(fb_pilot_df$start_datetime)]

fb_pilot_df$complete_datetime <- NULL

fb_pilot_df <- fb_pilot_df %>%
  dplyr::rename(datetime = start_datetime) %>%
  dplyr::mutate(date = datetime %>% date()) %>%
  dplyr::filter(!is.na(datetime))

#### Recent Data
fb_recent_df <- fb_recent_df %>%
  dplyr::rename(comments_label_swahili = comments_label) %>%
  dplyr::rename(comments_label         = comments_label_english) %>%
  
  dplyr::select(phone_hash, regno_clean, start_datetime, completion_datetime, psv_num,
                
                safety_label_en, speed_label_en, busfare_label, comments_label, comments_label_swahili,
                comment_driver_sentiment_relev,
                comment_driver_sentiment_code) %>%
  
  # matatu_route_stk_inst_srvy,
  # matatu_sacco_stk_inst_srvy,
  # matatu_n_seats_stk_inst_srvy,
  # n_stickers_installed,
  
  # drvr_feedback_treat_id, 
  # drvr_feedback_treat, 
  # drvr_feedback_treat_sticker,
  # drvr_feedback_treat_feedback) %>%
  dplyr::mutate(response_method = "shortcode",
                q_safety_rating_asked = "yes",
                q_speed_rating_v2_asked = "yes",
                q_busfare_asked = "yes",
                q_comment_asked = "yes",
                
                phone_hash = phone_hash %>% as.character(),
                pilot_number = "5",
                start_datetime = start_datetime %>% ymd_hm(tz = "Africa/Nairobi")) %>%
  dplyr::rename(q_safety_rating = safety_label_en,
                q_speed_rating_v2 = speed_label_en,
                q_busfare = busfare_label,
                q_comment = comments_label,
                q_comment_swahili = comments_label_swahili,
                
                # info_route = matatu_route_stk_inst_srvy,
                # info_sacco = matatu_sacco_stk_inst_srvy,
                # info_n_seats = matatu_n_seats_stk_inst_srvy,
                # info_n_stickers = n_stickers_installed,
                regno = regno_clean,
                
                comment_driver_sntmt_relev = comment_driver_sentiment_relev,
                comment_driver_sntmt_code = comment_driver_sentiment_code) %>%
  dplyr::mutate(q_safety_rating = q_safety_rating %>%
                  as.character() %>% tolower(),
                regno = regno %>% str_replace_all(" ", "") %>% toupper(),
                psv_num = psv_num %>% as.character()) %>%
  dplyr::mutate(comment_driver_sntmt_code_compl = 
                  comment_driver_sntmt_code == 1,
                comment_driver_sntmt_code_neg = 
                  comment_driver_sntmt_code == 2,
                comment_driver_sntmt_code_neut = 
                  comment_driver_sntmt_code == 3,
                comment_driver_sntmt_code_uncl = 
                  comment_driver_sntmt_code == 4) %>%
  dplyr::filter(!is.na(start_datetime)) %>%
  dplyr::rename(datetime = start_datetime) %>%
  dplyr::mutate(date = datetime %>% date())

fb_recent_df$regno[is.na(fb_recent_df$regno)] <- "UNKNOWN"

fb_recent_df$comment_driver_sntmt_code[fb_recent_df$comment_driver_sntmt_code == 0] <- NA

# Append -----------------------------------------------------------------------
fb_df <- bind_rows(
  fb_pilot_df, 
  fb_recent_df
)

# Cleanup after append ---------------------------------------------------------
fb_df <- fb_df %>%
  mutate(regno = regno %>%
           str_replace_all('^(.{3})(.*)$',
                           '\\1 \\2') %>%
           toupper()) %>%
  mutate(regno = case_when(
    regno == "UNK NOWN" ~ "UNKNOWN",
    TRUE ~ regno
  )) %>%
  dplyr::mutate(
    q_safety_rating_num = case_when(
      q_safety_rating == "very safe" ~ 4,
      q_safety_rating == "safe" ~ 3,
      q_safety_rating == "not safe" ~ 2,
      q_safety_rating == "not very safe" ~ 1
    ),
    
    q_speed_rating_v1_num = case_when(
      q_speed_rating_v1 == "dangerously fast" ~ 4,
      q_speed_rating_v1 == "fast" ~ 3,
      q_speed_rating_v1 == "okay" ~ 2,
      q_speed_rating_v1 == "too slow" ~ 1
    ),
    
    q_speed_rating_v2_num = case_when(
      q_speed_rating_v2 == "Very fast [80+]" ~ 5,
      q_speed_rating_v2 == "Fast [50-80]" ~ 4,
      q_speed_rating_v2 == "Average [30-50]" ~ 3,
      q_speed_rating_v2 == "Slow [10-30]" ~ 2,
      q_speed_rating_v2 == "Very slow [0-10 km/h]" ~ 1
    ),
    
    q_covid_measures_num = case_when(
      q_covid_measures == "yes, effective" ~ 3,
      q_covid_measures == "yes, but seemed limited" ~ 2,
      q_covid_measures == "no" ~ 1
    ),
    
    q_occupancy_num = case_when(
      q_occupancy == "more people than can fit" ~ 4,
      q_occupancy == "more people than seats" ~ 3,
      q_occupancy == "same number of people as seats" ~ 2,
      q_occupancy == "less people than seats" ~ 1,
    )
    
  )

# Order ------------------------------------------------------------------------
fb_df <- fb_df %>%
  dplyr::select(pilot_number, phone_hash, regno, psv_num, datetime, date, response_method,
                #info_sacco, info_route, info_n_stickers, info_n_seats,
                
                q_safety_rating, q_safety_rating_num, q_safety_rating_asked,
                q_covid_measures, q_covid_measures_num, q_covid_measures_asked,
                q_speed_rating_v1, q_speed_rating_v1_num, q_speed_rating_v1_asked,
                q_speed_rating_v2, q_speed_rating_v2_num, q_speed_rating_v2_asked,
                q_occupancy, q_occupancy_num, q_occupancy_asked,
                q_busfare, q_busfare_asked,
                q_comment, q_comment_swahili, q_comment_asked,
                
                #award_type, award_amount_posted, award_offer_end,
                
                comment_driver_sntmt_relev, comment_driver_sntmt_code,
                comment_driver_sntmt_code_compl, comment_driver_sntmt_code_neg,
                comment_driver_sntmt_code_neut, comment_driver_sntmt_code_uncl,
                
                # drvr_feedback_treat_id, drvr_feedback_treat, 
                # drvr_feedback_treat_sticker, drvr_feedback_treat_feedback,
                
                everything())

fb_pilot_df <- fb_pilot_df %>%
  dplyr::select(pilot_number, phone_hash, regno, psv_num, datetime, date, response_method,
                
                q_safety_rating, q_safety_rating_num, q_safety_rating_asked,
                q_covid_measures, q_covid_measures_num, q_covid_measures_asked,
                q_speed_rating_v1, q_speed_rating_v1_num, q_speed_rating_v1_asked,
                q_occupancy, q_occupancy_num, q_occupancy_asked,
                q_comment, q_comment_asked,
                
                #award_type, award_amount_posted, award_offer_end,
                
                everything())

fb_recent_df <- fb_recent_df %>%
  dplyr::select(pilot_number, phone_hash, regno, psv_num, datetime, date, response_method,
                #info_sacco, info_route, info_n_stickers, info_n_seats,
                
                q_safety_rating, q_safety_rating_num, q_safety_rating_asked,
                q_speed_rating_v2, q_speed_rating_v2_num, q_speed_rating_v2_asked,
                q_busfare, q_busfare_asked,
                q_comment, q_comment_swahili, q_comment_asked,
                
                comment_driver_sntmt_relev, comment_driver_sntmt_code,
                comment_driver_sntmt_code_compl, comment_driver_sntmt_code_neg,
                comment_driver_sntmt_code_neut, comment_driver_sntmt_code_uncl,
                
                # drvr_feedback_treat_id, drvr_feedback_treat, 
                # drvr_feedback_treat_sticker, drvr_feedback_treat_feedback,
                
                everything())

# Export -----------------------------------------------------------------------
#### All
saveRDS(fb_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                         "Rider Feedback - All",
                         "rider_feedback.Rds"))

write_parquet(fb_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                               "Rider Feedback - All",
                               "rider_feedback.parquet"))

write_csv(fb_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                           "Rider Feedback - All",
                           "rider_feedback.csv"))

write_dta(fb_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                           "Rider Feedback - All",
                           "rider_feedback.dta"))

#### Pilots 1 to 4
saveRDS(fb_pilot_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                               "Rider Feedback - Pilots 1 to 4",
                               "rider_feedback_p1to4.Rds"))

write_parquet(fb_pilot_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                                     "Rider Feedback - Pilots 1 to 4",
                                     "rider_feedback_p1to4.parquet"))

write_csv(fb_pilot_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                                 "Rider Feedback - Pilots 1 to 4",
                                 "rider_feedback_p1to4.csv"))

write_dta(fb_pilot_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                                 "Rider Feedback - Pilots 1 to 4",
                                 "rider_feedback_p1to4.dta"))

#### Pilot 5
saveRDS(fb_recent_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                                "Rider Feedback - Pilot 5",
                                "rider_feedback_p5.Rds"))

write_parquet(fb_recent_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                                      "Rider Feedback - Pilot 5",
                                      "rider_feedback_p5.parquet"))

write_csv(fb_recent_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                                  "Rider Feedback - Pilot 5",
                                  "rider_feedback_p5.csv"))

write_dta(fb_recent_df, file.path(dropbox_dir, "Data", "Rider Feedback - All", "FinalData",
                                  "Rider Feedback - Pilot 5",
                                  "rider_feedback_p5.dta"))



