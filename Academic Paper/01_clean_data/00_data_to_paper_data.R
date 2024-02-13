# Move Data to Paper Data Directory

# TODO: Scramble regno, sacco, and other pii in sensible way

# Load data --------------------------------------------------------------------
#### GPS Sensor Data: Daily
sensor_df <- readRDS(file.path(db_dir, "Data", "Sensor Data", "FinalData", "sensor_day.Rds"))

saveRDS(sensor_df,       file.path(data_dir, "RawData", "sensor_day.Rds"))
write_parquet(sensor_df, file.path(data_dir, "RawData", "sensor_day.parquet"))
write_csv(sensor_df,     file.path(data_dir, "RawData", "sensor_day.csv"))
write_dta(sensor_df,     file.path(data_dir, "RawData", "sensor_day.dta"))

#### Passenger Feedback
fb_df <- readRDS(file.path(db_dir, "Data", "Rider Feedback - All", "FinalData",
                           "Rider Feedback - All",
                           "rider_feedback.Rds"))

fb_df <- fb_df %>%
  # PSV Number not entered
  dplyr::filter(!(is.na(psv_num) & regno == "UNKNOWN")) %>%
  
  # One question needs to be answered
  dplyr::filter((!is.na(q_safety_rating) | 
                   !is.na(q_speed_rating_v1) |
                   !is.na(q_speed_rating_v2) |
                   !is.na(q_covid_measures) |
                   !is.na(q_occupancy) |
                   !is.na(q_comment)))

## Merge in additional classifications
comments_df <- readRDS(file.path(db_dir, "Data", "Rider Feedback", "FinalData", "comments_coded_all.Rds"))

fb_df <- fb_df %>%
  dplyr::select(-c(comment_driver_sntmt_relev, comment_driver_sntmt_code,
                   comment_driver_sntmt_code_compl, comment_driver_sntmt_code_neg,
                   comment_driver_sntmt_code_neut, comment_driver_sntmt_code_uncl)) %>%
  left_join(comments_df, by = "uid")

## Manual comment fix
fb_df$comment_driver_sntmt_relev[fb_df$uid == 35026] <- 0

## Remove feedback where regno is unknown
fb_df <- fb_df %>%
  dplyr::filter(regno != "UNKNOWN")

saveRDS(fb_df,       file.path(data_dir, "RawData", "passenger_feedback.Rds"))
write_parquet(fb_df, file.path(data_dir, "RawData", "passenger_feedback.parquet"))
write_csv(fb_df,     file.path(data_dir, "RawData", "passenger_feedback.csv"))
write_dta(fb_df,     file.path(data_dir, "RawData", "passenger_feedback.dta"))

#### Vehicle information
veh_data_df <- readRDS(file.path(db_dir, "Data", "Matatu Data", "FinalData", "vehicle_info.Rds"))

saveRDS(veh_data_df,       file.path(data_dir, "RawData", "gps_vehicle_info.Rds"))
write_parquet(veh_data_df, file.path(data_dir, "RawData", "gps_vehicle_info.parquet"))
write_csv(veh_data_df,     file.path(data_dir, "RawData", "gps_vehicle_info.csv"))
write_dta(veh_data_df,     file.path(data_dir, "RawData", "gps_vehicle_info.dta"))

#### Sticker Installation Survey
# Survey conducted when installed sticker on vehicle with GPS sensor
# (during pilot 5)
st_insll_df <- readRDS(file.path(db_dir, "Data", "Sticker Installation Survey", "FinalData", 
                                 "sticker_install_survey.Rds"))

saveRDS(st_insll_df,       file.path(data_dir, "RawData", "gps_vehicle_sticker_install_survey.Rds"))
write_parquet(st_insll_df, file.path(data_dir, "RawData", "gps_vehicle_sticker_install_survey.parquet"))
write_csv(st_insll_df,     file.path(data_dir, "RawData", "gps_vehicle_sticker_install_survey.csv"))
write_dta(st_insll_df,     file.path(data_dir, "RawData", "gps_vehicle_sticker_install_survey.dta"))


#### Vehicle Award Info
award_info_df <- read_xlsx(file.path(db_dir, "Data", "Matatu Data - Pilot", "Sticker Information", "vehicles_with_stickers.xlsx"))

award_info_df <- award_info_df %>%
  clean_names() %>%
  dplyr::select(reg_no, award_type, award_amount, award_offer_ends, qr_code_on_sticker, shortcode_on_sticker, pilot_number) %>%
  dplyr::rename(regno = reg_no) %>%
  dplyr::mutate(regno = regno %>%
                  str_squish() %>%
                  str_replace_all(" ", "") %>%
                  toupper() %>%
                  str_replace_all('^(.{3})(.*)$',
                                  '\\1 \\2'))

saveRDS(award_info_df,       file.path(data_dir, "RawData", "vehicle_award_info.Rds"))
write_parquet(award_info_df, file.path(data_dir, "RawData", "vehicle_award_info.parquet"))
write_csv(award_info_df,     file.path(data_dir, "RawData", "vehicle_award_info.csv"))
write_dta(award_info_df,     file.path(data_dir, "RawData", "vehicle_award_info.dta"))

#### GPS Installation Survey
gps_survey_df <- readRDS(file.path(db_dir, "Data", "Matatu Sensor Installation Survey", "FinalData", "psv_sensor_installation_clean.Rds"))

gps_survey_df <- gps_survey_df %>%
  clean_names() %>%
  dplyr::select(matatu_regno, matatu_exp_freq, 
                sacco, sacco_name_other,
                matatu_other_route, driver_is_matatu_owner,
                driver_age, driver_tenure, driver_route_tenure, driver_contract,
                n_drivers_per_veh_q, matatu_seats, matatu_quality_q, matatu_paint_q,
                matatu_wifi, matatu_amenities, matatu_other, route, stop_type,
                starttime, submissiondate) %>%
  dplyr::rename(regno = matatu_regno) %>%
  dplyr::mutate(regno = regno %>%
                  str_squish() %>%
                  str_replace_all(" ", "") %>%
                  toupper() %>%
                  str_replace_all('^(.{3})(.*)$',
                                  '\\1 \\2')) %>%
  dplyr::mutate(driver_tenure = driver_tenure %>% as.numeric()) %>%
  rename_at(vars(-regno), ~ paste0('gpssrvy_', .))

saveRDS(gps_survey_df,       file.path(data_dir, "RawData", "gps_install_survey.Rds"))
write_parquet(gps_survey_df, file.path(data_dir, "RawData", "gps_install_survey.parquet"))
write_csv(gps_survey_df,     file.path(data_dir, "RawData", "gps_install_survey.csv"))
write_dta(gps_survey_df,     file.path(data_dir, "RawData", "gps_install_survey.dta"))

