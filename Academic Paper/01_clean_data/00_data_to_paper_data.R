# Move Data to Paper Data Directory

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

st_insll_df <- st_insll_df %>%
  dplyr::rename()

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
