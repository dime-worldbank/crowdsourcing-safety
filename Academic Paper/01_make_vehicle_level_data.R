# Move Data to Paper Data Directory

# Load data --------------------------------------------------------------------
#### GPS Sensor Data: Daily
sensor_df <- readRDS(file.path(data_dir, "RawData", "sensor_day.Rds"))

#### Passenger Feedback
fb_df <- readRDS(file.path(data_dir, "RawData", "passenger_feedback.Rds"))

#### Vehicle information
veh_data_df <- readRDS(file.path(data_dir, "RawData", "gps_vehicle_info.Rds"))

#### Sticker Installation Survey
st_insll_df <- readRDS(file.path(data_dir, "RawData", "gps_vehicle_sticker_install_survey.Rds"))

#### Vehicle Award Info
award_info_df <- readRDS(file.path(data_dir, "RawData", "vehicle_award_info.Rds"))

# Aggregate Passenger Feedback -------------------------------------------------
fb_df <- fb_df %>%
  dplyr::filter(regno != "UNKNOWN") %>%
  group_by(regno, pilot_number) %>%
  dplyr::summarise(across(c(q_safety_rating_num,
                          q_covid_measures_num,
                          q_speed_rating_v1_num,
                          q_speed_rating_v2_num,
                          q_occupancy_num), 
                   mean,
                   na.rm=T)) %>%
  ungroup()

# Aggregate Sensor Data --------------------------------------------------------
sensor_df <- sensor_df %>%
  


# Merge data -------------------------------------------------------------------
sensor_df$reg_no

day_df <- fb_df %>%
  full_join(sensor_df, by = c("regno", "date"))

sensor_df


  
  


