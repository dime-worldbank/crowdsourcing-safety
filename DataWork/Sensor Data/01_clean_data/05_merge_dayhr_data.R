# Merge Sensor Data with Survey Data

# Load Data --------------------------------------------------------------------
sp_df <- readRDS(file.path(sensors_dir, "FinalData", "speedings_dayhr.Rds"))
ed_df <- readRDS(file.path(sensors_dir, "FinalData", "echodriving_dayhr.Rds"))
st_df <- readRDS(file.path(sensors_dir, "FinalData", "sensortracing_dayhr_dataonly.Rds"))
st_sf <- readRDS(file.path(sensors_dir, "FinalData", "sensortracing_dayhr_datapolyline.Rds"))

# Prep other data to merge into sensor data ------------------------------------
#### Vehicle data
veh_data_df <- readRDS(file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))
veh_data_df <- veh_data_df %>%
  dplyr::rename(regno_clean = reg_no) %>%
  dplyr::select(regno_clean, sacco, route,
                drvr_feedback_treat_id, drvr_feedback_treat,
                drvr_feedback_treat_sticker, drvr_feedback_treat_feedback) %>%
  dplyr::mutate(regno_clean = regno_clean %>% 
                  tolower() %>%
                  str_replace_all(" ", "")) %>%
  dplyr::filter(regno_clean != "unassigned")

#### Sticker installation survey
st_insll_df <- readRDS(file.path(sticker_install_survey_dir, "FinalData", 
                                 "sticker_install_survey.Rds"))
st_insll_df <- st_insll_df %>%
  dplyr::mutate(sticker_install_date = submission_date %>% date()) %>%
  dplyr::select(regno_clean, sticker_install_date, n_stickers_installed) %>%
  dplyr::mutate(sticker_installed = T) %>%
  dplyr::mutate(regno_clean = regno_clean %>% 
                  tolower() %>%
                  str_replace_all(" ", ""))

#### Rider feedback
# rider_feed_df <- readRDS(file.path(rider_feedback_dir, "FinalData", 
#                                    "rider_feedback.Rds"))
# 
# ## Vehicle level
# rider_feed_vehlevel_df <- rider_feed_df %>%
#   dplyr::filter(valid_psvnum %in% T,
#                 !is.na(completion_date)) %>%
#   dplyr::mutate(feedback_date = invite_datetime %>% date()) %>%
#   group_by(regno_clean) %>%
#   dplyr::summarise(n_rider_feedback_total = n(),
#                    date_first_rider_feedback = min(feedback_date)) %>%
#   dplyr::mutate(regno_clean = regno_clean %>% 
#                   tolower() %>%
#                   str_replace_all(" ", ""))

# Individual dataset fixes -----------------------------------------------------
# Ensure variable types are the same
ed_df$reg_no_id <- ed_df$reg_no_id %>% as.character()
sp_df$reg_no_id <- sp_df$reg_no_id %>% as.character()

# Restrict to current vehicles -------------------------------------------------
wailonid_df <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "user_ids.csv"))

sp_df <- sp_df[sp_df$reg_no_id %in% wailonid_df$id,]
ed_df <- ed_df[ed_df$reg_no_id %in% wailonid_df$id,]
st_df <- st_df[st_df$reg_no_id %in% wailonid_df$id,]
st_sf <- st_sf[st_sf$reg_no_id %in% wailonid_df$id,]

# Clean variables --------------------------------------------------------------
# Some regno are inconsisent in the raw data; rely on reg no from wailon id dataframe

ed_df$reg_no <- NULL
st_df$reg_no <- NULL
st_sf$reg_no <- NULL

# Echo/Sensor Data -------------------------------------------------------------
regno_df <- wailonid_df %>%
  dplyr::rename(reg_no    = nm,
                reg_no_id = id) %>%
  dplyr::select(reg_no, reg_no_id) %>%
  dplyr::mutate(regno_clean = reg_no %>%
                  as.character() %>%
                  str_replace_all(" ", "") %>%
                  tolower() %>%
                  str_sub(-7,-1),
                reg_no_id = reg_no_id %>% 
                  as.character())

sensor_df <- st_df %>%
  full_join(ed_df, by = c("reg_no_id", "datetime_eat")) %>%
  full_join(sp_df, by = c("reg_no_id", "datetime_eat")) %>%
  left_join(regno_df, by = "reg_no_id")

sensor_sf <- st_sf %>%
  full_join(ed_df, by = c("reg_no_id", "datetime_eat")) %>%
  full_join(sp_df, by = c("reg_no_id", "datetime_eat")) %>%
  left_join(regno_df, by = "reg_no_id")

# Date/time to EAT -------------------------------------------------------------
## Add Hour and Date
sensor_df <- sensor_df %>%
  dplyr::mutate(date = datetime_eat %>% as.Date(tz = "Africa/Nairobi"),
                hour = datetime_eat %>% hour())

sensor_sf <- sensor_sf %>%
  dplyr::mutate(date = datetime_eat %>% as.Date(tz = "Africa/Nairobi"),
                hour = datetime_eat %>% hour())

# Add installation date --------------------------------------------------------
sensor_df <- sensor_df %>%
  dplyr::group_by(regno_clean) %>%
  dplyr::mutate(install_datetime = min(datetime_eat[which(N_obs_speed > 0)])) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(install_date = install_datetime %>% as.Date(tz = "Africa/Nairobi"))

sensor_sf <- sensor_sf %>%
  dplyr::group_by(regno_clean) %>%
  dplyr::mutate(install_datetime = min(datetime_eat[which(N_obs_speed > 0)])) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(install_date = install_datetime %>% as.Date(tz = "Africa/Nairobi"))

# Complete data only dataframe -------------------------------------------------
sensor_df <- sensor_df %>%
  complete( nesting(reg_no_id,
                    regno_clean,
                    reg_no,
                    install_datetime,
                    install_date),
            
            nesting(datetime_eat,
                    date,
                    hour),
            
            fill = list(N_obs_speed = 0)
            
  ) %>%
  
  # Only keep if date/time is after the install date/time  
  dplyr::filter(datetime_eat >= install_datetime)

# Merge survey -----------------------------------------------------------------
# veh_data_df <- veh_data_df %>%
#   dplyr::rename(regno_clean = reg_no) %>%
#   dplyr::mutate(regno_clean = regno_clean %>% 
#                   tolower() %>%
#                   str_replace_all(" ", ""))

sensor_df <- sensor_df %>%
  #left_join(rider_feed_vehlevel_df, by = "regno_clean") %>%
  left_join(st_insll_df, by = "regno_clean") %>%
  left_join(veh_data_df, by = "regno_clean")

sensor_sf <- sensor_sf %>%
  #left_join(rider_feed_vehlevel_df, by = "regno_clean") %>%
  left_join(st_insll_df, by = "regno_clean") %>%
  left_join(veh_data_df, by = "regno_clean")

# Adjust Variables -------------------------------------------------------------
## Make reg number in format AAA AAAA (add space and make upper)
sensor_df <- sensor_df %>%
  mutate(regno_clean = regno_clean %>%
           str_replace_all('^(.{3})(.*)$',
                           '\\1 \\2') %>%
           toupper())

sensor_sf <- sensor_sf %>%
  mutate(regno_clean = regno_clean %>%
           str_replace_all('^(.{3})(.*)$',
                           '\\1 \\2') %>%
           toupper())

## Replace NAs
sensor_df <- sensor_df %>%
  dplyr::mutate_at(vars(contains("N_violation"),
                        contains("_valueg")), ~ replace_na(., 0))

sensor_sf <- sensor_sf %>%
  dplyr::mutate_at(vars(contains("N_violation"),
                        contains("_valueg")), ~ replace_na(., 0))

# Cleanup variable names -------------------------------------------------------
sensor_df <- sensor_df %>%
  dplyr::rename(gps_install_date = install_date,
                gps_install_datetime = install_datetime)

sensor_sf <- sensor_sf %>%
  dplyr::rename(gps_install_date = install_date,
                gps_install_datetime = install_datetime)

# Export -----------------------------------------------------------------------
write_parquet(sensor_df, file.path(sensors_dir, "FinalData", "sensor_dayhr.gz.parquet"), 
              compression = "gzip", compression_level = 5)
saveRDS(sensor_df, file.path(sensors_dir, "FinalData", "sensor_dayhr.Rds"))

saveRDS(sensor_sf, file.path(sensors_dir, "FinalData", "sensor_dayhr_polyline.Rds"))
