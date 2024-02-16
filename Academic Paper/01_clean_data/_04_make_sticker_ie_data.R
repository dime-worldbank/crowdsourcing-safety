# Make Dataset for Sticker IE

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

# Prep -------------------------------------------------------------------------
fb_sum_df <- fb_df %>%
  group_by(regno) %>%
  dplyr::summarise(first_feedback = min(date),
                   n_feedback = n()) %>%
  ungroup()

st_insll_df <- st_insll_df %>%
  distinct(regno, .keep_all = T)

# Merge ------------------------------------------------------------------------
sensor_df <- sensor_df %>%
  dplyr::select(-c(route, sacco)) %>%
  left_join(st_insll_df, by = "regno") %>%
  left_join(fb_sum_df, by = "regno") %>%
  left_join(veh_data_df, by = "regno") %>%
  left_join(gps_survey_df, by = "regno")

# Treatment variables ----------------------------------------------------------
sensor_df <- sensor_df %>%
  dplyr::mutate(
    
    stickers_installed = !is.na(sticker_install_date),
    
    sticker_group_not_installed = as.numeric((drvr_feedback_treat_sticker == 1) & (stickers_installed == 0)),
    
    days_since_stk_install = 
      as.numeric(difftime(date, sticker_install_date, units = "days")),
    
    days_since_stk_install_fct = days_since_stk_install %>%
      factor() %>%
      relevel("-1"),
    
    days_since_stk_install_post = as.numeric(days_since_stk_install >= 0),
    days_since_stk_install_post = days_since_stk_install_post %>%
      replace_na(0),
    
    date_wday = date %>% wday()
  )

# Outcome variables ------------------------------------------------------------
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

# Subset and stats -------------------------------------------------------------
veh_data_df %>%
  pull(drvr_feedback_treat_sticker) %>%
  table()

### Original vehicles
sensor_df <- sensor_df %>%
  dplyr::filter(!is.na(drvr_feedback_treat_sticker))

sensor_all_df <- sensor_df

sensor_df %>%
  distinct(regno, .keep_all = T) %>%
  pull(drvr_feedback_treat_sticker) %>%
  table()

### Vehicles with data
sensor_df <- sensor_df %>%
  dplyr::filter(date >= ymd("2022-08-01"), # 2022-08-01
                date <= ymd("2023-01-14")) %>% # 2023-01-16
  dplyr::filter(distance_minmax_latlon_daily_km >= 50) 

sensor_df %>%
  distinct(regno, .keep_all = T) %>%
  pull(drvr_feedback_treat_sticker) %>%
  table()

sensor_df %>%
  distinct(regno, .keep_all = T) %>%
  filter(drvr_feedback_treat_sticker == 1) %>%
  pull(sticker_install_date) %>%
  is.na() %>%
  table()
#

## Must have observation every week
# sensor_df <- sensor_df %>%
#   mutate(date_week = date %>% floor_date(unit = "month")) %>%
#   group_by(regno, date_week) %>%
#   dplyr::mutate(n_obs_regno_week = n()) %>%
#   ungroup()
# #
# regno_few_obs <- sensor_df %>%
#   dplyr::filter(date_week > min(date_week),
#                date_week < max(date_week)) %>%
#   #dplyr::filter(date_week < max(date_week)) %>%
#   filter(n_obs_regno_week <= 6) %>%
#   pull(regno) %>%
#   unique()
# 
# sensor_df <- sensor_df[!(sensor_df$regno %in% regno_few_obs),]

# Make variables for did -------------------------------------------------------
sensor_df <- sensor_df %>%
  dplyr::mutate(date_num = date %>% as.numeric(),
                sticker_install_date_num = sticker_install_date %>% as.numeric() %>% replace_na(0),
                regno_num = regno %>% factor() %>% as.numeric())

# Export -----------------------------------------------------------------------
saveRDS(sensor_df,
        file.path(data_dir, "FinalData", "sensor_sticker_ie_data.Rds"))

saveRDS(sensor_all_df,
        file.path(data_dir, "FinalData", "sensor_sticker_ie_any_data.Rds"))

# difftime(ymd("2022-08-01"), ymd("2022-09-15"))
# difftime(ymd("2022-11-30"), ymd("2023-01-14"))
# sensor_df$sticker_install_date %>% summary()
# 
# 
# ####
# sensor_df %>%
#   filter(stickers_installed == 1,
#          abs(days_since_stk_install) <= 30) %>%
#   group_by(days_since_stk_install) %>%
#   summarise(n_regno = n()) %>%
#   
#   ggplot() + 
#   geom_vline(xintercept = 0) +
#   geom_line(aes(x = days_since_stk_install,
#                 y = n_regno))
# 
