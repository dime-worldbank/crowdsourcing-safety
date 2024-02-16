# Number of Vehicle Stats: Sticker IE

# Load data --------------------------------------------------------------------
#### Sensor All
sensor_all_df <- readRDS(file.path(data_dir, "RawData", "sensor_day.Rds"))

#### Sensor IE
sensor_ie_df <- readRDS(file.path(data_dir, "FinalData", "sensor_sticker_ie_data.Rds"))
sensor_ie_any_df <- readRDS(file.path(data_dir, "FinalData", "sensor_sticker_ie_any_data.Rds"))

#### Vehicle information
veh_data_df <- readRDS(file.path(data_dir, "RawData", "gps_vehicle_info.Rds"))

#### GPS Installation Survey
gps_survey_df <- readRDS(file.path(data_dir, "RawData", "gps_install_survey.Rds"))

#### Sticker Installation Survey
st_insll_df <- readRDS(file.path(data_dir, "RawData", "gps_vehicle_sticker_install_survey.Rds"))

# Stats ------------------------------------------------------------------------


sensor_ie_any_df %>%
  pull(regno) %>%
  unique() %>%
  length()

sensor_ie_any_df %>%
  dplyr::filter(!is.na(sticker_install_date),
                abs(days_since_stk_install) <= 30) %>%
  group_by(regno, days_since_stk_install_post) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  
  group_by(days_since_stk_install_post) %>%
  dplyr::summarise(n_mean = mean(n),
                   n_median = median(n),
                   n_p25 = quantile(n, 0.1)) %>%
  ungroup()
  


# GPS install dates
gps_survey_df$gpssrvy_submissiondate %>% date() %>% min()
gps_survey_df$gpssrvy_submissiondate %>% date() %>% max()

# Sticker install dates
st_insll_df$sticker_install_date %>% min()
st_insll_df$sticker_install_date %>% max()

# Randomization
veh_data_df$drvr_feedback_treat_sticker %>% table()

# Collect GPS data for
sensor_ie_df %>%
  distinct(regno, .keep_all = T) %>%
  pull(drvr_feedback_treat_sticker) %>%
  table()

# Sticker installed
sensor_ie_df %>%
  distinct(regno, .keep_all = T) %>%
  pull(stickers_installed) %>%
  table()

# N vehicles -------------------------------------------------------------------
sensor_ie_df %>%
  filter(stickers_installed == 1,
         abs(days_since_stk_install) <= 30) %>%
  group_by(days_since_stk_install) %>%
  summarise(n_regno = n()) %>%
  
  ggplot() + 
  geom_vline(xintercept = 0) +
  geom_line(aes(x = days_since_stk_install,
                y = n_regno))

sensor_ie_any_df %>%
  filter(stickers_installed == 1,
         abs(days_since_stk_install) <= 30) %>%
  group_by(days_since_stk_install) %>%
  summarise(n_regno = n()) %>%
  
  ggplot() + 
  geom_vline(xintercept = 0) +
  geom_line(aes(x = days_since_stk_install,
                y = n_regno))
