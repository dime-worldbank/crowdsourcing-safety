# Number of Monthly Installations

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

# Monthly installations --------------------------------------------------------
sensor_df %>%
  # Installation date for each vehicle
  dplyr::filter(N_obs_speed > 0) %>%
  group_by(regno_clean) %>%
  dplyr::summarise(install_date = min(date)) %>%
  ungroup() %>%
  
  # Number of installations per month
  dplyr::mutate(intall_month = floor_date(install_date, unit = "months")) %>%
  group_by(intall_month) %>%
  dplyr::summarise(n = n())


