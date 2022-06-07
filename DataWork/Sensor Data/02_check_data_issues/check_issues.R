# Check Data Issues

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))
#wialon_df <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "user_ids.csv"))

# Checks -----------------------------------------------------------------------
end_date   <- sensor_df$date %>% max()
start_date <- end_date - 4

regno_nospeed <- sensor_df %>%
  dplyr::filter(date <= end_date,
                date >= start_date) %>%
  group_by(regno_clean) %>%
  dplyr::summarise(N_obs_speed = sum(N_obs_speed)) %>%
  ungroup() %>%
  dplyr::filter(N_obs_speed %in% 0) %>%
  pull(regno_clean)

sensor_df %>%
  distinct(regno_clean, .keep_all = T) %>%
  dplyr::filter(regno_clean %in% regno_nospeed) %>%
  pull(drvr_feedback_treat) %>%
  table()

sensor_df %>%
  distinct(regno_clean, .keep_all = T) %>%
  dplyr::filter(regno_clean %in% regno_nospeed) %>%
  pull(sacco) %>%
  table()

sensor_df %>%
  distinct(regno_clean, .keep_all = T) %>%  
  pull(sacco) %>%
  table()

## Data
nospeed_df <- sensor_df %>%
  dplyr::filter(reg_no_clean %in% regno_nospeed) %>%
  group_by(reg_no_clean) %>%
  dplyr::summarise(date_issue = min(date[N_obs_speed %in% 0])) %>%
  ungroup() %>%
  dplyr::mutate(days_issue = difftime(end_date, date_issue))

head(nospeed_df)

## Figure
sensor_df %>%
  dplyr::filter(regno_clean %in% regno_nospeed,
                date >= ymd("2022-01-01")) %>%
  dplyr::mutate(title = paste0(regno_clean, "\nInstalled = ", install_date)) %>%
  ggplot() +
  geom_col(aes(x = date,
               y = speed_p85)) +
  labs(y = "Speed (85th Percentile)",
       x = NULL) +
  facet_wrap(~title) +
  theme_minimal()

