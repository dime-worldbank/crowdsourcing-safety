# Check weird issues with data

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

df <- sensor_df %>%
  group_by(regno_clean, date) %>%
  dplyr::summarise(n = n()) %>%
  ungroup()

df$n %>% table()

# Check issues -----------------------------------------------------------------
## Repeat days

df <- sensor_df %>%
  group_by(regno_clean, date) %>%
  dplyr::summarise(n = n()) %>%
  ungroup()


