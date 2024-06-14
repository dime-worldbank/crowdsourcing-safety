# Matatu Crash Rates from Police Data

sr_df <- readRDS("~/Dropbox/World Bank/IEs/CrashMap-Nairobi/Data/Police Data/Crash Report Data/All Reports/sr_crashes.Rds")

sr_df <- sr_df %>%
  filter(crash_year >= 2018)

sr_df %>%
  pull(vehicle_matatu_any) %>%
  mean()

sr_df %>%
  filter(fatal_injury_any %in% 1) %>% 
  pull(vehicle_matatu_any) %>%
  mean()
