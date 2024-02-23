# Situation Report Sum Stats

# Load data --------------------------------------------------------------------
smtrns_db_dir <- "~/Dropbox/World Bank/IEs/CrashMap-Nairobi"

rtc_df <- readRDS(file.path(smtrns_db_dir, "Data", "Police Data", "Crash Report Data",
                            "All Reports", "sr_crashes.Rds"))

rtc_df <- rtc_df %>%
  dplyr::filter(crash_year %in% 2018:2020,
                fatal_injury_any %in% 1)

rtc_df$vehicle_matatu_any %>% mean()
rtc_df$vehicle_motorcycle_any %>% mean()


