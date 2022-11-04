# Clean NTSA Speed Data

# Load data --------------------------------------------------------------------
ntsa_df <- read_csv(file.path(ntsa_speed_dir, "RawData", "speed_limiter_transmission_data_20221004.csv"))

# Clean data -------------------------------------------------------------------
ntsa_df <- ntsa_df %>%
  clean_names() %>%
  dplyr::rename(date = date_4,
                regno = car_plate_registration_number) %>%
  dplyr::mutate(date = date %>% ymd(),
                date_time = date_16 %>% ymd_hms(),
                time = time %>% hms(),
                regno = regno %>% tolower()) %>%
  dplyr::select(-v1, -v2, -v3, -date_16) %>%
  dplyr::mutate(latitude_clean = longitude,
                longitude_clean = latitude) %>%
  dplyr::select(-latitude, -longitude) %>%
  dplyr::rename(latitude = latitude_clean,
                longitude = longitude_clean)

# Export data ------------------------------------------------------------------
write_csv(ntsa_df, file.path(ntsa_speed_dir, "FinalData", "speed_limiter_transmission_data_20221004.csv"))
saveRDS(ntsa_df, file.path(ntsa_speed_dir, "FinalData", "speed_limiter_transmission_data_20221004.Rds"))

