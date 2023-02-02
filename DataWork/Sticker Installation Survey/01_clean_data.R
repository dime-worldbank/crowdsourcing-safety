# Clean Sticker Installation Survey

# Load data --------------------------------------------------------------------
df <- read_csv(file.path(sticker_install_survey_pii_dir, "RawData - PII", 
                         "psv_sticker_installation_WIDE.csv"))

# Basic cleaning ---------------------------------------------------------------
df <- df %>%
  clean_names() %>%
  dplyr::rename(regno_clean = matatu_regno) %>% 
  dplyr::mutate(submission_date = submission_date %>% mdy_hms(tz = "America/New_York"),
                starttime       = starttime       %>% mdy_hms(tz = "America/New_York"),
                endtime         = endtime         %>% mdy_hms(tz = "America/New_York")) %>%
  dplyr::mutate(sticker_install_date = submission_date %>% date())

## Remove unneeded variables
df <- df %>%
  dplyr::select(-c(deviceid, subscriberid, simid, devicephonenum))

# Remove PII -------------------------------------------------------------------
df_nonpii <- df %>% 
  dplyr::select(-c(driver_name, driver_phone_no))

# Export -----------------------------------------------------------------------
saveRDS(df,        file.path(sticker_install_survey_pii_dir, "FinalData - PII", 
                             "sticker_install_survey.Rds"))

saveRDS(df_nonpii, file.path(sticker_install_survey_dir, "FinalData", 
                             "sticker_install_survey.Rds"))


