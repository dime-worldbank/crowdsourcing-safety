# Add name and phone number

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(onedrive_dir, "Data", "Matatu Data",
                            "FinalData - PII", "vehicle_info_survey.Rds"))

# Merge data -------------------------------------------------------------------
veh_df <- veh_df %>%
  dplyr::filter(drvr_feedback_treat_sticker %in% 1) %>%
  dplyr::select(reg_no, sacco, driver_name, driver_phone_no) %>%
  dplyr::mutate(driver_phone_no = paste0("no: ", driver_phone_no))

# Export -----------------------------------------------------------------------
write_csv(veh_df,
          file.path(onedrive_dir, "Data", "Matatu Data",
                    "FinalData - PII", "veh_info_sticker_june2022.csv"))

