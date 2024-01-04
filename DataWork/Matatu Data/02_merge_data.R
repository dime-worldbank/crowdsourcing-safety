# Merge Matatu Data Files into One Dataset

# Load data --------------------------------------------------------------------
veh_df     <- read_csv(file.path(matatu_data_dir, "RawData", "vehicle_info.csv"))
psv_num_df <- readRDS(file.path(matatu_data_dir, "FinalData", "individual_files", 
                                "psv_num.Rds"))
driver_rand_df <- read_csv(file.path(matatu_data_dir, "FinalData", "individual_files", 
                                     "sensors_driverfeedback_randomization.csv"))

ntsa_sacco_df <- read_csv(file.path(data_dir, "NTSA - Vehicle Info", "RawData", 
                                     "20220824 Sample Sacco vehicles.csv"))

ntsa_sacco_df <- ntsa_sacco_df %>%
  dplyr::rename(reg_no = "REGISTRATION NUMBER",
                ntsa_sacco = "SACCO NAME")

# Merge data -------------------------------------------------------------------
veh_df <- veh_df %>%
  left_join(psv_num_df, by = "id") %>%
  left_join(driver_rand_df, by = "reg_no") %>%
  dplyr::select(-blank_var)

# Cleanup ----------------------------------------------------------------------
veh_df <- veh_df %>%
  dplyr::rename(regno = reg_no) %>%
  dplyr::select(regno, sacco, route, psv_num,
                vehicle_type,
                drvr_feedback_treat_id, drvr_feedback_treat,
                drvr_feedback_treat_sticker, drvr_feedback_treat_feedback) %>%
  dplyr::filter(regno != "UNASSIGNED")

# Export -----------------------------------------------------------------------
saveRDS(veh_df, file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))


