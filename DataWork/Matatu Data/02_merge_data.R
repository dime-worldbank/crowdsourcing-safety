# Merge Matatu Data Files into One Dataset

# Load data --------------------------------------------------------------------
veh_df     <- read_csv(file.path(matatu_data_dir, "RawData", "vehicle_info.csv"))
psv_num_df <- readRDS(file.path(matatu_data_dir, "FinalData", "individual_files", 
                                "psv_num.Rds"))
driver_rand_df <- read_csv(file.path(matatu_data_dir, "FinalData", "individual_files", 
                                     "sensors_driverfeedback_randomization.csv"))

# Merge data -------------------------------------------------------------------
veh_df <- veh_df %>%
  left_join(psv_num_df, by = "reg_no") %>%
  left_join(driver_rand_df, by = "reg_no") %>%
  dplyr::select(-blank_var)

# Export -----------------------------------------------------------------------
saveRDS(veh_df, file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))


