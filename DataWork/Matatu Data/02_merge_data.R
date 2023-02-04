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

# veh_df$reg_no <- veh_df$reg_no %>% str_replace_all(" ", "")
# 
# veh_df <- veh_df %>%
#   left_join(ntsa_sacco_df, by = "reg_no")
# 
# a <- veh_df[,c("sacco", "ntsa_sacco")]

# Export -----------------------------------------------------------------------
saveRDS(veh_df, file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))


