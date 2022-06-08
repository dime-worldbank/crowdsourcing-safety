# Add name and phone number

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))
survey_df <- readRDS(file.path(onedrive_dir, "Data", "Matatu Sensor Installation Survey",
                               "FinalData - PII", "psv_sensor_installation_clean.Rds"))

survey_df$sacco <- NULL

# Merge data -------------------------------------------------------------------
veh_df <- readRDS(file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))

veh_df <- veh_df %>%
  dplyr::mutate(regno_clean = reg_no %>% 
                  tolower() %>%
                  str_replace_all(" ", ""))

veh_df <- veh_df %>%
  left_join(survey_df, by = "regno_clean") 

# Export -----------------------------------------------------------------------
saveRDS(veh_df,
        file.path(onedrive_dir, "Data", "Matatu Data",
                  "FinalData - PII", "vehicle_info_survey.Rds"))

