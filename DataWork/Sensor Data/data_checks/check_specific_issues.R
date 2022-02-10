# Check specific issues

# KCN 863S
# KCF 560D

echo_df <- readRDS(file.path(sensors_dir, "FinalData", "echodriving.Rds"))
dayhr_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_dayhr.Rds"))
st_df <- load_st_raw("all", "kcn863s")

echo_df <- echo_df %>%
  dplyr::filter(regno_clean %in% "kcn863s")

dayhr_df <- dayhr_df %>%
  dplyr::filter(regno_clean %in% "kcn863s")

dayhr_df$N_violation %>% hist()
dayhr_df$N_violation_acceleration 

