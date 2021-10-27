# Append Hourly Sensor Data

# Load data --------------------------------------------------------------------
echo_df <- file.path(sensors_dir, "FinalData", "echodriving_hourly_individual_files") %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS) 

saveRDS(echo_df, file.path(sensors_dir, "FinalData", "echodriving_dayhr.Rds"))
write_parquet(echo_df, file.path(sensors_dir, "FinalData", 
                                   "echodriving_dayhr.gz.parquet"), 
              compression = "gzip", compression_level = 5)

