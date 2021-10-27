# Merge Sensor Data with Survey Data

# Load Data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_dayhr.gz.Rds"))
sensor_sf <- readRDS(file.path(sensors_dir, "FinalData", "sensor_dayhr_polyline.gz.Rds"))

# Aggregate --------------------------------------------------------------------
sensor_nonsum_df <- sensor_df %>%
  group_by(reg_no, reg_no_id, regno_clean, sacco, route, date) %>%
  dplyr::summarise(speed_min = min(speed_min),
                   speed_p05 = weighted.mean(x = speed_p05, w = N_obs_speed),
                   speed_p10 = weighted.mean(x = speed_p10, w = N_obs_speed),
                   speed_p15 = weighted.mean(x = speed_p15, w = N_obs_speed),
                   speed_p25 = weighted.mean(x = speed_p25, w = N_obs_speed),
                   speed_p50 = weighted.mean(x = speed_p50, w = N_obs_speed),
                   speed_p75 = weighted.mean(x = speed_p75, w = N_obs_speed),
                   speed_p85 = weighted.mean(x = speed_p85, w = N_obs_speed),
                   speed_p90 = weighted.mean(x = speed_p90, w = N_obs_speed),
                   speed_p95 = weighted.mean(x = speed_p95, w = N_obs_speed),
                   speed_max = max(speed_max),
                   distance_minmax_latlon_km = sum(distance_minmax_latlon_km),
                   distance_km = sum(distance_km))

sensor_sum_df <- sensor_df %>%
  group_by(reg_no, reg_no_id, regno_clean, sacco, route, date) %>%
  dplyr::summarise_at(vars(contains("N_")),
                      sum)

sensor_agg_df <- merge(sensor_nonsum_df, sensor_sum_df, 
                       by = c("reg_no", "reg_no_id", "regno_clean", "sacco", "route", "date"))

# Aggregate Polylines ----------------------------------------------------------
sensor_polylineagg_sf <- sensor_sf %>%
  group_by(reg_no, reg_no_id, regno_clean, sacco, route, date) %>%
  summarize(geometry = st_union(geometry))

sensor_agg_sf <- right_join(sensor_polylineagg_sf,
                           sensor_agg_df, 
                           by = c("reg_no", "reg_no_id", "regno_clean", "sacco", "route", "date"))

# Export -----------------------------------------------------------------------
write_parquet(sensor_agg_df, file.path(sensors_dir, "FinalData", "sensor_day.gz.parquet"), 
              compression = "gzip", compression_level = 5)
saveRDS(sensor_agg_df, file.path(sensors_dir, "FinalData", "sensor_day.gz.Rds"))

saveRDS(sensor_agg_sf, file.path(sensors_dir, "FinalData", "sensor_day_polyline.gz.Rds"))



