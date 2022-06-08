# Speedings to Hourly Data

# TODO
# Total time
# Average speed when speeding (weighted by time)
# Can also use as threshold. Time when > X speed

# Load data --------------------------------------------------------------------
speedings_df <- readRDS(file.path(sensors_dir, "FinalData", "speedings.Rds"))

# Aggregate --------------------------------------------------------------------
speedings_sum_df <- speedings_df %>%
  dplyr::mutate(datetime_eat = datetime_eat %>% floor_date(unit = "hour")) %>%
  group_by(reg_no_id, datetime_eat) %>%
  dplyr::summarise(duration_seconds = sum(duration_seconds),
                   distance_km = sum(distance_km),
                   #max_speed_kmh = weighted.mean(max_speed_kmh, duration_seconds), # WEIGHT
                   #avg_speed_kmh = weighted.mean(avg_speed_kmh, duration_seconds),
                   speed_limit_kmh = speed_limit_kmh[1]) %>%
  dplyr::rename(distance_km_over_110kmh = distance_km,
                duration_seconds_over_110kmh = duration_seconds)
  
# Export -----------------------------------------------------------------------
saveRDS(speedings_sum_df, file.path(sensors_dir, "FinalData", "speedings_dayhr.Rds"))


