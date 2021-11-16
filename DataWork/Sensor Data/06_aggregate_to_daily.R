# Merge Sensor Data with Survey Data

# Load Data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_dayhr.Rds"))
sensor_sf <- readRDS(file.path(sensors_dir, "FinalData", "sensor_dayhr_polyline.Rds"))

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
  dplyr::summarise_at(vars(matches("\\bN_")),
                      sum)

sensor_agg_df <- merge(sensor_nonsum_df, sensor_sum_df, 
                       by = c("reg_no", "reg_no_id", "regno_clean", "sacco", "route", "date"))

# Aggregate Polylines ----------------------------------------------------------
# Using method of: summarize(geometry = st_union(geometry)) was inefficient, so
# using alternate method.

#### Add group_id to merge back into
sensor_agg_df <- sensor_agg_df %>%
  dplyr::mutate(group_id = paste(reg_no_id, date))

# Creating polylines doesn't work if only one point in linestring or if empty;
# we remove those before aggregating polylines
sensor_lines_sf <- sensor_sf %>%
  dplyr::filter(!is.na(distance_km),
                distance_km > 0) %>%
  arrange(hour) %>% # Need to arrange as need ordered coordinates at daily leel
  dplyr::mutate(group_id = paste(reg_no_id, date)) 

#### Aggregate Polylines
counter_i <<- 1 # to show progress
daily_polyline_sf <- lapply(unique(sensor_lines_sf$group_id), function(group_id_i){
  if((counter_i %% 100) == 0) print(paste0("Aggregating polylines to daily level: ", 
                                           counter_i, 
                                           " / ", 
                                           length(unique(sensor_lines_sf$group_id))))
  
  ## Grab polyline for vehicle on date
  sensor_lines_sf_i <- sensor_lines_sf[sensor_lines_sf$group_id %in% group_id_i,]

  ## Summarize
  # TAKES A LONG TIME AND CREATES MULTILINE, NOT LINE
  # polyline_sf <- sensor_lines_sf_i %>%
  #   group_by(group_id) %>%
  #   summarize(geometry = st_union(geometry))

  ## Grab coordinates as dataframe
  ## BELOW ASSUMES CORRECT ORDERING; HAD ISSUES OF WEIRD STRAIGHT LINES
  ## BEING CREATED
  coords_df <- sensor_lines_sf_i$geometry %>%
    st_coordinates() %>%
    as.data.frame()
  coords_df$group_id <- group_id_i

  ## Make polyline
  polyline_sf <- sf_linestring(coords_df,
                               x = "X",
                               y = "Y",
                               linestring_id = "group_id")
  st_crs(polyline_sf) <- 4326
  
  ## Add total distance between min/max lat & lon
  bbox_df <- st_bbox(polyline_sf) %>%
    as.matrix() %>%
    t() %>%
    as.data.frame()
  
  polyline_sf$distance_minmax_latlon_daily_km = distHaversine(p1 = c(bbox_df$xmin,
                                                                     bbox_df$ymin),
                                                              p2 = c(bbox_df$xmax,
                                                                     bbox_df$ymax))/1000
  
  counter_i <<- counter_i + 1
  return(polyline_sf)
}) %>% 
  bind_rows()

#### Add Distance Variables
daily_polyline_sf$distance_daily_km <- as.numeric(st_length(daily_polyline_sf))/1000 

#### Merge Back to Main Dataframes
sensor_agg_sf <- daily_polyline_sf %>%
  right_join(sensor_agg_df, by = "group_id")

sensor_agg_df <- sensor_agg_sf
sensor_agg_df$geometry <- NULL

#### Cleanup
sensor_agg_df$group_id <- NULL
sensor_agg_sf$group_id <- NULL

# Export -----------------------------------------------------------------------
## Data Only
write_parquet(sensor_agg_df, file.path(sensors_dir, "FinalData", "sensor_day.gz.parquet"), 
              compression = "gzip", compression_level = 5)
saveRDS(sensor_agg_df, file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

## Data with Polyline
saveRDS(sensor_agg_sf, file.path(sensors_dir, "FinalData", "sensor_day_polyline.Rds"))

