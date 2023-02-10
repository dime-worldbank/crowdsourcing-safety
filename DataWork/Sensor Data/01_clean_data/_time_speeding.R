# Time Speeding

# Aggregate data to calculate the following:
# 1. Total time moving
# 2. Total time moving above certain speeds (50 kph, 80, 100, etc)
# 3. Could also consider distance moving above certain speeds

# The files are originally stored per vehicle per day. So this code loads data 
# for a single vehicle in a day, then aggregates that. 

# Dataframe of files with speed data -------------------------------------------

## Raw sensor files
# Each file is for a separate vehicle & day
raw_sensor_files <- file.path(sensors_dir, "RawData", "sensor_tracing_individual_data") %>%
  list.files(pattern = "*.parquet",
             recursive = T,
             full.names = T) %>%
  rev()

# Grab some datasets with observations, so can play with those.
files_df <- map_df(raw_sensor_files[1:10000], function(x){
  
  speed_df <- read_parquet(x)
  
  df <- data.frame(filename = x,
                   n = nrow(speed_df))
  
  if(nrow(speed_df) > 0){
    df$n_speed_over_0 <- speed_df %>%
      dplyr::filter(speed > 0) %>%
      nrow()
  }
  
  return(df)
  
}) %>%
  dplyr::filter(n > 0) 

files_travel_df <- files_df %>%
  dplyr::filter(n_speed_over_0 > 1000)

# Aggregate --------------------------------------------------------------------
## Load single vehicle/day
speed_df <- read_parquet(files_travel_df$filename[1])


