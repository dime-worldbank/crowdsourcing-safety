# Time Speeding

# To calculate FOR EACH HOUR
# 1. Total time moving
# 2. Total time above 80km/hr
# 3. Total time above 100km/hr
# So for each vehicle & day, we should have a dataset that shows, for each hor,
# the above three stats

# Load data --------------------------------------------------------------------
## Raw sensor files
# Each file is for a separate vehicle & day
raw_sensor_files <- file.path(sensors_dir, "RawData", "sensor_tracing_individual_data") %>%
  list.files(pattern = "*.parquet",
             recursive = T,
             full.names = T) %>%
  rev()

## Load single vehicle/day
speed_df <- read_parquet(raw_sensor_files[1])

# Clean data -------------------------------------------------------------------



