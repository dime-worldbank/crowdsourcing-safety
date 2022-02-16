# Merge Sensor Data with Survey Data

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(sensor_install_survey_dir, "FinalData", "psv_sensor_installation_clean.Rds"))

ed_df <- readRDS(file.path(sensors_dir, "FinalData", "echodriving_dayhr.Rds"))
st_df <- readRDS(file.path(sensors_dir, "FinalData", "sensortracing_dayhr_dataonly.Rds"))
st_sf <- readRDS(file.path(sensors_dir, "FinalData", "sensortracing_dayhr_datapolyline.Rds"))

# Individual dataset fixes -----------------------------------------------------
# Ensure variable types are the same
ed_df$reg_no_id <- ed_df$reg_no_id %>% as.character()

# Echo/Sensor Data -------------------------------------------------------------
sensor_df <- st_df %>%
  full_join(ed_df, by = c("reg_no_id", "reg_no", "datetime_eat"))

sensor_sf <- st_sf %>%
  full_join(ed_df, by = c("reg_no_id", "reg_no", "datetime_eat"))

# Clean Reg No -----------------------------------------------------------------
clean_reg_no <- function(x){
  x %>%
    as.character() %>%
    str_replace_all(" ", "") %>%
    tolower() %>%
    str_sub(-7,-1)
}

sensor_df <- sensor_df %>%
  mutate(regno_clean = reg_no %>% clean_reg_no())

sensor_sf <- sensor_sf %>%
  mutate(regno_clean = reg_no %>% clean_reg_no())

# Merge survey -----------------------------------------------------------------
sensor_df <- sensor_df %>%
  left_join(survey_df, by = "regno_clean")

sensor_sf <- sensor_sf %>%
  left_join(survey_df, by = "regno_clean")

# Adjust Variables -------------------------------------------------------------
## Make reg number in format AAA AAAA (add space and make upper)
sensor_df <- sensor_df %>%
  mutate(regno_clean = regno_clean %>%
           str_replace_all('^(.{3})(.*)$',
                           '\\1 \\2') %>%
           toupper())

sensor_sf <- sensor_sf %>%
  mutate(regno_clean = regno_clean %>%
           str_replace_all('^(.{3})(.*)$',
                           '\\1 \\2') %>%
           toupper())

## Replace NAs
sensor_df <- sensor_df %>%
  dplyr::mutate_at(vars(contains("N_violation"),
                        contains("_valueg")), ~ replace_na(., 0))

sensor_sf <- sensor_sf %>%
  dplyr::mutate_at(vars(contains("N_violation"),
                        contains("_valueg")), ~ replace_na(., 0))

## Add Hour and Date
sensor_df <- sensor_df %>%
  dplyr::mutate(date = datetime_eat %>% as.Date(tz = "Africa/Nairobi"),
                hour = datetime_eat %>% hour())

sensor_sf <- sensor_sf %>%
  dplyr::mutate(date = datetime_eat %>% as.Date(tz = "Africa/Nairobi"),
                hour = datetime_eat %>% hour())

# Export -----------------------------------------------------------------------
write_parquet(sensor_df, file.path(sensors_dir, "FinalData", "sensor_dayhr.gz.parquet"), 
              compression = "gzip", compression_level = 5)
saveRDS(sensor_df, file.path(sensors_dir, "FinalData", "sensor_dayhr.Rds"))

saveRDS(sensor_sf, file.path(sensors_dir, "FinalData", "sensor_dayhr_polyline.Rds"))



