# Merge Sensor Data with Survey Data

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(sensor_install_survey_dir, "FinalData", "psv_sensor_installation_clean.Rds"))

ed <- read_parquet(file.path(sensors_dir, "FinalData", "echodriving.gz.parquet"))
st <- read_parquet(file.path(sensors_dir, "FinalData", "sensortracing.gz.parquet"))

# Clean Reg No -----------------------------------------------------------------
ed <- ed %>%
  dplyr::mutate(regno_clean = reg_no %>%
                  as.character() %>%
                  str_replace_all(" ", "") %>%
                  tolower() %>%
                  str_sub(-7,-1))

st <- st %>%
  dplyr::mutate(regno_clean = reg_no %>%
                  as.character() %>%
                  str_replace_all(" ", "") %>%
                  tolower() %>%
                  str_sub(-7,-1))

# Merge ------------------------------------------------------------------------
ed <- ed %>%
  left_join(survey_df, by = "regno_clean")

st <- st %>%
  left_join(survey_df, by = "regno_clean")

# Adjust Variables -------------------------------------------------------------
## Make reg number in format AAA AAAA (add space and make upper)

ed <- ed %>%
  mutate(regno_clean = regno_clean %>%
           str_replace_all('^(.{3})(.*)$',
                           '\\1 \\2') %>%
           toupper())

st <- st %>%
  mutate(regno_clean = regno_clean %>%
           str_replace_all('^(.{3})(.*)$',
                           '\\1 \\2') %>%
           toupper())

# Export -----------------------------------------------------------------------
write_parquet(ed, file.path(sensors_dir, "FinalData", "echodriving_clean.gz.parquet"), 
              compression = "gzip", compression_level = 5)

write_parquet(st, file.path(sensors_dir, "FinalData", "sensortracing_clean.gz.parquet"), 
              compression = "gzip", compression_level = 5)

