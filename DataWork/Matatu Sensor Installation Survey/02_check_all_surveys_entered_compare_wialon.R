# Check all Surveys Entered

# Check all surverys entered for all vehicles with sensor installed. Check
# sensors against Wialon data

# Load data --------------------------------------------------------------------
wialon_df <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "user_ids.csv"))
survey_df <- read_dta(file.path(sensor_install_survey_dir, "FinalData", "psv_sensor_installation.dta"))

survey_df <- survey_df %>%
  arrange(submissiondate) %>%
  distinct(matatu_regno, .keep_all = T)

# Cleanup Reg Nos --------------------------------------------------------------
survey_df$regno <- survey_df$matatu_regno %>% 
  str_replace_all(" ", "") %>% 
  toupper()

wialon_df$regno <- wialon_df$nm %>%
  str_replace_all("Hiace|Sprinter|Mazda Bongo|Scania|Noah", "") %>%
  str_replace_all(" ", "") %>% 
  toupper()

# Checks -----------------------------------------------------------------------
# In survey data, but not Wialon
survey_df$regno[!(survey_df$regno %in% wialon_df$regno)]

# In Wialon data, but not survey
wialon_df$regno[!(wialon_df$regno %in% survey_df$regno)]

wialon_df$regno %>% sort()
