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
  str_sub(-9, -1) %>% # sometimes "ABC 1234" other "A 123 456"
  str_replace_all(" ", "") %>% 
  str_squish() %>%
  toupper()

# Checks -----------------------------------------------------------------------
#### Make dataframes

# In survey data, but not Wialon
in_survey_not_wialon_df <- survey_df$regno[!(survey_df$regno %in% wialon_df$regno)]

if(length(in_survey_not_wialon_df) %in% 0){
  in_survey_not_wialon_df <- "NONE"
} else{
  # Add space after 3 characters to make easier to read
  in_survey_not_wialon_df <- in_survey_not_wialon_df %>%
    str_replace_all("^(.{3})(.*)$", "\\1 \\2")
}

in_survey_not_wialon_df <- in_survey_not_wialon_df %>%
  as.data.frame() %>%
  dplyr::rename(reg_no = ".") %>%
  dplyr::mutate(id = 1:n()) %>%
  dplyr::select(id, reg_no) %>%
  dplyr::mutate(last_updated = Sys.time())

# In Wialon data, but not survey
in_wialon_not_survey_df <- wialon_df$regno[!(wialon_df$regno %in% survey_df$regno)]

if(length(in_wialon_not_survey_df) %in% 0){
  in_wialon_not_survey_df <- "NONE"
} else{
  # Add space after 3 characters to make easier to read
  in_wialon_not_survey_df <- in_wialon_not_survey_df %>%
    str_replace_all("^(.{3})(.*)$", "\\1 \\2")
}

in_wialon_not_survey_df <- in_wialon_not_survey_df %>%
  as.data.frame() %>%
  dplyr::rename(reg_no = ".") %>%
  dplyr::mutate(id = 1:n()) %>%
  dplyr::select(id, reg_no) %>%
  dplyr::mutate(last_updated = Sys.time())

#### Export
write.csv(in_survey_not_wialon_df, 
          file.path(sensor_install_survey_dir,
                    "Outputs",
                    "data_in_survey_not_wialon.csv"), 
          row.names = F)

write.csv(in_wialon_not_survey_df, 
          file.path(sensor_install_survey_dir,
                    "Outputs",
                    "data_in_wialon_not_survey.csv"), 
          row.names = F)

#### Update to Google Drive
# Sensor Survey: Sensor Data But No Survey Data
sheet_write(data = in_wialon_not_survey_df,
            ss = "1KI6ocPsd-C31CkbWyceTXIel0c2vMuqgmgEOB0cG0DY",
            sheet = "vehicles")

# Sensor Data: In Survey But Not In Sensor Data
sheet_write(data = in_survey_not_wialon_df,
            ss = "1bFiBvpbIV9pf8VSJvWa_LUVNoOJEANl_rNUsDUQaEE8",
            sheet = "vehicles")
