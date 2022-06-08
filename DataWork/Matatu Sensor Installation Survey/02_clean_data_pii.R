# Clean Survey Data

# Load Data --------------------------------------------------------------------
#### Survey Data
survey_df <- read_dta(file.path(onedrive_dir, "Data", "Matatu Sensor Installation Survey",
                                "FinalData - PII", "psv_sensor_installation.dta"))

# Corrections ------------------------------------------------------------------
survey_df$matatu_sacco[survey_df$matatu_regno %in% "KCV 924M"] <- "Simba_Coach_Limited"

# Reg No and Sacco -------------------------------------------------------------
survey_df <- survey_df %>%
  dplyr::mutate(matatu_regno_clean = matatu_regno %>%
                  str_replace_all(" ", "") %>%
                  tolower() %>%
                  str_sub(-7,-1),
                matatu_sacco = case_when(
                  matatu_sacco == "OTHER" ~ sacco_name_other,
                  TRUE ~ matatu_sacco
                ),
                matatu_sacco = matatu_sacco %>%
                  str_replace_all("_", " ") %>%
                  str_replace_all("Sacco", "SACCO") %>%
                  str_squish(),
                matatu_sacco = case_when(
                  matatu_sacco == "DAR LUK SACCO" ~ "DAR LUX SACCO",
                  matatu_sacco == "Unique Shuttle" ~ "Unique Shuttle SACCO",
                  matatu_sacco == "Narok Safaris" ~ "Narok Safaris SACCO",
                  matatu_sacco == "Narok Shuttle transporter" ~ "Narok Shuttle Transporter",
                  TRUE ~ matatu_sacco
                )) %>%
  dplyr::rename(sacco = matatu_sacco,
                regno_clean = matatu_regno_clean) %>%
  distinct(regno_clean, .keep_all = T) 

# Route ------------------------------------------------------------------------
#survey_df <- survey_df %>%
#  left_join(sacco_df, by = "sacco")

# Subset Variables -------------------------------------------------------------
#survey_df <- survey_df %>%
#  dplyr::select(regno_clean, sacco, route) 

# Export -----------------------------------------------------------------------
saveRDS(survey_df, file.path(onedrive_dir, "Data", "Matatu Sensor Installation Survey",
                             "FinalData - PII", "psv_sensor_installation_clean.Rds"))
write_dta(survey_df, file.path(onedrive_dir, "Data", "Matatu Sensor Installation Survey",
                               "FinalData - PII", "psv_sensor_installation_clean.dta"))



