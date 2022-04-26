# Randomization

# R Version: 4.1.3 (ARM)
set.seed(42)

# Load Data --------------------------------------------------------------------
survey_df <- readRDS(file.path(sensor_install_survey_dir, "FinalData", 
                               "psv_sensor_installation_clean.Rds"))

# Sort data and randomize order ------------------------------------------------
# Data sorted to ensure randomization reproducibility
survey_df <- survey_df %>%
  arrange(matatu_regno) 

survey_df <- survey_df[sample(1:nrow(survey_df)),]  

# Randomize --------------------------------------------------------------------
# Cross randomization, stratefied by SACCO
# --> Sticker [75]
# ----> Driver feedback
# ----> No driver feedback
# --> No Sticker [75]
# ----> Driver feedback
# ----> No driver feedback

# 29 SACCOs

rand_list <- rep(c("sticker-driver_feedback",
                   "sticker-no_driver_feedback",
                   "no_sticker-driver_feedback",
                   "no_sticker-no_driver_feedback"), 3)

survey_df <- survey_df %>%
  group_by(sacco) %>%
  dplyr::mutate(rand_group = rand_list[1:n()]) %>%
  ungroup()

survey_df$rand_group %>% table()

# Export -----------------------------------------------------------------------
saveRDS(survey_df, file.path(sensor_install_survey_dir, "FinalData", 
                             "psv_sensor_installation_clean_randomized.Rds"))



