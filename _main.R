# Crowdsourcing Safety: The Value of Passenger Feedback for Measuring Driving Safety
# Main Script

## PARAMETERS

# Whether to delete all outputs (figures, table, processed data)
DELETE_OUTPUTS <- F

# Whether to run code
RUN_CODE <- F

# Filepaths --------------------------------------------------------------------

# Rob (Personal Comptuer)
if(Sys.info()[["user"]] == "robmarty"){
  data_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback/Academic Paper/Data"
  git_dir  <- "~/Documents/Github/crowdsourcing-safety"
  overleaf_dir <- "~/Dropbox/Apps/Overleaf/Crowdsourcing Safety Kenya Matatu Passenger Safety Project"
}

# Ruiwen (WB Computer)
if(Sys.info()[["user"]] == "wb575963"){
  data_dir <- "C:/Users/wb575963/Dropbox/PSV Rider Feedback/Academic Paper/Data"
  git_dir <- "C:/Users/wb575963/Github/crowdsourcing-safety"
}

tables_dir  <- file.path(overleaf_dir, "tables")
figures_dir <- file.path(overleaf_dir, "figures")

#tables_dir  <- file.path(git_dir, "output", "tables")
#figures_dir <- file.path(git_dir, "output", "figures")

# Packages ---------------------------------------------------------------------
#renv::init(project = github_dir)
if (!require("pacman")) install.packages("pacman")

pacman::p_load(magrittr,
               dplyr,
               readr,
               tidyr,
               stringr,
               lubridate,
               ggplot2,
               ggpubr,
               gt,
               sf,
               readxl,
               janitor,
               arrow,
               quanteda,
               tidytext,
               sentimentr,
               DescTools,
               forcats,
               tm,
               scales,
               haven,
               fixest,
               purrr,
               tibble,
               did,
               chatgpt)

# Delete outputs ---------------------------------------------------------------
if(DELETE_OUTPUTS){
  #### Delete processed/final data
  rds_files <- file.path(data_dir, "FinalData") %>%
    list.files(full.names = T,
               pattern = "*.Rds")
  
  for(data_i in rds_files) file.remove(data_i)
  
  #### Delete tables and figures
  tex_files <- tables_dir %>%
    list.files(full.names = T,
               pattern = "*.tex")
  
  png_files <- figures_dir %>%
    list.files(full.names = T,
               pattern = "*.png")
  
  for(file_i in c(tex_files, png_files)) file.remove(file_i)
}

# Parameters -------------------------------------------------------------------
DRIVING_WORDS <- "safe|drunk|accident|careless"
COVID_WORDS <- "covid|pandemic|mask|social distance|sanitizer|sanitiser"

# Run Code =====================================================================
if(RUN_CODE){
  
  # Clean data -----------------------------------------------------------------
  git_clean_data_dir <- file.path(git_dir, "01_clean_data")
  
  source(file.path(git_clean_data_dir, "01_feedback_outliers.R"))
  source(file.path(git_clean_data_dir, "02_classify_feedback_gpt.R"))
  source(file.path(git_clean_data_dir, "03_classify_feedback.R"))
  source(file.path(git_clean_data_dir, "04_make_vehicle_level_data.R"))
  
  # Passenger feedback analysis ------------------------------------------------
  git_analysis_dir <- file.path(git_dir, "02_analysis")
  
  source(file.path(git_analysis_dir, "feedback_distribution.R"))
  source(file.path(git_analysis_dir, "feedback_n_survey_response_per_vehicle.R"))
  source(file.path(git_analysis_dir, "feedback_over_time.R"))
  source(file.path(git_analysis_dir, "feedback_pilot_testing.R"))
  source(file.path(git_analysis_dir, "feedback_predict_driving_sentiment.R"))
  source(file.path(git_analysis_dir, "feedback_safety_speed_v1_crosstab.R"))
  source(file.path(git_analysis_dir, "feedback_safety_speed_v2_crosstab.R"))
  source(file.path(git_analysis_dir, "feedback_sentiment_vs_safety.R"))
  source(file.path(git_analysis_dir, "feedback_sum_stats.R"))
  source(file.path(git_analysis_dir, "feedback_telematics_consistency.R"))
  source(file.path(git_analysis_dir, "feedback_telematics_correlation.R"))
  source(file.path(git_analysis_dir, "feedback_top_pos_neg_words.R"))
  source(file.path(git_analysis_dir, "feedback_variables_correlation.R"))
  source(file.path(git_analysis_dir, "feedback_versions_sample_sizes.R"))
  source(file.path(git_analysis_dir, "telematics_distribution.R"))
  source(file.path(git_analysis_dir, "telematics_indicators_unsafe_feedback.R"))
  source(file.path(git_analysis_dir, "telematics_route_summary.R"))
  source(file.path(git_analysis_dir, "telematics_sticker_regression.R"))
  source(file.path(git_analysis_dir, "telematics_sum_stat.R"))
  source(file.path(git_analysis_dir, "telematics_variables_correlation.R"))
  
}



