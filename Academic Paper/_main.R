# Crowdsourcing Safety Paper

RUN_CODE <- T

# Filepaths --------------------------------------------------------------------

# Rob (Personal Comptuer)
if(Sys.info()[["user"]] == "robmarty"){
  db_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"
  data_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback/Academic Paper/Data"
  git_dir  <- "~/Documents/Github/PSV-Rider-Feedback/Academic Paper"
  overleaf_dir <- "~/Dropbox/Apps/Overleaf/Crowdsourcing Safety Kenya Matatu Passenger Safety Project"
}

# Ruiwen (WB Computer)
if(Sys.info()[["user"]] == "wb575963"){
  db_dir <- "C:/Users/wb575963/Dropbox/PSV Rider Feedback"
  data_dir <- "C:/Users/wb575963/Dropbox/PSV Rider Feedback/Academic Paper/Data"
  git_dir <- "C:/Users/wb575963/Github/PSV-Rider-Feedback/Academic Paper"
}

tables_dir  <- file.path(overleaf_dir, "tables")
figures_dir <- file.path(overleaf_dir, "figures")

# Packages ---------------------------------------------------------------------
library(magrittr)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(sf)
library(readxl)
library(janitor)
library(arrow)
library(quanteda)
library(wordcloud2)
library(tidytext)
library(sentimentr)
library(ggpubr)
library(DescTools)
library(forcats)
library(tm)
library(quanteda)
library(scales)
#library(cleanNLP)
#library(coreNLP)

# Parameters -------------------------------------------------------------------
DRIVING_WORDS <- "safe|drunk|accident|careless"
COVID_WORDS <- "covid|pandemic|mask|social distance|sanitizer|sanitiser"

# Run Code =====================================================================
if(RUN_CODE){
  
  # Clean data -----------------------------------------------------------------
  git_clean_data_dir <- file.path(git_dir, "01_clean_data")
  
  source(file.path(git_clean_data_dir, "01_feedback_outliers.R"))
  source(file.path(git_clean_data_dir, "02_classify_feedback.R"))
  source(file.path(git_clean_data_dir, "03_make_vehicle_level_data.R"))
  
  # Passenger feedback analysis ------------------------------------------------
  git_feedback_dir <- file.path(git_dir, "02_analysis", "passenger_feedback_analysis")
  
  source(file.path(git_feedback_dir, "ratings_sum_stats_table.R"))
  source(file.path(git_feedback_dir, "ratings_vs_comments_indiv_boxplot.R"))
  source(file.path(git_feedback_dir, "ratings_vs_comments_vehicle_scatter.R"))
  source(file.path(git_feedback_dir, "safety_speed_v1_crosstab.R"))
  source(file.path(git_feedback_dir, "safety_speed_v2_crosstab.R"))
  source(file.path(git_feedback_dir, "sentiment_distribution.R"))
  source(file.path(git_feedback_dir, "sentiment_vs_class.R"))
  source(file.path(git_feedback_dir, "top_pos_neg_words.R"))
  source(file.path(git_feedback_dir, "vehicle_indicators_distribution.R"))
  
  # Telematics data analysis ---------------------------------------------------
  git_telematics_dir <- file.path(git_dir, "02_analysis", "telematics_analysis")
  
  source(file.path(git_telematics_dir, "compare_variables.R"))
  source(file.path(git_telematics_dir, "indicator_distribution.R"))
  source(file.path(git_telematics_dir, "indicator_sum_stat_table.R"))
  
  # Telematics vs feedback analysis --------------------------------------------
  git_tele_vs_feed_dir <- file.path(git_dir, "02_analysis", "telematics_vs_feedback")
  
  source(file.path(git_tele_vs_feed_dir, "correlation_plot.R"))
  source(file.path(git_tele_vs_feed_dir, "scatter_plots.R"))
  
}

