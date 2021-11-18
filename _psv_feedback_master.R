# PSV Rider Feedback

# Filepaths --------------------------------------------------------------------
#### Root
if(Sys.info()[["user"]] == "robmarty"){
  dropbox_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"
  github_dir <- "~/Documents/Github/PSV-Rider-Feedback/"
}

if(Sys.info()[["user"]] == "WB521633"){
  dropbox_dir <- "C:/Users/wb521633/Dropbox/World Bank/IEs/PSV Rider Feedback"
  github_dir <- "C:/Users/wb521633/Documents/Github/PSV-Rider-Feedback/"
  onedrive_dir <- "C:/Users/wb521633/OneDrive - WBG/PSV Rider Feedback"
}

#### From Root
data_dir                  <- file.path(dropbox_dir, "Data")
sensors_dir               <- file.path(data_dir, "Sensor Data")
sensor_install_survey_dir <- file.path(data_dir, "Matatu Sensor Installation Survey")
sacco_route_dir           <- file.path(data_dir, "Sacco Route Data")

# Packages ---------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(lubridate)
library(gridExtra)
library(hrbrthemes)
library(readxl)
library(labelled)
library(haven)
library(stringr)
library(rmarkdown)
library(wesanderson)
library(knitr)
library(data.table)
library(DT)
library(syn)
library(kableExtra)
library(openssl)
library(ggmap)
library(lubridate)
library(httr)
library(jsonlite)
library(arrow)
library(furrr)
library(geosphere)
library(sf)
library(sfheaders)

# Functions --------------------------------------------------------------------
source(file.path(github_dir, "DataWork", "Sensor Data", "_wialon_api_functions.R"))

# Settings ---------------------------------------------------------------------
RUN_CODE <- F

OVERWRITE_EXTRACTED_DATA <- F

# Code -------------------------------------------------------------------------
if(RUN_CODE){
  # Process Sensor Data --------------------------------------------------------
  sensor_code_dir <- file.path(github_dir, "DataWork", "Sensor Data")
  
  # Check Wailon Token Still Valid
  source(file.path(sensor_code_dir, "00_check_wialon_token.R"))
  
  # Download Sensor Data
  source(file.path(sensor_code_dir, "02_download_data.R"))
  
  # Process Sensor Tracing Data
  source(file.path(sensor_code_dir, "03a_sensor_data_json_to_df.R"))
  source(file.path(sensor_code_dir, "03b_sensor_to_hourly_individual_files.R"))
  source(file.path(sensor_code_dir, "03c_append_sensor_hr_data.R"))
  
  # Process Echo driving Data
  source(file.path(sensor_code_dir, "04a_append_echodriving_raw.R"))
  source(file.path(sensor_code_dir, "04a_echodriving_to_hourly_individual_files.R"))
  source(file.path(sensor_code_dir, "04b_append_echodriving_hr_data.R"))
  
  # Create Day and Day/Hour data with sensor tracing + echo driving merged in
  source(file.path(sensor_code_dir, "05_merge_dayhr_data.R"))
  source(file.path(sensor_code_dir, "06_aggregate_to_daily.R"))
  
}








