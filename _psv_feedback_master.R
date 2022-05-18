# PSV Rider Feedback

# Settings ---------------------------------------------------------------------
#### CODE TO RUN
RUN_CODE <- F # If F, doesn't run any code, no matter the below settings

PROCESS_SENSOR_DATA        <- F # Download and clean sensor data (takes a while)
PROCESS_SENSOR_SURVEY_DATA <- F # Clean survey data. RUN STATA MASTER FIRST TO DOWNLOAD DATA.

#### Other
# Relevant for sensor data. If F, checks if data has already been downloaded. If
# already downloaded, skips downloading it. Set to T if want to redownload data.
OVERWRITE_EXTRACTED_DATA <- F

# Authenticate Google Sheets ---------------------------------------------------
if(F){
  # Code for processing sensor survey data uploads some data to google sheets
  # (so the sensor company can see issues with the survey that they can correct).
  # Can run these two lines to make sure google sheets in authenticated.
  # Relevant for: 03_check_all_surveys_entered_compare_wialon.R
  library(googlesheets4)
  gs4_auth()
}


# Filepaths --------------------------------------------------------------------
#### Root
# Rob (Personal Comptuer)
if(Sys.info()[["user"]] == "robmarty"){
  dropbox_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"
  onedrive_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"
  github_dir <- "~/Documents/Github/PSV-Rider-Feedback/"
}

# Rob (WB Computer)
if(Sys.info()[["user"]] == "WB521633"){
  dropbox_dir <- "C:/Users/wb521633/Dropbox/World Bank/IEs/PSV Rider Feedback"
  github_dir <- "C:/Users/wb521633/Documents/Github/PSV-Rider-Feedback/"
  onedrive_dir <- "C:/Users/wb521633/OneDrive - WBG/PSV Rider Feedback"
}

# Ruiwen (WB Computer)
if(Sys.info()[["user"]] == "wb575963"){
  dropbox_dir <- "C:/Users/wb575963/Dropbox/PSV Rider Feedback"
  github_dir <- "C:/Users/wb575963/Github/PSV-Rider-Feedback/"
  onedrive_dir <- "C:/Users/wb575963/WBG/Robert Andrew Marty - PSV Rider Feedback"
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
library(googlesheets4)
library(leaflet)
library(progress)
library(ggpubr)

# Functions --------------------------------------------------------------------
source(file.path(github_dir, "DataWork", "Sensor Data", "01_clean_data", "_wialon_api_functions.R"))
source(file.path(github_dir, "Functions", "load_sensortracing_raw.R"))

# Code -------------------------------------------------------------------------
if(RUN_CODE){
  
  # Process Sensor Data --------------------------------------------------------
  if(PROCESS_SENSOR_DATA){
    sensor_code_dir <- file.path(github_dir, "DataWork", "Sensor Data", "01_clean_data")
    
    # Check Wailon Token Still Valid
    source(file.path(sensor_code_dir, "00_check_wialon_token.R"))
    
    # Download Sensor Data
    source(file.path(sensor_code_dir, "02_download_data.R"))
    
    # Process Sensor Tracing Data
    # A. Sensor data downloaded as json. Convert json to dataframe/export as
    #    parquet file.
    # B. Aggregate sensor data to hourly level and save file for each vehicle
    #    and day (so file at hourly level for each vehilce and day). Saves file
    #    with data including polyline of route and a separate smaller file
    #    that just includes the data (with no polyline)
    # C. Append sensor hourly data, creating file with all data appended.
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
  
  # Process Sensor Installation Survey -----------------------------------------
  if(PROCESS_SENSOR_SURVEY_DATA){
    sensor_survey_code_dir <- file.path(github_dir, "DataWork", "Matatu Sensor Installation Survey")
    
    source(file.path(sensor_survey_code_dir, "02_clean_data.R"))
    source(file.path(sensor_survey_code_dir, "03_check_all_surveys_entered_compare_wialon.R"))
  }
  
  
}








