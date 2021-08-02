# Download Data from Wialon System

# Downloads data from two report types:
# (1) Echo Driving Report [Dataset of harsh driving events]
# (2) Sensor Tracing [Dataset for each time sensor records any value; speed/location]
# Creates a file for each day & vehicle

# Parameters -------------------------------------------------------------------
# If should skip data already scraped (checking vehicle and day). 
# If TRUE, re-scrapes data
# If FALSE, skips data already scraped
OVERWRITE_DATA_SENSORTRACING <- F
OVERWRITE_DATA_ECHODRIVING   <- F

# Load Token/IDS ---------------------------------------------------------------
# Load token associated with account and IDs (numeric IDs for vehicles, reports
# and resource)
wialon_token <- read.table(file.path(sensors_dir, "wialon_token", "wialon_token.txt"), stringsAsFactors = F)$V1

# Regenerate IDs - Needed in case additional vehicles added
source(file.path(github_dir, "DataWork", "Sensor Data", "01_get_wialon_ids.R")) 

users_df    <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "user_ids.csv"))
resource_df <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "resource_ids.csv"))
report_df   <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "report_ids.csv"))

resource_id    <- resource_df$id

# Dates to Scrape --------------------------------------------------------------
# Don't scrape today; wait until the day is complete so get full data.

scrape_until_date <- Sys.time() %>% with_tz(tzone = "UTC") %>% as.Date()

dates <- seq(from = ymd("2021-07-29"),
             to = scrape_until_date,
             by = 1
) %>%
  as.character()

# Download Echo Driving Report -------------------------------------------------
for(date_i in dates){
  print(date_i)
  
  file_name <- paste0("echodriving_", date_i, ".gz.parquet")
  file_dir <- file.path(sensors_dir, "RawData", "echo_driving_individual_data", file_name)
  
  if(!file.exists(file_dir) | OVERWRITE_DATA_ECHODRIVING){
    df_out <- map_df(unique(users_df$id), function(user_id_i){
      get_report(user_id = user_id_i, 
                 report_id = report_df$id[report_df$n %in% "Eco Driving Report"], 
                 resource_id = resource_id, 
                 datetime_begin = paste(date_i, "00:00:00") %>% ymd_hms(tz = "UTC") %>% as.numeric(), 
                 datetime_end = paste(date_i, "23:59:59") %>% ymd_hms(tz = "UTC") %>% as.numeric(),
                 wialon_token,
                 users_df)
    })
    
    write_parquet(df_out, file_dir, compression = "gzip", compression_level = 5)
  }
  
}

# Download Sensor Tracing Data -------------------------------------------------
for(date_i in dates){
  for(user_id_i in unique(users_df$id)){
    print(date_i)
    
    file_name <- paste0("sensortracing_", user_id_i, "_", date_i, ".gz.parquet")
    file_dir <- file.path(sensors_dir, "RawData", "sensor_tracing_individual_data", file_name)
    
    if(!file.exists(file_dir) | OVERWRITE_DATA_SENSORTRACING){
      
      df_out <- get_report(user_id = user_id_i, 
                           report_id = report_df$id[report_df$n %in% "Units Sensors Tracing - all"], 
                           resource_id = resource_id, 
                           datetime_begin = paste(date_i, "00:00:00") %>% ymd_hms(tz = "UTC") %>% as.numeric(), 
                           datetime_end = paste(date_i, "23:59:59") %>% ymd_hms(tz = "UTC") %>% as.numeric(),
                           wialon_token,
                           users_df)
      
      write_parquet(df_out, file_dir, compression = "gzip", compression_level = 5)
    }
    
  }
}


