# Download Data from Wialon System

# Downloads data from two report types:
# (1) Echo Driving Report [Dataset of harsh driving events]
# (2) Sensor Tracing [Dataset for each time sensor records any value; speed/location]
# Creates a file for each day & vehicle

# API LIMITS
# Main one seems to be: "No more than 200 executions within 5 minutes by a user from one IP address"
# We make 3 API calls in each 
# https://sdk.wialon.com/wiki/en/sidebar/remoteapi/apiref/limits/limits#:~:text=No%20more%20than%2010%2D%20API,be%20processed%20during%2010%20seconds

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
scrape_until_date <- scrape_until_date - 1

dates <- seq(from = ymd("2021-07-01"),
             to = scrape_until_date,
             by = 1
) %>%
  as.character()

#dates <- "2021-12-29"

# Download Echo Driving Report -------------------------------------------------
for(date_i in rev(dates)){
  print(date_i)
  
  file_name <- paste0("echodriving_", date_i, ".gz.parquet")
  file_dir <- file.path(sensors_dir, "RawData", "echo_driving_individual_data", file_name)
  
  if(!file.exists(file_dir) | OVERWRITE_DATA_ECHODRIVING){
    df_out <- map_df(unique(users_df$id), function(user_id_i){
      
      json_tmp <- get_report_raw_json(user_id = user_id_i, 
                                      report_id = report_df$id[report_df$n %in% "Eco Driving Report"], 
                                      resource_id = resource_id, 
                                      datetime_begin = paste(date_i, "00:00:00") %>% ymd_hms(tz = "UTC") %>% as.numeric(), 
                                      datetime_end = paste(date_i, "23:59:59") %>% ymd_hms(tz = "UTC") %>% as.numeric(),
                                      wialon_token,
                                      users_df)
      
      df_out_i <- report_json_to_df(results_list = json_tmp,
                                    report_id = report_df$id[report_df$n %in% "Eco Driving Report"],
                                    user_id = user_id_i,
                                    users_df = users_df)
      
      return(df_out_i)
    })
    
    write_parquet(df_out, file_dir, compression = "gzip", compression_level = 5)
    rm(df_out)
  }
  
}

# user_id = unique(users_df$id)[1]
# report_id = report_df$id[report_df$n %in% "Eco Driving Report"] 
# resource_id = resource_id
# datetime_begin = paste(date_i, "00:00:00") %>% ymd_hms(tz = "UTC") %>% as.numeric() 
# datetime_end = paste(date_i, "23:59:59") %>% ymd_hms(tz = "UTC") %>% as.numeric()
# wialon_token
# users_df

# Download Sensor Tracing Data -------------------------------------------------
for(date_i in rev(dates)){
  print(date_i)
  for(user_id_i in unique(users_df$id)){

    # Parquet is final data while rds is temp data, later to be turned into parquet
    file_name_parquet <- paste0("sensortracing_", user_id_i, "_", date_i, ".gz.parquet")
    file_name_rds     <- paste0("sensortracing_", user_id_i, "_", date_i, ".Rds")
    
    dir.create(file.path(sensors_dir, "RawData", "sensor_tracing_individual_data", date_i))
    dir.create(file.path(sensors_dir, "RawData", "sensor_tracing_individual_data_temp_raw_jsons", date_i))
    
    file_dir_parquet <- file.path(sensors_dir, "RawData", "sensor_tracing_individual_data", date_i, file_name_parquet)
    file_dir_rds <- file.path(sensors_dir, "RawData", "sensor_tracing_individual_data_temp_raw_jsons", date_i, file_name_rds)
    
    if(!(file.exists(file_dir_parquet) | file.exists(file_dir_rds)) | OVERWRITE_DATA_SENSORTRACING){
      print(user_id_i)
      
      df_out <- get_report_raw_json(user_id = user_id_i, 
                                    report_id = report_df$id[report_df$n %in% "Units Sensors Tracing - all"], 
                                    resource_id = resource_id, 
                                    datetime_begin = paste(date_i, "00:00:00") %>% ymd_hms(tz = "UTC") %>% as.numeric(), 
                                    datetime_end = paste(date_i, "23:59:59") %>% ymd_hms(tz = "UTC") %>% as.numeric(),
                                    wialon_token,
                                    users_df)
      
      saveRDS(df_out, file_dir_rds)
      rm(df_out)
      #write_parquet(df_out, file_dir, compression = "gzip", compression_level = 5)
    }
    
  }
}

