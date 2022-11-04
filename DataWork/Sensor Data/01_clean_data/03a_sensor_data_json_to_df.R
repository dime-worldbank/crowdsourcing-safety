# Convert sensor tracing data from json to dataframe

# TODO: ISSUE WITH: "C:/Users/wb521633/OneDrive - WBG/PSV Rider Feedback/Data/Sensor Data/RawData/sensor_tracing_individual_data/2021-12-29/sensortracing_24493303_2021-12-29.gz.parquet"
# -- Didn't download correctly. Wrong variables assigned.

# Load Token/IDS ---------------------------------------------------------------
# Load token associated with account and IDs (numeric IDs for vehicles, reports
# and resource)
users_df    <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "user_ids.csv"))
report_df   <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "report_ids.csv"))

report_id = report_df$id[report_df$n %in% "Units Sensors Tracing - all"]

# Process Sensor Tracing Data --------------------------------------------------
IN_FILES <- file.path(sensors_dir, "RawData", "sensor_tracing_individual_data_temp_raw_jsons") %>%
  list.files(pattern = "*.Rds",
             recursive = T,
             full.names = T) 

if(!(Sys.info()[["user"]] %in% "WB521633")){
  #IN_FILES <- rev(IN_FILES)
}

#IN_FILES <- IN_FILES %>% str_subset("2022-10-04|2022-10-03|2022-10-05") %>% str_subset("24647423|24271205|23256276")

tmp <- lapply(IN_FILES, function(file_i){
  print(file_i)
  
  ## Load Data
  json_tmp <- readRDS(file_i)
  
  ## Out File
  OUT_FILE_i <- file_i %>% 
    str_replace_all("/sensor_tracing_individual_data_temp_raw_jsons/",
                    "/sensor_tracing_individual_data/") %>%
    str_replace_all(".Rds", ".gz.parquet")
  
  ## User ID
  user_id_i <- OUT_FILE_i %>% 
    str_replace_all("sensortracing_", "") %>% 
    str_replace_all(".*/", "") %>%
    str_replace_all("_.*", "")
  
  date_folder <- OUT_FILE_i %>% str_replace_all("/sensortracing_.*", "")
  dir.create(date_folder)
  
  df_out <- report_json_to_df(results_list = json_tmp,
                              report_id = report_id,
                              user_id = user_id_i,
                              users_df = users_df)
  
  write_parquet(df_out, OUT_FILE_i, compression = "gzip", compression_level = 5)
  rm(df_out)
  file.remove(file_i)
  
  return("Done!")
})

