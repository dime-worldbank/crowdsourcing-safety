# Send select data to dropbox

# Send select, non-PII data to dropbox. This is done in case anyone needs to 
# analyze data but does not have a WB computer / cannot access OneDrive

# Data in Dropbox will always be overwriten by data in OneDrive

OVERWRITE_RAWDATA   <- F
OVERWRITE_FINALDATA <- T

# Sensor Data ------------------------------------------------------------------
#### Grab files
onedrive_files_tomove <- file.path(sensors_dir) %>%
  list.files(full.names = T,
             recursive = T)

onedrive_files_tomove_rawdata <- onedrive_files_tomove %>% 
  str_subset("RawData|individual_files|Outputs") 

onedrive_files_tomove_finaldata <- onedrive_files_tomove %>% 
  str_subset("FinalData")
onedrive_files_tomove_finaldata <- onedrive_files_tomove_finaldata[!(str_detect(onedrive_files_tomove_finaldata,
                                                                                "individual_files"))]

#### FinalData
print("Final Data")
files_length <- length(onedrive_files_tomove_finaldata)
i <- 1
for(onedrive_file_i in onedrive_files_tomove_finaldata){
  if((i %% 1) %in% 0) print(paste(i, "/", files_length))
  
  db_file_i <- onedrive_file_i %>%
    str_replace_all(onedrive_dir, dropbox_dir)
  
  file.copy(onedrive_file_i, db_file_i, overwrite = OVERWRITE_FINALDATA)
  
  i <- i + 1
}


#### RawData
print("Raw Data")
files_length <- length(onedrive_files_tomove_rawdata)
i <- 1
for(onedrive_file_i in onedrive_files_tomove_rawdata){
  if((i %% 1000) %in% 0) print(paste(i, "/", files_length))

  db_file_i <- onedrive_file_i %>%
    str_replace_all(onedrive_dir, dropbox_dir)

  file.copy(onedrive_file_i, db_file_i, overwrite = OVERWRITE_RAWDATA)

  i <- i + 1
}



