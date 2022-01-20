# Send select data to dropbox

# Send select, non-PII data to dropbox. This is done in case anyone needs to 
# analyze data but does not have a WB computer / cannot access OneDrive

# Data in Dropbox will always be overwriten by data in OneDrive

OVERWRITE_FILES <- F

onedrive_files_tomove <- file.path(sensors_dir) %>%
  list.files(full.names = T,
             recursive = T)

files_length <- length(onedrive_files_tomove)
i <- 1
for(onedrive_file_i in onedrive_files_tomove){
  if((i %% 100) %in% 0) print(paste(i, "/", files_length))
  
  db_file_i <- onedrive_file_i %>% 
    str_replace_all(onedrive_dir, dropbox_dir)
  
  file.copy(onedrive_file_i, db_file_i, overwrite = OVERWRITE_FILES)
  
  i <- i + 1
}

