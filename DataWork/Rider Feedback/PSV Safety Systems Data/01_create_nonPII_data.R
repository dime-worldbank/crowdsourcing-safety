# PSV Rider Feedback

read.csv_filename <- function(filepath){
  df <- read_csv(filepath)
  
  for(var in names(df)){
    df[[var]] <- df[[var]] %>% as.character()
    df[[paste0("asked_", var)]] <- "yes"
  } 
  
  df$file <- filepath
  
  return(df)
}

# Read Data --------------------------------------------------------------------
DATA_PATH <- file.path(onedrive_file_path, "Data", "Rider Feedback", "PSV Safety Systems Data", "RawData - PII")

data <- file.path(onedrive_file_path, "Data", "Rider Feedback", "PSV Safety Systems Data", "RawData - PII") %>%
  list.files(pattern = "*.csv", full.names = T) %>%
  lapply(read.csv_filename) %>%
  bind_rows()

# Prep Variables ---------------------------------------------------------------

#### Hash Phone Number
data$phone_hash <- data$`Phone Number` %>% as.factor() %>% as.numeric()
data$phone <- NULL

#### Remove other PII Vars
data$Name <- NULL
data$Email <- NULL

# Export -----------------------------------------------------------------------
write.csv(data, file.path(dropbox_file_path, "Data", "Rider Feedback", "PSV Safety Systems Data", "RawData", 
                          "qr_data.csv"), row.names = F)
saveRDS(data, file.path(dropbox_file_path, "Data", "Rider Feedback", "PSV Safety Systems Data", "RawData", 
                          "qr_data.Rds"))


