# PSV Rider Feedback

read.csv_filename <- function(filepath){
  df <- read.csv(filepath,
                 stringsAsFactors=F)
  df$file <- filepath
  return(df)
}

# Read Data --------------------------------------------------------------------
data <- file.path(onedrive_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "RawData - PII") %>%
  list.files(pattern = "*.csv", full.names = T) %>%
  lapply(read.csv_filename) %>%
  bind_rows()

# Prep Variables ---------------------------------------------------------------
#### Award Amount
data$award <- NA
data$award[grepl("100_KES", data$file)] <- 100
data$award[grepl("200_KES", data$file)] <- 200
data$award[grepl("Get_Airtime", data$file)] <- 50
data$award[grepl("Win_Airtime", data$file)] <- 50

#### Get or Win Award
data$win_get_award[grepl("Win_Airtime", data$file)] <- "win"
data$win_get_award[is.na(data$win_get_award)] <- "Get"

#### How Identify Vehicle
data$how_identif <- "number"
data$how_identif[grepl("Get_Airtime|Win_Airtime", data$file)] <- "reg no"

#### Hash Phone Number
data$phone_hash <- data$phone %>% as.factor() %>% as.numeric()
data$phone <- NULL

# Export -----------------------------------------------------------------------
write.csv(data, file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "RawData", 
                          "echo_data.csv"), row.names = F)
saveRDS(data, file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "RawData", 
                          "echo_data.Rds"))



