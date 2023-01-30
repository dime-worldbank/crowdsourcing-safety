# PSV Rider Feedback: Clean Data

read.csv_filename <- function(filepath){
  df <- read_csv(filepath)
  
  for(var in names(df)){
    df[[var]] <- df[[var]] %>% as.character()
    df[[paste0(var, "_asked")]] <- "yes"
  } 
  
  df$file <- filepath
  
  return(df)
}

# Load QR Data -----------------------------------------------------------------
qr_data <- file.path(onedrive_file_path, "Data", "Rider Feedback", "PSV Safety Systems Data", "RawData - PII") %>%
  list.files(pattern = "*.csv", full.names = T) %>%
  lapply(read.csv_filename) %>%
  bind_rows() %>%
  mutate(response_type = "QR",
         completed = TRUE) %>%
  dplyr::rename(phone = `Phone Number`,
                date = Created) %>%
  filter(!is.na(phone),
         nchar(phone) >= 5) %>%
  mutate(date = date %>% as.Date()) %>%
  dplyr::select(phone, date, completed, response_type)

# Load QR Data -----------------------------------------------------------------
sc_data <- file.path(onedrive_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "RawData - PII") %>%
  list.files(pattern = "*.csv", full.names = T) %>%
  lapply(read.csv_filename) %>%
  bind_rows() %>%
  mutate(response_type = "Shortcode",
         completed = !is.na(complete_date)) %>%
  dplyr::rename(date = start_date) %>%
  mutate(date = date %>% as.Date()) %>%
  filter(!is.na(date)) %>%
  dplyr::select(phone, date, completed, response_type)

# Export -----------------------------------------------------------------------
data <- bind_rows(qr_data,
                  sc_data) %>%
  mutate(completed = ifelse(completed %in% T, "Yes", "No"),
         phone = phone %>% as.character())

write.csv(data, file.path(onedrive_file_path, "Data", "Rider Feedback",
                          "All Data - PII", "rider_feedback.csv"), 
          row.names = F)



