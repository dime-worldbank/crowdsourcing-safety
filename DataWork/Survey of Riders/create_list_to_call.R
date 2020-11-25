# Create list of people to call

# Load QR Data -----------------------------------------------------------------
data <- read.csv(file.path(onedrive_file_path, "Data", "Rider Feedback",
                           "All Data - PII", "rider_feedback.csv"))

data <- data %>% 
  arrange(desc(date))

df_to_call <- bind_rows(
  data %>%
    filter(response_type %in% "QR") %>%
    head(8),
  
  data %>%
    filter(response_type %in% "Shortcode") %>%
    head(200)
)


df_to_call$phone <- sub("\\s+$", "", gsub('(.{3})', '\\1 ', df_to_call$phone))

write.csv(df_to_call, file.path(onedrive_file_path, "Data", "Survey of Riders",
                                "riders_to_call",
                                paste0("to_call_",
                                       Sys.time() %>% str_replace_all(" |-|:", ""),
                                       ".csv")),
          row.names = F)
