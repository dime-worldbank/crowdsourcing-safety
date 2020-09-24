# Append Data for Picking Winners

sc_data <- file.path(onedrive_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "RawData - PII") %>%
  list.files(pattern = "*.csv", full.names = T) %>%
  str_subset("Win_Airtime__SHORT") %>%
  read_csv() %>%
  dplyr::select(phone, complete_date) %>%
  filter(!is.na(complete_date)) %>%
  mutate(feedback_method = "shortcode")


qr_data <- file.path(onedrive_file_path, "Data", "Rider Feedback", "PSV Safety Systems Data", "RawData - PII", "Feedback by WB - Win Award.csv") %>%
  read_csv() %>%
  filter(`Leave any comments about the matatu or ride. For example, report unsafe driving or compliment the driver.` != "TEST") %>%
  dplyr::select(`Phone Number`, Created) %>%
  dplyr::rename(phone = "Phone Number",
                complete_date = Created) %>%
  mutate(feedback_method = "qr code")

data <- bind_rows(sc_data,
                  qr_data)

# Export -----------------------------------------------------------------------
write.csv(data, file.path(onedrive_file_path, "Data", "Awards", "Eligible Winners", "eligible_winners.csv"), row.names = F)


