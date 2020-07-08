# PSV Rider Feedback


data <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "RawData", 
                          "echo_data.Rds"))

data <- data %>%
  arrange(start_date)


data$complete_date %>% substring(1,10) %>% table()

data$start_date %>% hour() %>% hist()

data$phone_hash %>% table %>% table()


data$DRIVER.RATING._L %>% table()
data$SPEED.RATING._L %>% table()

a <- data[data$DRIVER.RATING._L %in% "Very Unsafe",]
data$DRIVER.RATING._L %>% table()

data$MATATU.AMENITIES._R



data$start_date %>% as.Date() %>% table()
