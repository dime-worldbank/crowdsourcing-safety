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



data <- data[data$how_identif %in% "reg no",]
data$MATATU.NUMBER._R <- data$MATATU.NUMBER._R %>% tolower() %>% str_replace_all(" ", "") 
data <- data[data$MATATU.NUMBER._R %in% "kbm199t",]
data$start_date_r <- data$start_date %>% ymd_hm() %>% round_date(unit = "hour")

data <- data %>%
  group_by(start_date_r) %>%
  dplyr::summarise(N = n())


kbm119t

