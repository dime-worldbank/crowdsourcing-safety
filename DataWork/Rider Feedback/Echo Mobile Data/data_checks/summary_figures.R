# PSV Rider Feedback

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "RawData", 
                          "echo_data.Rds"))

# Variables --------------------------------------------------------------------
data$completed <- data$complete_date != ""
data$start_date <- data$start_date %>% ymd_hm(tz = "Africa/Nairobi")
data$date <- data$start_date %>% as.Date
data$hour <- data$start_date %>% hour()
data$minute <- data$start_date %>% minute()
data$award <- data$award %>% as.factor()

data$MATATU.NUMBER._L <- data$MATATU.NUMBER._L %>% tolower() %>% str_squish()

data$matatu_num <- data$MATATU.NUMBER._L
data$matatu_num_correct <- data$matatu_num %in% paste0(20:31)

# Subset -----------------------------------------------------------------------
data <- data %>%
  filter(date >= "2020-06-26")

data_comp <- data[data$completed %in% T,]

# By Day -----------------------------------------------------------------------
#### Completion Daily
data %>%
  group_by(date, completed) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = date, y = N, group = completed, fill = completed)) +
  theme_minimal() +
  ggsave(filename = file.path(echo_figures, "completion_daily.png"),
         height = 4, width =7)

#### Award Daily
data_comp %>%
  group_by(date, award) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = date, y = N, group = award, fill = award)) +
  theme_minimal() +
  ggsave(filename = file.path(echo_figures, "award_daily.png"),
         height = 4, width =7)

data_comp %>%
  group_by(date, award) %>%
  summarise(N = n()) %>%
  pivot_wider(names_from = award, values_from = N) %>%
  dplyr::rename(N_100 = "100",
                N_200 = "200") %>%
  mutate(avg_per_veh_100 = (N_100 / 5) %>% round(2),
         avg_per_veh_200 = (N_200 / 7) %>% round(2)) %>%
  write.csv(file = file.path(echo_figures, "award_daily.csv"), row.names = F)

#### Matatu Number
data_comp %>%
  filter(matatu_num_correct %in% T) %>%
  group_by(date, matatu_num) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = date, y = N, group = matatu_num, fill = matatu_num)) +
  theme_minimal() +
  labs(fill = "Vehicle\nNumber") +
  ggsave(filename = file.path(echo_figures, "matatu_number_daily.png"),
         height = 4, width =7)

data_comp$matatu_num_correct %>% mean()
data_comp$matatu_num[data_comp$matatu_num_correct %in% F] %>% table %>% View()

data_comp$matatu_num_correct[data_comp$date >= "2020-07-02"] %>% mean()

# By Hour ----------------------------------------------------------------------
data_comp %>%
  group_by(hour) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = hour, y = N), fill = "dodgerblue4") +
  theme_minimal() +
  ggsave(filename = file.path(echo_figures, "by_hour.png"),
         height = 4, width =7)

data_comp[data_comp$date %in% as.Date(c("2020-06-26", "2020-06-27")),] %>%
  group_by(hour) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = hour, y = N), fill = "dodgerblue4") +
  theme_minimal() +
  labs(title = "June 26 and 27") +
  ggsave(filename = file.path(echo_figures, "by_hour_2627.png"),
         height = 4, width =7)

data_comp[!(data_comp$date %in% as.Date(c("2020-06-26", "2020-06-27"))),] %>%
  group_by(hour) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = hour, y = N), fill = "dodgerblue4") +
  theme_minimal() +
  labs(title = "June 28 Onwards") +
  ggsave(filename = file.path(echo_figures, "by_hour_not2627.png"),
         height = 4, width =7)

# 10 Min Interval Bus 22 -------------------------------------------------------
data_2627_v22 <- data_comp[data_comp$date %in% as.Date(c("2020-06-26", "2020-06-27")),] 
data_2627_v22 <- data_2627_v22[data_2627_v22$matatu_num %in% "22",]

data_2627_v22$min_round <- round(data_2627_v22$minute / 10)

data_2627_v22$complete_date_r <- data_2627_v22$complete_date %>% ymd_hm() %>% lubridate::round_date("15 minutes") 

