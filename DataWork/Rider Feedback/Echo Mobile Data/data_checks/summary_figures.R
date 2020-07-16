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

## Factors
data$DRIVER.RATING._L <- data$DRIVER.RATING._L %>% str_squish() %>%
  factor(levels = c("Very Safe", "Safe", "Unsafe", "Very Unsafe"))

data$SPEED.RATING._L <- data$SPEED.RATING._L %>% str_squish() %>%
  factor(levels = c("Too slow", "Okay", "Fast", "Dangerously Fast"))

data$MATATU.OCCUPANCY_L <- data$MATATU.OCCUPANCY_L %>% str_squish() %>%
  factor(levels = c("More people than can fit", "More people than seats", "Same number of people as seats", "Less people than seats"))

data$PASSENGER.HAND.SANITISATION_L <- data$PASSENGER.HAND.SANITISATION_L %>% str_squish() %>%
  factor(levels = c("Yes, effective", "Yes, but seemed limited", "Same number of people as seats", "No"))

data$WHEREGOING_R %>% table() %>% View()

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

### Survey award amount
data_comp %>%
  filter(matatu_num_correct %in% T) %>%
  group_by(date, award) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = date, y = N, group = award, fill = award)) +
  theme_minimal() +
  labs(fill = "Award \n Amount") +
  ggsave(filename = file.path(echo_figures, "award_daily.png"),
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
data_comp %>%
  filter(matatu_num %in% "22") %>%
  filter(date %in% as.Date(c("2020-06-26", "2020-06-27"))) %>%
  mutate(complete_date_r = complete_date %>% 
           ymd_hm(tz = "Africa/Nairobi") %>% 
           lubridate::round_date("10 minutes")) %>%
  group_by(complete_date_r) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = complete_date_r, y = N), fill = "dodgerblue4") +
  theme_minimal() +
  labs(title = "June 26 and 27") +
  ggsave(filename = file.path(echo_figures, "v22_by_10min_interval_2627.png"),
         height = 4, width =12)

data_comp %>%
  filter(matatu_num %in% "22") %>%
  filter(!(date %in% as.Date(c("2020-06-26", "2020-06-27")))) %>%
  mutate(complete_date_r = complete_date %>% 
           ymd_hm(tz = "Africa/Nairobi") %>% 
           lubridate::round_date("10 minutes")) %>%
  group_by(complete_date_r) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = complete_date_r, y = N), fill = "dodgerblue4") +
  theme_minimal() +
  labs(title = "June 28 Onwards") +
  ggsave(filename = file.path(echo_figures, "v22_by_10min_interval_not2627.png"),
         height = 4, width =12)

# Answer to Questions ----------------------------------------------------------
data_comp %>%
  group_by(DRIVER.RATING._L) %>%
  summarise(N = n()) %>%
  ggplot(aes(x = DRIVER.RATING._L, y = N)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = N), nudge_y = 30) +
  labs(x="", y="",
       title = "How would you rate your matatu driver?") +
  theme_minimal() +
    ggsave(filename = file.path(echo_figures, "matatu_driver_rate.png"),
           height = 3, width =6)

data_comp %>%
  group_by(SPEED.RATING._L) %>%
  summarise(N = n()) %>%
  ggplot(aes(x = SPEED.RATING._L, y = N)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = N), nudge_y = 40) +
  labs(x="", y="",
       title = "How would you describe your driver's speed?") +
  theme_minimal() +
  ggsave(filename = file.path(echo_figures, "matatu_speed_rate.png"),
         height = 3, width =6)

data_comp %>%
  group_by(MATATU.OCCUPANCY_L) %>%
  summarise(N = n()) %>%
  ggplot(aes(x = MATATU.OCCUPANCY_L, y = N)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = N), nudge_y = 50) +
  labs(x="", y="",
       title = "How many passengers are on the matatu?") +
  theme_minimal() +
  coord_flip() +
  ggsave(filename = file.path(echo_figures, "matatu_occupancy_rate.png"),
         height = 3, width =6)

data_comp %>%
  group_by(PASSENGER.HAND.SANITISATION_L) %>%
  summarise(N = n()) %>%
  ggplot(aes(x = PASSENGER.HAND.SANITISATION_L, y = N)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = N), nudge_y = 50) +
  labs(x="", y="",
       title = "Did the matatu take measures to prevent spread of COVID?") +
  theme_minimal() +
  ggsave(filename = file.path(echo_figures, "matatu_covidmeasure_rate.png"),
         height = 3, width =6)

# Answer to Questions - By Vehicle ---------------------------------------------
data_comp_veh <- data_comp %>%
  filter(matatu_num_correct %in% T) %>%
  group_by(matatu_num) %>%
  mutate(N_veh = n()) %>%
  ungroup() 

data_comp_hour <- data_comp %>%
  group_by(hour) %>%
  mutate(N_hour = n()) %>%
  ungroup()

#Hourly response by vehicle
# By hour by matatu
data_comp_veh %>%
  group_by(hour, matatu_num) %>%
  summarise(N = n()) %>%
  ggplot() +
  geom_col(aes(x = hour, y = N, group = matatu_num, fill = matatu_num)) +
  theme_minimal() +
  ggsave(filename = file.path(echo_figures, "by_hour_matatu.png"),
         height = 4, width =7)


data_comp_veh %>%
  group_by(DRIVER.RATING._L, matatu_num, N_veh) %>%
  summarise(N = n()) %>%
  mutate(N = N / N_veh) %>%
  ggplot(aes(x = matatu_num, y = N, group=DRIVER.RATING._L, fill=DRIVER.RATING._L)) +
  geom_col(color="black") +
  labs(x="", y="",
       title = "How would you rate your matatu driver?") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Matatu Number", y = "Proportion", fill = "") +
  ggsave(filename = file.path(echo_figures, "matatu_driver_rate_byveh.png"),
         height = 3, width =6)

data_comp_veh %>%
  group_by(SPEED.RATING._L, matatu_num, N_veh) %>%
  summarise(N = n()) %>%
  mutate(N = N / N_veh) %>%
  ggplot(aes(x = matatu_num, y = N, group=SPEED.RATING._L, fill=SPEED.RATING._L)) +
  geom_col(color="black") +
  labs(x="", y="",
       title = "How would you describe your driver's speed?") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Matatu Number", y = "Proportion", fill = "") +
  ggsave(filename = file.path(echo_figures, "matatu_speed_rate_byveh.png"),
         height = 3, width =6)

data_comp_veh %>%
  group_by(MATATU.OCCUPANCY_L, matatu_num, N_veh) %>%
  summarise(N = n()) %>%
  mutate(N = N / N_veh) %>%
  ggplot(aes(x = matatu_num, y = N, group=MATATU.OCCUPANCY_L, fill=MATATU.OCCUPANCY_L)) +
  geom_col(color="black") +
  labs(x="", y="",
       title = "How many passengers are on the matatu?") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Matatu Number", y = "Proportion", fill = "") +
  ggsave(filename = file.path(echo_figures, "matatu_occupancy_rate_byveh.png"),
         height = 3, width =6)


data_comp_veh %>%
  group_by(PASSENGER.HAND.SANITISATION_L, matatu_num, N_veh) %>%
  summarise(N = n()) %>%
  mutate(N = N / N_veh) %>%
  ggplot(aes(x = matatu_num, y = N, group=PASSENGER.HAND.SANITISATION_L, fill=PASSENGER.HAND.SANITISATION_L)) +
  geom_col(color="black") +
  labs(x="", y="",
       title = "Did the matatu take measures to prevent spread of COVID?") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Matatu Number", y = "Proportion", fill = "") +
  ggsave(filename = file.path(echo_figures, "matatu_covidmeasure_rate_byveh.png"),
         height = 3, width =6)

# covid response by hour
data_comp_hour %>%
  group_by(PASSENGER.HAND.SANITISATION_L, hour, N_hour) %>%
  summarise(N = n()) %>%
  mutate(N = N / N_hour) %>%
  ggplot(aes(x = hour, y = N, group=PASSENGER.HAND.SANITISATION_L, fill=PASSENGER.HAND.SANITISATION_L)) +
  geom_col(color="black") +
  labs(x="", y="",
       title = "Did the matatu take measures to prevent spread of COVID?") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Hour of the day", y = "Proportion", fill = "") +
  ggsave(filename = file.path(echo_figures, "matatu_covidmeasure_rate_byhour.png"),
         height = 3, width =6)

