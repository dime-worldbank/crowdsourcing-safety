# PSV Rider Feedback: Figures of Main Results

# Load Data --------------------------------------------------------------------
feedback_df <- readRDS(file.path(dropbox_dir, "Data", "Rider Feedback", "All Data", "FinalData", 
                                 "rider_feedback.Rds"))

sum_df <- feedback_df %>%
  group_by(award_type, date, reg_no, award_amount_posted) %>%
  summarise(n = n()) %>%
  dplyr::filter(reg_no != "UNKNOWN")

a <- sum_df %>%
  filter(award_type == "win") %>%
  group_by(reg_no, award_amount_posted) %>%
  summarise(n = sum(n))

##
a %>%
  dplyr::filter(award_amount_posted == 50) %>%
  pull(n) %>%
  summary()

0 Response = 1 Vehicle
1 Response = 4 Vehicle
2 Responses = 2 Vehicles
3 Responses = 1 Vehicle
6 Respones = 2 Vehicles


a %>%
  dplyr::filter(award_amount_posted == 50) %>%
  pull(n) %>%
  length()

##
0 Response = 9 Vehicles
1 Response = 6 Vehicles
2 Response = 3 Vehicles
3 Response  = 1 Vehicle
61 Response = 1 Vehicle

Hi Sveta - I checked the pilot data. We tested a "win award"



a %>%
  dplyr::filter(award_amount_posted == 100) %>%
  pull(n) %>% table()



a %>%
  dplyr::filter(award_amount_posted == 100) %>%
  pull(n) %>%
  length()

a %>%
  dplyr::filter(award_amount_posted == 100) %>%
  pull(n) %>%
  summary()
