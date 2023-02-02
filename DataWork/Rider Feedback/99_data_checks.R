# Rider Feedback

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_clean.Rds"))

df %>%
  dplyr::filter(valid_psvnum %in% T,
                !is.na(completion_date)) %>% 
  group_by(regno_clean, psv_num) %>%
  dplyr::summarise(n = n()) %>%
  arrange(-n) %>%
  head()

df %>%
  dplyr::filter(valid_psvnum %in% T,
                !is.na(completion_date)) %>% 
  dplyr::filter(invite_date >= ymd("2022-01-15")) %>%
  group_by(regno_clean, psv_num) %>%
  dplyr::summarise(n = n()) %>%
  arrange(-n) %>%
  head(10)

