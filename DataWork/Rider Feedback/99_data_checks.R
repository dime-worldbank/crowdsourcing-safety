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
  dplyr::filter(invite_date >= ymd("2022-01-20")) %>%
  group_by(regno_clean, psv_num) %>%
  dplyr::summarise(n = n()) %>%
  arrange(-n) %>%
  head(15)


a <- df %>%
  dplyr::filter(!is.na(completion_date)) %>%
  group_by(invite_date) %>%
  dplyr::summarise(valid_psvnum_1 = sum(valid_psvnum),
                   valid_psvnum_0 = sum(valid_psvnum == 0))

df <- df %>%
  dplyr::filter(!is.na(completion_date))
aa <- df[df$invite_date >= ymd("2023-01-27"),]
aaa <- aa[aa$valid_psvnum %in% F,]
