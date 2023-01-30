# Rider Feedback Analysis

# Load data --------------------------------------------------------------------
df <- read_csv(file.path(data_dir, "Rider Feedback", "rawdata", "rider_feedback.csv"))
valid_reg_df <- read_csv(file.path(data_dir, "Valid Reg Numbers", "valid_psv_nums.csv"))

df <- df %>%
  clean_names() %>%
  dplyr::mutate(psvnum_response = psvnum_response %>% as.numeric) %>%
  dplyr::filter(!is.na(psvnum_response)) %>%
  dplyr::mutate(date = start_date %>% substring(1,16) %>% ymd_hm() %>% date())

df <- df[df$psvnum_response %in% valid_reg_df$psv_num,]

# 190 seems to be cheating
a <- df[df$comments_response %in% "Dereva makini",]
a$psvnum_response
a$phone %>% table() %>% table()

df <- df[df$psvnum_response == 190,]

daily_df <- df %>%
  group_by(date) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  arrange(date)

daily_df %>% tail(7) %>% pull(n) %>% mean()

nrow(daily_df)

df %>%
  dplyr::filter(date %in% ymd("2023-01-05")) %>%
  pull(psvnum_response)



21/25 = 50.4/60

21*60/25






