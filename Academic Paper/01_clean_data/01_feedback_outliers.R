# Identify Cheating

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "RawData", "passenger_feedback.Rds"))

# Remove duplicates ------------------------------------------------------------
fb_df <- fb_df %>%
  dplyr::mutate(q_comment_nchar = nchar(q_comment)) %>%
  arrange(-q_comment_nchar) %>%
  distinct(phone_hash, regno, .keep_all = T)

# Clean data -------------------------------------------------------------------
#### Identify outlier
s_df <- fb_df %>%
  dplyr::filter(regno != "UNKNOWN") %>%
  dplyr::mutate(datetime_30m = datetime %>% floor_date(unit = "30 minutes")) %>%
  group_by(datetime_30m, regno) %>%
  dplyr::summarise(n = n()) %>%
  ungroup()

q1 <- quantile(s_df$n, 0.25) %>% as.numeric()
q3 <- quantile(s_df$n, 0.75) %>% as.numeric()
iqr <- q3 - q1
q3 + 1.5*iqr

fb_clean_df <- fb_df %>%
  dplyr::filter(regno != "UNKNOWN") %>%
  dplyr::mutate(datetime_30m = datetime %>% floor_date(unit = "30 minutes")) %>%
  group_by(datetime_30m, regno) %>%
  dplyr::mutate(n_feedback = n()) %>%
  ungroup() %>%
  
  dplyr::mutate(ptn_cheating = as.numeric(n_feedback >= 12)) %>%
  arrange(regno, date) %>%
  group_by(regno) %>%
  mutate(ptn_cheating_fill = cummax(ptn_cheating)) %>%
  ungroup()

fb_clean_df <- bind_rows(
  fb_clean_df,
  fb_df %>%
    dplyr::filter(regno == "UNKNOWN")
)

# Export data ------------------------------------------------------------------
saveRDS(fb_clean_df, file.path(data_dir, "FinalData", "passenger_feedback_clean.Rds"))






