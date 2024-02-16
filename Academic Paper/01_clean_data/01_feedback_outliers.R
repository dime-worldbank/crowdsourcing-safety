# Identify Cheating

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "RawData", "passenger_feedback.Rds"))

# Remove duplicates ------------------------------------------------------------
fb_df <- bind_rows(
  fb_df %>%
    filter(response_method == "shortcode") %>%
    arrange(date) %>%
    distinct(phone_hash, regno, .keep_all = T),
  
  fb_df %>%
    filter(response_method == "qr code")
)

# Remove outliers --------------------------------------------------------------
fb_df <- fb_df %>%
  dplyr::mutate(datetime_30m = datetime %>% floor_date(unit = "30 minutes")) %>%
  group_by(datetime_30m, regno, response_method) %>%
  dplyr::mutate(n_feedback_30mins = n()) %>%
  ungroup() %>%
  dplyr::filter(n_feedback_30mins <= 14)

# Add variables ----------------------------------------------------------------
fb_df <- fb_df %>%
  dplyr::mutate(q_comment_nchar = nchar(q_comment))

# Export data ------------------------------------------------------------------
saveRDS(fb_df, file.path(data_dir, "FinalData", "passenger_feedback_valid.Rds"))
