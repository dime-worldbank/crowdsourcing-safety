# Feedback: Ratings vs Comments

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

fb_df <- fb_df %>%
  dplyr::filter(regno != "UNKNOWN",
                ptn_cheating_fill %in% 0)

fb_df <- fb_df %>%
  dplyr::mutate(q_safety_rating_safe = (q_safety_rating == "Safe") | (q_safety_rating == "Very safe"),
                q_safety_rating_unsafe = (q_safety_rating == "Not safe") | (q_safety_rating == "Not very safe")) %>%
  dplyr::mutate(comment_driver_sntmt_code_compl = as.numeric(comment_driver_sntmt_code_compl),
                comment_driver_sntmt_code_neg = as.numeric(comment_driver_sntmt_code_neg))

fb_df %>%
  filter(q_safety_rating_safe %in% T) %>%
  pull(sentiment_snmtr) %>%
  hist()

fb_df %>%
  filter(q_safety_rating_unsafe %in% T) %>%
  pull(sentiment_snmtr) %>%
  hist()

fb_df %>%
  filter(q_safety_rating_unsafe %in% T) %>%
  pull(comment_driver_sntmt_code_compl) %>%
  hist()

fb_df$q_safety_rating_safe
fb_df$sentiment_snmtr

fb_df$q_safety_rating

fb_df$comment_driver_sntmt_relev %>% table()
fb_df$comment_driver_sntmt_code_compl

head(fb_df)
