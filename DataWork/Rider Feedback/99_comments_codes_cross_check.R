# Rider Feedback

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_clean.Rds"))

df <- df %>%
  dplyr::filter(comment_coded_check %in% T)

# Checks -----------------------------------------------------------------------
#### Relevance
table(df$comment_driver_sentiment_relev == df$comment_driver_sentiment_relev_check)
mean(df$comment_driver_sentiment_relev == df$comment_driver_sentiment_relev_check)

#### When both coded as relevant
table(df$comment_driver_sentiment_code == df$comment_driver_sentiment_code_check)
mean(df$comment_driver_sentiment_code == df$comment_driver_sentiment_code_check, na.rm = T)

df_code_diff <- df %>%
  dplyr::filter(comment_driver_sentiment_code != comment_driver_sentiment_code_check) %>%
  dplyr::select(case_key, comments_label, comments_label_english, comment_driver_sentiment_code, comment_driver_sentiment_code_check)

df_relev_diff <- df %>%
  dplyr::filter(comment_driver_sentiment_relev != comment_driver_sentiment_relev_check) %>%
  dplyr::select(case_key, comments_label, comments_label_english, comment_driver_sentiment_relev, comment_driver_sentiment_relev_check)
