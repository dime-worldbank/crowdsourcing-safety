# Merge Data

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "Rider Feedback", "FinalData",
                        "rider_feedback.Rds"))

comments_en_df <- readRDS(file.path(data_dir, "Rider Feedback", "FinalData", 
                                    "comments_translated",
                                    "comment_english.Rds"))

comments_coded_df <- file.path(data_dir, "Rider Feedback", "FinalData", 
                               "comments_coded", "coded") %>%
  list.files(pattern = "*.csv",
             full.names = T) %>%
  map_df(read_csv) %>%
  dplyr::select(c(case_key,
                  needs_crosschecking, comments_checking_relevance, coding_relevant_comments,
                  additional_notes)) %>%
  dplyr::mutate(comment_coded = T) %>%
  dplyr::rename(comment_driver_sentiment_relev = comments_checking_relevance,
                comment_driver_sentiment_code = coding_relevant_comments)

df <- df %>%
  left_join(comments_en_df, by = "case_key") %>%
  left_join(comments_coded_df, by = "case_key")

df$comment_coded[is.na(df$comment_coded)] <- F

saveRDS(df, file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_clean.Rds"))
write_csv(df, file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_clean.csv"))


