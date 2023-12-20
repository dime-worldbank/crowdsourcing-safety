# Classify Feedback

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean.Rds"))

# Cleanup ----------------------------------------------------------------------
df_sub <- df %>%
  dplyr::mutate(q_comment_nchar = nchar(q_comment)) %>%
  dplyr::filter(!is.na(q_comment)) %>%
  dplyr::filter(q_comment != "") %>%
  dplyr::filter(q_comment_nchar >= 3) %>%
  dplyr::mutate(q_comment = q_comment %>%
                  str_replace_all("[:punct:]", " ") %>%
                  str_squish() %>%
                  tolower())

# Sentiment --------------------------------------------------------------------
# Dictionary based approach
df_sub$sentiment_snmtr <- df_sub$q_comment %>%
  get_sentences() %>%
  sentiment() %>%
  pull(sentiment)

# Select variables and merge back into main ------------------------------------
df_sub <- df_sub %>%
  dplyr::select(uid, sentiment_snmtr)

df <- df %>%
  left_join(df_sub, by = "uid")

# Export -----------------------------------------------------------------------
saveRDS(df, file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))






