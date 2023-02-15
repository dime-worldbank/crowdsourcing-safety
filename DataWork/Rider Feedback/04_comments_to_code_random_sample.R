# Comments to Code
# Random Sample to Cross Check

set.seed(42)

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_clean.Rds"))

df <- df %>%
  dplyr::filter(comment_coded %in% T,
                !is.na(comment_driver_sentiment_relev),
                !is.na(comments_label_english))

df <- df[sample(1:nrow(df)),]

df_sample <- bind_rows(
  
  df %>%
    dplyr::filter(comment_driver_sentiment_relev %in% 0) %>%
    head(50),
  
  df %>%
    dplyr::filter(comment_driver_sentiment_code %in% 1) %>%
    head(100),
  
  df %>%
    dplyr::filter(comment_driver_sentiment_code %in% 2) %>%
    head(50),
  
  df %>%
    dplyr::filter(comment_driver_sentiment_code %in% 3) %>%
    head(50),
  
  df %>%
    dplyr::filter(comment_driver_sentiment_code %in% 4) %>%
    head(50)
  
)

df_sample <- df_sample[sample(1:nrow(df_sample)),]

df_sample <- df_sample %>%
  dplyr::mutate(comment_driver_sentiment_relev = "",
                comment_driver_sentiment_code = "") %>%
  dplyr::select(-c(needs_crosschecking, comment_coded, additional_notes))

write_csv(df_sample, 
          file.path(data_dir, "Rider Feedback", "FinalData", "comments_coded",
                    "to_code",
                    "comments_to_code_crosscheck_random_sample.csv"))


