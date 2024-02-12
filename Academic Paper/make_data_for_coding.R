
df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

df <- df %>%
  filter(is.na(comment_driver_sntmt_relev),
         !is.na(q_comment)) %>%
  distinct(q_comment_uid, .keep_all = T) %>%
  dplyr::select(q_comment_uid, q_comment,
                comment_driver_sntmt_relev,
                comment_driver_sntmt_code) %>%
  arrange(q_comment)

write_csv(df, "~/Desktop/comments_to_code.csv")
