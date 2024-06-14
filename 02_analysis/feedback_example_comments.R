

fb_df <- readRDS(file.path(data_dir, "FinalData", 
                           paste0("passenger_feedback_valid_class_",
                                  "main", "_",
                                  "cmntfilter",
                                  FALSE,
                                  "_",
                                  "dstnctpass",
                                  TRUE,
                                  ".Rds")))

fb_df %>%
  dplyr::filter(!is.na(q_comment)) %>%
  dplyr::filter(q_comment_nchar >= 4) %>%
  pull(q_comment) %>%
  unique() %>%
  paste(collapse = ";") %>%
  write.table("~/Desktop/comments.txt")

fb_df %>%
  dplyr::filter(!is.na(q_comment)) %>%
  dplyr::filter(q_comment_nchar >= 4) %>%
  pull(q_comment) %>%
  unique() %>%
  paste(collapse = ";") %>%
  write.table("~/Desktop/comments.txt")

fb_df %>%
  arrange(-q_comment_nchar) %>%
  pull(q_comment) %>%
  head()
