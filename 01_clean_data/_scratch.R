
fb_df <- readRDS(file.path(data_dir, "RawData", "passenger_feedback.Rds"))

fb_df <- fb_df %>%
  dplyr::mutate(q_comment = q_comment %>% 
                  tolower() %>%
                  str_squish(),
                q_comment = q_comment %>% 
                  str_replace_all(" ", "") %>%
                  str_replace_all("[:digit:]", "") %>%
                  str_replace_all("[:punct:]", ""),
                q_comment_nwords = q_comment %>% str_count("\\S+"))

fb_df <- fb_df %>%
  distinct(q_comment) %>%
  dplyr::mutate(q_comment_nchar = nchar(q_comment)) %>%
  dplyr::filter(q_comment_nchar >= 20)

nrow(fb_df)
