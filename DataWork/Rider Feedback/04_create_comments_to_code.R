# Create Comments to Code

if(T){
  df <- readRDS(file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_clean.Rds"))
  
  df <- df %>%
    dplyr::filter(valid_psvnum %in% T,
                  comment_coded %in% F,
                  !is.na(comments_label)) %>%
    dplyr::filter(comments_label_nchar >= 2) %>%
    dplyr::filter(completion_date >= ymd("2023-01-16") )
  
  df <- df %>%
    dplyr::mutate(needs_crosschecking = "",
                  comments_checking_relevance = "",
                  coding_relevant_comments = "",
                  additional_notes = "") %>%
    dplyr::select(-comment_coded)
  

 date_i <- Sys.Date() %>% as.character() %>% str_replace_all("-", "")
  
  write_csv(df, 
            file.path(data_dir, "Rider Feedback", "FinalData", "comments_coded",
                      "to_code",
                      paste0("comments_to_code_", date_i, ".csv")))
  
}