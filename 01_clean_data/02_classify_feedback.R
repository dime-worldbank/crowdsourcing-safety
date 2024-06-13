# Classify Feedback

set.seed(142)

for(type in c("main")){
  for(comment_filter in c(TRUE, FALSE)){
    for(distinct_pass in c(TRUE, FALSE)){
      
      # Load data --------------------------------------------------------------------
      df <- readRDS(file.path(data_dir, "FinalData", 
                              paste0("passenger_feedback_valid_",
                                     type, "_",
                                     "cmntfilter",
                                     comment_filter,
                                     "_",
                                     "dstnctpass",
                                     distinct_pass,
                                     ".Rds")))
      
      # Cleanup ----------------------------------------------------------------------
      df_sub <- df %>%
        dplyr::mutate(q_comment_nchar = nchar(q_comment)) %>%
        dplyr::filter(!is.na(q_comment)) %>%
        dplyr::filter(q_comment != "") %>%
        dplyr::filter(q_comment_nchar >= 3) %>%
        dplyr::mutate(q_comment = q_comment %>%
                        str_replace_all("[:punct:]", " ") %>%
                        str_squish() %>%
                        tolower()) %>%
        dplyr::mutate(q_comment_covid = q_comment %>%
                        str_replace_all("pandemic", " ") %>%
                        str_squish())
      
      # Sentiment --------------------------------------------------------------------
      # Dictionary based approach
      df_sub$sentiment_snmtr <- df_sub$q_comment %>%
        get_sentences() %>%
        sentiment() %>%
        pull(sentiment)
      df_sub$sentiment_snmtr[df_sub$sentiment_snmtr >= 1] <- 1
      df_sub$sentiment_snmtr[df_sub$sentiment_snmtr <= -1] <- -1
      
      df_sub$sentiment_snmtr_covid <- df_sub$q_comment_covid %>%
        get_sentences() %>%
        sentiment() %>%
        pull(sentiment)
      
      # Select variables and merge back into main ------------------------------------
      df_sub <- df_sub %>%
        dplyr::select(uid, sentiment_snmtr, sentiment_snmtr_covid)
      
      df <- df %>%
        left_join(df_sub, by = "uid")
      
      # Add text version of code -----------------------------------------------------
      df <- df %>%
        mutate(comment_driver_sntmt_code_str = case_when(
          comment_driver_sntmt_code == 1 ~ "Positive",
          comment_driver_sntmt_code == 2 ~ "Negative",
          comment_driver_sntmt_code == 3 ~ "Neutral",
          comment_driver_sntmt_code == 4 ~ "Unclear"
        ) %>%
          factor(levels = c("Positive",
                            "Negative",
                            "Neutral",
                            "Unclear")))
      
      # Export -----------------------------------------------------------------------
      saveRDS(df,
              file.path(data_dir, "FinalData", 
                        paste0("passenger_feedback_valid_class_",
                               type, "_",
                               "cmntfilter",
                               comment_filter,
                               "_",
                               "dstnctpass",
                               distinct_pass,
                               ".Rds")))
      
    }
  }
}



