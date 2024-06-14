# Classify Feedback

set.seed(142)

type <- "main"
comment_filter <- FALSE
distinct_pass <- TRUE

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
      
      # Add in chatGPT results -----------------------------------------------------
      #  Mode <- function(x) {
      #    ux <- unique(x)
      #    out <- ux[which.max(tabulate(match(x, ux)))]
      #    as.numeric(out)
      #  }
      #  
      #  ## ID
      # comment_id_df <- read_csv(file.path(data_dir, "FinalData ChatGPT Comment Codes", 
      #                                     "inputs",
      #                                     "comment_id.csv"))
      # comment_id_df <- comment_id_df %>%
      #   dplyr::select(uid, q_comment_id)
      # 
      # ## Ratings
      # chatgpt_df <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs") %>%
      #    list.files(pattern = "chatgpt_codes_",
      #               full.names = T) %>%
      #    map_df(read_csv)
      #  
      #  chatgpt_df <- chatgpt_df %>%
      #    group_by(q_comment_id) %>%
      #    dplyr::summarise(q_comment_rating = Mode(q_comment_rating)) %>%
      #    ungroup()
      #  
      #  ## Merge
      #  chatgpt_all_df <- comment_id_df %>%
      #    left_join(chatgpt_df, by = "q_comment_id") %>%
      #    dplyr::rename(comment_driver_gpt_code = q_comment_rating) %>%
      #    dplyr::select(uid, comment_driver_gpt_code)
      #  
      #  ## Merge with main dataframe
      #  df <- df %>%
      #    left_join(chatgpt_all_df, by = "uid")
      
      Mode <- function(x) {
        ux <- unique(x)
        out <- ux[which.max(tabulate(match(x, ux)))]
        as.numeric(out)
      }
      
      ## ID
      comment_id_df <- read_csv(file.path(data_dir, "FinalData ChatGPT Comment Codes", 
                                          "inputs",
                                          "comment_id.csv"))
      comment_id_df <- comment_id_df %>%
        dplyr::select(uid, q_comment_id)
      
      ## Ratings
      chatgpt_4o_df <- readRDS(file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", 
                                         "chatgpt_classification_2_gpt-4o.Rds"))
      
      chatgpt_35_df <- readRDS(file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", 
                                         "chatgpt_classification_3_gpt-3.5-turbo.Rds"))
      
      chatgpt_4o_category_df <- readRDS(file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", 
                                                  "chatgpt_category_1_gpt-4o.Rds"))
      
      chatgpt_4o_df <- chatgpt_4o_df %>%
        dplyr::select(q_comment_id, q_comment_rating) %>%
        dplyr::mutate(q_comment_rating = q_comment_rating %>% as.numeric()) %>%
        dplyr::rename(comment_driver_gpt_4o_code = q_comment_rating)
      
      chatgpt_35_df <- chatgpt_35_df %>%
        dplyr::select(q_comment_id, q_comment_rating) %>%
        dplyr::mutate(q_comment_rating = q_comment_rating %>% as.numeric()) %>%
        dplyr::rename(comment_driver_gpt_35_code = q_comment_rating)
      
      chatgpt_4o_category_df <- chatgpt_4o_category_df %>%
        dplyr::mutate(chatgpt_4o_cat_1 = q_comment_rating %>% str_detect("1"),
                      chatgpt_4o_cat_1p = q_comment_rating %>% str_detect("1p"),
                      chatgpt_4o_cat_1n = q_comment_rating %>% str_detect("1n"),
                      chatgpt_4o_cat_1r = q_comment_rating %>% str_detect("1r"),
                      
                      chatgpt_4o_cat_2 = q_comment_rating %>% str_detect("2"),
                      chatgpt_4o_cat_2p = q_comment_rating %>% str_detect("2p"),
                      chatgpt_4o_cat_2n = q_comment_rating %>% str_detect("2n"),
                      chatgpt_4o_cat_2r = q_comment_rating %>% str_detect("2r"),
                      
                      chatgpt_4o_cat_3 = q_comment_rating %>% str_detect("3"),
                      chatgpt_4o_cat_3p = q_comment_rating %>% str_detect("3p"),
                      chatgpt_4o_cat_3n = q_comment_rating %>% str_detect("3n"),
                      chatgpt_4o_cat_3r = q_comment_rating %>% str_detect("3r"),
                      
                      chatgpt_4o_cat_4 = q_comment_rating %>% str_detect("4"),
                      chatgpt_4o_cat_4p = q_comment_rating %>% str_detect("4p"),
                      chatgpt_4o_cat_4n = q_comment_rating %>% str_detect("4n"),
                      chatgpt_4o_cat_4r = q_comment_rating %>% str_detect("4r"),
                      
                      chatgpt_4o_cat_5 = q_comment_rating %>% str_detect("5")) %>%
        dplyr::rename(chatgpt_4o_cat = q_comment_rating) %>%
        dplyr::select(-c(uid, q_comment))
      
      # chatgpt_df <- chatgpt_df %>%
      #   group_by(q_comment_id) %>%
      #   dplyr::summarise(q_comment_rating = Mode(q_comment_rating)) %>%
      #   ungroup()
      
      ## Merge
      chatgpt_all_df <- comment_id_df %>%
        full_join(chatgpt_4o_df, by = "q_comment_id") %>%
        full_join(chatgpt_35_df, by = "q_comment_id") %>%
        full_join(chatgpt_4o_category_df, by = "q_comment_id") %>%
        dplyr::select(uid, 
                      comment_driver_gpt_4o_code,
                      comment_driver_gpt_35_code,
                      contains("chatgpt_4o_cat"))
      
      ## Merge with main dataframe
      df <- df %>%
        left_join(chatgpt_all_df, by = "uid")
      
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



