# Identify Cheating

for(type in c("main")){
  for(comment_filter in c(TRUE, FALSE)){
    for(distinct_pass in c(TRUE, FALSE)){
      
      # Load data --------------------------------------------------------------------
      if(type == "main"){
        fb_df <- readRDS(file.path(data_dir, "RawData", "passenger_feedback.Rds"))
      }
      
      # Filter based on comments ---------------------------------------------------
      if(comment_filter %in% T){
        
        fb_df <- fb_df %>%
          dplyr::mutate(q_comment = q_comment %>% 
                          tolower() %>%
                          str_squish(),
                        q_comment_cln = q_comment %>% 
                          str_replace_all(" ", "") %>%
                          str_replace_all("[:digit:]", "") %>%
                          str_replace_all("[:punct:]", ""),
                        q_comment_cln_nchar = nchar(q_comment_cln),
                        q_comment_nwords = q_comment %>% str_count("\\S+"))
        
        fb_df <- fb_df %>%
          filter(q_comment_cln_nchar >= 4)
        
        #fb_df$q_comment[fb_df$q_comment_cln_nchar %in% 4:6] %>% table() %>% View()
        
        fb_df_rm <- fb_df %>%
          dplyr::filter(q_comment_cln_nchar %in% 4:6,
                        !(q_comment %in% c("safe", "praise", "gentle", "quiet", "fast")))
        
        fb_df <- fb_df[!(fb_df$uid %in% fb_df_rm$uid),]
      }
      
      # Remove duplicates ------------------------------------------------------------
      if(distinct_pass %in% T){
        fb_df <- bind_rows(
          fb_df %>%
            filter(response_method == "shortcode") %>%
            arrange(date) %>%
            distinct(phone_hash, regno, .keep_all = T),
          
          fb_df %>%
            filter(response_method == "qr code")
        )
      }
      
      # Remove outliers --------------------------------------------------------------
      if(type == "main"){
        fb_df <- fb_df %>%
          dplyr::mutate(datetime_30m = datetime %>% floor_date(unit = "30 minutes")) %>%
          group_by(datetime_30m, regno, response_method) %>%
          dplyr::mutate(n_feedback_30mins = n()) %>%
          ungroup() %>%
          dplyr::mutate(q_comment_nchar = nchar(q_comment)) %>%
          dplyr::mutate(remove = ((n_feedback_30mins > 14) & (q_comment_nchar <= 50))) %>%
          dplyr::filter(remove %in% F)
          #dplyr::filter(n_feedback_30mins <= 14)
        
        # a <- fb_df %>%
        #   dplyr::mutate(datetime_30m = datetime %>% floor_date(unit = "30 minutes")) %>%
        #   group_by(datetime_30m, regno, response_method) %>%
        #   dplyr::mutate(n_feedback_30mins = n()) %>%
        #   ungroup() %>%
        #   dplyr::mutate(q_comment_nchar = nchar(q_comment)) %>%
        #   dplyr::filter((n_feedback_30mins <= 14) & (q_comment_nchar <= 50))

      } else{
        
        fb_df <- bind_rows(
          fb_df %>%
            filter(regno_unknown == 1),
          
          fb_df %>%
            filter(regno_unknown == 0) %>%
            dplyr::mutate(datetime_30m = datetime %>% floor_date(unit = "30 minutes")) %>%
            group_by(datetime_30m, regno, response_method) %>%
            dplyr::mutate(n_feedback_30mins = n()) %>%
            ungroup() %>%
            dplyr::filter(n_feedback_30mins <= 14)
        )
      }
      
      # Add variables ----------------------------------------------------------------
      fb_df <- fb_df %>%
        dplyr::mutate(q_comment_nchar = nchar(q_comment))
      
      # Export data ------------------------------------------------------------------
      saveRDS(fb_df, file.path(data_dir, "FinalData", 
                               paste0("passenger_feedback_valid_",
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