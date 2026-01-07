# Comments Inconsistent

# Load data --------------------------------------------------------------------
for(type in c("main")){
  for(comment_filter in c(TRUE, FALSE)){
    for(distinct_pass in c(TRUE, FALSE)){
      
      fb_df <- readRDS(file.path(data_dir, "FinalData", 
                                 paste0("passenger_feedback_valid_class_",
                                        type,
                                        "_cmntfilter",comment_filter,
                                        "_dstnctpass",distinct_pass,".Rds")))
      
      # Table ------------------------------------------------------------------------
      fb_df <- fb_df %>%
        dplyr::filter(comment_driver_sntmt_code_str %in% c("Positive", "Negative")) %>%
        dplyr::mutate(comment_driver_sntmt_code_str = case_when(
          comment_driver_sntmt_code_str == "Positive" ~ "Comment suggests safe driving",
          comment_driver_sntmt_code_str == "Negative" ~ "Comment suggests unsafe driving"
        )) %>%
        mutate(comment_driver_sntmt_code_str = comment_driver_sntmt_code_str %>%
                 as.character() %>%
                 factor(levels = c("Comment suggests safe driving", 
                                   "Comment suggests unsafe driving")))
      
      #### Prep data
      table(fb_df$q_safety_rating, fb_df$comment_driver_sntmt_code_str) 
      
      cross_tab_df <- table(fb_df$q_safety_rating, fb_df$comment_driver_sntmt_code_str) %>%
        as.matrix.data.frame() %>% 
        as.data.frame()
      
      # cross_tab_df$comment_var <- c("Comment suggests safe driving", 
      #                               "Comment suggests unsafe driving")
      
      cross_tab_df$comment_var <- c("Very Safe", "Safe", "Not Safe", "Very Not Safe")
      
      for(i in 1:2){
        cross_tab_df[[paste0("V",i,"_p")]] <- 
          round(cross_tab_df[[paste0("V",i)]] / 
                  sum(cross_tab_df[[paste0("V",i)]]) * 100) %>% 
          paste0("\\%")
      }
      
      cross_tab_df <- cross_tab_df %>%
        dplyr::mutate(tex = paste0(comment_var, " & ", 
                                   V1, " (", V1_p, ") & ",
                                   V2, " (", V2_p, ") \\\\ \n "))
      
      #### Make table
      file_name <- paste0("safety_comment_crosstab_",
                          type,
                          "_cmntfilter", comment_filter,
                          "_dstnctpass", distinct_pass, ".tex")
      
      sink(file.path(tables_dir, file_name))
      cat("\\begin{tabular}{l c|c} ")
      cat("\\hline ")
      
      cat(" & ")
      cat("Comment suggests safe driving & Comment suggests unsafe driving \\\\ \n ")
      # fb_df$q_safety_rating %>% 
      #   unique() %>% 
      #   sort() %>% 
      #   paste(collapse = " & ") %>%
      #   paste("\\\\ \n") %>%
      #   cat()
      # cross_tab_df$comment_var <- c("Comment suggests safe driving", 
      #                               "Comment suggests unsafe driving")
      cat("\\hline ")
      
      cross_tab_df$tex %>%
        paste(collapse = " ") %>%
        cat()
      
      cat("\\hline ")
      cat("\\end{tabular}")
      sink()
      
    }
  }
}

# Example comments -------------------------------------------------------------
comment_filter <- F
distinct_pass <- T
fb_df <- readRDS(file.path(data_dir, "FinalData", 
                           paste0("passenger_feedback_valid_class_",
                                  "main", "_",
                                  "cmntfilter",
                                  comment_filter,
                                  "_",
                                  "dstnctpass",
                                  distinct_pass,
                                  ".Rds")))

#### Rate safe, comment negative
# Unsafe driving" [bad data]
# Pay attention [bad data?]
# The driver drove the matatu very fast [fast != safe?]
# The driver was drunk [despite drunk, drove safely]
fb_df %>%
  filter(q_safety_rating_num %in% 3:4,
         comment_driver_sntmt_code == 2) %>%
  mutate(q_comment = tolower(q_comment),
         comment_len = nchar(q_comment)) %>%
  #distinct(q_comment, .keep_all = TRUE) %>%
  arrange(-comment_len) %>%
  pull(q_comment) 


#### Rate unsafe, comment positive
# the driver did not drink alcohol [but was unsafe otherwise?]
# Drive slowly [commanding to drive -- swahili Aendeshe gari pole pole], fast but not unsafe?
fb_df %>%
  filter(q_safety_rating_num %in% 1:2,
         comment_driver_sntmt_code == 1) %>%
  mutate(q_comment = tolower(q_comment),
         comment_len = nchar(q_comment)) %>%
  #distinct(q_comment, .keep_all = TRUE) %>%
  arrange(-comment_len) %>%
  pull(q_comment)


fb_df <- readRDS(file.path(data_dir, "FinalData", 
                           paste0("passenger_feedback_valid_class_",
                                  "main", "_",
                                  "cmntfilter",
                                  comment_filter,
                                  "_",
                                  "dstnctpass",
                                  distinct_pass,
                                  ".Rds")))

fb_df %>%
  filter(chatgpt_4o_cat == "4r") %>%
  pull(q_comment)

fb_df$chatgpt_4o_cat
