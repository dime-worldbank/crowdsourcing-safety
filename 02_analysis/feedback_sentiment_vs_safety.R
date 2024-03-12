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
        mutate(comment_driver_sntmt_code_str = comment_driver_sntmt_code_str %>%
                 as.character() %>%
                 factor(levels = c("Positive", "Negative")))
      
      #### Prep data
      table(fb_df$comment_driver_sntmt_code_str, fb_df$q_safety_rating) 
      
      cross_tab_df <- table(fb_df$comment_driver_sntmt_code_str, fb_df$q_safety_rating) %>%
        as.matrix.data.frame() %>% 
        as.data.frame()
      
      cross_tab_df$comment_var <- c("Positive", "Negative")
      
      for(i in 1:4){
        cross_tab_df[[paste0("V",i,"_p")]] <- 
          round(cross_tab_df[[paste0("V",i)]] / 
                  sum(cross_tab_df[[paste0("V",i)]]) * 100) %>% 
          paste0("\\%")
      }
      
      cross_tab_df <- cross_tab_df %>%
        dplyr::mutate(tex = paste0(comment_var, " & ", 
                                   V1, " (", V1_p, ") & ",
                                   V2, " (", V2_p, ") & ",
                                   V3, " (", V3_p, ") & ",
                                   V4, " (", V4_p, ") \\\\ \n "))
      
      #### Make table
      file_name <- paste0("safety_comment_crosstab_",
                          type,
                          "_cmntfilter", comment_filter,
                          "_dstnctpass", distinct_pass, ".tex")
      
      sink(file.path(tables_dir, file_name))
      cat("\\begin{tabular}{l c|c|c|c|c} ")
      cat("\\hline ")
      
      cat(" & ")
      fb_df$q_safety_rating %>% 
        unique() %>% 
        sort() %>% 
        paste(collapse = " & ") %>%
        paste("\\\\ \n") %>%
        cat()
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

#### Rate safe, comment negative
# Unsafe driving" [bad data]
# Pay attention [bad data?]
# The driver drove the matatu very fast [fast != safe?]
# The driver was drunk [despite drunk, drove safely]
fb_df %>%
  filter(q_safety_rating_num == 3:4,
         comment_driver_sntmt_code == 2) %>%
  pull(q_comment)

#### Rate unsafe, comment positive
# the driver did not drink alcohol [but was unsafe otherwise?]
# Drive slowly [commanding to drive -- swahili Aendeshe gari pole pole], fast but not unsafe?
fb_df %>%
  filter(q_safety_rating_num %in% 1:2,
         comment_driver_sntmt_code == 1) %>%
  pull(q_comment)

fb_df %>%
  filter(q_safety_rating_num == 1,
         comment_driver_sntmt_code == 1) %>%
  pull(q_comment_swahili)

