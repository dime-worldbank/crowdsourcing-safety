# Feedback Level Summary Statistics

# Load data --------------------------------------------------------------------
for(type in c("main", "wunknown")){
  for(comment_filter in c(TRUE, FALSE)){
    for(distinct_pass in c(TRUE, FALSE)){
      
      fb_df <- readRDS(file.path(data_dir, "FinalData", 
                                 paste0("passenger_feedback_valid_class_",
                                        type,
                                        "_cmntfilter",comment_filter,
                                        "_dstnctpass",distinct_pass,".Rds")))
      
      # Prep comments ----------------------------------------------------------------
      fb_df <- fb_df %>%
        mutate(sentiment_snmtr_class = case_when(
          sentiment_snmtr < -0.67 ~ "-0.67 to -1",
          sentiment_snmtr < -0.33 ~ "-0.33 to -0.67",
          sentiment_snmtr < 0 ~ "0 to -0.33",
          sentiment_snmtr < 0.33 ~ "0 to 0.33",
          sentiment_snmtr < 0.67 ~ "0.33 to 0.67",
          sentiment_snmtr <= 1 ~ "0.67 to 1",
        ) %>%
          factor(levels = c("-0.67 to -1", 
                            "-0.33 to -0.67",
                            "0 to -0.33",
                            "0 to 0.33",
                            "0.33 to 0.67",
                            "0.67 to 1")))
      
      fb_df <- fb_df %>%
        mutate(comment_driver_sntmt_code_str = comment_driver_sntmt_code_str %>%
                 as.character()) %>%
        mutate(comment_driver_sntmt_code_str = case_when(
          comment_driver_sntmt_code_str %in% c("Neutral", "Unclear") ~ "Neutral or not relevant",
          TRUE ~ comment_driver_sntmt_code_str
        )) %>%
        mutate(comment_driver_sntmt_code_str = case_when(
          comment_driver_sntmt_relev %in% 0 ~ "Neutral or not relevant",
          TRUE ~ comment_driver_sntmt_code_str
        )) %>%
        mutate(comment_driver_sntmt_code_str = comment_driver_sntmt_code_str %>%
                 factor(levels = c("Positive",
                                   "Negative",
                                   "Neutral or not relevant")))
      
      # Make table -------------------------------------------------------------------
      sum_var <- function(var, fb_df){
        fb_df$var <- fb_df[[var]]
        fb_df %>%
          dplyr::filter(!is.na(var)) %>%
          group_by(var) %>%
          dplyr::summarise(n = n()) %>%
          ungroup() %>%
          dplyr::mutate(prop = n / sum(n)) %>%
          dplyr::mutate(tex = paste0("~~~", var, " & ", n, " & ", round(prop*100, 1), "\\% \\\\ \n" )) %>%
          pull(tex) %>%
          paste(collapse = " ")
      }
      
      file_name <- paste0("feedback_level_sum_stat_",
                          type,
                          "_cmntfilter", comment_filter,
                          "_dstnctpass", distinct_pass, ".tex")
      
      # if(type == "main")     file_name <- "feedback_level_sum_stat.tex"
      # if(type == "wunknown") file_name <- "feedback_level_sum_stat_wunknown.tex"
      
      sink(file.path(tables_dir, file_name))
      
      cat("\\begin{tabular}{lcc} \n")
      cat("\\hline \n")
      cat("Response & N & Percent \\\\ \n")
      cat("\\hline \n")
      cat("\\multicolumn{3}{l}{\\textbf{Q1:} How safely is your matatu being driven?} \\\\ \n")
      sum_var("q_safety_rating", fb_df) %>% cat()
      
      cat("\\hline \n")
      cat("\\multicolumn{3}{l}{\\textit{Initial version of speed question}} \\\\ \n")
      cat("\\multicolumn{3}{l}{\\textbf{Q2:} How would you describe your matatu driver's speed?} \\\\ \n")
      sum_var("q_speed_rating_v1", fb_df) %>% cat()
      
      cat("\\hline \n")
      cat("\\multicolumn{3}{l}{\\textit{Updated version of speed question}} \\\\ \n")
      cat("\\multicolumn{3}{l}{\\textbf{Q3:} How fast does the matatu seem to be going?} \\\\ \n")
      sum_var("q_speed_rating_v2", fb_df) %>% cat()
      
      cat("\\hline \n")
      cat("\\multicolumn{3}{l}{\\textit{Only asked in 2020}} \\\\ \n")
      cat("\\multicolumn{3}{l}{\\textbf{Q4:} On the matatu, are there:} \\\\ \n")
      sum_var("q_occupancy",  fb_df) %>% cat()
      
      cat("\\hline \n")
      cat("\\multicolumn{3}{l}{\\textit{Only asked in 2020}} \\\\ \n")
      cat("\\multicolumn{3}{p{8cm}}{\\textbf{Q5:} Were measures taken to prevent the spread of COVID-19? E.g. Limiting passengers or providing sanitiser / wipes?} \\\\ \n")
      sum_var("q_covid_measures",  fb_df) %>% cat()
      
      # cat("\\hline \n")
      # cat("\\multicolumn{3}{p{8cm}}{\\textbf{Comment:} Polarity (-1 = most negative, 1 = most positive)} \\\\ \n")
      # sum_var("sentiment_snmtr_class",  fb_df) %>% cat()
      
      cat("\\hline \n")
      cat("\\multicolumn{3}{p{8cm}}{\\textbf{Comment:} Sentiment of driving related comments} \\\\ \n")
      sum_var("comment_driver_sntmt_code_str",  fb_df) %>% cat()
      
      cat("\\hline \n")
      cat("\\end{tabular}")
      
      sink()
      
    }
  }
}