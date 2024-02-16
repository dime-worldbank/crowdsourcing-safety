# Compare Feedback Answer

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_class.Rds"))

# Prep data --------------------------------------------------------------------
table(fb_df$q_safety_rating, fb_df$q_speed_rating_v1)

cross_tab_df <- table(fb_df$q_safety_rating, fb_df$q_speed_rating_v1) %>%
  as.matrix.data.frame() %>% 
  as.data.frame()

cross_tab_df$safe_var <- c("Very safe", "Safe", "Not safe", "Not very safe")

for(i in 1:4){
  cross_tab_df[[paste0("V",i,"_p")]] <- 
    round(cross_tab_df[[paste0("V",i)]] / 
            sum(cross_tab_df[[paste0("V",i)]]) * 100) %>% 
    paste0("\\%")
}

cross_tab_df <- cross_tab_df %>%
  dplyr::mutate(tex = paste0(safe_var, " & ", 
                            V1, " (", V1_p, ") & ",
                            V2, " (", V2_p, ") & ",
                            V3, " (", V3_p, ") & ",
                            V4, " (", V4_p, ") \\\\ \n "))

# Make table -------------------------------------------------------------------
sink(file.path(tables_dir, "safe_speed_v1_crosstab.tex"))
cat("\\begin{tabular}{l c|c|c|c|c} ")
cat("\\hline ")

cat(" & ")
fb_df$q_speed_rating_v1 %>% 
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
