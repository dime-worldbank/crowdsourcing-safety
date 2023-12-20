# Feedback Level Summary Statistics

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

fb_clean_df <- fb_df %>%
  dplyr::filter(regno != "UNKNOWN",
                ptn_cheating_fill %in% 0)

sum_var <- function(var, fb_df){
  fb_df$var <- fb_df[[var]]
  fb_df %>%
    dplyr::filter(!is.na(var)) %>%
    group_by(var) %>%
    dplyr::summarise(n = n()) %>%
    ungroup() %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::mutate(tex = paste0("~~~", var, " & ", n, " & ", round(prop*100, 0), "\\% \\\\ \n" )) %>%
    pull(tex) %>%
    paste(collapse = " ")
}

sink(file.path(tables_dir, "feedback_level_sum_stat.tex"))

cat("\\begin{tabular}{lcc} \n")
cat("\\hline \n")
cat("Response & N & Percent \\\\ \n")
cat("\\hline \n")
cat("\\multicolumn{3}{l}{\\textbf{Q1:} How safely is your matatu being driven?} \\\\ \n")
sum_var("q_safety_rating", fb_clean_df) %>% cat()

cat("\\hline \n")
cat("\\multicolumn{3}{l}{\\textbf{Q2:} How fast does the matatu seem to be going?} \\\\ \n")
sum_var("q_speed_rating_v2", fb_clean_df) %>% cat()

cat("\\hline \n")
cat("\\multicolumn{3}{l}{\\textit{Only asked in 2020}} \\\\ \n")
cat("\\multicolumn{3}{l}{\\textbf{Q3:} On the matatu, are there:} \\\\ \n")
sum_var("q_occupancy",  fb_clean_df) %>% cat()

cat("\\hline \n")
cat("\\multicolumn{3}{l}{\\textit{Only asked in 2020}} \\\\ \n")
cat("\\multicolumn{3}{p{8cm}}{\\textbf{Q4:} Were measures taken to precent the spread of COVID-19? E.g. Limiting passengers or providing sanitiser / wipes?} \\\\ \n")
sum_var("q_covid_measures",  fb_clean_df) %>% cat()

cat("\\hline \n")
cat("\\end{tabular}")

sink()

