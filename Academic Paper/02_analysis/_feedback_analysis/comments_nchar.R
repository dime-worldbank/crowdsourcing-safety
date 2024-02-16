# Comments: Number of Characters

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

# Figure -----------------------------------------------------------------------
nchar_df <- fb_df %>%
  mutate(
    q_comment_nchar = case_when(
      is.na(q_comment_nchar) ~ 0,
      TRUE ~ q_comment_nchar
    ),
    q_comment_cat = case_when(
      q_comment_nchar == 0 ~ "0",
      q_comment_nchar == 1 ~ "1",
      q_comment_nchar == 2 ~ "2",
      q_comment_nchar == 3 ~ "3",
      q_comment_nchar == 4 ~ "4",
      q_comment_nchar %in% 5:10 ~ "5 - 10",
      q_comment_nchar %in% 11:50 ~ "11 - 50",
      q_comment_nchar >= 51 ~ "$>$50"
    ),
    q_comment_cat = q_comment_cat %>%
      factor(levels = c("0",
                        "1",
                        "2",
                        "3",
                        "4",
                        "5 - 10",
                        "11 - 50",
                        "$>$50"
                        ))
    ) %>%
  group_by(q_comment_cat) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  mutate(percent = n / sum(n) * 100,
         percent_str = percent %>% round(1) %>% paste0("\\%"),
         tex = paste(q_comment_cat,
                     n,
                     percent_str,
                     sep = " & ") %>%
           paste0(" \\\\ \n"))


sink(file.path(tables_dir, "comment_nchar_table.tex"))

cat("\\begin{tabular}{ccc} \n")
cat("\\hline \n")
cat("N Characters in Comment & N Comments & \\% of Comments \n")
cat("\\hline \n")
nchar_df$tex %>%
  cat()
cat("\\hline \n")
cat("\\end{tabular}")

sink()


