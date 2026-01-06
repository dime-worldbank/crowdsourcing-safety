# Comment length by category

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "FinalData", 
                        paste0("passenger_feedback_valid_class_",
                               "main", "_",
                               "cmntfilter",
                               FALSE,
                               "_",
                               "dstnctpass",
                               TRUE,
                               ".Rds"))) 

df <- df %>%
  dplyr::filter(!is.na(chatgpt_4o_cat))

comment_df <- map_df(1:nrow(df), function(i){
  df_i <- df[i,]
  
  df_i$chatgpt_4o_cat %>%
    str_split(";") %>%
    unlist() %>%
    as.data.frame() %>%
    dplyr::rename(category = ".") %>%
    dplyr::mutate(q_comment = df_i$q_comment)
})

comment_df <- comment_df %>%
  dplyr::mutate(q_comment_nchar = nchar(q_comment),
                q_comment_nwords = str_count(q_comment, "\\S+"),
                category_num = category %>% str_replace_all("p|r|n", "")) %>%
  dplyr::filter(nchar(category_num) == 1) %>%
  dplyr::mutate(category_str = case_when(
    category_num == 1 ~ "Driving and Safety",
    category_num == 2 ~ "Adherence to COVID-19 Measures",
    category_num == 3 ~ "Service Quality and Passenger Experience",
    category_num == 4 ~ "Vehicle Condition",
    category_num == 5 ~ "Other"
  ))

nchar_df <- comment_df %>%
  group_by(category_num, category_str) %>%
  dplyr::summarise(nchar_mean = mean(q_comment_nchar),
                   nchar_p25 = quantile(q_comment_nchar, 0.25),
                   nchar_p50 = quantile(q_comment_nchar, 0.50),
                   nchar_p75 = quantile(q_comment_nchar, 0.75)) %>%
  ungroup() %>%
  dplyr::mutate(tex = paste0(category_str, " & ",
                             
                             round(nchar_p25, 1), " & ",
                             round(nchar_p50, 1), " & ",
                             round(nchar_mean, 1), " & ",
                             round(nchar_p75, 1), " \\\\ \n")) 

words_df <- comment_df %>%
  group_by(category_num, category_str) %>%
  dplyr::summarise(words_mean = mean(q_comment_nwords),
                   words_p25 = quantile(q_comment_nwords, 0.25),
                   words_p50 = quantile(q_comment_nwords, 0.50),
                   words_p75 = quantile(q_comment_nwords, 0.75)) %>%
  ungroup() %>%
  dplyr::mutate(tex = paste0(category_str, " & ",
                             
                             round(words_p25, 1), " & ",
                             round(words_p50, 1), " & ",
                             round(words_mean, 1), " & ",
                             round(words_p75, 1), " \\\\ \n")) 

sink(file.path(tables_dir, "feedback_comment_length_category.tex"))
cat("\\begin{tabular}{l | llll} \n")
cat("\\hline \n")
cat("Category & 25th Percentile & Median & Mean & 75th Percentile \\\\ \n")
cat("\\hline \n")
cat("  & \\multicolumn{4}{c}{N Characters} \\\\ \n")
nchar_df$tex %>% cat()

cat("\\hline \n")
cat("  & \\multicolumn{4}{c}{N Words} \\\\ \n")
words_df$tex %>% cat()
cat("\\hline \n")
cat("\\end{tabular} ")
sink()



