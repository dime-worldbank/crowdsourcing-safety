# N Responses per Vehicle

veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers.Rds"))

sum_df <- veh_df %>%
  dplyr::filter(!is.na(shortcode_on_sticker)) %>%
  group_by(n_feedback) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  arrange(n_feedback) %>%
  mutate(prop = n / sum(n)) %>%
  mutate(prop_cumul = cumsum(prop)) %>%
  
  mutate(prop = prop %>% round(3),
         prop_cumul = prop_cumul %>% round(3)) %>%
  mutate(tex = paste0(n_feedback, " & ", n, " & ", prop, " & ", prop_cumul, " \\\\ \n "))

sink(file.path(tables_dir, "n_responses.tex"))
cat("\\begin{tabular}{cccc} \n")
cat("\\hline \n")
cat("N Survey  & N        & Proportion of & Cumulative Proportion \\\\ \n")
cat("Responses & Vehicles & Vehicles      & of Vehicles \\\\ \n")
cat("\\hline \n")
sum_df$tex %>% cat()
cat("\\hline \n")
cat("\\end{tabular} ")
sink()
