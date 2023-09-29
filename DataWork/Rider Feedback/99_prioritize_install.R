
df <- readRDS(file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_clean.Rds"))

df <- df %>%
  dplyr::filter(valid_psvnum %in% T) %>%
  group_by(regno_clean, matatu_sacco_stk_inst_srvy, matatu_route_stk_inst_srvy) %>%
  dplyr::summarise(n_comments = n()) %>%
  arrange(-n_comments)

write_csv(df, "~/Desktop/veh_stickers.csv")
