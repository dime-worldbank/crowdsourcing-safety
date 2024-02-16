# N Responses per Vehicle

veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers.Rds"))

sum_df <- veh_df %>%
  dplyr::filter(!is.na(shortcode_on_sticker)) %>%
  group_by(n_feedback) %>%
  dplyr::summarise(n_vehicle = n()) %>%
  ungroup() 

sum_df$n_vehicle %>% sum()

sum_df$n_vehicle[sum_df$n_feedback == 0]
sum_df$n_vehicle[sum_df$n_feedback %in% 1:5] %>% sum()
sum_df$n_vehicle[sum_df$n_feedback %in% 6:10] %>% sum()
sum_df$n_vehicle[sum_df$n_feedback %in% 11:20] %>% sum()
sum_df$n_vehicle[sum_df$n_feedback %in% 21:50] %>% sum()
sum_df$n_vehicle[sum_df$n_feedback %in% 51:100] %>% sum()
sum_df$n_vehicle[sum_df$n_feedback %in% 101:500] %>% sum()
sum_df$n_vehicle[sum_df$n_feedback %in% 501:1500] %>% sum()

round(4/77, 2)
round(29/77, 2)
round(12/77, 2)
round(12/77, 2)
round(8/77, 2)
round(4/77, 2)
round(6/77, 2)
round(2/77, 2)

sum_df$n_vehicle[sum_df$n_feedback > 10] %>% sum()
