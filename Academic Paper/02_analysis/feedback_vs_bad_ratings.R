

fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

fb_df <- fb_df %>%
  dplyr::filter(regno != "UNKNOWN",
                ptn_cheating_fill %in% 0)

fb_df %>%
  dplyr::filter(q_safety_rating %in% "Not very safe") %>%
  pull(q_comment)

fb_df %>%
  dplyr::filter(q_speed_rating_v2 %in% "Very fast [80+]") %>%
  pull(q_comment)

veh_df %>%
  ggplot() +
  geom_point(aes(x = q_safety_rating_num,
                 y = q_speed_rating_v2_num))

veh_df %>%
  ggplot() +
  geom_point(aes(x = sentiment_snmtr,
                 y = q_speed_rating_v2_num))

veh_df %>%
  ggplot() +
  geom_point(aes(x = sentiment_snmtr,
                 y = q_safety_rating_num))
