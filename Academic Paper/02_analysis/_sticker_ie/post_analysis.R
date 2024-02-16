# Post analysis

# prop_time_over_90kph_base_80kph --> check daily. should be NA if never go fast! If base 0, should be NA!

# Load data --------------------------------------------------------------------
sensor_ie_any_df <- readRDS(file.path(data_dir, "FinalData", "sensor_sticker_ie_any_data.Rds"))

# Prep data --------------------------------------------------------------------
sum_df <- bind_rows(
  sensor_ie_any_df %>%
    dplyr::filter(days_since_stk_install >= 0,
                  days_since_stk_install <= 7,
                  stickers_installed %in% T) %>%
    group_by(regno, stickers_installed) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
    ungroup(),
  
  sensor_ie_any_df %>%
    dplyr::filter(date >= ymd("2022-11-01"),
                  stickers_installed %in% F) %>%
    group_by(regno, stickers_installed) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
    ungroup()
)

sum_df$stickers_installed %>% table()
sum_df$drvr_feedback_treat_sticker %>% table()

feols(prop_time_over_80kph_base_10kph ~ stickers_installed, data = sum_df)
feols(prop_time_over_90kph_base_10kph ~ stickers_installed, data = sum_df)
feols(prop_time_over_90kph_base_80kph ~ stickers_installed, data = sum_df)
feols(speed_mean ~ stickers_installed, data = sum_df)

feols(prop_time_over_80kph_base_10kph ~ 1 | 0 | stickers_installed ~ drvr_feedback_treat_sticker, data = sum_df)
feols(prop_time_over_90kph_base_10kph ~ 1 | 0 | stickers_installed ~ drvr_feedback_treat_sticker, data = sum_df)
feols(prop_time_over_90kph_base_80kph ~ 1 | 0 | stickers_installed ~ drvr_feedback_treat_sticker, data = sum_df)
feols(speed_mean ~ 1 | 0 | stickers_installed ~ drvr_feedback_treat_sticker, data = sum_df)

sum_df$drvr_feedback_treat_sticker
sum_df$prop_time_over_90kph_base_10kph


