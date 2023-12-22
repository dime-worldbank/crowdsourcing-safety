# Telematics Summary

# Load data --------------------------------------------------------------------
veh_df    <- readRDS(file.path(data_dir, "FinalData", "vehicle_level.Rds"))
#sensor_df <- readRDS(file.path(data_dir, "RawData", "sensor_day.Rds"))

# Prep data --------------------------------------------------------------------
veh_df <- veh_df %>%
  dplyr::select(
    #regno,
    
    prop_time_over_50kph_base_10kph,
    prop_time_over_60kph_base_10kph,
    prop_time_over_70kph_base_10kph,
    prop_time_over_80kph_base_10kph,
    prop_time_over_90kph_base_10kph,
    prop_time_over_100kph_base_10kph,
    prop_time_over_110kph_base_10kph,
    prop_time_over_120kph_base_10kph,
    
    rate_N_valueg_above0_5_base_10kph,
    rate_N_valueg_above0_5_acceleration_base_10kph,
    rate_N_valueg_above0_5_brake_base_10kph,
    rate_N_valueg_above0_5_turn_base_10kph,
    
    rate_N_valueg_above1_0_base_10kph,
    rate_N_valueg_above1_0_acceleration_base_10kph,
    rate_N_valueg_above1_0_brake_base_10kph,
    rate_N_valueg_above1_0_turn_base_10kph
  ) %>%
  dplyr::mutate(across(where(is.numeric), ~Winsorize(., probs = c(0, 0.95), na.rm = T))) %>%
  dplyr::filter(!is.na(prop_time_over_80kph_base_10kph))

cor_pvalue <- function(x,y){
  cor.test(x,y)$p.value
}

cor_df <- veh_df %>%
  pivot_longer(cols = -contains("prop_"),
               names_to = "viol_var",
               values_to = "viol_value") %>%
  pivot_longer(cols = contains("prop_"),
               names_to = "speed_var",
               values_to = "speed_value") %>%
  group_by(viol_var, speed_var) %>%
  dplyr::summarise(cor_coef = cor(viol_value, speed_value),
                   cor_pvalue = cor_pvalue(viol_value, speed_value)) %>%
  ungroup() %>%
  dplyr::mutate(stars = case_when(
    cor_pvalue <= 0.001 ~ "***",
    cor_pvalue <= 0.01 ~ "**",
    cor_pvalue <= 0.05 ~ "*",
    TRUE ~ ""
  )) %>%
  dplyr::mutate(viol_var = viol_var %>%
                  str_replace_all("rate_N_valueg_above", "") %>%
                  str_replace_all("0_5_base", "0_5_Any Type") %>%
                  str_replace_all("1_0_base", "1_0_Any Type") %>%
                  str_replace_all("_base_10kph", "") %>%
                  str_replace_all("10kph", "") %>%
                  str_replace_all("0_5", ">0.5g") %>%
                  str_replace_all("1_0", ">1g") %>%
                  str_replace_all("_", " ") %>%
                  str_squish() %>%
                  str_replace_all("acceleration", "Forward") %>%
                  str_replace_all("brake", "Backward") %>%
                  str_replace_all("turn", "Lateral") %>%
                  factor() %>%
                  fct_rev(),
                speed_var = speed_var %>%
                  str_replace_all("prop_time_over_", "") %>%
                  str_replace_all("kph_base_10kph", " km/h"),
                speed = speed_var %>% 
                  str_replace_all(" km/h", "") %>%
                  as.numeric()) %>%
  mutate(label = paste0(round(cor_coef, 2), "", stars))

veh_df %>%
  ggplot() +
  geom_point(aes(x = comment_driver_sntmt_code_neg,
                 y = rate_N_valueg_above1_0_acceleration_base_10kph))

cor_df %>%
  ggplot(aes(x = reorder(speed_var, speed),
             y = viol_var,
             fill = cor_coef,
             label = label)) +
  geom_tile(color = "white") +
  geom_text() +
  scale_fill_gradient2(low = "dodgerblue",
                       high = "darkorange",
                       mid = "white",
                       midpoint = 0) +
  labs(x = "Percent of time vehicle travels over speed",
       y = "N violations\nper hour",
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text = element_text(color = "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))

ggsave(filename = file.path(figures_dir, "viol_speed_cor.png"),
       height = 6, width = 9)

