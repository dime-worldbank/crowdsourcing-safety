# Telematics vs Feedback

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(ap_data_dir, "FinalData", "vehicle_level.Rds"))

# Prep data --------------------------------------------------------------------
veh_df <- veh_df %>%
  #filter(n_feedback >= 10) %>%
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
    rate_N_valueg_above1_0_turn_base_10kph,
    
    q_safety_prop_unsafe,
    q_safety_rating_num,
    q_speed_rating_v2_fast,
    q_speed_rating_v2_num,
    sentiment_snmtr,
    comment_driver_sntmt_code_compl,
    comment_driver_sntmt_code_neg
  ) %>%
  dplyr::mutate(across(starts_with("rate_N|prop_time"), ~Winsorize(., probs = c(0, 0.95), na.rm = T))) %>%
  #dplyr::mutate(across(where(is.numeric), ~Winsorize(., probs = c(0, 0.95), na.rm = T))) %>%
  dplyr::filter(!is.na(prop_time_over_80kph_base_10kph))

cor_pvalue <- function(x,y){
  cor.test(x,y)$p.value
}

cor_df <- veh_df %>%
  pivot_longer(cols = -c(contains("rate_N_"), contains("prop_time_over_")),
               names_to = "feedback_var",
               values_to = "feedback_value") %>%
  pivot_longer(cols = c(contains("rate_N_"), contains("prop_time_over_")),
               names_to = "telematics_var",
               values_to = "telematics_value") %>%
  dplyr::filter(!is.na(feedback_value),
                !is.na(telematics_value)) %>%
  group_by(feedback_var, telematics_var) %>%
  dplyr::summarise(cor_coef = cor(feedback_value, telematics_value),
                   cor_pvalue = cor_pvalue(feedback_value, telematics_value)) %>%
  ungroup() %>%
  dplyr::mutate(stars = case_when(
    cor_pvalue <= 0.001 ~ "***",
    cor_pvalue <= 0.01 ~ "**",
    cor_pvalue <= 0.05 ~ "*",
    TRUE ~ ""
  ))

#### Check scatterplots
veh_df %>%
  ggplot() +
  geom_point(aes(x = q_speed_rating_v2_fast,
                 y = prop_time_over_110kph_base_10kph))

veh_df %>%
  ggplot() +
  geom_point(aes(x = q_safety_prop_unsafe,
                 y = prop_time_over_90kph_base_10kph))

veh_df %>%
  ggplot() +
  geom_point(aes(x = sentiment_snmtr,
                 y = prop_time_over_80kph_base_10kph))

cor.test(veh_df$prop_time_over_80kph_base_10kph,
         veh_df$q_safety_rating_num)


#### Explore all correlations
long_df <- veh_df %>%
  pivot_longer(cols = -c(contains("rate_N_"), contains("prop_time_over_")),
               names_to = "feedback_var",
               values_to = "feedback_value") %>%
  pivot_longer(cols = c(contains("rate_N_"), contains("prop_time_over_")),
               names_to = "telematics_var",
               values_to = "telematics_value") %>%
  dplyr::filter(!is.na(feedback_value),
                !is.na(telematics_value)) %>%
  dplyr::filter(!(telematics_var %>% str_detect("50kph|60kph"))) %>%
  group_by(feedback_var, telematics_var) %>%
  mutate(cor = cor(feedback_value,
                   telematics_value))

for(feed_var_i in unique(long_df$feedback_var)){
  p <- long_df %>%
    dplyr::filter(feedback_var == feed_var_i) %>%
    mutate(title = paste0(telematics_var, "\nCor = ", round(cor,2) )) %>%
    ggplot() +
    geom_point(aes(x = feedback_value, y = telematics_value)) +
    facet_wrap(~reorder(title, -cor), scales = "free", ncol = 2) +
    labs(x = NULL,y=NULL,title=feed_var_i) +
    theme_classic2() +
    theme(strip.background = element_blank(),
          plot.title = element_text(face = "bold")) 

  ggsave(p, filename = file.path(db_dir, "Academic Paper", "Figures",
                                 "telematics_vs_feedback_scatter",
                                 paste0(feed_var_i, ".png")),
         height = 12,
         width = 10)
}
