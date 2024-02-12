# Sticker IE

y_vars <- c("prop_time_over_80kph_base_10kph",
            "prop_time_over_90kph_base_10kph",
            "prop_time_over_100kph_base_10kph",
            #"prop_time_over_110kph_base_10kph",
            #"prop_time_over_120kph_base_10kph",
            
            "prop_time_over_90kph_base_80kph",
            "prop_time_over_100kph_base_80kph",
            #"prop_time_over_110kph_base_80kph",
            #"prop_time_over_120kph_base_80kph",
            
            "rate_N_valueg_above0_5_base_10kph",
            "rate_N_valueg_above0_5_acceleration_base_10kph",
            "rate_N_valueg_above0_5_brake_base_10kph",
            "rate_N_valueg_above0_5_turn_base_10kph")

es_att_all_df <- map_df(y_vars, function(y_var){
  
  # Load data --------------------------------------------------------------------
  sensor_df <- readRDS(file.path(data_dir, "FinalData", "sensor_sticker_ie_data.Rds"))
  
  sensor_df$y_var <- sensor_df[[y_var]]
  
  # Estimate ---------------------------------------------------------------------
  #### Event Study
  es_att <- feols(y_var ~ days_since_stk_install_fct | date + regno,
                  vcov = ~regno+date,
                  data = sensor_df %>% filter(stickers_installed == TRUE))
  
  es_att_p_df <- summary(es_att)$coeftable %>% 
    as.data.frame() %>%
    clean_names()
  
  es_att_df <- es_att %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    rownames_to_column(var = "days_since_treat") %>%
    dplyr::mutate(days_since_treat = days_since_treat %>%
                    str_replace_all("days_since_stk_install_fct", "") %>%
                    as.numeric()) %>%
    mutate(beta = (x2_5_percent + x97_5_percent)/2,
           variable = y_var)
  
  es_att_df$p_value <- es_att_p_df$pr_t
  
  return(es_att_df)
})

# Cleanup ----------------------------------------------------------------------
es_att_all_df <- es_att_all_df %>%
  dplyr::mutate(variable_clean = case_when(
    variable == "prop_time_over_80kph_base_10kph" ~ "Prop time over 80 km/h\nwhen > 10 km/h",
    variable == "prop_time_over_90kph_base_10kph" ~ "Prop time over 90 km/h\nwhen > 10 km/h",
    variable == "prop_time_over_100kph_base_10kph" ~ "Prop time over 100 km/h\nwhen > 10 km/h",
    variable == "prop_time_over_90kph_base_80kph" ~ "Prop time over 90 km/h\nwhen > 80 km/h",
    variable == "prop_time_over_100kph_base_80kph" ~ "Prop time over 100 km/h\nwhen > 80 km/h",
    variable == "rate_N_valueg_above0_5_base_10kph" ~ "N violations/hour",
    variable == "rate_N_valueg_above0_5_acceleration_base_10kph" ~ "N acceleration\nviolations/hour",
    variable == "rate_N_valueg_above0_5_brake_base_10kph" ~ "N brake\nviolations/hour",
    variable == "rate_N_valueg_above0_5_turn_base_10kph" ~ "N turn\nviolations/hour"
  ),
  variable_clean = variable_clean %>%
    factor(levels = c("Prop time over 80 km/h\nwhen > 10 km/h",
                      "Prop time over 90 km/h\nwhen > 10 km/h",
                      "Prop time over 100 km/h\nwhen > 10 km/h",
                      "Prop time over 90 km/h\nwhen > 80 km/h",
                      "Prop time over 100 km/h\nwhen > 80 km/h",
                      "N violations/hour",
                      "N acceleration\nviolations/hour",
                      "N brake\nviolations/hour",
                      "N turn\nviolations/hour") %>%
             rev())) %>%
  mutate(p_value_fct = case_when(
    p_value <= 0.01 ~ "p < 0.01",
    p_value <= 0.05 ~ "p < 0.05",
    p_value <= 0.1  ~ "p < 0.1",
    p_value > 0.1 ~ "p > 0.1"
  ))

# Figure -----------------------------------------------------------------------
p <- es_att_all_df %>%
  filter(abs(days_since_treat) <= 30) %>%
  ggplot(aes(x = days_since_treat,
             y = beta,
             ymin = x2_5_percent,
             ymax = x97_5_percent,
             color = p_value_fct)) +
  geom_vline(xintercept = 0, color = "black", alpha = 0.7, linetype = "dotted") +
  geom_hline(yintercept = 0, color = "black", alpha = 0.7, linetype = "dotted") +
  geom_linerange() +
  geom_point() +
  labs(x = "Days Since Sticker Installation",
       y = "Coef\n+/- 95% CI",
       color = "Significance") +
  scale_color_manual(values = c("firebrick2", "orange", "gray50")) +
  facet_wrap(~variable_clean %>% fct_rev(), 
             scales = "free_y", 
             nrow = 2) +
  theme_classic2() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(face = "bold", size = 12),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "top") 

ggsave(p,
       filename = file.path(figures_dir, 
                            "sticker_ie_days_since_es.png"),
       height = 5, width = 14)
