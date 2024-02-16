# TWFE

set.seed(42)

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

# Estimate ---------------------------------------------------------------------
date_seq <- seq.Date(ymd("2022-11-30"), ymd("2022-12-31"), by = 1)

coef_all_df <- map_df(1:100, function(rand_i){
  print(rand_i)
  
  coef_df <- map_df(y_vars, function(y_var){
    
    sensor_df <- readRDS(file.path(data_dir, "FinalData", "sensor_sticker_ie_data.Rds"))
    
    sensor_df$y_var <- sensor_df[[y_var]]
    
    sensor_inst_df <- sensor_df %>%
      filter(sticker_group_not_installed == 1) %>%
      distinct(regno)
    
    sensor_inst_df$date_install_assign <- sample(x = date_seq, 
                                                 size = nrow(sensor_inst_df),
                                                 replace = T)
    
    sensor_df <- sensor_df %>%
      left_join(sensor_inst_df, by = "regno") %>%
      mutate(days_since_stk_install_post_assign = days_since_stk_install_post) %>%
      mutate(days_since_stk_install_post_assign = case_when(
        (date >= date_install_assign) & (sticker_group_not_installed == 1) ~ 1,
        TRUE ~ days_since_stk_install_post_assign
      ))
    
    #### ITT
    twfe_iv_itt <- feols(y_var ~ days_since_stk_install_post_assign | date + regno, 
                         vcov = ~regno+date,
                         data = sensor_df)
    
    twfe_iv_itt_df <- twfe_iv_itt %>%
      confint() %>%
      as.data.frame() %>%
      clean_names() %>%
      mutate(beta = (x2_5_percent + x97_5_percent) / 2) %>%
      mutate(variable = y_var,
             rand_num = rand_i,
             est_type = "ITT")
    
    #### ATT
    twfe_iv_att <- feols(y_var ~ 1 | date + regno | days_since_stk_install_post ~ days_since_stk_install_post_assign, 
                         vcov = ~regno+date,
                         data = sensor_df)
    
    twfe_iv_att_df <- twfe_iv_att %>%
      confint() %>%
      as.data.frame() %>%
      clean_names() %>%
      mutate(beta = (x2_5_percent + x97_5_percent) / 2) %>%
      mutate(variable = y_var,
             rand_num = rand_i,
             est_type = "ATT")
    
    #### Append
    results_df <- bind_rows(twfe_iv_itt_df,
                            twfe_iv_att_df)
    
    return(results_df)
  })
  
  return(coef_df)
  
})

# Cleanup ----------------------------------------------------------------------
coef_all_df <- coef_all_df %>%
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
  beta_round = round(beta, 3)) 

# Figure -----------------------------------------------------------------------
for(est_type_i in c("ATT", "ITT")){
  
  p <- coef_all_df %>%
    filter(est_type == est_type_i) %>%
    ggplot(aes(x = beta,
               xmin = x2_5_percent,
               xmax = x97_5_percent,
               y = rand_num,
               label = beta_round)) +
    geom_vline(xintercept = 0, color = "gray70") +
    geom_linerange() +
    geom_point() +
    labs(x = "Coef +/- 95% CI",
         y = "Random\nIteration") +
    facet_wrap(~variable_clean %>% fct_rev(),
               scales = "free_x", nrow = 2) +
    theme_classic2() +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text = element_text(color = "black", size = 8),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10, angle = 0, vjust = 0.5)) 
  
  ggsave(p,
         filename = file.path(figures_dir, 
                              paste0("sticker_ie_coefs_rand_date_",tolower(est_type_i),".png")),
         height = 4.5, width = 10)
}
