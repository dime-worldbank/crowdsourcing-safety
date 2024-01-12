# TWFE

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

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(data_dir, "FinalData", "sensor_sticker_ie_data.Rds"))

# sensor_df <- sensor_df %>%
#   dplyr::filter(date >= ymd("2022-09-01"),
#                 date <= ymd("2023-12-31"))

# Estimate ---------------------------------------------------------------------
coef_df <- map_df(y_vars, function(y_var){
  
  #y_var <- "prop_time_over_90kph_base_80kph"
  
  sensor_df$y_var <- sensor_df[[y_var]]
  
  # Estimate -------------------------------------------------------------------
  sensor_tc_df <- sensor_df %>% filter(!(sticker_group_not_installed == 1))
  
  twfe_att <- feols(y_var ~ days_since_stk_install_post | date + regno, 
                    vcov = ~regno+date,
                    data = sensor_tc_df)
  
  es_att <- feols(y_var ~ days_since_stk_install_post | date + regno,
                  vcov = ~regno+date,
                  data = sensor_df %>% filter(stickers_installed == TRUE))
  
  did_nt_attgt <- att_gt(yname = "y_var",
                         tname = "date_num",
                         idname = "regno_num",
                         gname = "sticker_install_date_num",
                         xformla = ~1,
                         data = sensor_tc_df,
                         control_group = "nevertreated",
                         clustervars = "regno",
                         allow_unbalanced_panel = T,
                         print_details = T)
  did_nt_attgt_simple <- aggte(did_nt_attgt, type = "simple", na.rm = TRUE)
  
  did_nyt_attgt <- att_gt(yname = "y_var",
                          tname = "date_num",
                          idname = "regno_num",
                          gname = "sticker_install_date_num",
                          xformla = ~1,
                          data = sensor_tc_df,
                          control_group = "notyettreated",
                          clustervars = "regno",
                          allow_unbalanced_panel = T,
                          print_details = T)
  did_nyt_attgt_simple <- aggte(did_nyt_attgt, type = "simple", na.rm = TRUE)
  
  # Dataframe of coefficients and CI -------------------------------------------
  did_nt_attgt_simple_df <- data.frame(beta = did_nt_attgt_simple$overall.att,
                                       x2_5_percent = did_nt_attgt_simple$overall.att + did_nt_attgt_simple$overall.se*qnorm(.025),
                                       x97_5_percent = did_nt_attgt_simple$overall.att + did_nt_attgt_simple$overall.se*qnorm(.975),
                                       estimation_type = "Dynamic DiD\nNever Treated")
  
  did_nyt_attgt_simple_df <- data.frame(beta = did_nyt_attgt_simple$overall.att,
                                        x2_5_percent = did_nyt_attgt_simple$overall.att + did_nyt_attgt_simple$overall.se*qnorm(.025),
                                        x97_5_percent = did_nyt_attgt_simple$overall.att + did_nyt_attgt_simple$overall.se*qnorm(.975),
                                        estimation_type = "Dynamic DiD\nNot Yet Treated")
  
  twfe_att_df <- twfe_att %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    mutate(beta = (x2_5_percent + x97_5_percent) / 2,
           estimation_type = "TWFE")
  
  es_att_df <- es_att %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    mutate(beta = (x2_5_percent + x97_5_percent) / 2,
           estimation_type = "Event Study")
  
  # Cleanup output -------------------------------------------------------------
  out_df <- bind_rows(did_nt_attgt_simple_df,
                      did_nyt_attgt_simple_df,
                      twfe_att_df,
                      es_att_df) %>%
    mutate(variable = y_var)
  
  return(out_df)
})

# Cleanup ----------------------------------------------------------------------
coef_df <- coef_df %>%
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
p_speed <- coef_df %>%
  dplyr::filter(variable %>% str_detect("prop_time")) %>%
  ggplot(aes(x = beta,
             xmin = x2_5_percent,
             xmax = x97_5_percent,
             y = estimation_type,
             label = beta_round)) +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_linerange() +
  geom_point() +
  geom_text(nudge_y = 0.4,
            size = 3) +
  labs(x = "Coef +/- 95% CI",
       y = NULL) +
  facet_wrap(~variable_clean %>% fct_rev(),
             scales = "free_x", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(size = 10)) 

p_viol <- coef_df %>%
  dplyr::filter(variable %>% str_detect("rate_N_")) %>%
  ggplot(aes(x = beta,
             xmin = x2_5_percent,
             xmax = x97_5_percent,
             y = estimation_type,
             label = beta_round)) +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_linerange() +
  geom_point() +
  geom_text(nudge_y = 0.4,
            size = 3) +
  labs(x = "Coef +/- 95% CI",
       y = NULL) +
  facet_wrap(~variable_clean %>% fct_rev(),
             scales = "free_x", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(size = 10)) 

p <- ggarrange(p_speed,
               p_viol,
               ncol = 1)

ggsave(p,
       filename = file.path(figures_dir, 
                            "sticker_ie_coefs.png"),
       height = 4.5, width = 10)

