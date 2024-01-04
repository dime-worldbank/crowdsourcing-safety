# Telematics Summary

for(veh_subset in c("all", "stickers")){
  
  # Load data --------------------------------------------------------------------
  veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level.Rds"))
  
  if(veh_subset == "stickers"){
    
    veh_df <- veh_df %>%
      dplyr::filter(!is.na(shortcode_on_sticker))
    
  } 
  
  # Prep data --------------------------------------------------------------------
  veh_df <- veh_df %>%
    dplyr::select(
      #regno,
      
      prop_time_over_80kph_base_10kph,
      prop_time_over_90kph_base_10kph,
      prop_time_over_100kph_base_10kph,
      prop_time_over_110kph_base_10kph,
      prop_time_over_120kph_base_10kph,
      
      prop_time_over_90kph_base_80kph,
      prop_time_over_100kph_base_80kph,
      prop_time_over_110kph_base_80kph,
      prop_time_over_120kph_base_80kph,
      
      rate_N_valueg_above0_5_base_10kph,
      rate_N_valueg_above0_5_acceleration_base_10kph,
      rate_N_valueg_above0_5_brake_base_10kph,
      rate_N_valueg_above0_5_turn_base_10kph,
      
      rate_N_valueg_above1_0_base_10kph,
      rate_N_valueg_above1_0_acceleration_base_10kph,
      rate_N_valueg_above1_0_brake_base_10kph,
      rate_N_valueg_above1_0_turn_base_10kph
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~Winsorize(., probs = c(0, 0.9), na.rm = T))) %>%
    dplyr::filter(!is.na(prop_time_over_80kph_base_10kph))
  
  cor_pvalue <- function(x,y){
    cor.test(x,y)$p.value
  }
  
  cor_value <- function(x,y){
    cor.test(x,y)$estimate
  }
  
  cor_df <- veh_df %>%
    pivot_longer(cols = -c(contains("prop_"), contains("speed_")),
                 names_to = "viol_var",
                 values_to = "viol_value") %>%
    pivot_longer(cols = c(contains("prop_"), contains("speed_")),
                 names_to = "speed_var",
                 values_to = "speed_value") %>%
    group_by(viol_var, speed_var) %>%
    dplyr::summarise(cor_coef = cor_value(viol_value, speed_value),
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
                  speed_var_type = ifelse(speed_var %>% str_detect("kph_base_10kph"),
                                          "Percent of time vehicle travels over speed\nwhen vehicle traveling > 10 km/h", 
                                          "Percent of time vehicle travels over speed\nwhen vehicle traveling > 80 km/h"),
                  speed_var = speed_var %>%
                    str_replace_all("prop_time_over_", "") %>%
                    str_replace_all("kph_base_10kph", " km/h") %>%
                    str_replace_all("kph_base_80kph", " km/h"),
                  speed = speed_var %>% 
                    str_replace_all(" km/h", "") %>%
                    as.numeric()) %>%
    mutate(label = paste0(round(cor_coef, 2), "", stars))
  
  coef_vec <- cor_df$cor_coef[!is.na(cor_df$cor_coef)]
  if(min(coef_vec) > 0){
    coef_min <- 0
    coef_max <- max(coef_vec)
  } else{
    coef_max <- coef_vec %>% abs() %>% max()
    coef_min <- -coef_max
  }

  cor_df %>%
    ggplot(aes(x = reorder(speed_var, speed),
               y = viol_var,
               fill = cor_coef,
               label = label)) +
    geom_tile(color = "white") +
    geom_text() +
    geom_hline(yintercept = 4.5, color = "black") +
    scale_fill_gradient2(low = "dodgerblue",
                         high = "darkorange",
                         mid = "white",
                         midpoint = 0,
                         limits = c(coef_min, coef_max)) +
    labs(x = NULL,
         y = "N violations\nper hour",
         fill = "Correlation") +
    theme_minimal() +
    theme(axis.text = element_text(color = "black"),
          axis.title.y = element_text(angle = 0, vjust = 0.5)) +
    facet_wrap(~speed_var_type,
               scales = "free_x",
               strip.position="bottom")
  
  ggsave(filename = file.path(figures_dir, 
                              paste0("viol_speed_cor_veh_",veh_subset,".png")),
         height = 6, width = 9)
  
}