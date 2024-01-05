# Telematics Distribution

for(veh_subset in c("all", "stickers")){
  
  # Load data --------------------------------------------------------------------
  veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers.Rds"))
  
  if(veh_subset == "stickers"){
    
    veh_df <- veh_df %>%
      dplyr::filter(!is.na(shortcode_on_sticker))
    
  }
  
  veh_df$rate_N_valueg_above1_0_base_80kph[veh_df$rate_N_valueg_above1_0_base_80kph >= 20] <- 20
  
  sum_df <- veh_df %>%
    dplyr::select(
      regno,
      
      prop_time_over_80kph_base_10kph,
      prop_time_over_100kph_base_10kph,
      prop_time_over_100kph_base_80kph,
      rate_N_valueg_above0_5_base_10kph,
      rate_N_valueg_above1_0_base_10kph
    ) %>%
    pivot_longer(cols = -regno) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(name_clean = case_when(
      name == "prop_time_over_80kph_base_10kph" ~ "Proportion of time\ntraveling over 80 km/h",
      name == "prop_time_over_100kph_base_10kph" ~ "Proportion of time\ntraveling over 100 km/h",
      name == "prop_time_over_100kph_base_80kph" ~ "Proportion of time\ntraveling over 100 km/h\nwhen traveling over 80 km/h",
      name == "rate_N_valueg_above0_5_base_10kph" ~ "N harsh driving\nviolations per hour, >0.5g",
      name == "rate_N_valueg_above1_0_base_10kph" ~ "N harsh driving\nviolations per hour, >1g"
    )) %>%
    dplyr::mutate(name_clean = name_clean %>%
                    factor(levels = c(
                      "Proportion of time\ntraveling over 80 km/h",
                      "Proportion of time\ntraveling over 100 km/h",
                      "Proportion of time\ntraveling over 100 km/h\nwhen traveling over 80 km/h",
                      "N harsh driving\nviolations per hour, >0.5g",
                      "N harsh driving\nviolations per hour, >1g"
                    )))
  
  sum_df %>%
    ggplot() +
    geom_histogram(aes(x = value),
                   fill = "dodgerblue",
                   color = "black") +
    facet_wrap(~name_clean,
               scales = "free") +
    labs(x = NULL,
         y = "N\nVehicles") +
    theme_classic2() +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.title.y = element_text(angle = 0, vjust = 0.5))
  
  ggsave(filename = file.path(figures_dir, 
                              paste0("telematics_hist_veh_",veh_subset,".png")),
         height = 3.5, width = 8)
  
}




