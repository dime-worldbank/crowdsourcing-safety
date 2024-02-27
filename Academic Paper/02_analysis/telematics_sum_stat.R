# Telematics Summary

for(veh_subset in c("all", "stickers")){
  
  # Load data --------------------------------------------------------------------
  veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_cmntfilterFALSE_dstnctpassTRUE.Rds"))
  
  if(veh_subset == "stickers"){
    
    veh_df <- veh_df %>%
      dplyr::filter(!is.na(shortcode_on_sticker))
    
  }
  
  # Prep data --------------------------------------------------------------------
  sum_df <- veh_df %>%
    dplyr::select(
      regno,
      
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
      rate_N_valueg_above0_5_turn_base_10kph#,
      
      # rate_N_valueg_above1_0_base_10kph,
      # rate_N_valueg_above1_0_acceleration_base_10kph,
      # rate_N_valueg_above1_0_brake_base_10kph,
      # rate_N_valueg_above1_0_turn_base_10kph
    ) %>%
    pivot_longer(cols = -regno) %>%
    filter(!is.na(value)) %>%
    mutate(value = case_when(
      str_detect(name, "prop_time") ~ value * 100,
      TRUE ~ value
    )) %>%
    group_by(name) %>%
    dplyr::summarise(min = min(value),
                     p25 = quantile(value, 0.25),
                     mean = mean(value),
                     p50 = quantile(value, 0.50),
                     p75 = quantile(value, 0.75),
                     max = max(value)
    ) %>%
    ungroup() %>%
    dplyr::mutate(across(where(is.numeric), ~round(.,2))) %>%
    mutate(speed_viol = case_when(
             name %>% str_detect("kph_base_10kph") ~ "speed_base10",
             name %>% str_detect("kph_base_80kph") ~ "speed_base80",
             TRUE ~ "violation"
           ),
           speed_name = name %>%
             str_replace_all("prop_time_over_", "") %>%
             str_replace_all("kph_base_10kph", " km/h") %>%
             str_replace_all("kph_base_80kph", " km/h"),
           viol_name = name %>%
             str_replace_all("rate_N_valueg_above", "") %>%
             str_replace_all("0_5_base", "0_5_Any Type") %>%
             str_replace_all("1_0_base", "1_0_Any Type") %>%
             str_replace_all("_base_10kph", "") %>%
             str_replace_all("10kph", "") %>%
             str_replace_all("0_5", "$>$0.5g") %>%
             str_replace_all("1_0", "$>$1g") %>%
             str_replace_all("_", " ") %>%
             str_squish() %>%
             str_replace_all("acceleration", "Forward") %>%
             str_replace_all("brake", "Backward") %>%
             str_replace_all("turn", "Lateral")
    ) %>%
    mutate(name_full = case_when(
      speed_viol == "speed_base10" ~ speed_name,
      speed_viol == "speed_base80" ~ speed_name,
      speed_viol == "violation" ~ viol_name,
    )) %>%
    mutate(tex = paste(name_full, min, p25, mean, p50, p75, max, sep = " & ") %>%
             paste("\\\\ \n"))
  
  speed_base10_df <- sum_df %>% 
    filter(speed_viol == "speed_base10") %>%
    mutate(speed = speed_name %>%
             str_replace_all(" km/h", "") %>%
             as.numeric()) %>%
    arrange(speed)
  
  speed_base80_df <- sum_df %>% 
    filter(speed_viol == "speed_base80") %>%
    mutate(speed = speed_name %>%
             str_replace_all(" km/h", "") %>%
             as.numeric()) %>%
    arrange(speed)
  
  viol_df  <- sum_df %>% 
    filter(speed_viol == "violation") %>%
    arrange(viol_name)
  
  # Table ------------------------------------------------------------------------
  sink(file.path(tables_dir, paste0("telematics_sum_stat_veh_",veh_subset,".tex")))
  
  cat("\\begin{tabular}{lllllll} ")
  cat("\\hline ")
  cat("Variable & Min & 25th Perc. & Mean & Median & 75th Perc. & Max \\\\ \n")

  cat("\\hline ")
  cat("\\multicolumn{7}{l}{\\textbf{Percent of time vehicle travels over speed}} \\\\ \n")
  speed_base10_df$tex %>% 
    paste(collapse = " ") %>%
    cat()
  
  cat("\\hline ")
  cat("\\multicolumn{7}{l}{\\textbf{Percent of time vehicle travels over speed when traveling over 80 km/h}} \\\\ \n")
  speed_base80_df$tex %>% 
    paste(collapse = " ") %>%
    cat()
  
  cat("\\hline ")
  cat("\\multicolumn{7}{l}{\\textbf{N violations per hour}} \\\\ \n")
  viol_df$tex %>% 
    paste(collapse = " ") %>%
    cat()
  
  cat("\\hline ")
  cat("\\end{tabular} ")
  
  sink()
  
}