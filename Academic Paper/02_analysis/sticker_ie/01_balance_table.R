# Balance Table

for(type in c("treat_control", "treat_sticker_nosticker")){
  
  # Load data --------------------------------------------------------------------
  sensor_df <- readRDS(file.path(data_dir, "FinalData", "sensor_sticker_ie_data.Rds"))
  
  # Define treatment -----------------------------------------------------------
  if(type == "treat_control"){
    
    sensor_df$treat <- sensor_df$drvr_feedback_treat_sticker
    
  } else if(type == "treat_sticker_nosticker"){
    
    sensor_df <- sensor_df %>%
      filter(drvr_feedback_treat_sticker %in% 1) %>%
      mutate(treat = as.numeric(stickers_installed))
  }
  
  # Filter -----------------------------------------------------------------------
  sensor_base_df <- sensor_df %>%
    dplyr::filter(date >= ymd("2022-08-01"),
                  date <= ymd("2022-08-31")) 
  
  # Make table -------------------------------------------------------------------
  veh_base_df <- sensor_base_df %>%
    dplyr::select(treat, regno,
                  
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
                  rate_N_valueg_above1_0_turn_base_10kph) %>%
    group_by(treat, regno) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_longer(cols = -c(treat, regno))
  
  tt_df <- map_df(unique(veh_base_df$name), function(name_i){
    
    veh_base_df_i <- veh_base_df[veh_base_df$name %in% name_i,]
    
    tt_result <- t.test(value ~ treat, data = veh_base_df_i)
    
    out_df <- data.frame(
      sd_control = veh_base_df_i %>% filter(treat == 0) %>% pull(value) %>% sd(),
      sd_treat   = veh_base_df_i %>% filter(treat == 1) %>% pull(value) %>% sd(),
      mean_control = tt_result$estimate[1] %>% as.numeric(),
      mean_treat   = tt_result$estimate[2] %>% as.numeric(),
      ci_lower = tt_result$conf.int[1],
      ci_upper = tt_result$conf.int[2],
      tt_pvalue = tt_result$p.value
    ) %>%
      mutate(t_minus_c = mean_treat - mean_control)
    
    out_df$variable <- name_i
    
    return(out_df)
  })
  
  tt_prop_speed <- tt_df %>%
    dplyr::filter(variable %>% str_detect("prop_time")) %>%
    mutate(variable_clean = variable %>%
             str_replace_all("prop_time_over_", 
                             "") %>%
             str_replace_all("_base_10kph", "") %>%
             str_replace_all("_base_80kph", "") %>%
             str_replace_all("kph", " km/h"),
           type = "speed",
           speed = variable %>%
             str_replace_all("prop_time_over_", "") %>%
             str_replace_all("_base_10kph", "") %>%
             str_replace_all("_base_80kph", "") %>%
             str_replace_all("kph", "") %>%
             as.numeric(),
           base = variable %>%
             str_replace_all(".*_base_", "") %>%
             str_replace_all("kph", "") %>%
             as.numeric())
  
  tt_harsh <- tt_df %>%
    dplyr::filter(variable %>% str_detect("rate_N")) %>%
    mutate(variable_clean = variable %>%
             str_replace_all("rate_N_valueg_", "") %>%
             str_replace_all("above0_5_", " >0.5g") %>%
             str_replace_all("above1_0_", " >1g") %>%
             str_replace_all("acceleration", " Forward") %>%
             str_replace_all("brake", " Backward") %>%
             str_replace_all("turn", " Lateral") %>%
             str_replace_all("base_10kph", "") %>%
             str_replace_all("_", " ") %>%
             str_squish(),
           type = "harsh") %>%
    mutate(variable_clean = case_when(
      variable_clean == ">0.5g" ~ ">0.5g Any type",
      variable_clean == ">1g"   ~ ">1g Any type",
      TRUE ~ variable_clean
    )) %>%
    mutate(variable_clean = variable_clean %>%
             str_replace_all(">", "$>$"))
  
  tt_all_df <- bind_rows(tt_prop_speed, 
                         tt_harsh)
  
  tt_all_df <- tt_all_df %>%
    dplyr::mutate(tex = paste(
      variable_clean,
      
      mean_treat %>% round(2),
      sd_treat %>% round(2),
      
      mean_control %>% round(2),
      sd_control %>% round(2),
      
      t_minus_c %>% round(2),
      tt_pvalue %>% round(2),
      sep = " & "
    ) %>%
      paste0(" \\\\ \n "))
  
  sink(file.path(tables_dir, paste0("sticker_ie_balance_table_",type,".tex")))
  cat("\\begin{tabular}{l ll | ll | ll} \n")
  cat("\\hline \n")
  
  if(type == "treat_control"){
    cat(" & \\multicolumn{2}{c|}{Treatment Group}      & \\multicolumn{2}{c|}{Control Group}           & \\multicolumn{2}{c|}{Treatment - Control} \\\\ \n")
    cat(" & \\multicolumn{2}{c|}{(Stickers Installed)} & \\multicolumn{2}{c|}{(No Stickers Installed)} & & \\\\ \n")
  } else if(type == "treat_sticker_nosticker"){
    cat(" & \\multicolumn{2}{c|}{Complier Group}      & \\multicolumn{2}{c|}{Non Complier Group}           & \\multicolumn{2}{c|}{Compliers - Non Compliers} \\\\ \n")
    cat(" & \\multicolumn{2}{c|}{(Stickers Installed)} & \\multicolumn{2}{c|}{(No Stickers Installed)} & & \\\\ \n")
  }
  
  cat("Varible & Mean & SD & Mean & SD & Difference & P-value \\\\ \n")
  
  cat("\\hline \n")
  cat(" \\multicolumn{7}{l}{Proportion of time traveling above speed, when traveling above 10 km/h} \\\\ \n")
  tt_all_df %>%
    filter(base == 10) %>%
    arrange(speed) %>%
    pull(tex) %>%
    cat()
  
  cat("\\hline \n")
  cat(" \\multicolumn{7}{l}{Proportion of time traveling above speed, when traveling above 80 km/h} \\\\ \n")
  tt_all_df %>%
    filter(base == 80) %>%
    arrange(speed) %>%
    pull(tex) %>%
    cat()
  
  cat("\\hline \n")
  cat(" \\multicolumn{7}{l}{Number of harsh driving events per hour} \\\\ \n")
  tt_all_df %>%
    filter(type == "harsh") %>%
    arrange(speed) %>%
    pull(tex) %>%
    cat()
  
  cat("\\hline \n")
  cat("\\end{tabular} ")
  sink()
}
