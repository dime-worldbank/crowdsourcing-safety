# Telematics Summary

comment_filter <- FALSE
distinct_pass <- TRUE

for(comment_filter in c(TRUE, FALSE)){
  for(distinct_pass in c(TRUE, FALSE)){
    
    # Load data --------------------------------------------------------------------
    veh_df <- readRDS(file.path(data_dir, "FinalData", 
                                paste0("vehicle_level_stickers_telematics_suff_feedback_cmntfilter",
                                       comment_filter,
                                       "_dstnctpass",distinct_pass,".Rds")))
    
    # Prep data --------------------------------------------------------------------
    veh_df <- veh_df %>%
      dplyr::select(
        
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
        
        # rate_N_valueg_above1_0_base_10kph,
        # rate_N_valueg_above1_0_acceleration_base_10kph,
        # rate_N_valueg_above1_0_brake_base_10kph,
        # rate_N_valueg_above1_0_turn_base_10kph,
        
        q_safety_prop_safe,
        q_safety_rating_num,
        q_speed_rating_v2_vfast,
        q_speed_rating_v2_num,
        #sentiment_snmtr,
        #sentiment_snmtr_prop_un0_1,
        #comment_driver_sntmt_code_compl,
        #comment_driver_sntmt_code_neg,
        comment_driver_sntmt_code_avg
      ) #%>%
    #dplyr::mutate(across(where(is.numeric), ~Winsorize(., probs = c(0, 0.9), na.rm = T))) 
    
    cor_pvalue <- function(x,y){
      cor.test(x,y)$p.value
    }
    
    cor_value <- function(x,y){
      cor.test(x,y)$estimate
    }
    
    cor_df <- veh_df %>%
      
      # 4 = Very safe; 1 = Not very safe; flip order so high value is unsafe, so
      # this variable is an unsafety rating
      #dplyr::mutate(q_safety_rating_num = 4 - q_safety_rating_num) %>%
      
      pivot_longer(cols = -c(starts_with("prop_"), starts_with("rate_"), starts_with("speed_p")),
                   names_to = "feedback_var",
                   values_to = "feedback_value") %>%
      pivot_longer(cols = c(starts_with("prop_"), starts_with("rate_"), starts_with("speed_p")),
                   names_to = "telematics_var",
                   values_to = "telematics_value") %>%
      group_by(feedback_var, telematics_var) %>%
      dplyr::summarise(cor_coef = cor_value(feedback_value, telematics_value),
                       cor_pvalue = cor_pvalue(feedback_value, telematics_value)) %>%
      ungroup() %>%
      dplyr::mutate(stars = case_when(
        cor_pvalue <= 0.001 ~ "***",
        cor_pvalue <= 0.01 ~ "**",
        cor_pvalue <= 0.05 ~ "*",
        TRUE ~ ""
      )) %>%
      dplyr::mutate(telematics_var_type = case_when(
        telematics_var %>% str_detect("_base_10kph") ~ "base10",
        telematics_var %>% str_detect("rate_") ~ "violation rate",
        telematics_var %>% str_detect("_base_80kph") ~ "base80",
        TRUE ~ "other"
      )) %>%
      dplyr::mutate(telematics_var = telematics_var %>%
                      str_replace_all("_base_10kph", "") %>%
                      str_replace_all("rate_N_valueg_above", "") %>%
                      str_replace_all("_base_10kph", "") %>%
                      str_replace_all("0_5", ">0.5g") %>%
                      str_replace_all("1_0", ">1g") %>%
                      str_replace_all("_", " ") %>%
                      str_squish() %>%
                      str_replace_all("acceleration", "Forward Violations/Hour") %>%
                      str_replace_all("brake", "Backward Violations/Hour") %>%
                      str_replace_all("turn", "Lateral Violations/Hour") %>%
                      factor() %>%
                      fct_rev() %>%
                      str_replace_all("prop time over", "Prop. Time Over") %>%
                      str_replace_all("kph", " km/h") %>%
                      str_replace_all(" base 80 km/h", ", when >80 km/h")) %>%
      dplyr::mutate(telematics_var = case_when(
        telematics_var == ">0.5g" ~ ">0.5g Any Violations/Hour",
        telematics_var == ">1g"   ~ ">1g Any Violations/Hour",
        TRUE ~ telematics_var
      )) %>%
      dplyr::mutate(telematics_sort = telematics_var %>%
                      str_replace_all(">80", "") %>%
                      str_replace_all("[:alpha:]", "") %>%
                      str_replace_all("[:punct:]", "") %>%
                      str_replace_all(">", "") %>%
                      str_squish() %>%
                      as.numeric()) %>%
      arrange(telematics_var_type, telematics_sort, telematics_var) %>%
      dplyr::mutate(telematics_sort = 1:n()) %>%
      dplyr::mutate(feedback_var_clean = case_when(
        feedback_var == "q_safety_rating_num" ~ "Average Safe\nRating",
        feedback_var == "q_speed_rating_v2_num" ~ "Average Speed\nRating",
        feedback_var == "sentiment_snmtr" ~ "Comment Sentiment\nAverage",
        feedback_var == "sentiment_snmtr_prop_un0_1" ~ "Comment Sentiment\n% Negative",
        feedback_var == "comment_driver_sntmt_code_compl" ~ "Positive Comment\n[Manual Code]",
        feedback_var == "comment_driver_sntmt_code_neg" ~ "Negative Comment\n[Manual Code]",
        feedback_var == "comment_driver_sntmt_code_avg" ~ "Sentiment of\nDriving Related\nComments",
        feedback_var == "q_safety_prop_unsafe" ~ "Percent\nRate Unsafe",
        feedback_var == "q_safety_prop_safe" ~ "Percent\nRate Safe",
        feedback_var == "q_speed_rating_v2_vfast" ~ "Percent Rate\nVery Fast",
        TRUE ~ feedback_var
      )) %>%
      dplyr::mutate(feedback_sort = case_when(
        feedback_var == "q_safety_rating_num" ~ 1,
        feedback_var == "q_safety_prop_safe" ~ 2,
        feedback_var == "q_speed_rating_v2_num" ~ 3,
        feedback_var == "q_speed_rating_v2_vfast" ~ 4,
        feedback_var == "sentiment_snmtr" ~ 5,
        feedback_var == "sentiment_snmtr_prop_un0_1" ~ 6,
        feedback_var == "comment_driver_sntmt_code_avg" ~ 7,
        TRUE ~ 0
      )) %>%
      mutate(label = paste0(round(cor_coef, 2), "", stars))
    
    cor_coef_abs <- cor_df$cor_coef %>% abs() %>% max()
    
    cor_df %>%
      ggplot(aes(x = reorder(feedback_var_clean, feedback_sort),
                 y = reorder(telematics_var, -telematics_sort),
                 fill = cor_coef,
                 label = label)) +
      geom_tile(color = "white") +
      geom_text() +
      geom_hline(yintercept = 4.5, color = "black") +
      geom_hline(yintercept = 9.5, color = "black") +
      #geom_hline(yintercept = 13.5, color = "black") +
      scale_fill_gradient2(low = "dodgerblue",
                           high = "darkorange",
                           mid = "white",
                           midpoint = 0,
                           limits = c(-cor_coef_abs, cor_coef_abs)) +
      labs(x = "Passenger Feedback Variable",
           y = "Telematics\nVariable",
           fill = "Correlation") +
      theme_minimal() +
      theme(axis.text = element_text(color = "black"),
            axis.title.y = element_text(angle = 0, vjust = 0.5))
    
    ggsave(filename = file.path(figures_dir, paste0("telematics_feedback_cor",
                                                    "_cmntfilter",comment_filter,
                                                    "_dstnctpass",distinct_pass,".png")),
           height = 6, width = 13)
    
    
  }
}

