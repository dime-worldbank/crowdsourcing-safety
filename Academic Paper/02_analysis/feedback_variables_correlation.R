# Telematics Summary

cor_pvalue <- function(x,y){
  cor.test(x,y)$p.value
}

cor_value <- function(x,y){
  cor.test(x,y)$estimate
}

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", 
                            "vehicle_level_stickers_suff_feedback_cmntfilterFALSE_dstnctpassTRUE.Rds"))


# Prep data --------------------------------------------------------------------
veh_df <- veh_df %>%
  dplyr::select(

    q_safety_rating_num, 
    q_safety_prop_safe,
    
    q_speed_rating_v1_num, 
    q_speed_rating_v1_dfast, 
    
    q_speed_rating_v2_num,
    q_speed_rating_v2_vfast,
    
    sentiment_snmtr, 
    sentiment_snmtr_prop_un0_1,
    
    comment_driver_sntmt_code_avg
  ) 

long_df <- map_df(names(veh_df), function(var_i){
  
  df_i <- veh_df %>%
    pivot_longer(cols = -all_of(var_i))
  
  names(df_i)[1] <- "value2"
  names(df_i)[2] <- "name1"
  names(df_i)[3] <- "value1"
  df_i$name2 <- var_i
  
  return(df_i)
})

cor_df <- long_df %>%
  filter(!is.na(value1),
         !is.na(value2)) %>%
  group_by(name1, name2) %>%
  dplyr::summarise(cor_coef = cor_value(value1, value2),
                   cor_pvalue = cor_pvalue(value1, value2)) %>%
  ungroup() %>%
  dplyr::mutate(stars = case_when(
    cor_pvalue <= 0.001 ~ "***",
    cor_pvalue <= 0.01 ~ "**",
    cor_pvalue <= 0.05 ~ "*",
    TRUE ~ ""
  )) %>%
  dplyr::mutate(name1_clean = case_when(
    name1 == "q_safety_rating_num" ~ "Average Safe\nRating",
    name1 == "q_safety_prop_safe" ~ "Percent\nRate Safe",
    name1 == "q_speed_rating_v1_num" ~ "Average Safe\nRating (V1)",
    name1 == "q_speed_rating_v1_dfast" ~ "Percent Rate\nDangerously Fast",
    name1 == "q_speed_rating_v2_num" ~ "Average Safe\nRating (V2)",
    name1 == "q_speed_rating_v2_vfast" ~ "Percent Rate\nVery Fast (80+)",
    name1 == "sentiment_snmtr" ~ "Comment\nSentiment\nAverage",
    name1 == "sentiment_snmtr_prop_un0_1" ~ "Comment\nSentiment\n% Negative",
    name1 == "comment_driver_sntmt_code_avg" ~ "Sentiment of\nCurrent Trip Driving\n[Manual Code]",
  )) %>%
  dplyr::mutate(name2_clean = case_when(
    name2 == "q_safety_rating_num" ~ "Average Safe\nRating",
    name2 == "q_safety_prop_safe" ~ "Percent\nRate Safe",
    name2 == "q_speed_rating_v1_num" ~ "Average Safe\nRating (V1)",
    name2 == "q_speed_rating_v1_dfast" ~ "Percent Rate\nDangerously Fast",
    name2 == "q_speed_rating_v2_num" ~ "Average Safe\nRating (V2)",
    name2 == "q_speed_rating_v2_vfast" ~ "Percent Rate\nVery Fast (80+)",
    name2 == "sentiment_snmtr" ~ "Comment\nSentiment\nAverage",
    name2 == "sentiment_snmtr_prop_un0_1" ~ "Comment\nSentiment\n% Negative",
    name2 == "comment_driver_sntmt_code_avg" ~ "Sentiment of\nCurrent Trip Driving\n[Manual Code]",
  )) %>%
  dplyr::mutate(name1_sort = case_when(
    name1 == "q_safety_rating_num" ~ 1,
    name1 == "q_safety_prop_safe" ~ 2,
    name1 == "q_speed_rating_v1_num" ~ 3,
    name1 == "q_speed_rating_v1_dfast" ~ 4,
    name1 == "q_speed_rating_v2_num" ~ 5,
    name1 == "q_speed_rating_v2_vfast" ~ 6,
    name1 == "sentiment_snmtr" ~ 7,
    name1 == "sentiment_snmtr_prop_un0_1" ~ 8,
    name1 == "comment_driver_sntmt_code_avg" ~ 9
  )) %>%
  dplyr::mutate(name2_sort = case_when(
    name2 == "q_safety_rating_num" ~ 1,
    name2 == "q_safety_prop_safe" ~ 2,
    name2 == "q_speed_rating_v1_num" ~ 3,
    name2 == "q_speed_rating_v1_dfast" ~ 4,
    name2 == "q_speed_rating_v2_num" ~ 5,
    name2 == "q_speed_rating_v2_vfast" ~ 6,
    name2 == "sentiment_snmtr" ~ 7,
    name2 == "sentiment_snmtr_prop_un0_1" ~ 8,
    name2 == "comment_driver_sntmt_code_avg" ~ 9
  )) %>%
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
  ggplot(aes(x = reorder(name1_clean, name1_sort),
             y = reorder(name2_clean, -name2_sort),
             fill = cor_coef,
             label = label)) +
  geom_tile(color = "white") +
  geom_text() +
  geom_abline(intercept = 10, slope = -1, linewidth = 0.5, color = "gray50") +
  scale_fill_gradient2(low = "dodgerblue",
                       high = "darkorange",
                       mid = "white",
                       midpoint = 0,
                       limits = c(coef_min, coef_max)) +
  labs(x = NULL,
       y =NULL,
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text = element_text(color = "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) 

ggsave(filename = file.path(figures_dir, 
                            "feedback_variables_correlation.png"),
       height = 6, width = 11)

