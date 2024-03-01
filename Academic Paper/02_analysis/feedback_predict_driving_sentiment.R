# Predict Driving Sentiment

require(quanteda)
require(quanteda.textmodels)
require(caret)

set.seed(42)

# Load data --------------------------------------------------------------------
comment_filter <- F
distinct_pass <- T
fb_df <- readRDS(file.path(data_dir, "FinalData", 
                           paste0("passenger_feedback_valid_class_",
                                  "main", "_",
                                  "cmntfilter",
                                  comment_filter,
                                  "_",
                                  "dstnctpass",
                                  distinct_pass,
                                  ".Rds")))

fb_df <- fb_df %>%
  dplyr::filter(!is.na(q_comment),
                !is.na(comment_driver_sntmt_code_str))

fb_pn_df <- fb_df %>%
  dplyr::filter(comment_driver_sntmt_code_str %in% c("Positive",
                                                     "Negative")) %>%
  dplyr::mutate(posneg = case_when(
    comment_driver_sntmt_code_str == "Positive" ~ 1,
    comment_driver_sntmt_code_str == "Negative" ~ 0))

fb_relev_df <- fb_df %>%
  dplyr::mutate(relev = case_when(
    comment_driver_sntmt_code_str == "Positive" ~ 1,
    comment_driver_sntmt_code_str == "Negative" ~ 1,
    comment_driver_sntmt_code_str == "Neutral" ~ 0,
    comment_driver_sntmt_code_str == "Unclear" ~ 0
  ) %>% factor())

# Estimate relevance -----------------------------------------------------------
fb_relev_df$fold <- sample(x = 1:5, size = nrow(fb_relev_df), replace = T)

fb_dfm <- fb_relev_df$q_comment %>%
  tokens(remove_punct = T,
         remove_symbols = T,
         remove_numbers = T,
         remove_separators = T,
         split_hyphens = T) %>%
  tokens_ngrams(n = 1:3) %>%
  dfm()

fold_i <- 1

result_all_list <- lapply(1:5, function(fold_i){
  
  ## Train/test
  dfm_train <- fb_dfm[fb_relev_df$fold != fold_i,]
  dfm_test  <- fb_dfm[fb_relev_df$fold == fold_i,]
  
  y_train <- fb_relev_df$relev[fb_relev_df$fold != fold_i]
  y_test  <- fb_relev_df$relev[fb_relev_df$fold == fold_i]
  
  ## Cleanup dfm
  featnames_both <- intersect(featnames(dfm_test), featnames(dfm_train))
  
  dfm_train <- dfm_match(dfm_train, features = featnames_both)
  dfm_test  <- dfm_match(dfm_test, features = featnames_both)
  
  ## ML
  tmod_nb <- textmodel_nb(dfm_train, y_train)
  
  pred_df <- data.frame(y = y_test %>% as.character() %>% as.vector(),
                        y_predict = predict(tmod_nb, newdata = dfm_test) %>% 
                          unlist() %>% 
                          as.vector() %>% 
                          as.numeric())
  pred_df$uid   <- fb_relev_df$uid[fb_relev_df$fold == fold_i]
  pred_df$regno <- fb_relev_df$regno[fb_relev_df$fold == fold_i]
  pred_df$q_comment <- fb_relev_df$q_comment[fb_relev_df$fold == fold_i]
  
  recall_i <- recall(pred_df$y_predict %>% factor(),
                     pred_df$y %>% factor(),
                     relevant = "1")
  
  precision_i <- precision(pred_df$y_predict %>% factor(),
                           pred_df$y %>% factor(),
                           relevant = "1")
  
  result_df <- data.frame(
    recall = recall_i,
    precision = precision_i,
    fold = fold_i %>% as.character()
  )
  
  return(list(result_df = result_df,
              pred_df = pred_df))
})

result_all_df <- map_df(1:5, function(i){
  result_all_list[[i]]$result_df
})

pred_df <- map_df(1:5, function(i){
  result_all_list[[i]]$pred_df
})

result_avg_df <- data.frame(
  recall = result_all_df$recall %>% mean(),
  precision = result_all_df$precision %>% mean(),
  fold = "Average"
) 

resuld_df <- bind_rows(
  result_all_df,
  result_avg_df
) %>%
  mutate(f1 = 2 * (precision * recall) / (precision + recall)) %>%
  mutate(tex = paste0(
    fold, " & ",
    round(recall, 3), " & ",
    round(precision, 3), " & ",
    round(f1, 3), " \\\\ \n "
  ))

sink(file.path(tables_dir, "predic_relev.tex"))
cat("\\begin{tabular}{clll} \n")
cat("\\hline \n")
cat("Fold & Recall & Precision & F1 \\\\ \n")
cat("\\hline \n")
resuld_df$tex[resuld_df$fold != "Average"] %>% cat()
cat("\\hline \n")
resuld_df$tex[resuld_df$fold == "Average"] %>% cat()
cat("\\hline \n")
cat("\\end{tabular}")
sink()

#table(pred_df$y, pred_df$y_predict)

# Sentiment figure -------------------------------------------------------------
fb_pn_df %>%
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_boxplot(aes(x = sentiment_snmtr,
                   y = comment_driver_sntmt_code_str),
               fill = "gray90") +
  labs(x = "Sentiment assigned by sentimentr",
       y = "Manually\ncoded\nsentiment") +
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("Most\nNegative",
                                "Neutral",
                                "Most\nPositive")) +
  theme_classic2() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

ggsave(filename = file.path(figures_dir, "pred_sentiment.png"),
       height = 2, width = 5)

# Correlation with telematics --------------------------------------------------
#### Load vehicle data
veh_df <- readRDS(file.path(data_dir, "FinalData", 
                            paste0("vehicle_level_stickers_telematics_suff_feedback_cmntfilter",
                                   comment_filter,
                                   "_dstnctpass",distinct_pass,".Rds")))

pred_df$sentiment <- pred_df$q_comment %>%
  str_replace_all("[:punct:]", " ") %>%
  str_squish() %>%
  get_sentences() %>%
  sentiment() %>%
  pull(sentiment)

veh_sentiment_df <- pred_df %>%
  dplyr::filter(y_predict %in% 1) %>%
  dplyr::mutate(sentiment_pos = as.numeric(sentiment > 0)) %>%
  group_by(regno) %>%
  dplyr::summarise(sentiment_avg = mean(sentiment),
                   sentiment_pos = mean(sentiment_pos)) %>%
  ungroup()
  
veh_df <- veh_df %>%
  left_join(veh_sentiment_df, by = "regno")

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

    sentiment_avg,
    sentiment_pos,
    comment_driver_sntmt_code_avg
  ) 

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
    feedback_var == "sentiment_avg" ~ "Average Comment\nSentiment",
    feedback_var == "sentiment_pos" ~ "Proportion\nPositive Comments",
    feedback_var == "comment_driver_sntmt_code_avg" ~ "Proportion\nPositive Comments\n[Manually Coded]",
    TRUE ~ feedback_var
  )) %>%
  dplyr::mutate(feedback_sort = case_when(
    feedback_var == "sentiment_avg" ~ 1,
    feedback_var == "sentiment_pos" ~ 2,
    feedback_var == "comment_driver_sntmt_code_avg" ~ 3,
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

ggsave(filename = file.path(figures_dir, 
                            "cor_alg_sentiment.png"),
       height = 6, width = 9)





