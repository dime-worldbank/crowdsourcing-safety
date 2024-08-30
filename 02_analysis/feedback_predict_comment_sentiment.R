# Feedback: Predict Comment Sentiment

require(quanteda)
require(quanteda.textmodels)
require(caret)

set.seed(42)

ai_name_cat <- "AI (chatGPT-4o)\nDriving Safety Category"
ai_name_4o <- "AI (chatGPT-4o)"
ai_name_35 <- "AI (chatGPT-3.5)"
ml_name <- "Machine learning (SVM)"
nlp_name <- "NLP sentiment classification\n(sentimentr)"

# Load data --------------------------------------------------------------------
#fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_class_main_cmntfilterFALSE_dstnctpassTRUE.Rds"))

fb_df <- readRDS(file.path(data_dir, "FinalData", 
                        paste0("passenger_feedback_valid_class_",
                               "main", "_",
                               "cmntfilter",
                               FALSE,
                               "_",
                               "dstnctpass",
                               TRUE,
                               ".Rds")))

# Prep dataset -----------------------------------------------------------------
## Cleanup comment variable
fb_df <- fb_df %>%
  dplyr::mutate(q_comment_nchar = nchar(q_comment)) %>%
  dplyr::filter(!is.na(q_comment)) %>%
  dplyr::filter(q_comment != "") %>%
  dplyr::filter(q_comment_nchar >= 3) %>%
  dplyr::mutate(q_comment = q_comment %>%
                  str_replace_all("[:punct:]", " ") %>%
                  str_squish() %>%
                  tolower())

## Cleanup outcome
fb_df <- fb_df %>%
  dplyr::mutate(comment_driver_sntmt_code = case_when(
    comment_driver_sntmt_code == 4 ~ 3,
    TRUE ~ comment_driver_sntmt_code
  )) %>%
  dplyr::mutate(q_comment_nchar = nchar(q_comment)) 

fb_df$comment_driver_sntmt_code[(fb_df$comment_driver_sntmt_relev %in% 0) & 
                                  is.na(fb_df$comment_driver_sntmt_code)] <- 3

fb_df <- fb_df %>%
  dplyr::mutate(chatgpt_4o_cat_pos = case_when(
    chatgpt_4o_cat_1p %in% TRUE ~ 1, 
    chatgpt_4o_cat_1n %in% TRUE ~ 2,
    TRUE ~ 3
  ))

## Unique comment
fb_df <- fb_df %>%
  dplyr::mutate(q_comment = q_comment %>%
                  tolower()) %>%
  dplyr::select(q_comment, 
                comment_driver_sntmt_code, 
                sentiment_snmtr, 
                comment_driver_gpt_4o_code,
                comment_driver_gpt_35_code,
                chatgpt_4o_cat_pos) %>%
  distinct(q_comment, .keep_all = T) %>%
  dplyr::filter(!is.na(q_comment))

fb_df <- fb_df %>%
  dplyr::mutate(sentiment_snmtr_code = case_when(
    sentiment_snmtr > 0 ~ 1,
    sentiment_snmtr < 0 ~ 2,
    sentiment_snmtr == 0 ~ 3
  ))

fb_df <- fb_df %>%
  dplyr::mutate(q_comment_nchar = nchar(q_comment)) %>%
  arrange(-q_comment_nchar)

# Make folds -------------------------------------------------------------------

## Randomly sort
fb_df <- fb_df[sample(1:nrow(fb_df)),]

## Make folds
folds_vec <- rep(1:10, 500)
folds_vec <- folds_vec[1:nrow(fb_df)]

# Make features ----------------------------------------------------------------
fb_dfm <- fb_df %>%
  pull(q_comment) %>%
  tokens(remove_punct = TRUE, remove_number = TRUE) %>%
  tokens_remove(pattern = stopwords("en")) %>% 
  tokens_wordstem() %>%
  tokens_ngrams(n = 1) %>%  
  dfm() #%>%
#dfm_trim(min_docfreq = 0.01, max_docfreq = 0.99)

# Implement model --------------------------------------------------------------
results_df <- map_df(unique(folds_vec), function(fold_i){
  print(fold_i)
  
  # Features
  fb_dfm_train <- dfm_subset(fb_dfm, folds_vec != fold_i)
  fb_dfm_test  <- dfm_subset(fb_dfm, folds_vec == fold_i)
  
  # Y
  y_train <- fb_df$comment_driver_sntmt_code[folds_vec != fold_i]
  y_test  <- fb_df$comment_driver_sntmt_code[folds_vec == fold_i]
  y_test_comment <- fb_df$q_comment[folds_vec == fold_i]
  
  # Run model
  tmod <- textmodel_svm(fb_dfm_train, y_train)
  
  # Restrict features in training set that are also in test set
  fb_dfm_test_matched <- dfm_match(fb_dfm_test, features = featnames(fb_dfm_train))
  
  # Results
  y_pred <- predict(tmod, newdata = fb_dfm_test_matched)
  results_i_df <- data.frame(y_pred = y_pred,
                             y_test = y_test,
                             q_comment = y_test_comment)
  
  return(results_i_df)
})

# Make results function --------------------------------------------------------
make_results <- function(y_pred, y_test){
  
  confusion_matrix <- confusionMatrix(factor(y_pred), factor(y_test))
  
  # Confusion matrix details
  cm <- confusion_matrix$table
  
  # Precision, recall, and F1 score for each class
  precision <- diag(cm) / rowSums(cm)
  recall <- diag(cm) / colSums(cm)
  f1_score <- 2 * precision * recall / (precision + recall)
  
  # Data frame with metrics for each class
  metrics_df <- data.frame(
    Class = levels(factor(y_test)),
    Precision = precision,
    Recall = recall,
    F1_Score = f1_score
  )
  
  metrics_long_df <- metrics_df %>%
    pivot_longer(cols = -Class)
  
  return(metrics_long_df)
}

# Make metrics -----------------------------------------------------------------
fb_df$all_one <- 1
make_results(fb_df$all_one,
             fb_df$comment_driver_sntmt_code)

metrics_all_df <- bind_rows(
  make_results(results_df$y_pred,
               results_df$y_test) %>%
    mutate(alg = ml_name),
  
  make_results(fb_df$sentiment_snmtr_code,
               fb_df$comment_driver_sntmt_code) %>%
    mutate(alg = nlp_name),
  
  make_results(fb_df$comment_driver_gpt_4o_code,
               fb_df$comment_driver_sntmt_code) %>%
    mutate(alg = ai_name_4o),
  
  make_results(fb_df$comment_driver_gpt_35_code,
               fb_df$comment_driver_sntmt_code) %>%
    mutate(alg = ai_name_35),
  
  make_results(fb_df$chatgpt_4o_cat_pos,
               fb_df$comment_driver_sntmt_code) %>%
    mutate(alg = ai_name_cat)
)

metrics_all_df <- metrics_all_df %>%
  dplyr::mutate(class_str = case_when(
    Class == 1 ~ "Safe",
    Class == 2 ~ "Unsafe",
    Class == 3 ~ "Neutral or\nnot relevant"
  )) %>%
  dplyr::mutate(class_str = class_str %>%
                  factor(levels = c("Safe",
                                    "Unsafe",
                                    "Neutral or\nnot relevant")),
                name = name %>%
                  str_replace_all("_", " "),
                alg = alg %>%
                  factor(levels = c(ai_name_cat,
                                    ai_name_4o,
                                    ai_name_35,
                                    ml_name,
                                    nlp_name)) %>%
                  fct_rev())

# Overall results --------------------------------------------------------------
metrics_all_df %>%
  dplyr::filter(alg != ai_name_cat) %>%
  ggplot(aes(x = class_str,
             y = value,
             fill = alg)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = round(value, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  facet_wrap(~name) +
  scale_fill_manual(values = c("gray",
                               "darkorange",
                               "green2",
                               "forestgreen")) +
  labs(x = NULL,
       y = NULL,
       fill = "Algorithm") +
  ylim(0, 1) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")) 

ggsave(filename = file.path(figures_dir,
                            "class_comments_results.png"),
       height = 4, width = 14)

# Example comments -------------------------------------------------------------
results_lim_df <- results_df %>%
  dplyr::select(y_pred, q_comment)

fb_lim_df <- fb_df %>%
  dplyr::select(q_comment,
                comment_driver_sntmt_code,
                sentiment_snmtr_code,
                comment_driver_gpt_4o_code)

comment_df <- fb_lim_df %>%
  left_join(results_lim_df, by = "q_comment") %>%
  dplyr::rename(svm_pred = y_pred) %>%
  dplyr::select(q_comment,
                comment_driver_sntmt_code,
                sentiment_snmtr_code,
                svm_pred,
                comment_driver_gpt_4o_code)

# 1 = safe
# 2 = unsafe
# 3 = Neutral / not relevant

comment_df %>%
  filter(q_comment %in% "from my perspective the driver drives safely")

comment_df %>%
  filter(q_comment %in% "matatu is clean but very fast")

comment_df %>%
  filter(q_comment %in% "drives fast")

comment_df %>%
  filter(q_comment %in% "nice car")



# chatGPT Category Results -----------------------------------------------------
metrics_all_df %>%
  dplyr::filter(alg %in% c(ai_name_cat,
                           ai_name_35,
                           ai_name_4o)) %>%
  ggplot(aes(x = class_str,
             y = value,
             fill = alg)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = round(value, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  facet_wrap(~name) +
  scale_fill_manual(values = c("green2",
                               "forestgreen",
                               "darkorange")) +
  labs(x = NULL,
       y = NULL,
       fill = "Algorithm") +
  ylim(0, 1) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")) 

ggsave(filename = file.path(figures_dir,
                            "class_comments_results_chatGPTonly.png"),
       height = 4, width = 14)

# Make metrics by length of comments -------------------------------------------
# results_df <- results_df %>%
#   dplyr::mutate(q_comment_nchar = nchar(q_comment))
# 
# fb_df <- fb_df %>%
#   dplyr::mutate(q_comment_nchar = nchar(q_comment))
# 
# metrics_char_df <- map_df(seq(5, 100, 5), function(n_char_thresh){
#   results_df_i <- results_df[results_df$q_comment_nchar >= n_char_thresh,]
#   fb_df_i      <- fb_df[fb_df$q_comment_nchar >= n_char_thresh,]
# 
#   metrics_i_df <- bind_rows(
#     make_results(results_df_i$y_pred,
#                  results_df_i$y_test) %>%
#       mutate(alg = "SVM"),
# 
#     make_results(fb_df_i$sentiment_snmtr_code,
#                  fb_df_i$comment_driver_sntmt_code) %>%
#       mutate(alg = "Off-the-shelf\n(sentimentr)"),
# 
#     make_results(fb_df_i$comment_driver_gpt_code,
#                  fb_df_i$comment_driver_sntmt_code) %>%
#       mutate(alg = "chatGPT")
#   )
# 
#   metrics_i_df$n_char_thresh <- n_char_thresh
# 
#   return(metrics_i_df)
# })
# 
# metrics_char_df <- metrics_char_df %>%
#   dplyr::mutate(class_str = case_when(
#     Class == 1 ~ "Positive",
#     Class == 2 ~ "Negative",
#     Class == 3 ~ "Not related"
#   )) %>%
#   dplyr::mutate(class_str = class_str %>%
#                   factor(levels = c("Positive",
#                                     "Negative",
#                                     "Not related")),
#                 name = name %>%
#                   str_replace_all("_", " "),
#                 alg = alg %>%
#                   factor(levels = c("chatGPT",
#                                     "SVM",
#                                     "Off-the-shelf\n(sentimentr)")) %>%
#                   fct_rev())
# 
# # Results by character length --------------------------------------------------
# metrics_char_df %>%
#   ggplot(aes(x = n_char_thresh,
#              y = value,
#              color = alg)) +
#   geom_line() +
#   facet_grid(class_str~name)
# 
