# Feedback: Ratings vs Comments

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

fb_df <- fb_df %>%
  dplyr::mutate(q_safety_rating_safe = (q_safety_rating == "Safe") | (q_safety_rating == "Very safe"),
                q_safety_rating_unsafe = (q_safety_rating == "Not safe") | (q_safety_rating == "Not very safe")) %>%
  dplyr::mutate(comment_driver_sntmt_code_compl = as.numeric(comment_driver_sntmt_code_compl),
                comment_driver_sntmt_code_neg = as.numeric(comment_driver_sntmt_code_neg))

# Figures ----------------------------------------------------------------------
fb_stacked_df <- bind_rows(
  fb_df %>% mutate(type = "All Comments"),
  
  fb_df %>% 
    dplyr::filter(q_comment %>% tolower() %>% str_detect("safe|drunk|accident|careless")) %>%
    mutate(type = "Driving Related\nComments")
)

p_safe <- fb_stacked_df %>%
  dplyr::filter(!is.na(q_safety_rating)) %>%
  dplyr::mutate(q_safety_rating = fct_rev(q_safety_rating)) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_boxplot(aes(x = sentiment_snmtr,
                   y = q_safety_rating,
                   fill = type)) +
  labs(x = "Sentiment",
       y = NULL,
       title = "A. Distribution of sentiment by how safely\npassengers rate matatu driving",
       fill = NULL) +
  scale_fill_manual(values = c("dodgerblue", "darkorange")) +
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("-1 (Negative)", "0", "1 (Positive)")) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 10)) #+
  #guides(fill = guide_legend(reverse = TRUE))

p_speed <- fb_stacked_df %>%
  dplyr::filter(!is.na(q_speed_rating_v2)) %>%
  dplyr::mutate(q_speed_rating_v2 = fct_rev(q_speed_rating_v2)) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_boxplot(aes(x = sentiment_snmtr,
                   y = q_speed_rating_v2,
                   fill = type)) +
  labs(x = "Sentiment",
       y = NULL,
       title = "B. Distribution of sentiment by how\nfast passengers rate matatu driving",
       fill = NULL) +
  scale_fill_manual(values = c("dodgerblue", "darkorange")) +
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("-1 (Negative)", "0", "1 (Positive)")) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 10)) 

p <- ggarrange(p_safe, p_speed,
               common.legend = T,
               legend = "top")

ggsave(p, filename = file.path(figures_dir, "feedback_ratings_v_sentiment.png"),
       height = 3, width = 8)

# Sentiment vs Manual Coded ----------------------------------------------------
p <- fb_df %>%
  dplyr::filter(!is.na(comment_driver_sntmt_code)) %>%
  dplyr::mutate(comment_driver_sntmt_code_str = case_when(
    comment_driver_sntmt_code == 1 ~ "Compliment",
    comment_driver_sntmt_code == 2 ~ "Negative",
    comment_driver_sntmt_code == 3 ~ "Neutral",
    comment_driver_sntmt_code == 4 ~ "Unclear"
  )) %>%
  dplyr::mutate(comment_driver_sntmt_code_str = comment_driver_sntmt_code_str %>%
                  fct_rev()) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_boxplot(aes(x = sentiment_snmtr,
                   y = comment_driver_sntmt_code_str),
               fill = "gray90") +
  labs(x = "Sentiment",
       y = NULL,
       title = "Distribution of sentiment by manual ratings\nof comments") +
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("-1 (Negative)", "0", "1 (Positive)")) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 10))
  
ggsave(p, filename = file.path(figures_dir, "feedback_manualcode_vs_sentiment.png"),
       height = 2, width = 4)
