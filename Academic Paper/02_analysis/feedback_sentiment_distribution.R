# Feedback Sentiment Distribution

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

# fb_df <- fb_df %>%
#   dplyr::filter(regno != "UNKNOWN",
#                 ptn_cheating_fill %in% 0) %>%
#   dplyr::filter(!is.na(sentiment_snmtr))

# Prep data --------------------------------------------------------------------
fb_stacked_df <- bind_rows(
  fb_df %>%
    mutate(type = "All Comments"),
  
  fb_df %>%
    dplyr::filter(q_comment %>% str_detect(DRIVING_WORDS)) %>%
    mutate(type = "Driving\nComments"),
  
  fb_df %>%
    dplyr::filter(q_comment %>% str_detect(COVID_WORDS)) %>%
    mutate(type = "COVID-19\nComments")
) %>%
  dplyr::filter(!is.na(sentiment_snmtr)) %>%
  dplyr::filter(q_comment_nchar >= 10) #%>%
  
  # group_by(type) %>%
  # dplyr::mutate(n = n()) %>%
  # ungroup() %>%
  # 
  # dplyr::mutate(type = paste0(type, "\n[N = ", n, "]"))

# Figure -----------------------------------------------------------------------
fb_stacked_df %>%
  mutate(type = type %>% fct_rev()) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_boxplot(aes(x = sentiment_snmtr,
                   y = type)) +
  labs(x = "Sentiment",
       y = NULL) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"))
  
ggsave(filename = file.path(figures_dir, "feedback_sentiment_distribution.png"),
       height = 2, width = 5)

