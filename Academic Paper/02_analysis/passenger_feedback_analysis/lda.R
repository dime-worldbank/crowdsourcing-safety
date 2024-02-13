# Topic Classification

library(topicmodels)

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

comment_lda <- fb_df$q_comment[!is.na(fb_df$q_comment)] %>%
  dfm() %>%
  LDA(k = 4,
      control = list(seed = 1234))

ap_topics <- tidy(comment_lda, matrix = "gamma")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(gamma, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -gamma)
