# Embedding Cluster Figure

# Load data --------------------------------------------------------------------
n_valid_comments <- readRDS(file.path(data_dir, "FinalData", 
                           paste0("passenger_feedback_valid_class_",
                                  "main", "_",
                                  "cmntfilter",
                                  FALSE,
                                  "_",
                                  "dstnctpass",
                                  TRUE,
                                  ".Rds"))) %>%
  dplyr::filter(!is.na(chatgpt_4o_cat)) %>%
  nrow()

cluster_df <- readRDS(file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs",
                                "fb_comments_with_embeddings_cluster_nneighbor5.Rds"))


cluster_df$q_comment_chatgpt_summary_lb <- sapply(
  strsplit(cluster_df$q_comment_chatgpt_summary, "\\s+"),
  function(x) paste(x, collapse = " ") |>
    strsplit(" ") |>
    unlist() |>
    split(ceiling(seq_along(x) / 3)) |>
    sapply(paste, collapse = " ") |>
    paste(collapse = "\n")
)

top5_per_category_df <- cluster_df %>%
  group_by(q_comment_rating) %>%
  slice_max(order_by = n_alltweets, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::mutate(q_comment_rating_num = q_comment_rating %>%
                  str_replace_all("n|r|p", ""),
                q_comment_rating_sent = q_comment_rating %>%
                  str_replace_all("1|2|3|4|5", "")) %>%
  dplyr::filter(q_comment_rating_num != "5") %>%
  dplyr::mutate(q_comment_rating_sent = 
                  case_when(q_comment_rating_sent == "p" ~ "Positive",
                            q_comment_rating_sent == "r" ~ "Recommendation",
                            q_comment_rating_sent == "n" ~ "Negative"),
                q_comment_rating_sent = q_comment_rating_sent %>%
                  factor(levels = c("Positive", "Negative", "Recommendation")),
                q_comment_rating_num = case_when(
                  q_comment_rating_num == "1" ~ "Driving and Safety",
                  q_comment_rating_num == "2" ~ "Adherence to\nCOVID-19 Measures",
                  q_comment_rating_num == "3" ~ "Service Quality and\nPassenger Experience",
                  q_comment_rating_num == "4" ~ "Vehicle Condition"
                 ),
                q_comment_rating_num = q_comment_rating_num %>%
                  factor(levels = c("Driving and Safety",
                                    "Adherence to\nCOVID-19 Measures",
                                    "Service Quality and\nPassenger Experience",
                                    "Vehicle Condition"))) %>%
  dplyr::mutate(q_comment_chatgpt_summary = q_comment_chatgpt_summary %>%
                  str_replace_all("Generally positive, good service, responsive, comfortable, efficient", 
                                  "Generally positive, good service,\nresponsive, comfortable, efficient")) %>%
  dplyr::mutate(q_comment_chatgpt_summary = q_comment_chatgpt_summary %>%
                  str_replace_all("Clean, comfortable, safe, well-maintained, moderate speed", 
                                  "Clean, comfortable,\nsafe, well-maintained,\nmoderate speed")) %>%
  dplyr::mutate(q_comment_chatgpt_summary = q_comment_chatgpt_summary %>%
                  str_replace_all("COVID-19 safety measures need improvement", 
                                  "COVID-19 safety measures\nneed improvement")) %>%
  dplyr::mutate(q_comment_chatgpt_summary = q_comment_chatgpt_summary %>%
                  str_replace_all("Positive compliance with COVID-19 measures", 
                                  "Positive compliance with\nCOVID-19 measures")) %>%
  dplyr::mutate(q_comment_chatgpt_summary = q_comment_chatgpt_summary %>%
                  str_replace_all("Matatu operators ensure COVID-19 safety", 
                                  "Matatu operators ensure\nCOVID-19 safety")) %>%
  dplyr::mutate(q_comment_chatgpt_summary = q_comment_chatgpt_summary %>%
                  str_replace_all("Safe driving with COVID-19 precautions", 
                                  "Safe driving with\nCOVID-19 precautions")) %>%
  dplyr::mutate(q_comment_chatgpt_summary = q_comment_chatgpt_summary %>%
                  str_replace_all("Universal mask-wearing in crowds", 
                                  "Universal mask-wearing\nin crowds")) %>%
  dplyr::mutate(q_comment_chatgpt_summary = q_comment_chatgpt_summary %>%
                  str_replace_all("Clean car, careful and polite driver", 
                                  "Clean car, careful\nand polite driver")) %>%
  dplyr::mutate(q_comment_chatgpt_summary = q_comment_chatgpt_summary %>%
                  str_replace_all("a", 
                                  "a")) %>%
  dplyr::mutate(q_comment_chatgpt_summary = q_comment_chatgpt_summary %>%
                  str_replace_all("a", 
                                  "a")) %>%
  
  dplyr::mutate(prop = n_alltweets/n_valid_comments,
                per = round(prop*100, 2) %>% paste0("%"),
                n_text = paste0(n_alltweets, " (", per, ")"))

top5_per_category_df %>%
  ggplot(    aes(
    x = n_alltweets,
    y = reorder_within(q_comment_chatgpt_summary, n_alltweets, q_comment_rating_num),
    fill = q_comment_rating_sent,
    label = n_text,
  )) +
  geom_col(color = "black") +
  geom_text(nudge_x = 15, size = 3.5) +
  facet_wrap(~ q_comment_rating_num, scales = "free_y") +
  scale_fill_manual(values = c("green4", "red2", "dodgerblue")) +
  scale_y_reordered() +
  labs(x = "N Tweets",
       y = "Summary\nof Tweets\nin Cluster",
       fill = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(color = "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))

ggsave(file.path(figures_dir, "tweet_cluster_summaries.png"),
       width = 13,
       height = 7)

