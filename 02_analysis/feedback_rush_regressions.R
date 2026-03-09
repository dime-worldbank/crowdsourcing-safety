# Compare ratings: short vs long comments

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", 
                           paste0("passenger_feedback_valid_class_",
                                  "main", "_",
                                  "cmntfilter",
                                  FALSE,
                                  "_",
                                  "dstnctpass",
                                  TRUE,
                                  ".Rds")))

# Prep dataframe ---------------------------------------------------------------
table(fb_df$q_comment_nchar <= 2)
table(fb_df$q_comment_nchar >= 10)

fb_long_df <- fb_df %>%
  dplyr::filter(q_comment_nchar <= 3 | q_comment_nchar >= 10) %>%
  dplyr::mutate(q_comment_nchar_max2 = as.numeric(q_comment_nchar <= 3)) %>%
  dplyr::select(phone_hash, q_comment_nchar_max2, q_speed_rating_v1, q_speed_rating_v2, q_safety_rating) %>%
  pivot_longer(cols = -c(phone_hash, q_comment_nchar_max2)) %>%
  dplyr::filter(!is.na(value),
                !is.na(q_comment_nchar_max2)) %>%
  
  group_by(name, q_comment_nchar_max2, value) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  
  group_by(name, q_comment_nchar_max2) %>%
  dplyr::mutate(n_total = sum(n)) %>%
  ungroup() %>%
  
  dplyr::mutate(percent = n / n_total * 100,
                comment_length = case_when(
                  q_comment_nchar_max2 == 1 ~ "Comment: <= 3 characters",
                  q_comment_nchar_max2 == 0 ~ "Comment: >= 10 characters"
                ),
                label = paste0(n, " [", round(percent, 1), "%]"))
 
fb_long_df

# Figures ----------------------------------------------------------------------
p1 <- fb_long_df %>%
  dplyr::filter(name == "q_safety_rating") %>%
  dplyr::mutate(value = value %>% fct_rev()) %>%
  ggplot() +
  geom_col(aes(y = value,
               x = percent),
           fill = "darkorange",
           alpha = 0.4) +
  geom_text(aes(y = value,
               x = percent + 10,
               label = label),
            size = 3) +
  facet_wrap(~comment_length) +
  labs(x = "% of Comments",
       y = NULL,
       title = "A. How safely is your matatu being driven?") +
  scale_x_continuous(labels = scales::label_number(suffix = "%"),
                     limits = c(0, 110),
                     breaks = c(0, 25, 50, 75, 100)) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(size=10),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 11))
  
p2 <- fb_long_df %>%
  dplyr::filter(name == "q_speed_rating_v1") %>%
  dplyr::mutate(value = value %>% fct_rev()) %>%
  ggplot() +
  geom_col(aes(y = value,
               x = percent),
           fill = "darkorange",
           alpha = 0.4) +
  geom_text(aes(y = value,
                x = percent + 10,
                label = label),
            size = 3) +
  facet_wrap(~comment_length) +
  labs(x = "% of Comments",
       y = NULL,
       title = "B. How would you describe your matatu driver's speed?") +
  scale_x_continuous(labels = scales::label_number(suffix = "%"),
                     limits = c(0, 110),
                     breaks = c(0, 25, 50, 75, 100)) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(size=10),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 11))  

p3 <- fb_long_df %>%
  dplyr::filter(name == "q_speed_rating_v2") %>%
  dplyr::mutate(value = value %>% fct_rev()) %>%
  ggplot() +
  geom_col(aes(y = value,
               x = percent),
           fill = "darkorange",
           alpha = 0.4) +
  geom_text(aes(y = value,
                x = percent + 10,
                label = label),
            size = 3) +
  facet_wrap(~comment_length) +
  labs(x = "% of Comments",
       y = NULL,
       title = "C. How fast does the matatu seem to be going?") +
  scale_x_continuous(labels = scales::label_number(suffix = "%"),
                     limits = c(0, 110),
                     breaks = c(0, 25, 50, 75, 100)) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(size=10),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 11))  

p <- ggarrange(p1, p2, p3, ncol = 1)
ggsave(p, filename = file.path(figures_dir, "ratings_by_comment_length.png"),
       height = 6, width = 6.5)
