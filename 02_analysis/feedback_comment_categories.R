# Summarize Categories

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

# Figure -----------------------------------------------------------------------
fb_long_df <- fb_df %>%
  dplyr::select(uid, 
                chatgpt_4o_cat_1p,
                chatgpt_4o_cat_1n,
                chatgpt_4o_cat_1r,
                
                chatgpt_4o_cat_2p,
                chatgpt_4o_cat_2n,
                chatgpt_4o_cat_2r,
                
                chatgpt_4o_cat_3p,
                chatgpt_4o_cat_3n,
                chatgpt_4o_cat_3r,
                
                chatgpt_4o_cat_4p,
                chatgpt_4o_cat_4n,
                chatgpt_4o_cat_4r,
                
                chatgpt_4o_cat_5) %>%
  pivot_longer(cols = -uid) %>%
  group_by(name) %>%
  dplyr::summarise(n = sum(value, na.rm = T)) %>%
  ungroup() %>%
  dplyr::mutate(name = name %>%
                  str_replace_all("chatgpt_4o_cat_", ""),
                category = name %>%
                  str_replace_all("n|p|r", ""),
                type = name %>%
                  str_replace_all("1|2|3|4|5", "")) %>%
  dplyr::mutate(type = case_when(
    type == "n" ~ "Negative",
    type == "p" ~ "Positive",
    type == "r" ~ "Recommendation"
  ) %>%
    factor(levels = c("Positive",
                      "Negative",
                      "Recommendation")) %>%
    fct_rev()) %>%
  dplyr::mutate(category = case_when(
    category == 1 ~ "Driving and\nSafety",
    category == 2 ~ "Adherence to\nCOVID-19 Measures",
    category == 3 ~ "Service Quality and\nPassenger Experience",
    category == 4 ~ "Vehicle Condition",
    category == 5 ~ "Other"
  ) %>%
    factor(levels = c("Driving and\nSafety",
                      "Adherence to\nCOVID-19 Measures",
                      "Service Quality and\nPassenger Experience",
                      "Vehicle Condition",
                      "Other")) %>%
    fct_rev())

n_valid_comments <- fb_df %>%
  dplyr::filter(!is.na(chatgpt_4o_cat)) %>%
  nrow()

fb_long_df$prop_commments <- fb_long_df$n / n_valid_comments

fb_long_df <- fb_long_df %>%
  dplyr::mutate(per_commments_str = round(prop_commments * 100, 2) %>% paste0("%"),
                label = paste0(n, " (", per_commments_str, ")")) 

fb_long_df %>%
  #dplyr::filter(category != "Other") %>%
  ggplot(aes(y = category,
             x = prop_commments,
             fill = type)) +
  geom_col(position = position_dodge(width = 0.9),
           color = "black") +
  geom_text(aes(label = label),
            hjust = -0.05,
            position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("dodgerblue",
                               "red",
                               "green3"),
                    na.translate = F,
                    guide = guide_legend(reverse = TRUE)) +
  labs(fill = NULL,
       x = "Percent of Comments",
       y = NULL) +
  scale_x_continuous(labels = scales::percent,
                     limits = c(0, 0.6)) +
  theme_classic2() +
  theme(axis.text = element_text(color = "black")) 

ggsave(filename = file.path(figures_dir,
                            "categroy_comments_results.png"),
       height = 4, width = 8)

# Example comments --------------------------------------------------------------

fb_df %>%
  pull(q_comment) %>%
  str_subset("phone")

#### 1 - Safety
fb_df %>%
  dplyr::filter(chatgpt_4o_cat_1p %in% T) %>%
  pull(q_comment) %>%
  unique()
  
fb_df %>%
  dplyr::filter(chatgpt_4o_cat_1n %in% T) %>%
  pull(q_comment) %>%
  unique()

fb_df %>%
  dplyr::filter(chatgpt_4o_cat_1r %in% T) %>%
  pull(q_comment) %>%
  unique()


#### 2 - COVID
fb_df %>%
  dplyr::filter(chatgpt_4o_cat_2p %in% T) %>%
  pull(q_comment) %>%
  unique()

fb_df %>%
  dplyr::filter(chatgpt_4o_cat_2n %in% T) %>%
  pull(q_comment) %>%
  unique()

fb_df %>%
  dplyr::filter(chatgpt_4o_cat_2r %in% T) %>%
  pull(q_comment) %>%
  unique()

#### 3 - Service Quality and Passenger Experience
fb_df %>%
  dplyr::filter(chatgpt_4o_cat_3p %in% T) %>%
  pull(q_comment) %>%
  unique()

fb_df %>%
  dplyr::filter(chatgpt_4o_cat_3n %in% T) %>%
  pull(q_comment) %>%
  unique()

fb_df %>%
  dplyr::filter(chatgpt_4o_cat_3r %in% T) %>%
  pull(q_comment) %>%
  unique()

#### 4 - Vehicle Conditions
fb_df %>%
  dplyr::filter(chatgpt_4o_cat_4p %in% T) %>%
  pull(q_comment) %>%
  unique()

fb_df %>%
  dplyr::filter(chatgpt_4o_cat_4n %in% T) %>%
  pull(q_comment) %>%
  unique()

fb_df %>%
  dplyr::filter(chatgpt_4o_cat_4r %in% T) %>%
  pull(q_comment) %>%
  unique()

# Other comments ---------------------------------------------------------------
fb_df <- fb_df %>%
  dplyr::mutate(q_comment_nchar_car = case_when(
    q_comment_nchar <= 5 ~ "<5",
    q_comment_nchar <= 10 ~ "6-10",
    q_comment_nchar <= 20 ~ "11-20",
    q_comment_nchar <= 30 ~ "21-30",
    q_comment_nchar <= 40 ~ "31-40",
    q_comment_nchar <= 50 ~ "41-50",
    q_comment_nchar <= 100 ~ "51-100",
    q_comment_nchar > 100 ~ ">100"
  ) %>%
    factor(levels = c("<5",
                      "6-10",
                      "11-20",
                      "21-30",
                      "31-40",
                      "41-50",
                      "51-100",
                      ">100"))) 

nchar_df <- fb_df %>%
  dplyr::filter(!is.na(chatgpt_4o_cat_5)) %>%
  mutate(type = case_when(
    chatgpt_4o_cat_5 %in% T ~ "Topic: Other",
    TRUE ~ "Topic: 1-4"
  )) %>%
  group_by(q_comment_nchar_car, type) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  
  group_by(type) %>%
  dplyr::mutate(total = sum(n)) %>%
  ungroup() %>%
  
  dplyr::mutate(prop = n / total,
                per = round(prop*100,2) %>% paste0("%") ) #%>%
  #pivot_wider(id_cols = q_comment_nchar_car,
  #            names_from = type,
  #            values_from = prop)

nchar_df %>%
  ggplot(aes(y = q_comment_nchar_car,
             x = prop)) +
  geom_col(fill = "dodgerblue3") +
  geom_text(aes(label = per),
            hjust = -0.2) +
  labs(fill = NULL,
       x = NULL,
       y = "Number of\ncharacters\nin comments") +
  scale_x_continuous(labels = scales::percent,
                     limits = c(0, 0.6)) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  facet_wrap(~type)

ggsave(filename = file.path(figures_dir,
                            "categroy_comments_nchar.png"),
       height = 3, width = 8)
