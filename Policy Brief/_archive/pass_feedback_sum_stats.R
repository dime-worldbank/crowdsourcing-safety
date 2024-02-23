# Figures

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(ap_data_dir, "FinalData", "vehicle_level.Rds"))
fb_df  <- readRDS(file.path(ap_data_dir, "FinalData", "passenger_feedback_clean.Rds"))

fb_clean_df <- fb_df %>%
  dplyr::filter(regno != "UNKNOWN",
                ptn_cheating_fill %in% 0)

# Remove data ------------------------------------------------------------------
fb_df %>%
  dplyr::filter(response_method %in% "shortcode",
                !is.na(regno)) %>%
  nrow()

fb_df %>%
  dplyr::filter(response_method %in% "shortcode",
                regno == "UNKNOWN") %>%
  nrow()

fb_df %>%
  dplyr::filter(response_method %in% "shortcode",
                regno != "UNKNOWN") %>%
  nrow()

fb_df %>%
  dplyr::filter(regno != "UNKNOWN") %>%
  nrow()

fb_df %>%
  dplyr::filter(regno != "UNKNOWN",
                ptn_cheating_fill %in% 0) %>%
  nrow()

# N Stickers -------------------------------------------------------------------
veh_df %>%
  dplyr::filter(!is.na(n_feedback)) %>%
  nrow()

veh_df$award_amount[veh_df$n_feedback >= 0]

summary(veh_df$n_feedback)

veh_df$n_feedback %>% sort()
table(veh_df$n_feedback >= 10)
table(veh_df$n_feedback >= 100)
table(veh_df$n_feedback >= 500)

# Overall stats ----------------------------------------------------------------
sum_var <- function(var, fb_df){
  fb_df$var <- fb_df[[var]]
  fb_df %>%
    dplyr::filter(!is.na(var)) %>%
    group_by(var) %>%
    dplyr::summarise(n = n()) %>%
    ungroup() %>%
    dplyr::mutate(prop = n / sum(n))
}

sum_var("q_safety_rating",   fb_clean_df)
sum_var("q_speed_rating_v2", fb_clean_df)
sum_var("q_occupancy",       fb_clean_df)
sum_var("q_covid_measures",  fb_clean_df)

fb_clean_df$response_method %>% table()
fb_df$response_method %>% table()

# Safe Distribution ------------------------------------------------------------
veh_df %>%
  dplyr::filter(n_feedback >= 10,
                !is.na(q_safety_rating_num)) %>%
  dplyr::mutate(q_safety_rating_num = round(q_safety_rating_num * 4) / 4) %>%
  group_by(q_safety_rating_num) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x = q_safety_rating_num,
               y = n),
           width = 0.2,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = q_safety_rating_num,
                y = n + 1,
                label = n)) +
  scale_x_continuous(limits = c(1, 4.1),
                     breaks = seq(1, 4, 0.5)) +
  labs(x = "Vehicle Average",
       y = "N Vehicles",
       title = "Vehicle averages of: 'How safely is your matatu being driven?'",
       subtitle = "1 = 'Very Unsafe'; 4 = 'Very Safe'",
       caption = "Only showing vehicles with 10 or more survey responses.") +
  theme_classic2() +
  theme(plot.title = element_text(size = 9),
        plot.subtitle = element_text(size = 8, face = "bold"),
        plot.caption = element_text(size = 6),
        axis.title = element_text(size = 8)) 

ggsave(filename = file.path(brief_figures_dir, "safety_distribution.png"),
       height = 2.25, width = 4)

# Proportion rate as unsafe ----------------------------------------------------
veh_df %>%
  dplyr::filter(n_feedback >= 10,
                !is.na(q_safety_rating_num)) %>%
  
# fb_clean_df %>%
#   dplyr::mutate(unsafe = (q_safety_rating == "not safe") | 
#                   (q_safety_rating == "not very safe")) %>%
#   dplyr::filter(!is.na(q_safety_rating_num)) %>%
  # group_by(regno) %>%
  # dplyr::summarise(n_feedback = n(),
  #                  sum_unsafe = sum(unsafe),
  #                  prop_unsafe = mean(unsafe)) %>%
  # dplyr::filter(n_feedback >= 10) %>%
  dplyr::mutate(q_safety_prop_unsafe = round(q_safety_prop_unsafe * 50) / 50) %>%
  
  group_by(q_safety_prop_unsafe) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  
  ggplot() +
  geom_col(aes(x = q_safety_prop_unsafe,
               y = n),
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = q_safety_prop_unsafe,
                y = n + 1,
                label = n)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Percent",
       y = "N Vehicles",
       title = "Percent of respondents that rate driving as 'Unsafe' or\n'Very unsafe'",
       caption = "Only showing vehicles with 10 or more survey responses.") +
  theme_classic2() +
  theme(plot.title = element_text(size = 9),
        plot.subtitle = element_text(size = 8, face = "bold"),
        plot.caption = element_text(size = 6),
        axis.title = element_text(size = 8)) 

ggsave(filename = file.path(brief_figures_dir, "prop_unsafe_distribution.png"),
       height = 2.25, width = 4)

# Comments word cloud ----------------------------------------------------------
set.seed(42)

word_freq <- fb_df %>%
  dplyr::mutate(nchar_comment = nchar(q_comment)) %>%
  dplyr::filter(nchar_comment >= 10) %>%
  dplyr::pull(q_comment) %>%
  tolower() %>%
  tokens(remove_punct = T,
         remove_symbols = T,
         remove_numbers = T) %>%
  tokens_ngrams(n = 1) %>%
  unlist() %>%
  table() %>%
  as.data.frame() %>%
  dplyr::rename(word = ".",
                freq = Freq) %>%
  dplyr::mutate(word = as.character(word)) %>%
  dplyr::filter(!(word %in% stopwords()),
                freq >= 5) %>%
  dplyr::mutate(word_nchar = nchar(word)) %>%
  dplyr::filter(word_nchar >= 3) %>%
  dplyr::mutate(freq = log(freq,10)) %>%
  arrange(-freq) 

word_freq$sentiment <- sentiment(word_freq$word)$sentiment

word_freq$color <- scales::rescale(word_freq$sentiment, to = c(-1, 1))
word_freq$color <- scales::col_numeric(c("red", "gray50", "green"), domain = c(-1,1))(word_freq$color)

word_freq_sum <- word_freq[1:150,]
wordcloud2(data = word_freq_sum,
           size = 0.15,
           shape = "circle",
           color = word_freq_sum$color,
           shuffle = F,
           ellipticity = 0.2)

# Example comments -------------------------------------------------------------
fb_sub_df <- fb_df %>%
  dplyr::mutate(nchar_comment = nchar(q_comment)) %>%
  dplyr::filter(nchar_comment >= 30) %>%
  dplyr::mutate(q_comment = q_comment %>%
                  str_replace_all("[:punct:]", " ") %>%
                  str_squish() %>%
                  tolower())

sent_df <- fb_sub_df$q_comment %>%
  get_sentences %>%
  sentiment()
fb_sub_df$sentiment_score <- sent_df$sentiment

fb_sub_df %>%
  pull(q_comment) %>%
  str_subset("hardwork")

fb_sub_df$q_comment %>% str_subset("work") %>% unique()

