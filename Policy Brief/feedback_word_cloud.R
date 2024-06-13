# Figures

# Load data --------------------------------------------------------------------
fb_df  <- readRDS(file.path(ap_data_dir, "FinalData", "passenger_feedback_valid_class_main_cmntfilterFALSE_dstnctpassTRUE.Rds"))

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
word_freq$color <- scales::col_numeric(c("red", "gray50", "chartreuse3"), domain = c(-1,1))(word_freq$color)

word_freq_clean <- bind_rows(
  word_freq %>%
    filter(sentiment == 0) %>%
    head(70),
  
  word_freq %>%
    filter(sentiment != 0)
) %>%
  arrange(-freq)

#word_freq_sum <- word_freq[1:150,]

word_freq_clean$color[word_freq_clean$word == "government"] <- "#7F7F7F"

wordcloud2(data = word_freq_clean,
           size = 0.15,
           shape = "circle",
           color = word_freq_clean$color,
           shuffle = F,
           ellipticity = 0.2)






