# Figures

library(wordcloud2)
library(wordcloud)

# Load data --------------------------------------------------------------------
fb_df  <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_class_main_cmntfilterFALSE_dstnctpassTRUE.Rds"))

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

set.seed(42)
wordcloud(words = word_freq_clean$word, 
          freq = word_freq_clean$freq,
          scale = c(1.5, 0.1),
          min.freq = 0,
          max.words=200, 
          random.order=FALSE, rot.per=0.35, 
          ordered.colors = T,
          colors=word_freq_clean$color)

# 
# wordcloud(word_freq_clean$word,
#           word_freq_clean$freq)
# # word_freq_clean_lim <- word_freq_clean
# # 
# # ggplot(data = word_freq_clean_lim) +
# #   geom_text_wordcloud_area(aes(
# #     label = word, 
# #     size = freq
# #   ),
# #   color = word_freq_clean_lim$color) +
# #   scale_size_area(max_size = 10) +
# #   theme_minimal()
# 
# 
