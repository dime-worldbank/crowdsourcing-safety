# Feedback Comments

library(tm)
library(quanteda)

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

# fb_df <- fb_df %>%
#   dplyr::filter(regno != "UNKNOWN",
#                 ptn_cheating_fill %in% 0) %>%
#   dplyr::filter(!is.na(sentiment_snmtr))

fb_df <- fb_df %>%
  dplyr::filter(!is.na(sentiment_snmtr))

# Comments word cloud ----------------------------------------------------------
make_word_df <- function(fb_df){
  
  word_df <- fb_df$q_comment %>%
    tokens(remove_punct = T,
           remove_symbols = T,
           remove_numbers = T) %>%
    dfm() %>%
    tidy() %>%
    group_by(term) %>%
    dplyr::summarise(freq = n()) %>%
    ungroup() %>%
    arrange(-freq) %>%
    
    dplyr::filter(!(term %in% stopwords())) 
  
  word_df$sentiment <- word_df$term %>% get_sentences() %>% sentiment %>% pull(sentiment)
  word_df$term <- word_df$term %>% tools::toTitleCase()
  
  return(word_df)
}

add_example <- function(word_df, fb_df){
  
  out <- list()
  
  for(i in 1:nrow(word_df)){
    term_i <- word_df$term[i]
    out[[i]] <- tolower(fb_df$q_comment) %>% str_subset(tolower(term_i)) %>% head(1)
    
    fb_df <- fb_df[tolower(fb_df$q_comment) != out[[i]],]
  }
  
  out %>% unlist()
  
  # lapply(word_df$term, function(term_i){
  #   tolower(fb_df$q_comment) %>% str_subset(tolower(term_i)) %>% head(1)
  # }) %>%
  #   unlist()
  
}

#### Good
fb_good_df <- fb_df %>%
  dplyr::filter(sentiment_snmtr >= 0.3)

word_good_df <- fb_good_df %>%
  make_word_df() %>%
  dplyr::filter(sentiment >= 0.5) %>%
  head(15)

word_good_df$example <- add_example(word_good_df, fb_good_df)

#### Bad
fb_bad_df <- fb_df %>%
  dplyr::filter(sentiment_snmtr <= -0.3)

word_bad_df <- fb_bad_df %>%
  make_word_df() %>%
  dplyr::filter(sentiment <= -0.5) %>%
  head(15)

word_bad_df$example <- add_example(word_bad_df, fb_bad_df)

# Make table -------------------------------------------------------------------



# 
# 
# 
# fb_good_df <- fb_df %>%
#   dplyr::filter(sentiment_snmtr >= 0.1) %>%
#   make_word_df() %>%
#   dplyr::filter(sentiment >= 0.5)
# 
# fb_bad_df <- fb_df %>%
#   dplyr::filter(sentiment_snmtr <= -0.1) %>%
#   make_word_df() %>%
#   dplyr::filter(sentiment <= -0.5)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# set.seed(42)
# 
# word_freq <- fb_df %>%
#   dplyr::mutate(nchar_comment = nchar(q_comment)) %>%
#   dplyr::filter(nchar_comment >= 10) %>%
#   dplyr::pull(q_comment) %>%
#   tolower() %>%
#   tokens(remove_punct = T,
#          remove_symbols = T,
#          remove_numbers = T) %>%
#   tokens_ngrams(n = 1) %>%
#   unlist() %>%
#   table() %>%
#   as.data.frame() %>%
#   dplyr::rename(word = ".",
#                 freq = Freq) %>%
#   dplyr::mutate(word = as.character(word)) %>%
#   dplyr::filter(!(word %in% stopwords()),
#                 freq >= 5) %>%
#   dplyr::mutate(word_nchar = nchar(word)) %>%
#   dplyr::filter(word_nchar >= 3) %>%
#   arrange(-freq) %>%
#   dplyr::mutate(prop = freq / sum(freq)) %>%
#   dplyr::mutate(percent = round(prop,4)*100,
#                 percent = paste0(percent, "%")) %>%
#   dplyr::mutate(freq_per = paste0(freq, " (", percent, ")"))
# 
# word_freq$sentiment <- sentiment(word_freq$word)$sentiment
# word_freq$word <- word_freq$word %>% tools::toTitleCase()
# 
# word_freq %>%
#   dplyr::filter(sentiment >= 0.75) %>%
#   head(20)
# 
# word_freq %>%
#   dplyr::filter(sentiment <= -0.5) %>%
#   head(20)
# 
# word_freq %>%
#   dplyr::filter(sentiment >= 0.5) %>%
#   head(20) %>%
#   ggplot(aes(y = reorder(word, freq),
#              x = freq)) +
#   geom_col(fill = "forestgreen") +
#   geom_text(aes(label = freq_per,
#                 x = freq + 180)) +
#   labs(y = NULL,
#        x = "Number of Times Word Appears in Comments") +
#   scale_x_continuous(limits = c(0, 1600)) +
#   theme_classic2() +
#   theme(axis.text.y = element_text(color = "black", face = "bold"))
# 
# word_freq %>%
#   dplyr::filter(sentiment <= -0.5) %>%
#   head(20) %>%
#   ggplot(aes(y = reorder(word, freq),
#              x = freq)) +
#   geom_col(fill = "firebrick4") +
#   geom_text(aes(label = freq_per,
#                 x = freq + 4)) +
#   labs(y = NULL,
#        x = "Number of Times Word Appears in Comments") +
#   #scale_x_continuous(limits = c(0, 1600)) +
#   theme_classic2() +
#   theme(axis.text.y = element_text(color = "black", face = "bold"))
# 
# fb_df$q_comment %>% str_subset("tired")
# 
# word_freq$ %>% table()
# 
# 
# word_freq$color <- scales::rescale(word_freq$sentiment, to = c(-1, 1))
# word_freq$color <- scales::col_numeric(c("red", "gray50", "green"), domain = c(-1,1))(word_freq$color)
# 
# word_freq_sum <- word_freq[1:150,]
# wordcloud2(data = word_freq_sum,
#            size = 0.15,
#            shape = "circle",
#            color = word_freq_sum$color,
#            shuffle = F,
#            ellipticity = 0.2)
# 
# # Example comments -------------------------------------------------------------
# fb_sub_df <- fb_df %>%
#   dplyr::mutate(nchar_comment = nchar(q_comment)) %>%
#   dplyr::filter(nchar_comment >= 30) %>%
#   dplyr::mutate(q_comment = q_comment %>%
#                   str_replace_all("[:punct:]", " ") %>%
#                   str_squish() %>%
#                   tolower())
# 
# sent_df <- fb_sub_df$q_comment %>%
#   get_sentences %>%
#   sentiment()
# fb_sub_df$sentiment_score <- sent_df$sentiment
# 
# fb_sub_df %>%
#   pull(q_comment) %>%
#   str_subset("hardwork")
# 
# fb_sub_df$q_comment %>% str_subset("work") %>% unique()
# 
# 
# 
