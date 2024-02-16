# Feedback Comments

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_class.Rds"))

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
    
  }
  
  out %>% unlist()
  
}

add_figure_label <- function(word_df){
  word_df %>%
    mutate(percent = round(prop * 100,2),
           percent = paste0(percent, "%"),
           label = paste0(freq, " (", percent, ")")) 
}

#### Good
fb_good_df <- fb_df %>%
  dplyr::filter(sentiment_snmtr > 0)

word_good_df <- fb_good_df %>%
  make_word_df() %>%
  dplyr::filter(sentiment > 0) %>%
  head(15)

word_good_df$example <- add_example(word_good_df, fb_good_df)
word_good_df$prop <- word_good_df$freq / nrow(fb_good_df)

#### Bad
fb_bad_df <- fb_df %>%
  dplyr::filter(sentiment_snmtr < 0)

word_bad_df <- fb_bad_df %>%
  make_word_df() %>%
  dplyr::filter(sentiment < 0) %>%
  head(15)

word_bad_df$example <- add_example(word_bad_df, fb_bad_df)
word_bad_df$prop <- word_bad_df$freq / nrow(fb_bad_df)

# Make figures -----------------------------------------------------------------
p_good <- word_good_df %>%
  add_figure_label() %>%
  ggplot(aes(y = reorder(term, freq),
             x = freq)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = label,
                x = freq + 65),
            size = 3) +
  labs(y = NULL,
       x = "Number (Percent) of Positive Comments Word Appears In",
       title = "A. Top Positive Words from Positive Comments") +
  scale_x_continuous(limits = c(0, 570)) +
  theme_classic2() +
  theme(axis.text.y = element_text(color = "black", face = "bold", size = 9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 10))

p_bad <- word_bad_df %>%
  add_figure_label() %>%
  ggplot(aes(y = reorder(term, freq),
             x = freq)) +
  geom_col(fill = "firebrick4") +
  geom_text(aes(label = label,
                x = freq + 5.5),
            size = 3) +
  labs(y = NULL,
       x = "Number (Percent) of Negative Comments Word Appears In",
       title = "B. Top Negative Words from Negative Comments") +
  scale_x_continuous(limits = c(0, 53)) +
  theme_classic2() +
  theme(axis.text.y = element_text(color = "black", face = "bold")) +
  theme(axis.text.y = element_text(color = "black", face = "bold", size = 9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 10))

p <- ggarrange(p_good, p_bad, nrow = 1)

ggsave(p, filename = file.path(figures_dir, "top_words.png"),
       height = 2.75, width = 9)

# Make table -------------------------------------------------------------------
word_df <- word_bad_df
make_tex <- function(word_df){
  word_df %>%
    dplyr::mutate(percent = round(prop*100,2),
                  percent = paste0(percent, "\\%")) %>%
    dplyr::mutate(tex = paste(term, freq, percent, example, sep = " & ") %>% paste0(" \\\\ \n"))
}

word_good_df <- make_tex(word_good_df)
word_bad_df  <- make_tex(word_bad_df)

sink(file.path(tables_dir, "good_word_example.tex"))
cat("\\begin{tabular}{c|c|c|p{10cm}} ")
cat("\\hline \n")
cat("Word & N & Percent & Example Comment \\\\ \n")
cat("\\hline \n")
word_good_df$tex %>%
  paste(collapse = " ") %>%
  cat()
cat("\\hline \n")
cat("\\end{tabular}")
sink()

sink(file.path(tables_dir, "bad_word_example.tex"))
cat("\\begin{tabular}{c|c|c|p{10cm}} ")
cat("\\hline \n")
cat("Word & N & Percent & Example Comment \\\\ \n")
cat("\\hline \n")
word_bad_df$tex %>%
  paste(collapse = " ") %>%
  cat()
cat("\\hline \n")
cat("\\end{tabular}")
sink()

# Grab example comments  -------------------------------------------------------
fb_good_df %>%
  dplyr::filter(nchar(q_comment) >= 20) %>%
  pull(q_comment) %>%
  tolower() %>%
  str_subset("speed") %>%
  unique() %>%
  head(50)

fb_bad_df %>%
  dplyr::filter(nchar(q_comment) >= 20) %>%
  pull(q_comment) %>%
  tolower() %>%
  str_subset("speed") %>%
  unique() %>%
  head(40)
