# Tom Harris
# Text Analysis 1
# ...

# n.b. Before any new scripts, run psv_feedback_master.R

# Installing further required packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyr,
  dplyr,
  plyr,
  ggpmisc,
  wesanderson,
  plotly,
  ggplot2,
  stargazer,
  tidymodels,
  data.table,
  tidyverse,
  haven,
  lfe
)

#### Loading and joining data ####

# Loading feedback data
feedback_data <-
  readRDS(file.path(rider_feedback_dir, "FinalData", "rider_feedback_clean.Rds"))

# ...

# Relevant variables being:
# comments_label
# comments_label_english
# comment_coded (T/F if coded)
# comment_driver_sentiment_relev (0/1 if relevant — related to driver / safety)
# comment_driver_sentiment_code (sentiment code)



# load necessary packages
library(tidytext)
library(dplyr)
library(tm)

# load your data
feedback_data <- read.csv("your_file_path.csv")

# define safety related keywords
safety_keywords <- c("speed", "brake", "stop", "fast", "slow", "danger", "safe", "unsafe", "risk")

# select only the comments that contain safety related keywords
feedback_data <- feedback_data[grep(paste(safety_keywords, collapse = "|"), 
                                    feedback_data$comments_label_english, ignore.case = TRUE),]

# clean up text
feedback_data$comments_label_english <- tolower(feedback_data$comments_label_english)  # convert to lower case
feedback_data$comments_label_english <- gsub('[[:punct:]]', '', feedback_data$comments_label_english)  # remove punctuation
feedback_data$comments_label_english <- gsub('[[:cntrl:]]', '', feedback_data$comments_label_english)  # remove control characters
feedback_data$comments_label_english <- gsub('\\d+', '', feedback_data$comments_label_english)  # remove numbers

# create a corpus
corpus <- Corpus(VectorSource(feedback_data$comments_label_english))

# create a document term matrix
dtm <- DocumentTermMatrix(corpus)

# convert the document term matrix into a data frame
feedback_data_tidy <- tidy(dtm)

# calculate sentiment
nrc_sentiment <- get_sentiments("nrc")  # using the NRC sentiment lexicon
feedback_data_tidy <- feedback_data_tidy %>% 
  inner_join(nrc_sentiment) %>% 
  count(index, sentiment) %>% 
  spread(sentiment, n, fill = 0)

# the columns 'positive', 'negative' and 'neutral' now contain the sentiment scores for each comment





