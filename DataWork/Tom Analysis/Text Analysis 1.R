# Tom Harris
# Text Analysis 1
# ...

# n.b. Before any new scripts, run psv_feedback_master.R

# Installing further required packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyr, dplyr, plyr, ggpmisc, wesanderson, plotly, ggplot2, stargazer, tidymodels,
  data.table, tidyverse, haven, lfe, tidytext, tm, wordcloud, textdata, syuzhet
)

#### Loading and joining data ####

# Loading feedback data
feedback_data <-
  readRDS(file.path(rider_feedback_dir, "FinalData", "rider_feedback_clean.Rds"))

# Relevant variables being:
# comments_label
# comments_label_english
# comment_coded (T/F if coded)
# comment_driver_sentiment_relev (0/1 if relevant — related to driver / safety)
# comment_driver_sentiment_code (sentiment code)


#### World Cloud ####

# clean up text
feedback_data$comments_label_english <- tolower(feedback_data$comments_label_english) # convert to lower case
feedback_data$comments_label_english <- gsub("[[:punct:]]", "", feedback_data$comments_label_english) # remove punctuation
feedback_data$comments_label_english <- gsub("[[:cntrl:]]", "", feedback_data$comments_label_english) # remove control characters
feedback_data$comments_label_english <- gsub("\\d+", "", feedback_data$comments_label_english) # remove numbers

# create a corpus
corpus <- Corpus(VectorSource(feedback_data$comments_label_english))

# remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# create a document term matrix
dtm <- DocumentTermMatrix(corpus)

# calculate term frequency
term_frequency <- colSums(as.matrix(dtm))

# create a data frame
df <- data.frame(term = names(term_frequency), freq = term_frequency)

# create a word cloud
wordcloud(
  words = df$term, freq = df$freq, min.freq = 1,
  max.words = 200, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)


#### Sentiment Analysis ####

# # define safety related keywords
# safety_keywords <- c("speed", "brake", "stop", "fast", "slow", "danger", "safe", "unsafe", "risk")
#
# # select only the comments that contain safety related keywords
# feedback_data <- feedback_data[grep(paste(safety_keywords, collapse = "|"),
#   feedback_data$comments_label_english,
#   ignore.case = TRUE
# ), ]

# remove rows with NA in comments_label_english
feedback_data <- feedback_data[complete.cases(feedback_data$comments_label_english), ]

# clean up text
feedback_data$comments_label_english <- tolower(feedback_data$comments_label_english) # convert to lower case
feedback_data$comments_label_english <- gsub("[[:punct:]]", "", feedback_data$comments_label_english) # remove punctuation
feedback_data$comments_label_english <- gsub("[[:cntrl:]]", "", feedback_data$comments_label_english) # remove control characters
feedback_data$comments_label_english <- gsub("\\d+", "", feedback_data$comments_label_english) # remove numbers

# get sentiment
feedback_data$sentiment <- get_sentiment(feedback_data$comments_label_english)

# you may want to classify the sentiment into positive, negative and neutral
feedback_data$sentiment_class <- ifelse(feedback_data$sentiment > 0, "positive",
  ifelse(feedback_data$sentiment < 0, "negative", "neutral")
)


#### adfadfadf ####

# count the number of comments in each sentiment category
sentiment_counts <- as.data.frame(table(feedback_data$sentiment_class))

# rename the columns
colnames(sentiment_counts) <- c("sentiment_class", "n")

# display the counts
print(sentiment_counts)

# create a bar chart of sentiment categories
ggplot(sentiment_counts, aes(x = sentiment_class, y = n)) +
  geom_bar(stat = "identity", fill = "navy", alpha = 0.8) +
  labs(x = "Sentiment", y = "Number of comments") +
  theme_minimal()



#### Assessing correlation with safety label ####

# recode safety_label_en into numerical values
feedback_data$safety_label_coded <- recode(feedback_data$safety_label_en,
  "Not Very Safe" = 1,
  "Not Safe" = 2,
  "Safe" = 3,
  "Very Safe" = 4
)


# convert safety_label_en into a numeric variable
feedback_data$safety_label_coded <- as.numeric(as.character(feedback_data$safety_label_coded))

# calculate Spearman's rank correlation
correlation <- cor(feedback_data$sentiment, feedback_data$safety_label_coded, method = "spearman")

# print the correlation
print(correlation)



## Plot

# Define custom color breakpoints
custom_breaks <- c(-2, -1, -0.75, -0.5, 0, 0.5, 0.75, 1, 2)

plot <- ggplot(feedback_data, aes(
  x = safety_label_coded, y = sentiment,
  text = comments_label_english,
  color = sentiment
)) +
  geom_point(
    alpha = 0.7, aes(color = sentiment),
    position = position_jitter(width = 0.2, height = 0.2)
  ) +
  scale_color_gradientn(
    colors = brewer.pal(11, "Spectral"),
    values = scales::rescale(custom_breaks),
    breaks = custom_breaks,
    name = "Sentiment Score"
  ) +
  geom_smooth(method = "lm", se = TRUE, color = "maroon", aes(group = 1)) +
  labs(x = "Safety Label", y = "Sentiment Score") +
  theme_minimal() +
  guides(color = guide_colorbar(barwidth = 1, barheight = 10, title.position = "top"))


plot
ggplotly(plot)
