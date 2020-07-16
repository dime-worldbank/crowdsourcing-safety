# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("rlang")
# install.packages("remotes")
# remotes::install_github("r-lib/rlang")

library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(stringr)

# PSV Rider Feedback

User <- System["USER"]
if (User == "meyhar") {
  dropbox_file_path <- "~/Dropbox/PSV Rider Feedback"
  echo_figures <- "~/Dropbox/PSV Rider Feedback/Data/Rider Feedback/Echo Mobile Data/Outputs/figures"
}

# RM/FC file path
if (User == "") {
  folder <- ""
}

data <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "RawData", 
                          "echo_data.Rds"))

data <- data %>%
  arrange(start_date)


data$complete_date %>% substring(1,10) %>% table()

data$start_date %>% hour() %>% hist()

data$phone_hash %>% table %>% table()

data$DRIVER.RATING._L %>% table()
data$SPEED.RATING._L %>% table()

a <- data[data$DRIVER.RATING._L %in% "Very Unsafe",]
data$DRIVER.RATING._L %>% table()


data$MATATU.AMENITIES._R



data$start_date %>% as.Date() %>% table()

most_common_word=function(x){
  
  #Split every word into single words for counting
  splitTest=strsplit(x," ")
  
  #Counting words
  count=table(splitTest)
  
  #Sorting to select only the highest value, which is the first one
  count=count[order(count, decreasing=TRUE)][1]
  
  #Return the desired character. 
  #By changing this you can choose whether it show the number of times a word repeats
  return(names(count))
}

  # Get the string length for feedback question
data$length <- str_length(data$FEEDBACK_L)
  summarize(data$FEEDBACK_L)
  
  data_long <- subset(data, data$length > 15)
  