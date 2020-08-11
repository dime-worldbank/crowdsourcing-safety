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
  
  # Number of correct matatu numbers, correct license plate numbers
  
  data$MATATU.NUMBER._L <- str_replace_all(data$MATATU.NUMBER._L, fixed(" "), "")
  data$matatu_num <- data$MATATU.NUMBER._L
  bus_numbers <- c("kbm119t",   "kbw613j", "kcm436k", "kbu592k",  "kbl783e",  
                   "kcr574k",  "kbk582j", "kbs089s",   "kbb512l", "kch349e", "kaz619z", "kbr133v", "kbb681k",
                   "kbl187z",
                   "kbk715u")
  
  
  data$matatu_num_correct <- data$matatu_num %in% paste0(20:31) | data$matatu_num %in% bus_numbers
  numbers_only <- function(x) !grepl("\\D", x)
  data$number_check <- numbers_only(data$matatu_num)
  data_sub <- subset(data, select = c("number_check", "matatu_num"))
table(data$matatu_num_correct, data$number_check)

# check
data_check <- subset(data, select = c("matatu_num_correct", "matatu_num", "number_check"))
