# LOAD EACH SACCO, RANDOMIZE AND SAVE
#-----------------------------

rm(list=ls()) ## Clears console

#install.packages("xlsx")
library(tidyverse)
library(dplyr)
library(readxl)
library(xlsx)


# Setup path to dropbox
if(Sys.info()[["user"]] == "meyhar") 
  dropbox_file_path <- "~/Dropbox/PSV Rider Feedback"
  
github <- "~/Documents/Github/PSV-Rider-Feedback/"

# Setup path to data
sacco_listing <- file.path(dropbox_file_path, "Data", "Matatu Data", "Listing_202101", "sacco_listings_digitized")
output <- file.path(dropbox_file_path, "Data", "Matatu Data", "Listing_202101", "randomization", "output")

# Load and append all the sacco files
sacco_data <- sacco_listing %>%
  list.files(pattern = "*.xlsx", full.names= T) 

for (f in sacco_data) {
  file_f <- read_excel(f)
  set.seed(1344)
  file_f$uni <- sample(1:200, nrow(file_f), replace = F) 
  file_f <- file_f[order(file_f$uni),]
  f <- f %>% str_replace_all("/Users/meyhar/Dropbox/PSV Rider Feedback/Data/Matatu Data/Listing_202101/sacco_listings_digitized", "")
  f <- f %>% str_replace_all(".xlsx", "")
  f <- paste0(f, "_assignment.xlsx")
  write.xlsx(file_f, file.path(output, f))
}

 
