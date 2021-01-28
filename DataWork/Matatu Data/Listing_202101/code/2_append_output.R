
#APPEND ALL RANDOMIZED SACCOS IN ONE FILE
#-----------------------------------

rm(list=ls()) ## Clears console

library(tidyverse)
library(dplyr)
library(readxl)

library(xlsx)


# Setup path to dropbox
if(Sys.info()[["user"]] == "meyhar") 
  
  dropbox_file_path <- "~/Dropbox/PSV Rider Feedback"
  
github <- "~/Documents/Github/PSV-Rider-Feedback/"


# Setup path to data
sacco_assignment <- file.path(dropbox_file_path, "Data", "Matatu Data",
                              "Listing_202101", "randomization", "output")

output <- file.path(dropbox_file_path, "Data", "Matatu Data", "Listing_202101", "randomization")


# Load and append all the sacco files
sacco_data <- sacco_assignment %>%
  list.files(pattern = "*.xlsx", full.names= T) %>%
  lapply(read_excel) %>%
  bind_rows()

sacco_data = select(sacco_data, -'...8',-'...1')

write.xlsx(sacco_data, file.path(output, "sacco_randomization_appended.xlsx"))

