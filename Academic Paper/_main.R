# Crowdsourcing Safety Paper

# Filepaths --------------------------------------------------------------------

# Rob (Personal Comptuer)
if(Sys.info()[["user"]] == "robmarty"){
  db_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"
  data_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback/Academic Paper/Data"
  git_dir  <- "~/Documents/Github/PSV-Rider-Feedback"
  overleaf_dir <- "~/Dropbox/Apps/Overleaf/Crowdsourcing Safety Kenya Matatu Passenger Safety Project"
}

# Ruiwen (WB Computer)
if(Sys.info()[["user"]] == "wb575963"){
  db_dir <- "C:/Users/wb575963/Dropbox/PSV Rider Feedback"
  data_dir <- "C:/Users/wb575963/Dropbox/PSV Rider Feedback/Academic Paper/Data"
  git_dir <- "C:/Users/wb575963/Github/PSV-Rider-Feedback"
}

tables_dir  <- file.path(overleaf_dir, "tables")
figures_dir <- file.path(overleaf_dir, "figures")

# Packages ---------------------------------------------------------------------
library(magrittr)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(sf)
library(readxl)
library(janitor)
library(arrow)
library(quanteda)
library(wordcloud2)
library(tidytext)
library(sentimentr)
library(ggpubr)
#library(cleanNLP)
#library(coreNLP)
