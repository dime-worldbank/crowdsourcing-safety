# Crowdsourcing Safety Paper

# Filepaths --------------------------------------------------------------------

# Rob (Personal Comptuer)
if(Sys.info()[["user"]] == "robmarty"){
  db_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"
  data_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback/Academic Paper/Data"
  git_dir  <- "~/Documents/Github/PSV-Rider-Feedback"
}

# Ruiwen (WB Computer)
if(Sys.info()[["user"]] == "wb575963"){
  db_dir <- "C:/Users/wb575963/Dropbox/PSV Rider Feedback"
  data_dir <- "C:/Users/wb575963/Dropbox/PSV Rider Feedback/Academic Paper/Data"
  git_dir <- "C:/Users/wb575963/Github/PSV-Rider-Feedback"
}

# Packages ---------------------------------------------------------------------
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

