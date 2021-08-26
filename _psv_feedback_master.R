# PSV Rider Feedback

# Filepaths --------------------------------------------------------------------
#### Root
if(Sys.info()[["user"]] == "robmarty") dropbox_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"
if(Sys.info()[["user"]] == "WB521633") dropbox_dir <- "C:/Users/wb521633/Dropbox/World Bank/IEs/PSV Rider Feedback"
if(Sys.info()[["user"]] == "meyhar")   dropbox_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"

if(Sys.info()[["user"]] == "WB521633") onedrive_dir <- "C:/Users/wb521633/OneDrive - WBG/PSV Rider Feedback"

if(Sys.info()[["user"]] == "robmarty") github_dir <- "~/Documents/Github/PSV-Rider-Feedback/"
if(Sys.info()[["user"]] == "meyhar")   github_dir <- "~/Documents/Github/PSV-Rider-Feedback/"

#### From Root
sensors_dir <- file.path(dropbox_dir, "Data", "Sensor Data")

# Packages ---------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(lubridate)
library(gridExtra)
library(hrbrthemes)
library(readxl)
library(labelled)
library(haven)
library(stringr)
library(rmarkdown)
library(wesanderson)
library(knitr)
library(data.table)
library(DT)
library(syn)
library(kableExtra)
library(openssl)
library(ggmap)
library(lubridate)
library(httr)
library(jsonlite)
library(arrow)

# Functions --------------------------------------------------------------------
source(file.path(github_dir, "DataWork", "Sensor Data", "_wialon_api_functions.R"))
