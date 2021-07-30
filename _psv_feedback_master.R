# PSV Rider Feedback

# Filepaths --------------------------------------------------------------------
#### Main
if(Sys.info()[["user"]] == "robmarty") dropbox_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"
if(Sys.info()[["user"]] == "WB521633") dropbox_dir <- "C:/Users/wb521633/Dropbox/World Bank/IEs/PSV Rider Feedback"
if(Sys.info()[["user"]] == "meyhar")   dropbox_dir <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"

if(Sys.info()[["user"]] == "WB521633") onedrive_dir <- "C:/Users/wb521633/OneDrive - WBG/PSV Rider Feedback"

if(Sys.info()[["user"]] == "robmarty") github_dir <- "~/Documents/Github/PSV-Rider-Feedback/"
if(Sys.info()[["user"]] == "meyhar") github_dir <- "~/Documents/Github/PSV-Rider-Feedback/"

#### From Main
sensors_dir <- file.path(dropbox_dir, "Data", "Sensor Data")

#echo_figures <- file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "Outputs", "figures")

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
library(httr)
library(jsonlite)

# Functions --------------------------------------------------------------------
source(file.path(github_dir, "DataWork", "Sensor Data", "_wialon_api_functions.R"))
