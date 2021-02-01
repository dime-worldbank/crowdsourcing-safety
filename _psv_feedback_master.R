# PSV Rider Feedback

# Filepaths --------------------------------------------------------------------
#### Main
if(Sys.info()[["user"]] == "robmarty") dropbox_file_path <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"
if(Sys.info()[["user"]] == "WB521633") dropbox_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/PSV Rider Feedback"
if(Sys.info()[["user"]] == "meyhar")   dropbox_file_path <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"

if(Sys.info()[["user"]] == "WB521633") onedrive_file_path <- "C:/Users/wb521633/OneDrive - WBG/PSV Rider Feedback"

if(Sys.info()[["user"]] == "meyhar") robmarty <- "~/Documents/Github/PSV-Rider-Feedback/"
if(Sys.info()[["user"]] == "meyhar") github_file_path <- "~/Documents/Github/PSV-Rider-Feedback/"

#### From Main
sensors_pilot_dir <- file.path(dropbox_file_path, "Data", "Sensors", "pilot")

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
