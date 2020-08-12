# PSV Rider Feedback

# Filepaths --------------------------------------------------------------------
#### Main
if(Sys.info()[["user"]] == "robmarty") dropbox_file_path <- "~/Dropbox/World Bank/IEs/PSV Rider Feedback"
if(Sys.info()[["user"]] == "WB521633") dropbox_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/PSV Rider Feedback"

if(Sys.info()[["user"]] == "WB521633") onedrive_file_path <- "C:/Users/wb521633/OneDrive - WBG/PSV Rider Feedback"

#### From Main
echo_figures <- file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "Outputs", "figures")

# Packages ---------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(lubridate)
library(haven)
library(hrbrthemes)
library(readxl)

