# PSV Rider Feedback: Clean Data

# Load Data --------------------------------------------------------------------
## Shortcode Data
sc_data <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "RawData", 
                             "echo_data.Rds"))

## QR Data
qr_data <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "PSV Safety Systems Data", "RawData", 
                             "qr_data.Rds"))

## Sticker Data
sticker_df <- read_xlsx(file.path(dropbox_file_path, "Data", "Matatu Data", 
                                  "Sticker Information", "psv_sticker_information.xlsx"))

# SC Data ----------------------------------------------------------------------
sc_data <- sc_data %>%
  dplyr::select(start_date, invite_date, complete_date,
                MATATU.NUMBER._L,
                DRIVER.RATING._L,
                SPEED.RATING._L,
                MATATU.OCCUPANCY_L,
                PASSENGER.HAND.SANITISATION_L,
                WHEREGOING_L,
                FEEDBACK_R,
                
                MATATU.NUMBER._L_asked,
                DRIVER.RATING._L_asked,
                SPEED.RATING._L_asked,
                MATATU.OCCUPANCY_L_asked,
                PASSENGER.HAND.SANITISATION_L_asked,
                WHEREGOING_L_asked,
                FEEDBACK_R_asked,
                phone_hash,
                file) %>%
  mutate(response_method = "shortcode")

## Clean names
names(sc_data) <- names(sc_data) %>% 
  tolower() %>% 
  str_replace_all("._r|._l", "") %>%
  str_replace_all("\\.", "_")

names(sc_data) <- names(sc_data) %>%
  str_replace_all("matatu_occupanc", "occupancy") %>%
  str_replace_all("passenger_hand_sanitisatio", "covid_measures") %>%
  str_replace_all("wheregoin", "where_going") %>%
  str_replace_all("feedbac", "feedback") %>%
  str_replace_all("matatu_no", "matatu_number")

## Complete Date - Add Seconds
sc_data$complete_date <- paste0(sc_data$complete_date, ":00")
sc_data$complete_date[sc_data$complete_date %in% ":00"] <- ""

# QR Data ----------------------------------------------------------------------
## Clean Names
names(qr_data) <- names(qr_data) %>%
  str_replace_all("How would you rate your matatu driver\\?", "driver_rating") %>%
  str_replace_all("Did the matatu take measures to help prevent the spread of the coronavirus\\? For example, limiting the number of people in the matatu or providing sanitary wipes\\?", "covid_measures") %>%
  str_replace_all("How would you describe your driver’s speed\\?", "speed_rating") %>%
  str_replace_all("How many passengers are on the matatu\\?", "occupancy") %>%
  str_replace_all("When choosing a matatu, how important are amenities \\(eg, music, wifi\\)\\?", "amenity_importance") %>%
  str_replace_all("Leave any comments about the matatu or ride. For example, report unsafe driving or compliment the driver.", "feedback")

qr_data <- qr_data %>%
  dplyr::rename(complete_date = Created,
                qr_web_address = Source)

## Select Variables
qr_data <- qr_data %>%
  dplyr::select(complete_date, driver_rating, driver_rating_asked,
                covid_measures, covid_measures_asked,
                speed_rating, speed_rating_asked,
                occupancy, occupancy_asked,
                amenity_importance, amenity_importance_asked,
                feedback, feedback_asked,
                qr_web_address,
                file) %>%
  mutate(response_method = "qr code")

## Remove testing
qr_data$feedback <- qr_data$feedback %>% tolower() %>% str_squish()
qr_data <- qr_data[!(qr_data$feedback %in% c("test", "testing")),]

# Append and standardize variables ---------------------------------------------
data <- bind_rows(qr_data, sc_data)

data <- data %>%
  dplyr::rename(complete_datetime = complete_date) %>%
  mutate(complete_datetime = complete_datetime %>% ymd_hms) %>%
  mutate(complete_date = complete_datetime %>% as.Date)

data <- data %>%
  mutate_if(is.character, tolower) %>%
  mutate_if(is.character, str_squish)

data$covid_measures[data$covid_measures %in% "yes - but measures are limited"] <- "yes, but seemed limited"
data$covid_measures[data$covid_measures %in% "yes - but measures are limited - no"] <- "yes, but seemed limited"

data$speed_rating[data$speed_rating %in% "much too fast - speed seemed to put my life in danger"] <- "dangerously fast"
data$speed_rating[data$speed_rating %in% "too fast - felt unsafe"] <- "fast"
data$speed_rating[data$speed_rating %in% "fine - drove at appropriate & safe speed"] <- "okay"

data$occupancy[data$occupancy %in% "there are less people than seats."] <- "less people than seats"
data$occupancy[data$occupancy %in% "there are more people than can fit (people hanging off side)"] <- "more people than can fit"
data$occupancy[data$occupancy %in% "there are more people than seats"] <- "more people than seats"
data$occupancy[data$occupancy %in% "there are the same number of people as seats"] <- "same number of people as seats"

for(var in names(data) %>% str_subset("_asked")){
  data[[var]][!(data[[var]] %in% "yes")] <- "no"
}

for(var in names(data)) data[[var]][data[[var]] %in% ""] <- NA

data$file <- data$file %>% str_replace_all(".*/", "")

## Add uid
data$uid <- 1:nrow(data)

# Clean Sticker Data -----------------------------------------------------------
sticker_df <- sticker_df %>%
  mutate(plate      = plate %>% tolower %>% str_squish %>% str_replace_all(" ", ""),
         psv_number = psv_number %>% tolower %>% str_squish %>% str_replace_all(" ", "")) %>%
  dplyr::rename(matatu_number = psv_number,
                reg_no = plate)

# License Plate Number ---------------------------------------------------------
#### [1] Matatu number, numeric
data$matatu_number <- data$matatu_number %>%
  str_replace_all("psv no", "") %>%
  str_replace_all("psv", "") %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_squish() %>%
  str_replace_all(" ", "") %>%
  tolower()

psv_no_df <- sticker_df %>%
  dplyr::select(matatu_number, reg_no) %>%
  dplyr::rename(reg_no_1 = reg_no)

data <- merge(data, psv_no_df, by = "matatu_number", all.x = T, all.y = F)

#### [2] Closest Plate Match
# TODO: First use valid matches, then only do this on remaining ones for shortcode
#       Process takes a bit, so just to reduce time.
# Determine levenstein distance to closest plate match and, if distance is small
# use that plate
plates <- sticker_df$reg_no[!(sticker_df$pilot_number %in% 1)]

leven_df <- lapply(1:nrow(data), function(i){
  # Returns dataframe of levenstein distance of closest plate number and the plate
  # number
  if((i %% 500) %in% 0) print(i)
  
  leven_dists <- adist(data$matatu_number[i], plates) %>%
    as.vector()
  
  plate_i <- plates[which.min(leven_dists)]
  leven_dist_i <- min(leven_dists)
  
  if(length(plate_i) %in% 0) plate_i <- NA
  
  out_df <- data.frame(reg_no_closest = plate_i,
                       reg_no_closest_dist = leven_dist_i)
  
  return(out_df)
}) %>%
  bind_rows()

data <- bind_cols(data, leven_df)

data$reg_no_2 <- NA

use_reg_no <- data$reg_no_closest_dist %in% 0:1 & nchar(data$matatu_number) %in% 7
data$reg_no_2[use_reg_no] <- data$matatu_number[use_reg_no]

#### Clean Reg Number
data$reg_no <- data$reg_no_2
data$reg_no[is.na(data$reg_no)] <- data$reg_no_1[is.na(data$reg_no)]

data$reg_no_1 <- NULL
data$reg_no_2 <- NULL

# Merge in Sticker Data --------------------------------------------------------
sticker_df <- sticker_df %>%
  dplyr::select(-c(matatu_number, wfp_form_id, notes)) %>%
  dplyr::rename(qr_code_on_sticker = qr_code,
                shortcode_on_sticker = shortcode)

## Merge for shortcode
sc_data <- merge(data %>% filter(response_method == "shortcode"), 
                 sticker_df %>% dplyr::select(-qr_web_address), 
                 by = "reg_no", all.x=T, all.y=F)

## Merge for QR code
qr_data <- merge(data %>% filter(response_method == "qr code"), 
                 sticker_df %>% dplyr::select(-reg_no), 
                 by = "qr_web_address", all.x=T, all.y=F)

## Append
data <- bind_rows(sc_data, qr_data)

# Pilot Number -----------------------------------------------------------------
# If plate number invalid, can figure out pilot number in some cases using
# survey file name and date completed

data$file %>% table() %>% View()
data$pilot_number %>% is.na %>% table





# ***** EDIT HERE ***** --------------------------------------------------------
# Label variables

if(F){
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Award Variables --------------------------------------------------------------
  #### Award Amount
  data$award_posted <- ""
  data$award_posted[grepl("100_KES", data$file)] <- "100"
  data$award_posted[grepl("200_KES", data$file)] <- "200"
  data$award_posted[grepl("Get_Airtime", data$file)] <- "50"
  data$award_posted[grepl("Win_Airtime", data$file)] <- "50"
  data$award_posted[grepl("SHORT", data$file) & grepl("Get_Airtime", data$file)] <- "50" # Unspecified?
  
  #### Award Actual
  data$award_actual <- data$award_posted
  data$award_actual[data$award_actual %in% c("100", "200") & data$start_date >= "2020-06-28"] <- "20"
  data$award_actual[data$start_date >= "2020-07-20" & data$award_actual %in% "20"] <- "0"
  data$award_actual[data$start_date >= "2020-08-31" & data$award_actual %in% "50"] <- "0"
  data$award_actual[data$start_date >= "2020-08-31" & data$award_actual %in% "Unspecified"] <- "0"
  data$award_actual[grepl("SHORT", data$file) & grepl("Get_Airtime", data$file)] <- "10" # Unspecified?
  data$award_actual[data$start_date >= "2020-08-31" & data$award_actual %in% "10"] <- "0"
  
  #### Get or Win Award
  data$win_get_award[grepl("Win_Airtime", data$file)] <- "Win"
  data$win_get_award[is.na(data$win_get_award)] <- "Get"
  
  #### Award Actual - Get Win
  data$award_actual_getwin <- paste(data$win_get_award, data$award_actual, "KES")
  data$award_actual_getwin[grepl("\\b0\\b", data$award_actual_getwin)] <- "None"
  data$award_actual_getwin <- data$award_actual_getwin %>%
    factor(levels = c("None",  "Get 10 KES", "Win 50 KES", "Get 20 KES", "Get 50 KES",
                      "Get 100 KES", "Get 200 KES"))
  
  #### Award Posted - Get Win
  data$award_posted_getwin <- paste(data$win_get_award, data$award_posted, "KES")
  data$award_posted_getwin[grepl("\\b0\\b", data$award_posted_getwin)] <- "None"
  
  # Add variables ----------------------------------------------------------------
  
  #### N Questions
  data$N_questions <- 6
  data$N_questions[grepl("SHORT", data$file)] <- 3
  
  #### How Identify Vehicle
  data$how_identif <- "number"
  data$how_identif[grepl("Get_Airtime|Win_Airtime", data$file)] <- "reg no"
  
  # Create variables -------------------------------------------------------------
  data <- data %>%
    mutate(start_date = start_date %>% ymd_hm,
           complete_date = complete_date %>% ymd_hm) %>%
    mutate(date = start_date %>% as.Date(),
           hour = start_date %>% hour(),
           time_to_complete_mins = difftime(start_date, complete_date, units = "mins")) %>%
    mutate(week = start_date %>% week) %>%
    mutate(week_date = ymd( "2020-01-01") + lubridate::weeks(week - 1))
  
  data$reg_no_raw <- NA
  data$reg_no_raw[data$how_identif %in% "reg no"] <-
    data$matatu_no[data$how_identif %in% "reg no"]
  
  data$reg_no_clean <- data$reg_no_raw %>%
    tolower() %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_squish() %>%
    str_replace_all(" ", "")
  
  data <- data %>%
    mutate(feedback_nwords = feedback %>% str_count("\\S+"))
  
  # Number of questions answered, out of questions always asked
  data$N_qs_answered <- data %>%
    dplyr::select(c("matatu_no", "driver_rating", "speed_rating", 
                    "occupancy", "covid_measures", "feedback")) %>%
    apply(1, function(x) sum(!is.na(x)))
  
  # Completed survey
  data$completed_survey <- !is.na(data$complete_date)
  
  # Closest Plate Match ----------------------------------------------------------
  plates <- charc_df$psv_number[charc_df$pilot_number %in% 2]
  
  leven_df <- lapply(1:nrow(data), function(i){
    
    # levenstein distance to closest plate number and plate number
    
    if((i %% 500) %in% 0) print(i)
    
    leven_dists <- adist(data$reg_no_clean[i], charc_df$psv_number[charc_df$pilot_number %in% 2]) %>%
      as.vector()
    
    plate_i <- plates[which.min(leven_dists)]
    leven_dist_i <- min(leven_dists)
    
    if(length(plate_i) %in% 0) plate_i <- NA
    
    out_df <- data.frame(reg_no_closest = plate_i,
                         reg_no_closest_dist = leven_dist_i)
    
    return(out_df)
  }) %>%
    bind_rows()
  
  data <- bind_cols(data, leven_df)
  
  # Clean matatu number ----------------------------------------------------------
  data$matatu_no_clean <- data$matatu_no %>% 
    tolower() %>%
    str_squish() %>%
    str_replace_all(" ", "")
  
  data$matatu_no_clean[data$reg_no_closest_dist %in% 1] <-
    data$reg_no_closest[data$reg_no_closest_dist %in% 1]
  
  data$matatu_no_clean[!(data$matatu_no_clean %in% charc_df$psv_number)] <- NA
  
  data$matatu_no_valid <- !(data$matatu_no_clean %in% NA)
  
  # Remove unneeded variables ----------------------------------------------------
  data <- data %>%
    dplyr::select(-c(file))
  
  # Label variables --------------------------------------------------------------
  var_label(data$start_date) <- "Survey Start Date/Time"
  var_label(data$invite_date) <- "Survey Invite Date/Time"
  var_label(data$complete_date) <- "Survey Completion Date/Time"
  var_label(data$matatu_no) <- "Matatu number, as entered"
  var_label(data$driver_rating) <- "How would you rate your Matatu driver?"
  var_label(data$speed_rating) <- "How would you describe your Matatu driver's speed?"
  var_label(data$occupancy) <- "On the Matatu, are there"
  var_label(data$covid_measures) <- "Were measures taken to prevent spread of COVID-19? e.g. Limiting passengers or providing sanitiser / wipes?"
  var_label(data$where_going) <- "Where are you traveling to? (e.g., stage or location where you'll get off the matatu)"
  var_label(data$feedback) <- "Would you like to leave a comment about the matatu or ride?"
  var_label(data$phone_hash) <- "Phone number (hashed)"
  var_label(data$award_posted) <- "Award amount (posted on sticker)"
  var_label(data$award_actual) <- "Award amount (actual - advertised in survey)"
  var_label(data$win_get_award) <- "Win or get award?"
  var_label(data$how_identif) <- "How identify matatu on shortcode"
  var_label(data$date) <- "Survey date"
  var_label(data$hour) <- "Survey hour"
  var_label(data$reg_no_raw) <- "Reg no, as entered"
  var_label(data$reg_no_clean) <- "Reg no, cleaned"
  var_label(data$reg_no_closest) <- "Reg no, closest to correct on"
  var_label(data$reg_no_closest_dist) <- "Levenstein distance from reg no to closest reg no"
  var_label(data$matatu_no_clean) <- "Matatu number - only valid"
  var_label(data$matatu_no_valid) <- "Is matatu number valid?" 
  var_label(data$time_to_complete_mins) <- "Time to complete survey (minutes)" 
  var_label(data$completed_survey) <- "Completed survey" 
  
  # Export -----------------------------------------------------------------------
  saveRDS(data, file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "FinalData", 
                          "echo_data.Rds"))
  write_dta(data, file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "FinalData", 
                            "echo_data.dta"))
}

