# PSV Rider Feedback: Clean Data

# 1. LOAD DATA =================================================================
## Shortcode Data
sc_data <- readRDS(file.path(dropbox_dir, "Data", "Rider Feedback - Pilot", "Echo Mobile Data", "RawData", 
                             "echo_data.Rds"))

## QR Data
qr_data <- readRDS(file.path(dropbox_dir, "Data", "Rider Feedback - Pilot", "PSV Safety Systems Data", "RawData", 
                             "qr_data.Rds"))

## Sticker Data
sticker_df <- read_xlsx(file.path(dropbox_dir, "Data", "Matatu Data - Pilot", 
                                  "Sticker Information", "psv_sticker_information.xlsx"))

# 2. CLEAN INDIVIDUAL DATASETS =================================================
# Sticker data needs to be cleaned first, as we merge it with qr/shortcode in
# next step

# ** 2.1 Sticker Data ----------------------------------------------------------
sticker_df <- sticker_df %>%
  mutate(plate      = plate %>% tolower %>% str_squish %>% str_replace_all(" ", ""),
         psv_number = psv_number %>% tolower %>% str_squish %>% str_replace_all(" ", "")) %>%
  dplyr::rename(matatu_number = psv_number,
                reg_no = plate)

# ** 2.2 Shortcode Data --------------------------------------------------------

# **** 2.2.1 Main Cleanup ------------------------------------------------------
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

# **** 2.2.2 Merge with sticker, account for mispellings -----------------------

## Cleanup
sc_data$matatu_number <- sc_data$matatu_number %>%
  str_replace_all("psv no", "") %>%
  str_replace_all("psv", "") %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_squish() %>%
  str_replace_all(" ", "") %>%
  tolower()

## Registration number dataframe
reg_no_df <- sticker_df %>%
  dplyr::select(matatu_number) %>%
  filter(!is.na(matatu_number)) %>%
  mutate(matatu_number_is_valid = T) 

#### Split into data where have valid plate and not
sc_data <- merge(sc_data, reg_no_df, by = "matatu_number", all.x = T, all.y = F)

data_regVALID_T <- sc_data[sc_data$matatu_number_is_valid %in% T,]
data_regVALID_F <- sc_data[is.na(sc_data$matatu_number_is_valid),]

data_regVALID_T$matatu_number_valid <- data_regVALID_T$matatu_number

#### Add Closest Plate Match and Leven Distance
# Determine levenstein distance to closest plate match and, if distance is small
# use that plate
plates <- reg_no_df$matatu_number[nchar(reg_no_df$matatu_number) %in% 7] %>% unique()

leven_df <- lapply(1:nrow(data_regVALID_F), function(i){
  # Returns dataframe of levenstein distance of closest plate number and the plate
  # number
  if((i %% 500) %in% 0) print(i)
  
  leven_dists <- adist(data_regVALID_F$matatu_number[i], plates) %>%
    as.vector()
  
  plate_i <- plates[which.min(leven_dists)]
  leven_dist_i <- min(leven_dists)
  
  if(length(plate_i) %in% 0) plate_i <- NA
  
  out_df <- data.frame(matatu_number_closest = plate_i,
                       matatu_number_closest_dist = leven_dist_i)
  
  return(out_df)
}) %>%
  bind_rows()

## Add closest matatu number if within levenstein distance of 1
data_regVALID_F <- bind_cols(data_regVALID_F, leven_df)
data_regVALID_F$matatu_number_valid <- NA
data_regVALID_F$matatu_number_valid[data_regVALID_F$matatu_number_closest_dist %in% 1:2] <- 
  data_regVALID_F$matatu_number_closest[data_regVALID_F$matatu_number_closest_dist %in% 1:2]

## Append data back together
sc_data <- bind_rows(data_regVALID_T,
                     data_regVALID_F)

## Manually add reg no
# The "generic sticker" was only put on one matatu
sc_data$matatu_number_valid[grepl("GNRIC_MATATU", sc_data$file)] <- "kba970c"

## Merge sticker data
sc_data <- merge(sc_data, 
                 sticker_df %>%
                   filter(!is.na(matatu_number)), 
                 by.x = "matatu_number_valid",
                 by.y = "matatu_number",
                 all.x = T,
                 all.y = F)

# ** 2.3 QR Code Data ----------------------------------------------------------
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
                qr_web_address, phone_hash,
                file) %>%
  mutate(response_method = "qr code")

## Remove testing
qr_data$feedback <- qr_data$feedback %>% tolower() %>% str_squish()
qr_data <- qr_data[!(qr_data$feedback %in% c("test", "testing")),]

## Merge with sticker
qr_data <- merge(qr_data, 
                 sticker_df %>%
                   filter(!is.na(qr_web_address)), 
                 by = "qr_web_address",
                 all.x=T,
                 all.y=F)

# 3. APPEND AND STANDARDIZE VARIABLES ==========================================

# ** 3.1 Append ----------------------------------------------------------------
data <- bind_rows(qr_data, sc_data)

# ** 3.2 Cleanup Variables -----------------------------------------------------

# **** 3.2.1 General -----------------------------------------------------------

## Character
data <- data %>%
  mutate_if(is.character, tolower) %>%
  mutate_if(is.character, str_squish)

## Replace "" with NA
for(var in names(data)) data[[var]][data[[var]] %in% ""] <- NA

# **** 3.2.2 Variety -----------------------------------------------------------
## Datetime variables
data <- data %>%
  dplyr::rename(complete_datetime = complete_date) %>%
  mutate(complete_datetime = complete_datetime %>% ymd_hms) %>%
  mutate(complete_date = complete_datetime %>% as.Date) %>%
  
  dplyr::rename(start_datetime = start_date) %>%
  mutate(start_datetime = start_datetime %>% ymd_hm())

## Datetime and date
data$datetime <- data$complete_datetime
data$datetime[is.na(data$datetime)] <- data$start_datetime[is.na(data$datetime)]

data$date <- data$datetime %>% date()
data$month <- data$date %>% substring(1,7)

## Completed survey
data$completed_survey <- !is.na(data$complete_datetime)

## Reg number
data$reg_no[is.na(data$reg_no)] <- "UNKNOWN"

## Vehicle ID
data <- data %>%
  mutate(vehicle_id = reg_no %>% 
           na_if("UNKNOWN") %>%
           as.factor() %>%
           as.numeric()) 

## Cleanup file name
data$file <- data$file %>% str_replace_all(".*/", "")

## Feedback - number of words
data$feedback_nwords <- data$feedback %>% n_words()
data$feedback_nwords[is.na(data$feedback)] <- NA

## Pilot number
# Add one so starts at 1, not 0
data$pilot_number <- data$pilot_number + 1

## Add uid
data$uid <- 1:nrow(data)

## **** 3.2.3 Ratings/feedback variables --------------------------------------

data$covid_measures[data$covid_measures %in% "yes - measures seem effective"] <- "yes, effective"
data$covid_measures[data$covid_measures %in% "yes - but measures are limited"] <- "yes, but seemed limited"
data$covid_measures[data$covid_measures %in% "yes - but measures are limited - no"] <- "yes, but seemed limited"

data$speed_rating[data$speed_rating %in% "much too fast - speed seemed to put my life in danger"] <- "dangerously fast"
data$speed_rating[data$speed_rating %in% "too fast - felt unsafe"] <- "fast"
data$speed_rating[data$speed_rating %in% "fine - drove at appropriate & safe speed"] <- "okay"

data$occupancy[data$occupancy %in% "there are less people than seats."] <- "less people than seats"
data$occupancy[data$occupancy %in% "there are more people than can fit (people hanging off side)"] <- "more people than can fit"
data$occupancy[data$occupancy %in% "there are more people than seats"] <- "more people than seats"
data$occupancy[data$occupancy %in% "there are the same number of people as seats"] <- "same number of people as seats"

## Cleanup "_asked" varibles
for(var in names(data) %>% str_subset("_asked")){
  data[[var]][!(data[[var]] %in% "yes")] <- "no"
}

# **** 3.2.4 Award Amount ------------------------------------------------------
data <- data %>%
  dplyr::rename(award_amount_posted = award_amount)

# **** 3.2.5 Win/Get Award -----------------------------------------------------
# We have this information by knowing the liscence plate. However, in cases where
# we don't have the plate number, we can use the survey that they answered.

data$award_type[grepl("matatu_survey__win_airtime", data$file)] <- "win"

get_award_files <- c("matatu_survey__get_airtime", 
                     "matatu_survey__200_kes_incentive",
                     "matatu_survey__100_kes_incentive") %>%
  paste(collapse = "|")
data$award_type[grepl(get_award_files, data$file)] <- "get"

# **** 3.2.6 Pilot Number ------------------------------------------------------
# If plate number invalid, can figure out pilot number in some cases using
# survey file name and date completed

# 4. FINALIZE ==================================================================

# ** 4.1 Subset to Relevant Variables ------------------------------------------

data <- data %>%
  dplyr::select(-c(invite_date, qr_web_address, file, notes, matatu_number_is_valid, wfp_form_id))

# ** 4.2 Order variables -------------------------------------------------------
data <- data %>%
  dplyr::select(uid, reg_no,
                start_datetime, complete_date, complete_datetime, 
                everything())

# ** 4.3 Label variables -------------------------------------------------------
var_label(data$start_datetime) <- "Shortcode Survey Start Date/Time"
var_label(data$datetime) <- "Survey Date/Time"
var_label(data$date) <- "Survey Date"
var_label(data$month) <- "Survey Month"
var_label(data$complete_datetime) <- "Survey Completion Date/Time"
var_label(data$complete_date) <- "Survey Completion Date"
var_label(data$matatu_number) <- "Matatu number, as entered"
var_label(data$matatu_number_valid) <- "Matatu number, valid"
var_label(data$driver_rating) <- "How would you rate your Matatu driver?"
var_label(data$speed_rating) <- "How would you describe your Matatu driver's speed?"
var_label(data$occupancy) <- "On the Matatu, are there..."
var_label(data$covid_measures) <- "Were measures taken to prevent spread of COVID-19? e.g. Limiting passengers or providing sanitiser / wipes?"
var_label(data$where_going) <- "Where are you traveling to? (e.g., stage or location where you'll get off the matatu)"
var_label(data$feedback) <- "Would you like to leave a comment about the matatu or ride?"
var_label(data$phone_hash) <- "Phone number (hashed)"
var_label(data$amenity_importance) <- "How important are amenities (e.g., wifi) when choosing a matatu to rider?"
var_label(data$award_amount_posted) <- "Award amount (KES - posted on sticker)"
#var_label(data$award_actual) <- "Award amount (actual - advertised in survey)"
var_label(data$award_type) <- "Win or get award?"
var_label(data$award_offer_end) <- "Award offer ending date"
var_label(data$response_method) <- "Used shortcode or QR code"
var_label(data$reg_no) <- "Registration Number"
var_label(data$qr_code) <- "Whether vehicle had a qr code"
var_label(data$shortcode) <- "Whether vehicle had a shortcode"
var_label(data$stickers_installed) <- "Date stickers installed"
var_label(data$pilot_number) <- "Pilot round number"
var_label(data$matatu_number_closest) <- "Matatu number, closest"
var_label(data$matatu_number_closest_dist) <- "Matatu number, closest [levenstein distance]"
var_label(data$uid) <- "Response Unique ID"

var_label(data$feedback_asked) <- "Was 'feedback' question asked in survey?"
var_label(data$driver_rating_asked) <- "Was 'driver_rating' question asked in survey?"
var_label(data$covid_measures_asked) <- "Was 'covid_measures' question asked in survey?"
var_label(data$where_going_asked) <- "Was 'where_going' question asked in survey?"
var_label(data$speed_rating_asked) <- "Was 'speed_rating' question asked in survey?"
var_label(data$amenity_importance_asked) <- "Was 'amenity_importance' question asked in survey?"
var_label(data$occupancy_asked) <- "Was 'occupancy' question asked in survey?"
var_label(data$matatu_number_asked) <- "Was 'matatu_number' question asked in survey?"
var_label(data$flier) <- "Flier type given to driver"

#var_label(data$how_identif) <- "How identify matatu on shortcode"
#var_label(data$date) <- "Survey date"
#var_label(data$hour) <- "Survey hour"
#var_label(data$reg_no_raw) <- "Reg no, as entered"
#var_label(data$reg_no_clean) <- "Reg no, cleaned"
#var_label(data$reg_no_closest) <- "Reg no, closest to correct on"
#var_label(data$reg_no_closest_dist) <- "Levenstein distance from reg no to closest reg no"
#var_label(data$time_to_complete_mins) <- "Time to complete survey (minutes)" 
var_label(data$completed_survey) <- "Answered all questions in survey" 
var_label(data$vehicle_id) <- "Vehicle ID" 

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(dropbox_dir, "Data", "Rider Feedback - Pilot", "All Data", "FinalData", 
                        "rider_feedback.Rds"))
write_dta(data, file.path(dropbox_dir, "Data", "Rider Feedback - Pilot", "All Data", "FinalData", 
                          "rider_feedback.dta"))

