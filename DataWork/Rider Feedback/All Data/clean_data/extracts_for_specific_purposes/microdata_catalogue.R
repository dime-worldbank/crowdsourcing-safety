# Create dataset for microdata catalogue

# Load data --------------------------------------------------------------------
data <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "All Data", "FinalData", 
                          "rider_feedback.Rds"))

# Apply Labels to Factors ------------------------------------------------------
#### Missing values
data$driver_rating[is.na(data$driver_rating) & data$driver_rating_asked %in% "yes"] <- "DIDNT ANSWER"
data$driver_rating[is.na(data$driver_rating) & data$driver_rating_asked %in% "no"] <- "NOT ASKED"

data$covid_measures[is.na(data$covid_measures) & data$covid_measures_asked %in% "yes"] <- "DIDNT ANSWER"
data$covid_measures[is.na(data$covid_measures) & data$covid_measures_asked %in% "no"] <- "NOT ASKED"

data$speed_rating[is.na(data$speed_rating) & data$speed_rating_asked %in% "yes"] <- "DIDNT ANSWER"
data$speed_rating[is.na(data$speed_rating) & data$speed_rating_asked %in% "no"] <- "NOT ASKED"

data$occupancy[is.na(data$occupancy) & data$occupancy_asked %in% "yes"] <- "DIDNT ANSWER"
data$occupancy[is.na(data$occupancy) & data$occupancy_asked %in% "no"] <- "NOT ASKED"

data$amenity_importance[is.na(data$amenity_importance) & data$amenity_importance_asked %in% "yes"] <- "DIDNT ANSWER"
data$amenity_importance[is.na(data$amenity_importance) & data$amenity_importance_asked %in% "no"] <- "NOT ASKED"

data$where_going[is.na(data$where_going) & data$where_going_asked %in% "yes"] <- "DIDNT ANSWER"
data$where_going[is.na(data$where_going) & data$where_going_asked %in% "no"] <- "NOT ASKED"

data$award_offer_end[is.na(data$award_offer_end) & is.na(data$award_amount_posted)] <- "UNKNOWN"

#### Label
data <- data %>%
  mutate(driver_rating = 
           case_when(driver_rating == "very safe" ~ 1,
                     driver_rating == "safe" ~ 2,
                     driver_rating == "unsafe" ~ 3,
                     driver_rating == "very unsafe" ~ 4,
                     driver_rating == "DIDNT ANSWER" ~ -98,
                     driver_rating == "NOT ASKED" ~ -99) %>%
           haven::labelled(c(`very safe` = 1,
                             safe = 2,
                             unsafe = 3,
                             `very unsafe` = 4,
                             `DIDNT ANSWER` = -98,
                             `NOT ASKED` = -99),
                           "How would you rate your Matatu driver?"),
         
         covid_measures = 
           case_when(covid_measures == "no" ~ 1,
                     covid_measures == "yes, but seemed limited" ~ 2,
                     covid_measures == "yes, effective" ~ 3,
                     covid_measures == "DIDNT ANSWER" ~ -98,
                     covid_measures == "NOT ASKED" ~ -99) %>%
           haven::labelled(c("no" = 1,
                             "yes, but seemed limited" = 2,
                             "yes, effective" = 3,
                             "DIDNT ANSWER" = -98,
                             "NOT ASKED" = -99),
                           "Were measures taken to prevent spread of COVID-19?"),
         
         speed_rating = 
           case_when(speed_rating == "too slow" ~ 1,
                     speed_rating == "okay" ~ 2,
                     speed_rating == "fast" ~ 3,
                     speed_rating == "dangerously fast" ~ 4,
                     speed_rating == "DIDNT ANSWER" ~ -98,
                     speed_rating == "NOT ASKED" ~ -99) %>%
           haven::labelled(c("too slow" = 1,
                             "okay" = 2,
                             "fast" = 3,
                             "dangerously fast" = 4,
                             "DIDNT ANSWER" = -98,
                             "NOT ASKED" = -99),
                           "How would you describe your Matatu driver's speed?"),
         
         occupancy = 
           case_when(occupancy == "less people than seats" ~ 1,
                     occupancy == "same number of people as seats" ~ 2,
                     occupancy == "more people than seats" ~ 3,
                     occupancy == "more people than can fit" ~ 4,
                     occupancy == "DIDNT ANSWER" ~ -98,
                     occupancy == "NOT ASKED" ~ -99) %>%
           haven::labelled(c("less people than seats" = 1,
                             "same number of people as seats" = 2,
                             "more people than seats" = 3,
                             "more people than can fit" = 4,
                             "DIDNT ANSWER" = -98,
                             "NOT ASKED" = -99),
                           "On the Matatu, are there..."),
         
         feedback_asked = 
           case_when(feedback_asked == "no" ~ 1,
                     feedback_asked == "yes" ~ 2) %>%
           haven::labelled(c("no" = 1,
                             "yes" = 2),
                           "Was the `feedback` question asked in the survey?"),
         
         vehicle_id = vehicle_id %>% 
           replace_na(-97) %>%
           haven::labelled(c("UNKNOWN" = -97),
                           "Vehicle ID"),
         
         award_amount_posted = award_amount_posted %>% 
           replace_na(-97) %>%
           haven::labelled(c("None" = 0,
                             "50 KES" = 50,
                             "100 KES" = 100,
                             "200 KES" = 200,
                             "UNKNOWN" = -97),
                           "Award amount (posted on sticker)"),
         
         award_type = 
           case_when(award_type == "none" ~ 1,
                     award_type == "get" ~ 2,
                     award_type == "win" ~ 3) %>%
           haven::labelled(c("None" = 1,
                             "Get" = 2,
                             "Win" = 3),
                           "Award Type"),
         
         response_method = 
           case_when(response_method == "qr code" ~ 1,
                     response_method == "shortcode" ~ 2) %>%
           haven::labelled(c(`qr code` = 1,
                             shortcode = 2),
                           "How answered survey"),
         
         user_id = phone_hash %>% as.factor() %>% as.numeric(),
         
         award_offer_end = award_offer_end # %>% ymd() # keep as character so keeps "none"
  )

var_label(data$award_offer_end) <- "Award offer ending date"
var_label(data$user_id) <- "Unique user ID"

# Subset variables -------------------------------------------------------------
data <- data %>%
  dplyr::select(uid, user_id, vehicle_id, date, response_method,
                driver_rating, speed_rating, occupancy, covid_measures, feedback, feedback_asked,
                award_amount_posted, award_type, award_offer_end)

# Export -----------------------------------------------------------------------
write_dta(data, file.path(dropbox_file_path, "Data", "Rider Feedback", "All Data", "FinalData", 
                          "extracts_for_specific_purposes", "microdata-catalogue",
                          "rider_feedback.dta"))


