# Create dataset for microdata catalogue

# Load data --------------------------------------------------------------------
data <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "All Data", "FinalData", 
                          "rider_feedback.Rds"))

# Subset variables -------------------------------------------------------------
data <- data %>%
  dplyr::select(uid, vehicle_id, start_datetime, complete_datetime, response_method,
                driver_rating, covid_measures, speed_rating, occupancy, feedback,
                award_amount_posted, award_type, award_offer_end)

# Apply Labels to Factors ------------------------------------------------------
data <- data %>%
  mutate(driver_rating = driver_rating %>% as.factor(),
         covid_measures = covid_measures %>% as.factor(),
         speed_rating = speed_rating %>% as.factor(),
         occupancy = occupancy %>% as.factor()) %>%
  mutate(driver_rating = labelled(driver_rating, c(`very safe` = 1,
                                                   safe = 2,
                                                   unsafe = 3,
                                                   `very unsafe` = 4)),
         covid_measures = labelled(covid_measures, c(no = 1,
                                                     `yes, but seemed limited` = 2,
                                                     `yes, effective` = 3)),
         speed_rating = labelled(speed_rating, c(`too slow` = 1,
                                                 `okay` = 2,
                                                 `fast` = 3,
                                                 `dangerously fast` = 4)),
         occupancy = labelled(occupancy, c(`less people than seats` = 1,
                                           `same number of people as seats` = 2,
                                           `more people than seats` = 3,
                                           `more people than can fit` = 4)))


# Export -----------------------------------------------------------------------
write_dta(data, file.path(dropbox_file_path, "Data", "Rider Feedback", "All Data", "FinalData", 
                          "extracts_for_specific_purposes", "microdata-catalogue",
                          "rider_feedback.dta"))


