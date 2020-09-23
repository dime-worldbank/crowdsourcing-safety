# PSV Rider Feedback: Clean Data

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "RawData", 
                          "echo_data.Rds"))

charc_df <- read_xlsx(file.path(dropbox_file_path, "Data", "Matatu Data", 
                                "Characteristics", "RawData", "vehicle_basic_info.xlsx"))
charc_df <- charc_df %>%
  mutate(plate      = plate %>% str_replace_all(" ", "") %>% tolower(),
         psv_number = psv_number %>% str_replace_all(" ", "") %>% tolower())

# Select variables and clean names ---------------------------------------------
## Select useful variables
data <- data %>%
  dplyr::select(start_date, invite_date, complete_date,
                MATATU.NUMBER._R,
                DRIVER.RATING._L,
                SPEED.RATING._L,
                MATATU.OCCUPANCY_L,
                PASSENGER.HAND.SANITISATION_L,
                WHEREGOING_L,
                FEEDBACK_R,
                phone_hash,
                file) 

## Clean names
names(data) <- names(data) %>% 
  tolower() %>% 
  str_replace_all("._r|._l", "") %>%
  str_replace_all("\\.", "_")

data <- data %>%
  dplyr::rename(occupancy = matatu_occupanc,
                covid_measures = passenger_hand_sanitisatio,
                where_going = wheregoin,
                feedback = feedbac,
                matatu_no = matatu_number)

## Replace "" with NA
for(var in names(data)) data[[var]][data[[var]] %in% ""] <- NA

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


