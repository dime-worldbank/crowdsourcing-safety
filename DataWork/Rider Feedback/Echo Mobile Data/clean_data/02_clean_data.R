# PSV Rider Feedback: Clean Data

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "RawData", 
                          "echo_data.Rds"))

charc_df <- read_xlsx(file.path(dropbox_file_path, "Data", "Matatu Data", 
                                "Characteristics", "RawData", "vehicle_basic_info.xlsx"))

# Select variables and clean names ---------------------------------------------
data <- data %>%
  dplyr::select(start_date, invite_date, complete_date,
                MATATU.NUMBER._R,
                DRIVER.RATING._L,
                SPEED.RATING._L,
                MATATU.OCCUPANCY_L,
                PASSENGER.HAND.SANITISATION_L,
                WHEREGOING_L,
                FEEDBACK_R,
                award, win_get_award, phone_hash, how_identif) 

names(data) <- names(data) %>% 
  tolower() %>% 
  str_replace_all("._r|._l", "") %>%
  str_replace_all("\\.", "_")

data <- data %>%
  dplyr::rename(occupancy = matatu_occupanc,
                covid_measures = passenger_hand_sanitisatio,
                where_going = wheregoin,
                feedback = feedbac)

#### Award Amount
data$award <- NA
data$award[grepl("100_KES", data$file)] <- 100
data$award[grepl("200_KES", data$file)] <- 200
data$award[grepl("Get_Airtime", data$file)] <- 50
data$award[grepl("Win_Airtime", data$file)] <- 50

#### Get or Win Award
data$win_get_award[grepl("Win_Airtime", data$file)] <- "win"
data$win_get_award[is.na(data$win_get_award)] <- "Get"

#### How Identify Vehicle
data$how_identif <- "number"
data$how_identif[grepl("Get_Airtime|Win_Airtime", data$file)] <- "reg no"

# Create variables -------------------------------------------------------------
data <- data %>%
  mutate(date = start_date %>% as.Date(),
         hour = start_date %>% hour())

data$reg_no_raw <- NA
data$reg_no_raw[data$how_identif %in% "reg no"] <-
  data$matatu_number[data$how_identif %in% "reg no"]



