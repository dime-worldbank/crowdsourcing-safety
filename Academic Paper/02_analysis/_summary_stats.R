# Summary Statistics

# Load data --------------------------------------------------------------------
#### Passenger feedback
fb_raw_df <- readRDS(file.path(data_dir, "RawData", "passenger_feedback.Rds"))
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid.Rds"))

#### Vehicle level data
veh_df        <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers.Rds"))
veh_stc_df    <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers.Rds"))
veh_stc_tl_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_telematics.Rds"))

# N Passenger Feedback ---------------------------------------------------------
## Raw
nrow(fb_raw_df)

## Remove duplicates
fb_raw_df %>%
  distinct(phone_hash, regno, .keep_all = T) %>%
  nrow()

## Remove potential cheating
nrow(fb_df)

## Method
fb_df$response_method %>% table()


# ----> TODO, by response method ??

## Duplicate phone numbers
dup_phone <- fb_raw_df %>%
  group_by(phone_hash) %>%
  dplyr::summarise(n_responses = n()) %>%
  ungroup() %>%
  
  group_by(n_responses) %>%
  dplyr::summarise(n_passengers = n()) %>%
  ungroup() %>%
  
  mutate(prop_passengers = n_passengers / sum(n_passengers)) %>%
  
  arrange(n_responses) %>%
  mutate(prop_passengers_cumsum = cumsum(prop_passengers))

dup_phone %>%
  dplyr::filter(n_responses >= 20) %>%
  dplyr::summarise(n = sum(n_passengers))

## Removing duplicates
nrow(fb_all_df)

## Known reg number
fb_all_df %>%
  dplyr::filter(regno != "UNKNOWN") %>%
  nrow()

## Potential cheating
fb_all_df %>%
  dplyr::filter(regno != "UNKNOWN") %>%
  pull(ptn_cheating) %>%
  table()

## Potential cheating, fill
fb_all_df %>%
  dplyr::filter(regno != "UNKNOWN") %>%
  pull(ptn_cheating_fill) %>%
  table()

#### Response method

# Surveys: Identify vehicles vs cant -------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_class.Rds"))
fb_wunknown_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_class_wunknown.Rds"))

nrow(fb_df)
nrow(fb_wunknown_df)
nrow(fb_wunknown_df) - nrow(fb_df)
# N Vehicles -------------------------------------------------------------------
## Total vehicles
nrow(veh_df)

## Total with stickers
veh_df %>%
  dplyr::filter(!is.na(shortcode_on_sticker)) %>%
  nrow()

## Total with telematics devices
veh_df %>%
  dplyr::filter(!is.na(prop_time_over_0kph_base_10kph)) %>%
  nrow()

## Total with stickers and telematics device
veh_df %>%
  dplyr::filter(!is.na(shortcode_on_sticker)) %>%
  dplyr::filter(!is.na(prop_time_over_0kph_base_10kph)) %>%
  nrow()

## Total vehicles with stickers, with >= 10 responses
nrow(veh_stc_df)

## Total vehicles with (1) stickers and >= 10 responses, and (2) telematics device
nrow(veh_stc_tl_df)

# Other vehicle stats ----------------------------------------------------------
## Median and 75th percentile number of responses per vehicle
veh_df$n_feedback %>% summary()

## Number of vehicles with QR code but not shortcode
veh_df %>%
  dplyr::filter(!is.na(qr_code_on_sticker)) %>%
  dplyr::filter( (qr_code_on_sticker %in% "yes") & (shortcode_on_sticker %in% "no") ) %>%
  nrow()


  



