# Summary Statistics

# Load data --------------------------------------------------------------------
#### Passenger feedback
fb_raw_df <- readRDS(file.path(data_dir, "RawData", "passenger_feedback.Rds"))
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_main_cmntfilterFALSE_dstnctpassTRUE.Rds"))

#### Vehicle level data
veh_df        <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_cmntfilterFALSE_dstnctpassTRUE.Rds"))
veh_stc_df    <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_cmntfilterFALSE_dstnctpassTRUE.Rds"))
veh_stc_tl_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_telematics_cmntfilterFALSE_dstnctpassTRUE.Rds"))

# N Observations ---------------------------------------------------------------
## Raw
nrow(fb_raw_df)

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
  dplyr::filter(n_responses == 1) %>%
  pull(prop_passengers)

dup_phone %>%
  dplyr::filter(n_responses %in% 1:5) %>%
  pull(prop_passengers) %>%
  sum()

dup_phone$n_responses %>% max()

dup_phone %>%
  dplyr::filter(n_responses %in% 10:50) %>%
  pull(n_passengers) %>%
  sum()

dup_phone %>%
  pull(n_passengers) %>%
  sum()

178/6251

dup_phone %>%
  dplyr::filter(n_responses >= 20) %>%
  dplyr::summarise(n = sum(n_passengers))

## Removing duplicates
nrow(fb_all_df)

## Remove duplicates
fb_raw_df %>%
  distinct(phone_hash, regno, .keep_all = T) %>%
  nrow()

## Cheating
nrow(fb_df)

6386 - 4595

## Response method
fb_df$response_method %>% table()

# N feedback per vehicle -------------------------------------------------------
## All vehicles
table(veh_stc_df$n_feedback == 0)
veh_stc_df$n_feedback %>% summary()

## Without QR codes
veh_stc_noqr_df <- veh_stc_df %>%
  dplyr::filter(shortcode_on_sticker == "yes")

table(veh_stc_noqr_df$n_feedback == 0)
veh_stc_noqr_df$n_feedback %>% summary()

# QR responses on vehicles with just QR ----------------------------------------
df_tmp <- veh_df %>%
  group_by(shortcode_on_sticker) %>%
  dplyr::summarise(n_feedback = sum(n_feedback),
                   n_feedback_qr = sum(n_feedback_qr),
                   n_feedback_sms = sum(n_feedback_sms)) %>%
  ungroup()

df_tmp

df_tmp$n_feedback[df_tmp$shortcode_on_sticker %in% "no"] / sum(df_tmp$n_feedback)

# N comments just a number -----------------------------------------------------

fb_comment_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_class_main_cmntfilterFALSE_dstnctpassTRUE.Rds"))

fb_comment_df <- fb_comment_df %>%
  dplyr::filter(!is.na(q_comment)) %>%
  dplyr::mutate(q_comment_clean = q_comment %>%
                  str_replace_all("[:digit:]", ""),
                q_comment_clean_nchar = nchar(q_comment_clean))

# 5% just a number.
mean(fb_comment_df$q_comment_clean_nchar == 0)

mean(fb_comment_df$q_comment_nchar <= 2)

# Installation dates -----------------------------------------------------------
gps_install_df <- read_dta(file.path(data_dir, "RawData", "gps_install_survey.dta"))
gps_install_df$gpssrvy_submissiondate %>% min()
gps_install_df$gpssrvy_submissiondate %>% max()

gps_sticker_install_df <- read_dta(file.path(data_dir, "RawData", "gps_vehicle_sticker_install_survey.dta"))
gps_sticker_install_df$sticker_install_date %>% min()
gps_sticker_install_df$sticker_install_date %>% max()

feedback_df <- read_dta(file.path(data_dir, "RawData", "passenger_feedback.dta"))
feedback_df %>%
  group_by(pilot_number) %>%
  dplyr::summarise(date_min = min(date),
                   date_max = max(date)) %>%
  ungroup()





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


  



