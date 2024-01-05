# Summary Statistics

# Load data --------------------------------------------------------------------
#### Passenger feedback
fb_all_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean.Rds"))
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

#### Vehicle level data
veh_df        <- readRDS(file.path(data_dir, "FinalData", "vehicle_level.Rds"))
veh_stc_df    <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers.Rds"))
veh_stc_tl_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_telematics.Rds"))

# N Passenger Feedback ---------------------------------------------------------
## Total start survey
nrow(fb_all_df)

## Unknown reg no
table(fb_all_df$regno == "UNKNOWN")
mean(fb_all_df$regno == "UNKNOWN")

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


