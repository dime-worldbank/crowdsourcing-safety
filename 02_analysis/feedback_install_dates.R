# Sticker Installation Dates

# Load data --------------------------------------------------------------------
feed_df <- readRDS(file.path(data_dir, "FinalData", 
                             "passenger_feedback_valid_main_cmntfilterFALSE_dstnctpassFALSE.Rds"))


feed_df <- feed_df %>%
  group_by(regno, pilot_number) %>%
  dplyr::summarise(datetime = min(datetime)) %>%
  ungroup()

feed_df %>%
  group_by(pilot_number) %>%
  dplyr::summarise(datetime_start = min(datetime),
                   datetime_end = max(datetime)) %>%
  ungroup()


df <- readRDS("~/Desktop/rd_imp_polyline_every10years.Rds")

