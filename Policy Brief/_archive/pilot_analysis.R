# Analyse Pilots

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(ap_data_dir, "FinalData", "vehicle_level.Rds"))

# N responses per vehicle ------------------------------------------------------
veh_df %>%
  dplyr::filter(!is.na(n_feedback)) %>%
  pull(n_feedback_1wk) %>%
  sort()

veh_clean_df <- veh_df %>%
  dplyr::filter(pilot_number %in% 1:5,
                #shortcode_on_sticker = "yes",
                !is.na(award_type)) %>%
  dplyr::mutate(award = paste0(pilot_number, ": ", award_type, ": KES", award_amount)) %>%
  dplyr::mutate(n_feedback_daily = n_feedback_1wk)

veh_clean_df %>% 
  group_by(pilot_number, award_type, award_amount, shortcode_on_sticker) %>%
  dplyr::summarise(n_vehicle = n(),
                   n_min = min(n_feedback_daily),
                   n_median = median(n_feedback_daily),
                   
                   n_mean = mean(n_feedback_daily),
                   n_max = max(n_feedback_daily)) %>%
  arrange(pilot_number, award_type, award_amount) 

veh_clean_df %>%
  ggplot() +
  geom_boxplot(aes(x = n_feedback_daily,
                   y = award))

