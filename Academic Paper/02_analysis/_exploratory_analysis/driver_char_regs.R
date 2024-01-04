# Driver Characteristic Regressions

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level.Rds"))

veh_df <- veh_df %>%
  dplyr::mutate(prop_time_over_90kph_base_10kph_ln = log(prop_time_over_90kph_base_10kph + 1))

lm(prop_time_over_90kph_base_10kph ~ factor(gpssrvy_route), data = veh_df) %>%
  summary()

lm(prop_time_over_80kph_base_10kph ~ factor(gpssrvy_stop_type), data = veh_df) %>%
  summary()

veh_df$gpssrvy_route

plot(veh_df$gpssrvy_driver_tenure,
     veh_df$prop_time_over_80kph_base_10kph)

veh_df$gpssrvy_driver_tenure

veh_df$prop_time_over_90kph_base_10kph %>% log() %>% hist()
