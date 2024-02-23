# Telematics Figure

# Load data --------------------------------------------------------------------
veh_df    <- readRDS(file.path(ap_data_dir, "FinalData", "vehicle_level_all202.Rds"))

veh_df <- veh_df %>%
  filter(!is.na(speed_max)) %>%
  mutate(vehicle_type = case_when(
    # 14 seater
    vehicle_type == "Mazda Bongo" ~ "Hiace",
    vehicle_type == "Noah" ~ "Hiace",
    vehicle_type == "Sprinter" ~ "Hiace",
    
    vehicle_type == "Man HB4" ~ "Scania",
    
    TRUE ~ vehicle_type
  ))

veh_df$gpssrvy_matatu_seats <- as.numeric(veh_df$gpssrvy_matatu_seats)
veh_df$gpssrvy_matatu_seats[veh_df$vehicle_type == "Hiace"] %>% summary()
veh_df$gpssrvy_matatu_seats[veh_df$vehicle_type == "Scania"] %>% summary()

# Summary stats ----------------------------------------------------------------
# veh_df <- veh_df %>%
#   dplyr::filter(!is.na(gpssrvy_driver_age),
#                 !is.na(gpssrvy_driver_tenure),
#                 !is.na(gpssrvy_driver_is_matatu_owner),
#                 !is.na(vehicle_type),
#                 !is.na(distance_minmax_latlon_daily_km),
#                 !is.na(gpssrvy_route))

veh_df <- veh_df %>%
  mutate(gpssrvy_matatu_wifi = gpssrvy_matatu_wifi %>% as.numeric()) %>%
  dplyr::mutate(gpssrvy_route = ifelse(is.na(gpssrvy_route), "Other", gpssrvy_route))

veh_df$gpssrvy_matatu_wifi[veh_df$gpssrvy_matatu_wifi == -1] <- NA

sum(!is.na(veh_df$gpssrvy_driver_age))
sum(!is.na(veh_df$gpssrvy_driver_tenure))
sum(!is.na(veh_df$gpssrvy_driver_is_matatu_owner))
sum(!is.na(veh_df$vehicle_type))
sum(!is.na(veh_df$distance_minmax_latlon_daily_km))
sum(!is.na(veh_df$gpssrvy_route))

veh_df$gpssrvy_driver_age %>% summary()
veh_df$gpssrvy_driver_tenure %>% summary()
veh_df$gpssrvy_driver_is_matatu_owner %>% summary()
mean(veh_df$vehicle_type == "Hiace", na.rm = T)
mean(veh_df$vehicle_type == "Scania", na.rm = T)
veh_df$distance_minmax_latlon_daily_km %>% summary()
mean(veh_df$gpssrvy_route == "Nairobi - Eldoret", na.rm = T) %>% round(2)
mean(veh_df$gpssrvy_route == "Nairobi - Marsabit", na.rm = T) %>% round(2)
mean(veh_df$gpssrvy_route == "Nairobi - Meru", na.rm = T) %>% round(2)
mean(veh_df$gpssrvy_route == "Nairobi - Mombasa", na.rm = T) %>% round(2)
mean(veh_df$gpssrvy_route == "Nairobi - Nakuru", na.rm = T) %>% round(2)
mean(veh_df$gpssrvy_route == "Nairobi - Namanga", na.rm = T) %>% round(2)
mean(veh_df$gpssrvy_route == "Nairobi - Naruk", na.rm = T) %>% round(2)
mean(veh_df$gpssrvy_route == "Other", na.rm = T) %>% round(2)

# Regressions ------------------------------------------------------------------
make_lm_df <- function(lm, dep_var){
  lm %>%
    confint() %>%
    as.data.frame() %>%
    clean_names() %>%
    mutate(b = (x2_5_percent + x97_5_percent)/2,
           dep_var = dep_var) %>%
    rownames_to_column("var") %>%
    filter(var != "(Intercept)") 
}

veh_df <- veh_df %>%
  mutate(#gpssrvy_driver_age = gpssrvy_driver_age / 10,
         #gpssrvy_driver_tenure = gpssrvy_driver_tenure / 10,
         distance_minmax_latlon_daily_km = distance_minmax_latlon_daily_km / 100)

lm1_df <- feols(log(speed_mean)                           ~ gpssrvy_driver_age + gpssrvy_driver_tenure + gpssrvy_driver_is_matatu_owner + vehicle_type + distance_minmax_latlon_daily_km + gpssrvy_route, data = veh_df, vcov = ~gpssrvy_route) %>% make_lm_df("Average\nSpeed")
lm2_df <- feols(log(prop_time_over_80kph_base_10kph)                ~ gpssrvy_driver_age + gpssrvy_driver_tenure + gpssrvy_driver_is_matatu_owner + vehicle_type + distance_minmax_latlon_daily_km + gpssrvy_route, data = veh_df, vcov = ~gpssrvy_route) %>% make_lm_df("Percent of\nTime Drive\nOver 80 km/h")
lm3_df <- feols(log(prop_time_over_90kph_base_10kph)                ~ gpssrvy_driver_age + gpssrvy_driver_tenure + gpssrvy_driver_is_matatu_owner + vehicle_type + distance_minmax_latlon_daily_km + gpssrvy_route, data = veh_df, vcov = ~gpssrvy_route) %>% make_lm_df("Percent of\nTime Drive\nOver 90 km/h")
lm4_df <- feols(log(prop_time_over_100kph_base_10kph)               ~ gpssrvy_driver_age + gpssrvy_driver_tenure + gpssrvy_driver_is_matatu_owner + vehicle_type + distance_minmax_latlon_daily_km + gpssrvy_route, data = veh_df, vcov = ~gpssrvy_route) %>% make_lm_df("Percent of\nTime Drive\nOver 100 km/h")
lm5_df <- feols(log(rate_N_valueg_above0_5_base_10kph)              ~ gpssrvy_driver_age + gpssrvy_driver_tenure + gpssrvy_driver_is_matatu_owner + vehicle_type + distance_minmax_latlon_daily_km + gpssrvy_route, data = veh_df, vcov = ~gpssrvy_route) %>% make_lm_df("Harsh Violation\nRate")
lm6_df <- feols(log(rate_N_valueg_above0_5_acceleration_base_10kph) ~ gpssrvy_driver_age + gpssrvy_driver_tenure + gpssrvy_driver_is_matatu_owner + vehicle_type + distance_minmax_latlon_daily_km + gpssrvy_route, data = veh_df, vcov = ~gpssrvy_route) %>% make_lm_df("Harsh Acceleration\nRate")
lm7_df <- feols(log(rate_N_valueg_above0_5_brake_base_10kph)        ~ gpssrvy_driver_age + gpssrvy_driver_tenure + gpssrvy_driver_is_matatu_owner + vehicle_type + distance_minmax_latlon_daily_km + gpssrvy_route, data = veh_df, vcov = ~gpssrvy_route) %>% make_lm_df("Harsh Brake\nRate")
lm8_df <- feols(log(rate_N_valueg_above0_5_turn_base_10kph)         ~ gpssrvy_driver_age + gpssrvy_driver_tenure + gpssrvy_driver_is_matatu_owner + vehicle_type + distance_minmax_latlon_daily_km + gpssrvy_route, data = veh_df, vcov = ~gpssrvy_route) %>% make_lm_df("Harsh Turn\nRate")

lm_df <- bind_rows(lm1_df,
                   lm2_df,
                   lm3_df,
                   lm4_df,
                   lm5_df,
                   lm6_df,
                   lm7_df,
                   lm8_df) %>%
  mutate(var_clean = case_when(
    var == "gpssrvy_driver_age" ~ "Driver age",
    var == "gpssrvy_driver_tenure" ~ "Driver tenture",
    var == "gpssrvy_driver_is_matatu_owner" ~ "Matatu owner drives vehicle",
    var == "vehicle_typeScania" ~ "[Vehicle type] Scania (~50+ seats)",
    var == "distance_minmax_latlon_daily_km" ~ "Avg. distance travel (100 km)",
    var == "gpssrvy_routeNairobi - Marsabit" ~ "[Route] Nairobi - Marsabit",
    var == "gpssrvy_routeNairobi - Meru" ~ "[Route] Nairobi - Meru",
    var == "gpssrvy_routeNairobi - Mombasa" ~ "[Route] Nairobi - Mombasa",
    var == "gpssrvy_routeNairobi - Nakuru" ~ "[Route] Nairobi - Nakuru",
    var == "gpssrvy_routeNairobi - Namanga" ~ "[Route] Nairobi - Namanga",
    var == "gpssrvy_routeNairobi - Naruk" ~ "[Route] Nairobi - Naruk",
    var == "gpssrvy_routeOther" ~ "[Route] Other"
  ) %>%
    factor(levels = rev(c("Driver age",
                      "Driver tenture",
                      "Matatu owner drives vehicle",
                      "[Vehicle type] Scania (~50+ seats)",
                      "Avg. distance travel (100 km)",
                      "[Route] Nairobi - Marsabit",
                      "[Route] Nairobi - Meru",
                      "[Route] Nairobi - Mombasa",
                      "[Route] Nairobi - Nakuru",
                      "[Route] Nairobi - Namanga",
                      "[Route] Nairobi - Naruk",
                      "[Route] Other")))) %>%
  mutate(dep_var = dep_var %>%
           factor(levels = c("Average\nSpeed",
                             "Percent of\nTime Drive\nOver 80 km/h",
                             "Percent of\nTime Drive\nOver 90 km/h",
                             "Percent of\nTime Drive\nOver 100 km/h",
                             "Harsh Violation\nRate",
                             "Harsh Acceleration\nRate",
                             "Harsh Brake\nRate",
                             "Harsh Turn\nRate")))

lm_df %>%
  ggplot(aes(xmin = x2_5_percent,
             xmax = x97_5_percent,
             x = b,
             y = var_clean)) +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_linerange() +
  geom_point() + 
  labs(x = "Coefficient (+/- 95% CI)",
       y = NULL) +
  facet_wrap(~dep_var, nrow = 2) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(color = "black", size = 8.5))

ggsave(filename = file.path(brief_figures_dir, "telematics_regs.png"),
       height = 5.5, width = 8)
