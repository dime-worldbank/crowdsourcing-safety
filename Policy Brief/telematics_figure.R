# Telematics Figure

# Load data --------------------------------------------------------------------
veh_df    <- readRDS(file.path(ap_data_dir, "FinalData", "vehicle_level_all202_cmntfilterFALSE_dstnctpassTRUE.Rds"))
sensor_df <- readRDS(file.path(ap_data_dir, "RawData", "sensor_day.Rds"))

veh_df <- veh_df %>%
  filter(!is.na(speed_max))

# Distribution -----------------------------------------------------------------
p_dist <- veh_df %>%
  dplyr::select(regno,
                prop_time_over_80kph_base_10kph,
                prop_time_over_100kph_base_10kph,
                prop_time_over_90kph_base_80kph,
                prop_time_over_100kph_base_80kph,
                rate_N_valueg_above0_5_base_10kph) %>%
  pivot_longer(cols = -regno) %>%
  mutate(name_clean = case_when(
    name == "prop_time_over_80kph_base_10kph" ~ "Proportion of time\ntraveling over\n80 km/h",
    name == "prop_time_over_100kph_base_10kph" ~ "Proportion of time\ntraveling over\n100 km/h",
    #name == "prop_time_over_120kph_base_10kph" ~ "Proportion of time\ntraveling over 120 km/h",
    name == "prop_time_over_90kph_base_80kph" ~ "Proportion of time\ntraveling over\n90 km/h relative to\ntime over 80 km/h",
    name == "prop_time_over_100kph_base_80kph" ~ "Proportion of time\ntraveling over\n100 km/h relative to\ntime over 80 km/h",
    name == "rate_N_valueg_above0_5_base_10kph" ~ "N harsh driving\nviolations per hour"
  ) %>%
    factor(levels = c("Proportion of time\ntraveling over\n80 km/h",
                      "Proportion of time\ntraveling over\n100 km/h",
                     # "Proportion of time\ntraveling over\n120 km/h",
                      "Proportion of time\ntraveling over\n90 km/h relative to\ntime over 80 km/h",
                      "Proportion of time\ntraveling over\n100 km/h relative to\ntime over 80 km/h",
                      "N harsh driving\nviolations per hour"))) %>%
  
  ggplot() +
  geom_histogram(aes(x = value,
                     fill = name_clean),
                 color = "black") +
  facet_wrap(~name_clean,
             scales = "free",
             nrow = 1) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "A. Distribution of select telematics indicators") +
  scale_fill_manual(values = c("darkorange",
                                "darkorange",
                                "gold1",
                                "gold1",
                                "coral1")) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 10),
        legend.position = "none") 

# Consistency ------------------------------------------------------------------
sensor_week_df <- sensor_df %>%
  mutate(date_week = date %>% floor_date(unit = "week"),
         time_over_100kph_secs = time_over_80kph_secs / time_over_10kph_secs,
         N_valueg_above0_5     = N_valueg_above0_5 / (time_over_10kph_secs/60),
         year = date_week %>% year) %>%
  filter(year == 2022) %>%
  group_by(regno, date_week) %>%
  summarise(across(c(time_over_100kph_secs,
                     N_valueg_above0_5), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  
  group_by(regno) %>%
  dplyr::mutate(N_valueg_above0_5 = Winsorize(N_valueg_above0_5, probs = c(0, 0.95), na.rm = T),
                speed_rank = mean(time_over_100kph_secs, na.rm = T),
                harsh_rank = mean(N_valueg_above0_5, na.rm = T),
                n_weeks = sum(!is.na(speed_rank))) %>%
  ungroup() %>%
  
  dplyr::filter(n_weeks >= 35,
                harsh_rank != Inf) %>%
  
  mutate(speed_rank = speed_rank %>% as.factor() %>% as.numeric(),
         harsh_rank = harsh_rank %>% as.factor() %>% as.numeric())

p_speed <- sensor_week_df %>%
  dplyr::filter( (speed_rank >= 142) ) %>% # (speed_rank >= 146) | (speed_rank <= 4)
  dplyr::mutate(regno = reorder(regno, -speed_rank)) %>%
  ggplot() +
  geom_col(aes(x = date_week,
               y = time_over_100kph_secs),
           fill = "firebrick4") +
  labs(x = NULL,
       y = "Percent",
       title = "B. Percent of time driving over 100 km/h, vehicles with highest percentages") +
  scale_y_continuous(limits = c(0, 0.65), labels = scales::percent) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b") + 
  theme_classic2() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(face = "bold", size = 10),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "gray90"),
        strip.text = element_text(size = 8)) +
  facet_wrap(~regno,
             nrow = 1) 

p_harsh <- sensor_week_df %>%
  dplyr::filter( (harsh_rank >= 142) ) %>% # (harsh_rank >= 146) | (harsh_rank <= 4)
  dplyr::mutate(regno = reorder(regno, -harsh_rank)) %>%
  ggplot() +
  geom_col(aes(x = date_week,
               y = N_valueg_above0_5),
           fill = "firebrick4") +
  labs(x = NULL,
       y = "Rate",
       title = "C. Harsh driving violation rates (number of harsh violation events per hour), vehicles with highest rates") +
  scale_y_continuous(limits = c(0, 0.21)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b") + 
  theme_classic2() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(face = "bold", size = 10),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "gray90"),
        strip.text = element_text(size = 8)) +
  facet_wrap(~regno,
             nrow = 1) 

# Correlation ------------------------------------------------------------------
veh_cor_df <- veh_df %>%
  dplyr::mutate(prop_time_over_100kph_base_10kph = Winsorize(prop_time_over_100kph_base_10kph, probs = c(0, 0.99), na.rm = T),
                prop_time_over_90kph_base_10kph = Winsorize(prop_time_over_90kph_base_10kph, probs = c(0, 0.99), na.rm = T),
                prop_time_over_80kph_base_10kph = Winsorize(prop_time_over_80kph_base_10kph, probs = c(0, 0.99), na.rm = T),
                
                prop_time_over_100kph_base_80kph = Winsorize(prop_time_over_100kph_base_80kph, probs = c(0, 0.99), na.rm = T),
                prop_time_over_90kph_base_80kph = Winsorize(prop_time_over_90kph_base_80kph, probs = c(0, 0.99), na.rm = T),
                
                rate_N_valueg_above0_5_base_10kph = Winsorize(rate_N_valueg_above0_5_base_10kph, probs = c(0, 0.99), na.rm = T),
                rate_N_valueg_above0_5_acceleration_base_10kph = Winsorize(rate_N_valueg_above0_5_acceleration_base_10kph, probs = c(0, 0.99), na.rm = T),
                rate_N_valueg_above0_5_brake_base_10kph = Winsorize(rate_N_valueg_above0_5_brake_base_10kph, probs = c(0, 0.99), na.rm = T),
                rate_N_valueg_above0_5_turn_base_10kph = Winsorize(rate_N_valueg_above0_5_turn_base_10kph, probs = c(0, 0.99), na.rm = T)) 

cor_theme <- theme(plot.title = element_text(face = "bold", size = 10),
                   axis.title = element_text(size = 10))

p_cor_1 <- veh_cor_df %>%
  ggplot(aes(x = prop_time_over_80kph_base_10kph,
             y = rate_N_valueg_above0_5_base_10kph)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, se = F, color = "orange") +
  stat_cor(method = "pearson", label.x = 0.2, color = "black") +
  theme_classic2() +
  cor_theme +
  labs(x = "Proportion of time travel over 80 km/h",
       y = "Harsh violations, N per hour",
       title = "D. Speeding over 80 km/h vs\nharsh violations")

# p_cor_2 <- veh_cor_df %>%
#   ggplot(aes(x = prop_time_over_90kph_base_10kph,
#              y = rate_N_valueg_above0_5_base_10kph)) +
#   geom_point() +
#   geom_smooth(method='lm', formula= y~x, se = F, color = "orange") +
#   stat_cor(method = "pearson", label.x = 0.1, color = "black") +
#   theme_classic2() +
#   cor_theme +
#   labs(x = "Proportion of time travel over 90 km/h",
#        y = "Harsh violations, N per hour",
#        title = "E. Speeding over 90 km/h vs\nharsh violations")
# 
# p_cor_3 <- veh_cor_df %>%
#    ggplot(aes(x = prop_time_over_100kph_base_10kph,
#               y = rate_N_valueg_above0_5_base_10kph)) +
#   geom_point() +
#   geom_smooth(method='lm', formula= y~x, se = F, color = "orange") +
#   stat_cor(method = "pearson", label.x = 0.1, color = "black") +
#   theme_classic2() +
#   cor_theme +
#   labs(x = "Proportion of time travel over 100 km/h",
#        y = "Harsh violations, N per hour",
#        title = "F. Speeding over 100 km/h vs\nharsh violations")

p_cor_2 <- veh_cor_df %>%
  ggplot(aes(x = rate_N_valueg_above0_5_acceleration_base_10kph,
             y = rate_N_valueg_above0_5_brake_base_10kph)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, se = F, color = "orange") +
  stat_cor(method = "pearson", label.y = 0.75, color = "black") +
  theme_classic2() +
  cor_theme +
  labs(x = "Harsh acceleration, N per hour",
       y = "Harsh braking, N per hour",
       title = "E. Harsh acceleration vs\nharsh braking")

p_cor_3 <- veh_cor_df %>%
  ggplot(aes(x = rate_N_valueg_above0_5_acceleration_base_10kph,
             y = rate_N_valueg_above0_5_turn_base_10kph)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, se = F, color = "orange") +
  stat_cor(method = "pearson", label.x = 0, color = "black") +
  theme_classic2() +
  cor_theme +
  labs(x = "Harsh acceleration, N per hour",
       y = "Harsh turning, N per hour",
       title = "F. Harsh acceleration vs\nharsh turning")

p_cor_4 <- veh_cor_df %>%
  ggplot(aes(x = rate_N_valueg_above0_5_brake_base_10kph,
             y = rate_N_valueg_above0_5_turn_base_10kph)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, se = F, color = "orange") +
  stat_cor(method = "pearson", label.x = 0, color = "black") +
  theme_classic2() +
  cor_theme +
  labs(x = "Harsh braking, N per hour",
       y = "Harsh turning, N per hour",
       title = "G. Harsh braking vs\nharsh turning")

# Arrange/export ---------------------------------------------------------------
p_cor_12 <- ggarrange(p_cor_1, p_cor_2, nrow = 1)
p_cor_34 <- ggarrange(p_cor_3, p_cor_4, nrow = 1)

p <- ggarrange(p_dist,
               p_speed,
               p_harsh,
               p_cor_12,
               p_cor_34,
               ncol = 1)

ggsave(p, filename = file.path(brief_figures_dir, "telematics_info.png"),
       height = 11, width = 8.5) # 8.5

