# Consistency of Indicators

# Load data --------------------------------------------------------------------
sensor_df   <- readRDS(file.path(data_dir, "RawData", "sensor_day.Rds"))
st_insll_df <- readRDS(file.path(data_dir, "RawData", "gps_vehicle_sticker_install_survey.Rds"))
feedback_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_class_main_cmntfilterFALSE_dstnctpassTRUE.Rds"))

# Telematics consistency -------------------------------------------------------
st_insll_df <- st_insll_df %>%
  distinct(regno, .keep_all = T)

sensor_df <- sensor_df %>%
  left_join(st_insll_df, by = "regno") %>%
  filter(!is.na(sticker_install_date))

sensor_week_df <- sensor_df %>%
  mutate(date_week = date %>% floor_date(unit = "week"),
         time_over_100kph_secs = time_over_80kph_secs / time_over_10kph_secs,
         year = date_week %>% year) %>%
  filter(year == 2022) %>%
  group_by(regno, date_week) %>%
  summarise(across(c(time_over_100kph_secs,
                     N_valueg_above0_5), mean, na.rm = TRUE)) %>%
  ungroup()

p_speed <- sensor_week_df %>%
  ggplot() +
  geom_col(aes(x = date_week,
               y = time_over_100kph_secs),
           fill = "firebrick4") +
  labs(x = NULL,
       y = "Proportion",
       title = "A. Proportion of time driving over 100 km/h") +
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
             nrow = 3)

p_harsh <- sensor_week_df %>%
  ggplot() +
  geom_col(aes(x = date_week,
               y = N_valueg_above0_5),
           fill = "firebrick4") +
  labs(x = NULL,
       y = "N",
       title = "B. Number of Harsh Driving Violations") +
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
             nrow = 3)

p <- ggarrange(p_speed,
               p_harsh, 
               ncol = 1)

ggsave(p,
       filename = file.path(figures_dir, 
                            "telematics_consistency.png"),
       height = 7, width = 8)

# Feedback consistency: safety -------------------------------------------------
feedback_sum_df <- feedback_df %>%
  mutate(date_week = floor_date(date, unit = "week")) %>%
  dplyr::filter(!is.na(q_safety_rating_num)) %>%
  group_by(regno, date_week) %>%
  dplyr::summarise(n_response = n(),
                   q_safety_rating_mean = mean(q_safety_rating_num),
                   q_safety_rating_safe_prop = mean(q_safety_rating_num %in% 3:4),
                   q_safety_rating_unsafe_prop = mean(q_safety_rating_num %in% 1:2),
                   
                   q_safety_rating_safe_sum = sum(q_safety_rating_num %in% 3:4),
                   q_safety_rating_unsafe_sum = sum(q_safety_rating_num %in% 1:2)) %>%
  ungroup() %>%
  
  filter(n_response >= 5) %>%
  group_by(regno) %>%
  mutate(n_veh_weeks = n(),
         n_response_mean = mean(n_response)) %>%
  ungroup() %>%
  
  filter(n_veh_weeks >= 3) %>%
  
  mutate(regno = paste0(regno, "\nAvg. Weekly N: ", 
                        round(n_response_mean, 0)))

feedback_sum_stack_prop_df <- feedback_sum_df %>%
  pivot_longer(cols = c(q_safety_rating_safe_prop,
                        q_safety_rating_unsafe_prop)) %>%
  mutate(name = case_when(
    name == "q_safety_rating_safe_prop" ~ "Safe",
    name == "q_safety_rating_unsafe_prop" ~ "Unsafe"
  ) %>%
    factor(levels = c("Unsafe", "Safe"))) 

p_feed_safe <- feedback_sum_stack_prop_df %>%
  ggplot() +
  geom_col(aes(x = date_week,
               y = value,
               fill = name)) +
  scale_fill_manual(values = c("firebrick",
                               "dodgerblue")) +
  labs(x = NULL,
       y = "Proportion",
       fill = "Passenger\nFeedback\nRating",
       title = "A. Passenger safety rating") + 
  scale_x_date(date_breaks = "1 months", date_labels = "%b") + 
  theme_classic2() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 10),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "gray90"),
        strip.text = element_text(size = 7)) +
  facet_wrap(~regno,
             scales = "free_x",
             nrow = 2)

# Feedback consistency: speed --------------------------------------------------
feedback_sum_df <- feedback_df %>%
  mutate(date_week = floor_date(date, unit = "week")) %>%
  dplyr::filter(!is.na(q_speed_rating_v2)) %>%
  group_by(regno, date_week) %>%
  dplyr::summarise(n_response = n(),
                   q_speed_rating_v2_mean = mean(q_speed_rating_v2_num),
                   
                   q_speed_rating_v2_slow_prop = mean(q_speed_rating_v2_num %in% 1:3),
                   q_speed_rating_v2_fast_prop = mean(q_speed_rating_v2_num %in% 4),
                   q_speed_rating_v2_vfast_prop = mean(q_speed_rating_v2_num %in% 5),
                   
                   q_speed_rating_v2_slow_sum = sum(q_speed_rating_v2_num %in% 1:3),
                   q_speed_rating_v2_fast_sum = sum(q_speed_rating_v2_num %in% 4),
                   q_speed_rating_v2_vfast_sum = sum(q_speed_rating_v2_num %in% 5)) %>%
  ungroup() %>%
  
  filter(n_response >= 5) %>%
  group_by(regno) %>%
  mutate(n_veh_weeks = n(),
         n_response_mean = mean(n_response)) %>%
  ungroup() %>%
  
  filter(n_veh_weeks >= 3) %>%
  
  mutate(regno = paste0(regno, "\nAvg. Weekly N: ", 
                        round(n_response_mean, 0)))

feedback_sum_stack_prop_df <- feedback_sum_df %>%
  pivot_longer(cols = c(q_speed_rating_v2_slow_prop,
                        q_speed_rating_v2_fast_prop,
                        q_speed_rating_v2_vfast_prop)) %>%
  mutate(name = case_when(
    name == "q_speed_rating_v2_slow_prop" ~ "Slow - Average",
    name == "q_speed_rating_v2_fast_prop" ~ "Fast",
    name == "q_speed_rating_v2_vfast_prop" ~ "Very Fast"
  ) %>%
    factor(levels = c("Very Fast", "Fast", "Slow - Average"))) 

p_feed_speed <- feedback_sum_stack_prop_df %>%
  ggplot() +
  geom_col(aes(x = date_week,
               y = value,
               fill = name)) +
  scale_fill_manual(values = c("firebrick",
                               "orange",
                               "dodgerblue")) +
  labs(x = NULL,
       y = "Proportion",
       fill = "Passenger\nFeedback\nRating",
       title = "B. Passenger speed rating") + 
  scale_x_date(date_breaks = "1 months", date_labels = "%b") + 
  theme_classic2() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text = element_text(size = 7),
        plot.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "gray90"),
        strip.text = element_text(size = 7)) +
  facet_wrap(~regno,
             scales = "free_x",
             nrow = 2)
#p_feed_speed

# Feedback consistency: export --------------------------------------------------
p <- ggarrange(p_feed_safe,
               p_feed_speed,
               ncol = 1)

ggsave(p, filename = file.path(figures_dir,
                               "feedback_consistency.png"),
       height = 5, width = 10)

