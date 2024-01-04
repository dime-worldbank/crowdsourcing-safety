# Figures

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers.Rds"))

veh_df[,c("q_safety_rating_num", "q_speed_rating_v1_num", "q_speed_rating_v2_num")]

# Overall Distribution =========================================================

# Safe Distribution ------------------------------------------------------------
p_safe <- veh_df %>%
  dplyr::filter(!is.na(q_safety_rating_num)) %>%
  dplyr::mutate(q_safety_rating_num = round(q_safety_rating_num * 4) / 4) %>%
  group_by(q_safety_rating_num) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x = q_safety_rating_num,
               y = n),
           width = 0.2,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = q_safety_rating_num,
                y = n + 0.6,
                label = n)) +
  scale_x_continuous(limits = c(1, 4.1),
                     breaks = seq(1, 4, 0.5)) +
  scale_y_continuous(limits = c(0, 14)) +
  labs(x = "Vehicle Average",
       y = "N Vehicles",
       title = "A. Vehicle averages of: 'How safely is your matatu being driven?'",
       subtitle = "1 = 'Very Unsafe'; 4 = 'Very Safe'") +
  theme_classic2() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6),
        axis.title = element_text(size = 8)) 

# Speed Distribution -----------------------------------------------------------
p_speed <- veh_df %>%
  dplyr::filter(!is.na(q_speed_rating_v2_num)) %>%
  dplyr::mutate(q_speed_rating_v2_num = round(q_speed_rating_v2_num * 4) / 4) %>%
  group_by(q_speed_rating_v2_num) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x = q_speed_rating_v2_num,
               y = n),
           width = 0.2,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = q_speed_rating_v2_num,
                y = n + 0.6,
                label = n)) +
  scale_x_continuous(limits = c(1, 4.1),
                     breaks = seq(1, 4, 0.5)) +
  scale_y_continuous(limits = c(0, 14)) +
  labs(x = "Vehicle Average",
       y = NULL,
       title = "B. Vehicle averages of: 'How fast does the matatu seem to be going?'",
       subtitle = "1 = 'Very slow [0-10 km/h]'; 5 = 'Very fast [80+]'") +
  theme_classic2() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6),
        axis.title = element_text(size = 8)) 

# Arrange / export -------------------------------------------------------------
p <- ggarrange(p_safe, p_speed,
               nrow = 1) 

ggsave(p, filename = file.path(figures_dir, "safe_speed_distribution.png"),
       height = 2.25, width = 8)

# Unsafe Distribution ==========================================================

# Proportion rate as unsafe ----------------------------------------------------
p_unsafe <- veh_df %>%
  dplyr::filter(!is.na(q_safety_rating_num)) %>%
  dplyr::mutate(q_safety_prop_unsafe = round(q_safety_prop_unsafe * 50) / 50) %>%
  
  group_by(q_safety_prop_unsafe) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  
  ggplot() +
  geom_col(aes(x = q_safety_prop_unsafe,
               y = n),
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = q_safety_prop_unsafe,
                y = n + 1,
                label = n)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  scale_y_continuous(limits = c(0, 13)) +
  labs(x = "Percent",
       y = "N Vehicles",
       title = "A. Percent of respondents that rate driving as 'Unsafe' or\n'Very unsafe'") +
  theme_classic2() +
  theme(plot.title = element_text(size = 7.9, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6),
        axis.title = element_text(size = 8)) 

# Proportion rate as fast ------------------------------------------------------
p_fast <- veh_df %>%
  dplyr::filter(!is.na(q_speed_rating_v2_fast)) %>%
  dplyr::mutate(q_speed_rating_v2_fast = round(q_speed_rating_v2_fast * 50) / 50) %>%
  
  group_by(q_speed_rating_v2_fast) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  
  ggplot() +
  geom_col(aes(x = q_speed_rating_v2_fast,
               y = n),
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = q_speed_rating_v2_fast,
                y = n + 1,
                label = n)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(-0.01, 0.25)) +
  scale_y_continuous(limits = c(0, 13)) +
  labs(x = "Percent",
       y = NULL,
       title = "B. Percent of respondents that indicate matatu is going\n'Very fast [80+]'") +
  theme_classic2() +
  theme(plot.title = element_text(size = 7.9, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6),
        axis.title = element_text(size = 8)) 

# Arrange / export -------------------------------------------------------------
p <- ggarrange(p_unsafe, p_fast,
               nrow = 1) 

ggsave(p, filename = file.path(figures_dir, "unsafe_fast_distribution.png"),
       height = 2.25, width = 8)

