# Figures

COL_TEXT_SIZE <- 3

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers.Rds"))

custom_theme <- theme_classic2() +
  theme(plot.title = element_text(size = 8, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text = element_text(color = "black", size = 7)) 

# Level plots ------------------------------------------------------------------
COL_WIDTH <- 0.18

veh_stack_df <- veh_df %>%
  dplyr::select(regno, 
                q_safety_rating_num, q_speed_rating_v1_num, q_speed_rating_v2_num) %>%
  pivot_longer(cols = -regno) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = round(value * 5) / 5) %>%
  group_by(name, value) %>%
  group_by(name, value) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() 

p_safe <- veh_stack_df %>%
  dplyr::filter(name == "q_safety_rating_num") %>%
  ggplot() +
  geom_col(aes(x = value,
               y = n),
           width = COL_WIDTH,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = value,
                y = n + 1,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(limits = c(1, 4.1),
                     breaks = seq(1, 4, 1),
                     labels = c("1 Very not safe",
                                "2 Not safe",
                                "3 Safe",
                                "4 Very safe")) +
  #scale_y_continuous(limits = c(0, 13)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "A. Vehicle average of: 'How safely is your matatu\nbeing driven?'") +
  custom_theme

p_speed_1 <- veh_stack_df %>%
  dplyr::filter(name == "q_speed_rating_v1_num") %>%
  ggplot() +
  geom_col(aes(x = value,
               y = n),
           width = COL_WIDTH,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = value,
                y = n + 1,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(limits = c(1, 4.2),
                     breaks = seq(1, 4, 1),
                     labels = c("Too slow",
                                "Okay",
                                "Fast",
                                "Dangerously\nfast")) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "B. Vehicle average of: 'How would you\ndescribe your matatu driver's speed?") +
  custom_theme

p_speed_2 <- veh_stack_df %>%
  dplyr::filter(name == "q_speed_rating_v2_num") %>%
  ggplot() +
  geom_col(aes(x = value,
               y = n),
           width = COL_WIDTH,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = value,
                y = n + 0.6,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(limits = c(1, 5.2),
                     breaks = seq(1, 5, 1),
                     labels = c("Very slow\n[0-10 km/h]",
                                "Slow\n[10-30]",
                                "Average\n[30-50]",
                                "Fast\n[50-80]",
                                "Very fast\n[80+]")) +
  scale_y_continuous(limits = c(0, 7)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "C. Vehicle average of 'How fast does the matatu\nseem to be going?'") +
  custom_theme

p_level <- ggarrange(p_safe, p_speed_1, p_speed_2, ncol = 1)

# Percent plots ----------------------------------------------------------------
veh_stack_df <- veh_df %>%
  dplyr::select(regno, 
                q_safety_prop_unsafe, q_speed_rating_v1_fast, q_speed_rating_v1_dfast, q_speed_rating_v2_fast) %>%
  pivot_longer(cols = -regno) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = round(value * 50) / 50) %>%
  group_by(name, value) %>%
  group_by(name, value) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() 

p_prop_safe <- veh_stack_df %>%
  dplyr::filter(name == "q_safety_prop_unsafe") %>%
  ggplot() +
  geom_col(aes(x = value,
               y = n),
           #width = COL_WIDTH,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = value,
                y = n + 1,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(-0.01, 0.3)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "D. Percent of passengers that rate driving as\n'Unsafe' or 'Very unsafe'") +
  custom_theme

p_prop_speed_v1 <- veh_stack_df %>%
  dplyr::filter(name == "q_speed_rating_v1_dfast") %>%
  ggplot() +
  geom_col(aes(x = value,
               y = n),
           #width = COL_WIDTH,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = value,
                y = n + 1,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(-0.01, 0.3)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "E. Percent of passengers that rate driving as\n'Fast' or 'Dangerously fast'") +
  custom_theme

p_prop_speed_v2 <- veh_stack_df %>%
  dplyr::filter(name == "q_speed_rating_v2_fast") %>%
  ggplot() +
  geom_col(aes(x = value,
               y = n),
           #width = COL_WIDTH,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = value,
                y = n + 1,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(-0.01, 0.3)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "F. Percent of passengers that rate driving as\n'Very fast [80 km/h +]'") +
  custom_theme

p_prop <- ggarrange(p_prop_safe, p_prop_speed_v1, p_prop_speed_v2, ncol = 1)

# Arrange/Export ---------------------------------------------------------------
p <- ggarrange(p_level, p_prop)

ggsave(p, filename = file.path(figures_dir, "pass_rating_distribution.png"),
       height = 4.5, width = 6)



# 
# 
# 
# 
# 
# 
# 
# 
# # Overall Distribution =========================================================
# 
# # Safe Distribution ------------------------------------------------------------
# p_safe <- veh_df %>%
#   dplyr::filter(!is.na(q_safety_rating_num)) %>%
#   dplyr::mutate(q_safety_rating_num = round(q_safety_rating_num * 4) / 4) %>%
#   group_by(q_safety_rating_num) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   ggplot() +
#   geom_col(aes(x = q_safety_rating_num,
#                y = n),
#            width = 0.2,
#            color = "black",
#            fill = "dodgerblue") +
#   geom_text(aes(x = q_safety_rating_num,
#                 y = n + 0.6,
#                 label = n)) +
#   scale_x_continuous(limits = c(1, 4.1),
#                      breaks = seq(1, 4, 0.5)) +
#   scale_y_continuous(limits = c(0, 14)) +
#   labs(x = "Vehicle Average",
#        y = "N Vehicles",
#        title = "A. Vehicle averages of: 'How safely is your matatu being driven?'",
#        subtitle = "1 = 'Very Unsafe'; 4 = 'Very Safe'") +
#   theme_classic2() +
#   theme(plot.title = element_text(size = 8, face = "bold"),
#         plot.subtitle = element_text(size = 8),
#         plot.caption = element_text(size = 6),
#         axis.title = element_text(size = 8)) 
# 
# # Speed Distribution -----------------------------------------------------------
# p_speed <- veh_df %>%
#   dplyr::filter(!is.na(q_speed_rating_v2_num)) %>%
#   dplyr::mutate(q_speed_rating_v2_num = round(q_speed_rating_v2_num * 4) / 4) %>%
#   group_by(q_speed_rating_v2_num) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   ggplot() +
#   geom_col(aes(x = q_speed_rating_v2_num,
#                y = n),
#            width = 0.2,
#            color = "black",
#            fill = "dodgerblue") +
#   geom_text(aes(x = q_speed_rating_v2_num,
#                 y = n + 0.6,
#                 label = n)) +
#   scale_x_continuous(limits = c(1, 4.1),
#                      breaks = seq(1, 4, 0.5)) +
#   scale_y_continuous(limits = c(0, 14)) +
#   labs(x = "Vehicle Average",
#        y = NULL,
#        title = "B. Vehicle averages of: 'How fast does the matatu seem to be going?'",
#        subtitle = "1 = 'Very slow [0-10 km/h]'; 5 = 'Very fast [80+]'") +
#   theme_classic2() +
#   theme(plot.title = element_text(size = 8, face = "bold"),
#         plot.subtitle = element_text(size = 8),
#         plot.caption = element_text(size = 6),
#         axis.title = element_text(size = 8)) 
# 
# # Arrange / export -------------------------------------------------------------
# p <- ggarrange(p_safe, p_speed,
#                nrow = 1) 
# 
# ggsave(p, filename = file.path(figures_dir, "safe_speed_distribution.png"),
#        height = 2.25, width = 8)
# 
# # Unsafe Distribution ==========================================================
# 
# # Proportion rate as unsafe ----------------------------------------------------
# p_unsafe <- veh_df %>%
#   dplyr::filter(!is.na(q_safety_rating_num)) %>%
#   dplyr::mutate(q_safety_prop_unsafe = round(q_safety_prop_unsafe * 50) / 50) %>%
#   
#   group_by(q_safety_prop_unsafe) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   
#   ggplot() +
#   geom_col(aes(x = q_safety_prop_unsafe,
#                y = n),
#            color = "black",
#            fill = "dodgerblue") +
#   geom_text(aes(x = q_safety_prop_unsafe,
#                 y = n + 1,
#                 label = n)) +
#   scale_x_continuous(labels = scales::percent_format(scale = 100)) +
#   scale_y_continuous(limits = c(0, 13)) +
#   labs(x = "Percent",
#        y = "N Vehicles",
#        title = "A. Percent of respondents that rate driving as 'Unsafe' or\n'Very unsafe'") +
#   theme_classic2() +
#   theme(plot.title = element_text(size = 7.9, face = "bold"),
#         plot.subtitle = element_text(size = 8),
#         plot.caption = element_text(size = 6),
#         axis.title = element_text(size = 8)) 
# 
# # Proportion rate as fast ------------------------------------------------------
# p_fast <- veh_df %>%
#   dplyr::filter(!is.na(q_speed_rating_v2_fast)) %>%
#   dplyr::mutate(q_speed_rating_v2_fast = round(q_speed_rating_v2_fast * 50) / 50) %>%
#   
#   group_by(q_speed_rating_v2_fast) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   
#   ggplot() +
#   geom_col(aes(x = q_speed_rating_v2_fast,
#                y = n),
#            color = "black",
#            fill = "dodgerblue") +
#   geom_text(aes(x = q_speed_rating_v2_fast,
#                 y = n + 1,
#                 label = n)) +
#   scale_x_continuous(labels = scales::percent_format(scale = 100),
#                      limits = c(-0.01, 0.25)) +
#   scale_y_continuous(limits = c(0, 13)) +
#   labs(x = "Percent",
#        y = NULL,
#        title = "B. Percent of respondents that indicate matatu is going\n'Very fast [80+]'") +
#   theme_classic2() +
#   theme(plot.title = element_text(size = 7.9, face = "bold"),
#         plot.subtitle = element_text(size = 8),
#         plot.caption = element_text(size = 6),
#         axis.title = element_text(size = 8)) 
# 
# # Arrange / export -------------------------------------------------------------
# p <- ggarrange(p_unsafe, p_fast,
#                nrow = 1) 
# 
# ggsave(p, filename = file.path(figures_dir, "unsafe_fast_distribution.png"),
#        height = 2.25, width = 8)
# 
