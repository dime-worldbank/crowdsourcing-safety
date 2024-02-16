# Figures

COL_TEXT_SIZE <- 3

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_suff_feedback.Rds"))

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
                q_safety_rating_num, q_speed_rating_v1_num, q_speed_rating_v2_num,
                sentiment_snmtr, comment_driver_sntmt_code_avg) %>%
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
                y = n + 1.5,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(limits = c(1, 4.1),
                     breaks = seq(1, 4, 1),
                     labels = c("1 Very not safe",
                                "2 Not safe",
                                "3 Safe",
                                "4 Very safe")) +
  scale_y_continuous(limits = c(0, 15)) +
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
                y = n + 2,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(limits = c(1, 4.2),
                     breaks = seq(1, 4, 1),
                     labels = c("Too slow",
                                "Okay",
                                "Fast",
                                "Dangerously\nfast")) +
  scale_y_continuous(limits = c(0, 21)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "C. Vehicle average of: 'How would you\ndescribe your matatu driver's speed?") +
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
                y = n + 0.9,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(limits = c(1, 5.2),
                     breaks = seq(1, 5, 1),
                     labels = c("Very slow\n[0-10 km/h]",
                                "Slow\n[10-30]",
                                "Average\n[30-50]",
                                "Fast\n[50-80]",
                                "Very fast\n[80+]")) +
  scale_y_continuous(limits = c(0, 9)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "E. Vehicle average of 'How fast does the matatu\nseem to be going?'") +
  custom_theme

p_snmtr <- veh_stack_df %>%
  dplyr::filter(name == "sentiment_snmtr") %>%
  ggplot() +
  geom_col(aes(x = value,
               y = n),
           width = COL_WIDTH,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = value,
                y = n + 1.5,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(limits = c(-1, 1.1),
                     breaks = seq(-1, 1, 1),
                     labels = c("-1 Most Negative",
                                "0 Neutral",
                                "1 Most Positive")) +
  scale_y_continuous(limits = c(0, 18)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "G. Vehicle average of: comment sentiment") +
  custom_theme

p_driving_snmtr <- veh_stack_df %>%
  dplyr::filter(name == "comment_driver_sntmt_code_avg") %>%
  ggplot() +
  geom_col(aes(x = value,
               y = n),
           width = COL_WIDTH,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = value,
                y = n + 2.2,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(limits = c(-1.1, 1.1),
                     breaks = seq(-1, 1, 1),
                     labels = c("-1 Most Negative",
                                "0 Neutral",
                                "1 Most Positive")) +
  scale_y_continuous(limits = c(0, 27)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "I. Vehicle average of: Sentiment of driving\nrelated comments") +
  custom_theme

p_level <- ggarrange(p_safe, p_speed_1, p_speed_2, p_snmtr, p_driving_snmtr, ncol = 1)

# Percent plots ----------------------------------------------------------------
veh_stack_df <- veh_df %>%
  dplyr::select(regno, 
                q_safety_prop_unsafe, q_speed_rating_v1_fast, q_speed_rating_v1_dfast, q_speed_rating_v2_vfast,
                sentiment_snmtr_prop_un0_1,
                comment_driver_sntmt_code_compl_prop_neg) %>%
  pivot_longer(cols = -regno) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = round(value * 50) / 50) %>%
  group_by(name, value) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() 

veh_stack_sntmt_df <- veh_df %>%
  dplyr::select(regno, 
                sentiment_snmtr_prop_un0_1,
                comment_driver_sntmt_code_compl_prop_neg) %>%
  pivot_longer(cols = -regno) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(value = round(value * 10) / 10) %>%
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
                y = n + 1.75,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(-0.01, 0.22)) +
  scale_y_continuous(limits = c(0, 21)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "B. Percent of passengers that rate driving as\n'Unsafe' or 'Very unsafe'") +
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
                y = n + 1.75,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(-0.01, 0.22)) +
  scale_y_continuous(limits = c(0, 21)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "D. Percent of passengers that rate driving as\n'Fast' or 'Dangerously fast'") +
  custom_theme

p_prop_speed_v2 <- veh_stack_df %>%
  dplyr::filter(name == "q_speed_rating_v2_vfast") %>%
  ggplot() +
  geom_col(aes(x = value,
               y = n),
           #width = COL_WIDTH,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = value,
                y = n + 0.8,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(-0.01, 0.22)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "F. Percent of passengers that rate driving as\n'Very fast [80 km/h +]'") +
  custom_theme

p_prop_snmtr_neg <- veh_stack_sntmt_df %>%
  dplyr::filter(name == "sentiment_snmtr_prop_un0_1") %>%
  ggplot() +
  geom_col(aes(x = value,
               y = n),
           #width = COL_WIDTH,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = value,
                y = n + 1.5,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(-0.05, 0.77)) +
  scale_y_continuous(limits = c(0, 22)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "H. Percent of passengers that give negative\ncomment") +
  custom_theme

p_prop_sntmt_neg <- veh_stack_sntmt_df %>%
  dplyr::filter(name == "comment_driver_sntmt_code_compl_prop_neg") %>%
  ggplot() +
  geom_col(aes(x = value,
               y = n),
           #width = COL_WIDTH,
           color = "black",
           fill = "dodgerblue") +
  geom_text(aes(x = value,
                y = n + 2.2,
                label = n),
            size = COL_TEXT_SIZE) +
  scale_x_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(-0.05, 1.05)) +
  scale_y_continuous(limits = c(0, 27)) +
  labs(x = NULL,
       y = "N Vehicles",
       title = "J. Percent of passengers that give negative\ncomments on driving") +
  custom_theme

p_prop <- ggarrange(p_prop_safe, p_prop_speed_v1, p_prop_speed_v2, p_prop_snmtr_neg, p_prop_sntmt_neg, ncol = 1)

# Arrange/Export ---------------------------------------------------------------
p <- ggarrange(p_level, p_prop)

ggsave(p, filename = file.path(figures_dir, "pass_rating_distribution.png"),
       height = 7.5, width = 6)



