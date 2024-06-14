# Passenger Feedback vs GPS Data

theme_custom <- theme(plot.title = element_text(face = "bold", size = 8.2),
                      axis.title.y = element_text(angle = 0, vjust = 0.5, size = 7),
                      axis.title.x = element_text(size = 7),
                      plot.caption = element_text(size = 6))

# Load data --------------------------------------------------------------------
veh_df    <- readRDS(file.path(ap_data_dir, "FinalData", "vehicle_level_all202_cmntfilterFALSE_dstnctpassTRUE.Rds"))

veh_df <- veh_df %>%
  dplyr::filter(n_feedback >= 10)

# Safety rating ----------------------------------------------------------------
p1 <- veh_df %>%
  dplyr::filter(!is.na(prop_time_over_80kph_base_10kph)) %>%
  ggplot(aes(x = q_safety_rating_num,
             y = prop_time_over_80kph_base_10kph)) +
  geom_smooth(method='lm', 
              formula= y~x, 
              se=F,
              color = "orange") +
  stat_cor(method = "pearson", color = "black", size = 3) +
  geom_point(size = 2) +
  labs(x = "Average Safety Rating (Higher = Safer)\n[Passenger Feedback Data]",
       y = "Percent of\ntime vehicle\ntraveling\nover 80 km/h\n[Sensor Data]",
       title = "A. Passenger safety rating vs.\npercent of time vehicle speeds") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  theme_classic2() +
  theme_custom

# Speed rating -----------------------------------------------------------------
p2 <- veh_df %>%
  dplyr::filter(!is.na(q_speed_rating_v2_vfast)) %>%
  ggplot(aes(x = q_speed_rating_v2_vfast,
             y = prop_time_over_90kph_base_10kph)) + # prop_time_over_100kph_base_10kph
  geom_smooth(method='lm', 
              formula= y~x, 
              se=F,
              color = "orange") +
  stat_cor(method = "pearson", color = "black", size = 3) +
  geom_point(size = 2) +
  labs(x = "Percent of respondents that rate vehicle\nas going 'Very fast (80+ km/h)'\n[Passenger Feedback Data]",
       y = "Percent of\ntime vehicle\ntraveling\nover 90 km/h\n[Sensor Data]",
       title = "B. Passenger speed rating vs. percent of\ntime vehicle travels over 90 km/h") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_x_continuous(labels = percent_format(scale = 100)) +
  theme_classic2() +
  theme_custom

p2

# Export -----------------------------------------------------------------------
p <- ggarrange(p1, p2, nrow = 1)

ggsave(p,
       filename = file.path(brief_figures_dir, "speed80kph_vs_safety.png"),
       height = 2.5, width = 7)





