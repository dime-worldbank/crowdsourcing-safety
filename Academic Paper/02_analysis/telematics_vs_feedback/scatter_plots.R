# Passenger Feedback vs GPS Data

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_telematics_suff_feedback.Rds"))

p_safe <- veh_df %>%
  dplyr::filter(!is.na(prop_time_over_80kph_base_10kph),
                !is.na(q_safety_rating_num)) %>%
  ggplot(aes(x = q_safety_rating_num,
             y = prop_time_over_80kph_base_10kph)) +
  geom_smooth(method='lm', 
              formula= y~x, 
              se=F,
              color = "orange") +
  geom_point(size = 2) +
  labs(x = "Average Safety Rating (Higher = Safer)",
       y = "Percent of\ntime vehicle\ntraveling\nabove 80 km/h",
       title = "A. Percent of time traveling above 80 km/h vs passenger safety rating") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  theme_classic2() +
  theme(plot.title = element_text(size = 8.2, face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 7),
        axis.title.x = element_text(size = 7),
        plot.caption = element_text(size = 6))

p_speed <- veh_df %>%
  dplyr::filter(!is.na(prop_time_over_120kph_base_80kph),
                !is.na(q_speed_rating_v2_fast)) %>%
  ggplot(aes(x = q_speed_rating_v2_fast,
             y = prop_time_over_120kph_base_80kph)) +
  geom_smooth(method='lm', 
              formula= y~x, 
              se=F,
              color = "orange") +
  geom_point(size = 2) +
  labs(x = "Percent of passengers who rate vehicles as traveling very fast (> 80 km/h)",
       y = "Percent of\ntime vehicle\ntraveling\nabove 110 km/h\nwhen >80 km/h",
       title = "B. Percent of time traveling above 120 km/h when >80 km/h vs proportion\nof passengers who rate vehicles as traveling very fast (> 80 km/h)") +
  scale_x_continuous(labels = percent_format(scale = 100)) +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  theme_classic2() +
  theme(plot.title = element_text(size = 8.2, face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 7),
        axis.title.x = element_text(size = 7),
        plot.caption = element_text(size = 6))

p <- ggarrange(p_safe, p_speed, nrow = 1)

ggsave(p, filename = file.path(figures_dir, "telematics_vs_feedback_scatter.png"),
       height = 2.5, width = 10.1)
