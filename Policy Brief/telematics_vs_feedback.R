# Passenger Feedback vs GPS Data

# Load data --------------------------------------------------------------------
veh_df    <- readRDS(file.path(ap_data_dir, "FinalData", "vehicle_level_all202.Rds"))

veh_df <- veh_df %>%
  dplyr::filter(n_feedback >= 10)

cor.test(veh_df$prop_time_over_80kph_base_10kph,
         veh_df$q_safety_rating_num)

veh_df %>%
  dplyr::filter(!is.na(prop_time_over_80kph_base_10kph)) %>%
  ggplot(aes(x = q_safety_rating_num,
             y = prop_time_over_80kph_base_10kph)) +
  geom_smooth(method='lm', 
              formula= y~x, 
              se=F,
              color = "orange") +
  geom_point(size = 2) +
  labs(x = "Average Safety Rating (Higher = Safer)\n[Passenger Feedback Data]",
       y = "Percent of\ntime vehicle\ntraveling\nabove 80 km/h\n[Sensor Data]",
       title = "Passenger safety rating vs. percent of time vehicle speeds",
       caption = "Each dot represents 1 vehicle.") +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  theme_classic2() +
  theme(plot.title = element_text(size = 8.2),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 7),
        axis.title.x = element_text(size = 7),
        plot.caption = element_text(size = 6))

ggsave(filename = file.path(brief_figures_dir, "speed80kph_vs_safety.png"),
       height = 2.25, width = 4.5)
