# N feedback over time

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_class_main_cmntfilterFALSE_dstnctpassTRUE.Rds"))

veh_df <- fb_df %>%
  group_by(regno) %>%
  dplyr::summarise(date_min = min(date),
                   date_max = max(date),
                   n = n()) %>%
  ungroup() %>%
  mutate(n_weeks = difftime(date_max, date_min, units = "weeks") %>%
           as.numeric())

veh_df %>%
  pull(n_weeks) %>%
  summary()

veh_df %>%
  filter(n >= 10) %>%
  pull(n_weeks) %>%
  summary()

veh_df %>%
  ggplot(aes(x = n_weeks)) +
  geom_histogram(color = "black",
                 fill = "dodgerblue") + 
  labs(x = "Number of Weeks",
       y = "Number of\nVehicles",
       title = "Number of weeks passengers provide feedback across vehicles") +
  theme_classic2() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(face = "bold", size = 12))

ggsave(filename = file.path(figures_dir, "feedback_over_time.png"),
       height = 3, width = 6.25)


