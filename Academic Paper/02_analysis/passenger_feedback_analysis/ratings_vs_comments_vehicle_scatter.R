# Feedback vs Comments

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers.Rds"))

# Figure -----------------------------------------------------------------------
p_unsafe <- veh_df %>%
  ggplot() +
  geom_vline(aes(xintercept = 0),
             color = "gray50") +
  geom_point(aes(x = sentiment_snmtr,
                 y = q_safety_prop_unsafe)) +
  labs(x = "Average Sentiment",
       y = "Proportion\nRate\nUnsafe",
       title = "A. Vehicle level sentiment vs unsafe ratings") +
  scale_x_continuous(limits = c(-1, 1.1),
                     breaks = c(-1, 0, 1),
                     labels = c("-1 (Negative)", "0 (Neutral)", "1 (Positive)")) +
  theme_classic2() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 12)) 

p_fast <- veh_df %>%
  ggplot() +
  geom_vline(aes(xintercept = 0),
             color = "gray50") +
  geom_point(aes(x = sentiment_snmtr,
                 y = q_speed_rating_v2_fast)) +
  labs(x = "Average Sentiment",
       y = "Proportion\nRate\nVery Fast",
       title = "B. Vehicle level sentiment vs fast ratings") +
  scale_x_continuous(limits = c(-1, 1.1),
                     breaks = c(-1, 0, 1),
                     labels = c("-1 (Negative)", "0 (Neutral)", "1 (Positive)")) +
  theme_classic2() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 12)) 

# Arrange/Export ---------------------------------------------------------------
p <- ggarrange(p_unsafe,
               p_fast,
               nrow = 1)

ggsave(p, filename = file.path(figures_dir, "feedback_rating_sentiment_vehicle.png"),
       height = 3, width = 9.25)

