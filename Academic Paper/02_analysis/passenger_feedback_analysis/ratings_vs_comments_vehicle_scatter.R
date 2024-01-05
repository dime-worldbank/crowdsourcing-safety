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
       title = "A. Vehicle level sentiment vs\nproportion rate unsafe") +
  scale_x_continuous(limits = c(-1, 1.15),
                     breaks = c(-1, 0, 1),
                     labels = c("-1 (Negative)", "0 (Neutral)", "1 (Positive)")) +
  theme_classic2() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text = element_text(color = "black"),
        axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 10)) 

p_fast_v1 <- veh_df %>%
  ggplot() +
  geom_vline(aes(xintercept = 0),
             color = "gray50") +
  geom_point(aes(x = sentiment_snmtr,
                 y = q_speed_rating_v1_dfast)) +
  labs(x = "Average Sentiment",
       y = "Proportion\nRate\nDangerously\nFast",
       title = "B. Vehicle level sentiment vs\nproportion rate dangerously fast") +
  scale_x_continuous(limits = c(-1, 1.15),
                     breaks = c(-1, 0, 1),
                     labels = c("-1 (Negative)", "0 (Neutral)", "1 (Positive)")) +
  theme_classic2() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text = element_text(color = "black"),
        axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 10)) 

p_fast_v2 <- veh_df %>%
  ggplot() +
  geom_vline(aes(xintercept = 0),
             color = "gray50") +
  geom_point(aes(x = sentiment_snmtr,
                 y = q_speed_rating_v2_fast)) +
  labs(x = "Average Sentiment",
       y = "Proportion\nRate\nVery Fast",
       title = "C. Vehicle level sentiment vs\nproportion rate very fast (80 km/h +)") +
  scale_x_continuous(limits = c(-1, 1.15),
                     breaks = c(-1, 0, 1),
                     labels = c("-1 (Negative)", "0 (Neutral)", "1 (Positive)")) +
  theme_classic2() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text = element_text(color = "black", size = 9),
        axis.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 10)) 

# Arrange/Export ---------------------------------------------------------------
p <- ggarrange(p_unsafe,
               p_fast_v1,
               p_fast_v2,
               nrow = 1)

ggsave(p, filename = file.path(figures_dir, "feedback_rating_sentiment_vehicle.png"),
       height = 2.5, width = 11)

