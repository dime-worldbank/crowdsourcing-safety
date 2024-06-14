# Figures: All 150

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_all202_cmntfilterFALSE_dstnctpassTRUE.Rds"))
feedback_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_class_main_cmntfilterFALSE_dstnctpassTRUE.Rds"))

veh_long_df <- veh_df %>%
  dplyr::select(regno,
                prop_time_over_80kph_base_10kph,
                prop_time_over_100kph_base_10kph,
                rate_N_valueg_above0_5_base_10kph) %>%
  pivot_longer(cols = -regno) %>%
  mutate(name_clean = case_when(
    name == "prop_time_over_80kph_base_10kph" ~ "Proportion time vehicle\ndrives over 80km/h",
    name == "prop_time_over_100kph_base_10kph" ~ "Proportion time vehicle\ndrives over 100km/h",
    name == "rate_N_valueg_above0_5_base_10kph" ~ "N harsh driving\nevents / hour"
  ) %>%
    fct_rev())

veh_long_df %>%
  dplyr::filter(name %>% str_detect("prop")) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "dodgerblue",
                 color = "black") +
  facet_wrap(~name_clean) +
  theme_classic2() +
  theme(strip.background = element_blank()) +
  labs(x = "Proportion",
       y = "N Vehicles") +
  ylim(0, 100)


veh_long_df %>%
  dplyr::filter(name %>% str_detect("rate")) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "dodgerblue",
                 color = "black") +
  facet_wrap(~name_clean) +
  theme_classic2() +
  theme(strip.background = element_blank()) +
  labs(x = "N harsh events / hour",
       y = "N Vehicles") +
  ylim(0, 100)






veh_df %>%
  dplyr::filter(n_feedback >= 10) %>%
  dplyr::select(regno,
                q_safety_prop_unsafe,
                q_speed_rating_alt_dfast,
                comment_driver_sntmt_code_compl_prop_neg) %>%
  pivot_longer(cols = -regno) %>%
  mutate(name_clean = case_when(
    name == "q_safety_prop_unsafe" ~ "Proportion of passengers\nthat rate driving as\n'Unsafe' or 'Very unsafe'",
    name == "q_speed_rating_alt_dfast" ~ "Proportion of passengers\nthat rate driving as\n'Dangerously fast' or 'Very fast [80 km/h +]'",
    name == "comment_driver_sntmt_code_compl_prop_neg" ~ "Proportion of passengers\nthat give negative\ncomment on driving"
  ) %>%
    fct_rev()) %>%
  
  ggplot() +
  geom_histogram(aes(x = value),
                 fill = "dodgerblue",
                 color = "black") +
  facet_wrap(~name_clean) +
  labs(x = "Proportion",
       y = "N vehicles") +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

veh_dfa <- veh_df[veh_df$n_feedback >= 10,]
cor.test(rank(veh_dfa$prop_time_over_120kph_base_10kph),
         rank(veh_dfa$q_safety_rating_num))
plot(veh_dfa$prop_time_over_120kph_base_10kph,
     veh_dfa$q_safety_rating_num)

feedback_df$comment_driver_sntmt_code_str %>%
  is.na %>% table()

feedback_df$comment_driver_sntmt_code_str %>%
  table()

feedback_df %>%
  dplyr::filter(comment_driver_sntmt_code_str == "Negative",
                sentiment_snmtr > 0) %>%
  pull(q_comment) %>%
  sort()

feedback_df %>%
  dplyr::filter(comment_driver_sntmt_code_str == "Positive",
                sentiment_snmtr > 0) %>%
  pull(q_comment) %>%
  sort()

feedback_df$sentiment_snmtr_bin <- feedback_df$sentiment_snmtr
feedback_df$sentiment_snmtr_bin[feedback_df$sentiment_snmtr_bin > 0] <- 1
feedback_df$sentiment_snmtr_bin[feedback_df$sentiment_snmtr_bin < 0] <- -1

feedback_df$comment_driver_sntmt_code_str[feedback_df$comment_driver_sntmt_code_str %in% "Unclear"] <- "Neutral"

table(feedback_df$comment_driver_sntmt_code_str,
      feedback_df$sentiment_snmtr_bin)

example$q_comment[example$sentiment_snmtr < 0] %>%
  sort()

sentiment("Unsafe driving")
sentiment("The driver is stubborn")


feedback_df %>%
  dplyr::filter(comment_driver_sntmt_code_str %in% 
                  c("Neutral",
                    "Negative",
                    "Positive")) %>%
  dplyr::mutate(comment_driver_sntmt_code_str = comment_driver_sntmt_code_str %>%
                  fct_rev()) %>%
  ggplot() +
  geom_boxplot(aes(y = comment_driver_sntmt_code_str,
                   x = sentiment_snmtr),
               fill = "gray90") +
  geom_vline(xintercept = 0,
             color = "red",
             alpha = 0.5) +
  theme_classic2() +
  labs(x = "Polarity from sentimentr",
       y = "Manually coded polarity")


veh_df$n_feedback[veh_df$comment_driver_sntmt_code_avg %in% -1]


