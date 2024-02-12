# Figures: All 150

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_all202.Rds"))

veh_df <- veh_df %>%
  filter(!is.na(speed_mean))

# Speed ------------------------------------------------------------------------
veh_df %>%
  select(regno,
         prop_time_over_80kph_base_10kph,
         prop_time_over_90kph_base_10kph,
         prop_time_over_100kph_base_10kph) %>%
  pivot_longer(cols = -regno) %>%
  mutate(name_clean = case_when(
    name == "prop_time_over_80kph_base_10kph" ~ "Percent of time travling above 80 km/h",
    name == "prop_time_over_90kph_base_10kph" ~ "Percent of time travling above 90 km/h",
    name == "prop_time_over_100kph_base_10kph" ~ "Percent of time travling above 100 km/h"
  )) %>%
  mutate(name_clean = name_clean %>%
           factor(levels = c("Percent of time travling above 80 km/h",
                             "Percent of time travling above 90 km/h",
                             "Percent of time travling above 100 km/h"))) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30,
                 fill = "dodgerblue",
                 color = "black") +
  labs(x = NULL,
       y = "N Vehicles") + 
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~name_clean,
             ncol = 1,
             scales = "free_y")

# Speed above 80 ---------------------------------------------------------------
veh_df %>%
  select(regno,
         prop_time_over_90kph_base_80kph,
         prop_time_over_100kph_base_80kph) %>%
  pivot_longer(cols = -regno) %>%
  mutate(name_clean = case_when(
    name == "prop_time_over_90kph_base_80kph" ~ "Percent of time travling above 90 km/h\nwhen traveling above 80 km/h",
    name == "prop_time_over_100kph_base_80kph" ~ "Percent of time travling above 100 km/h\nwhen traveling above 80 km/h"
  )) %>%
  mutate(name_clean = name_clean %>%
           factor(levels = c("Percent of time travling above 90 km/h\nwhen traveling above 80 km/h",
                             "Percent of time travling above 100 km/h\nwhen traveling above 80 km/h"))) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30,
                 fill = "dodgerblue",
                 color = "black") +
  labs(x = NULL,
       y = "N Vehicles") + 
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~name_clean,
             ncol = 1,
             scales = "free_y")

# Harsh driving ----------------------------------------------------------------
veh_df %>%
  select(regno,
         rate_N_valueg_above0_5_base_10kph,
         rate_N_valueg_above0_5_acceleration_base_10kph,
         rate_N_valueg_above0_5_brake_base_10kph,
         rate_N_valueg_above0_5_turn_base_10kph) %>%
  pivot_longer(cols = -regno) %>%
  mutate(name_clean = case_when(
    name == "rate_N_valueg_above0_5_base_10kph" ~ "Any type",
    name == "rate_N_valueg_above0_5_acceleration_base_10kph" ~ "Acceleration",
    name == "rate_N_valueg_above0_5_brake_base_10kph" ~ "Braking",
    name == "rate_N_valueg_above0_5_turn_base_10kph" ~ "Turning",
  )) %>%
  mutate(name_clean = name_clean %>%
           factor(levels = c("Any type",
                             "Acceleration",
                             "Braking",
                             "Turning"))) %>%
  mutate(value = ifelse(value >= 2, 2, value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30,
                 fill = "dodgerblue",
                 color = "black") +
  labs(x = NULL,
       y = "N Vehicles") + 
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  scale_x_continuous(breaks = c(0,1,2),
                     labels = c("0", "1", ">2")) +
  facet_wrap(~name_clean,
             ncol = 1,
             scales = "free_y")

# Comparisons ------------------------------------------------------------------

veh_df %>%
  mutate(rate_N_valueg_above0_5_base_10kph = 
           Winsorize(rate_N_valueg_above0_5_base_10kph, probs = c(0, 0.98)),
         
         prop_time_over_100kph_base_80kph = 
           Winsorize(prop_time_over_90kph_base_80kph, probs = c(0, 0.98))) %>%
  
  ggplot(aes(x = rate_N_valueg_above0_5_base_10kph,
             y = prop_time_over_90kph_base_80kph )) +
  geom_point() +
  geom_smooth(method='lm', 
              formula= y~x,
              se = FALSE,
              color = "darkorange") +
  labs(x = "Average harsh driving event violations per hour",
       y = "Proportion\ntime traveling\nabove 90 km/h\nwhen above 80 km/h") +
  theme_classic2() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))










