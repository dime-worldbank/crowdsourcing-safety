# Quick figures to visualize data

# Echo Driving =================================================================
echo_df <- read_parquet(file.path(sensors_dir, "FinalData", "echodriving_clean.gz.parquet"))

# Prep Data --------------------------------------------------------------------
echo_df <- echo_df %>%
  dplyr::mutate(date = begin_datetime_eat %>% as.Date()) %>%
  dplyr::filter(value_g >= 0.3,
                date >= as.Date("2021-08-01")) %>%
  distinct(begin_datetime_eat, reg_no, .keep_all = T) 

echo_N_df <- echo_df %>%
  group_by(regno_clean, route, date) %>%
  dplyr::summarise(N_violations = n()) %>%
  group_by(regno_clean, route) %>%
  summarise(N_days = n(),
            N_violations = sum(N_violations)) %>%
  dplyr::filter(N_days >= 7) %>%
  mutate(viol_daily = N_violations / N_days) %>%
  arrange(viol_daily) #%>%
  #mutate(veh_id = paste0("Vehicle ", 1:n())) 

# Figure -----------------------------------------------------------------------
p <- echo_N_df %>%
  mutate(regno_clean = regno_clean %>% as.factor(),
         regno_clean = fct_reorder(regno_clean, viol_daily)) %>%
  ggplot() +
  geom_col(aes(x = viol_daily,
               y = fct_reorder(regno_clean, viol_daily),
               fill = route),
           color = "black") +
  labs(title = "Average Daily Harsh Acceleration Events",
       note = "Harsh acceleration = accelerating more than 0.3m/s^2",
       fill = "Route",
       x = NULL,
       y = NULL) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold",
                                  color = "black")) 

ggsave(p, filename = file.path(dropbox_dir, 
                               "Presentations", 
                               "MOA Meeting 2021-10-09",
                               "figures",
                               "allveh_average_violation.png"),
       height = 8, width = 7)

# Figure -----------------------------------------------------------------------


echo_one_df <- echo_df %>%
  dplyr::filter(reg_no_id %in% 23256184) %>%
  dplyr::rename(latitude = latitude_begin,
                longitude = longitude_begin)

library(leaflet.extras)

leaflet() %>%
  addTiles() %>%
  addHeatmap(data = echo_one_df,
             radius = 5)



echo_N_df %>%
  mutate(reg_no_id = reg_no_id %>% as.factor(),
         reg_no_id = fct_reorder(reg_no_id, viol_daily)) %>%
  ggplot() +
  geom_col(aes(x = viol_daily,
               y = fct_reorder(reg_no_id, viol_daily)),
           color = "black",
           fill = "brown2") +
  labs(title = "Average Daily Harsh Acceleration Events",
       note = "Harsh acceleration = accelerating more than 0.3m/s^2",
       x = NULL,
       y = NULL) +
  theme_classic2() +
  theme(plot.title = element_text(face = "bold",
                                  color = "black")) 







echo_N_df %>%
  ggplot() +
  geom_line(aes(x = date,
                y = N)) +
  facet_wrap(~reg_no)




# Dataset of N Daily Violations by Type ----------------------------------------
## Total number of violations per day per vehicle
echo_sum_df <- echo_df %>%
  dplyr::mutate(violation = violation %>% tolower,
                viol_acc = violation %>% str_detect("acc"),
                viol_brake = violation %>% str_detect("brake"),
                viol_turn = violation %>% str_detect("turn"),
                viol_any = violation %>% str_detect("accel|brake|turn")) %>%
  dplyr::filter(viol_any %in% T,
                value_g > 0.3) %>%
  distinct(begin_datetime, reg_no, .keep_all = T) %>%
  dplyr::mutate(date = begin_datetime %>% as.Date) %>%
  group_by(date, reg_no) %>%
  dplyr::summarise_at(vars(viol_acc, viol_brake, viol_turn, viol_any), sum) %>%
  
  # Total number of violations
  ungroup() %>%
  group_by(reg_no) %>%
  dplyr::mutate(viol_any_total = sum(viol_any)) %>%
  
  # Add total to name
  dplyr::mutate(reg_no = paste0(toupper(reg_no), "\nTotal = ", viol_any_total)) 

# Order factor by total violations (for some reason doesnt work in mutate)
echo_sum_df$reg_no <- fct_reorder(echo_sum_df$reg_no, echo_sum_df$viol_any_total, .desc = T)

# Long Dataset
echo_sum_long_df <- echo_sum_df %>%
  pivot_longer(cols = c(viol_acc, viol_brake, viol_turn, viol_any)) %>%
  dplyr::mutate(name = case_when(name == "viol_acc" ~ "Acceleration",
                                 name == "viol_brake" ~ "Brake",
                                 name == "viol_turn" ~ "Turn",
                                 name == "viol_any" ~ "Any"))

# Daily Figure -----------------------------------------------------------------
p <- echo_sum_long_df %>%
  dplyr::filter(name %in% "Any") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  geom_point(size = 0.5) +
  facet_wrap(~reg_no) +
  labs(x = NULL,
       title = "Daily Violations") 
ggsave(p, filename = file.path(sensors_dir, "Outputs", "n_violations_daily_allvehicles.png"),
       height = 7, width = 11)

# Weekly Violations ------------------------------------------------------------
echo_sum_long_weekly_df <- echo_sum_long_df %>%
  mutate(week = date %>% round_date(unit = "week")) %>%
  group_by(reg_no, week, name) %>%
  dplyr::summarise(value = sum(value))

p <- echo_sum_long_weekly_df %>%
  dplyr::filter(name %in% "Any") %>%
  ggplot(aes(x = week, y = value)) +
  geom_line() +
  geom_point(size = 0.5) +
  facet_wrap(~reg_no) +
  labs(x = NULL,
       title = "Weekly Violations") 
ggsave(p, filename = file.path(sensors_dir, "Outputs", "n_violations_weekly_allvehicles.png"),
       height = 7, width = 11)
