# Quick figures to visualize data

# Echo Driving =================================================================
echo_df <- read_parquet(file.path(sensors_dir, "FinalData", "echodriving.gz.parquet"))

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
