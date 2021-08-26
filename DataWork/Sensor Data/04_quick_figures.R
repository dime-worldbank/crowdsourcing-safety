# Quick figures to visualize data

# Echo Driving =================================================================
echo_df <- read_parquet(file.path(sensors_dir, "FinalData", "echodriving.gz.parquet"))

# Daily Violations -------------------------------------------------------------
## Total number of violations per day per vehicle
echo_sum_df <- echo_df %>%
  dplyr::mutate(violation = violation %>% tolower) %>%
  dplyr::filter(violation %>% str_detect("accel|brake|turn"),
                value_g > 0.3) %>%
  distinct(begin_datetime, reg_no) %>%
  dplyr::mutate(date = begin_datetime %>% as.Date) %>%
  group_by(date, reg_no) %>%
  dplyr::summarise(N = n()) %>%
  
  # Total number of violations
  ungroup() %>%
  group_by(reg_no) %>%
  dplyr::mutate(N_total = sum(N)) %>%
  
  # Add total to name
  dplyr::mutate(reg_no = paste0(toupper(reg_no), "\nTotal = ", N_total)) 

# Order factor by total violations (for some reason doesnt work in mutate)
echo_sum_df$reg_no <- fct_reorder(echo_sum_df$reg_no, echo_sum_df$N_total, .desc = T)

p <- echo_sum_df %>%
  ggplot(aes(x = date, y = N)) +
  geom_line() +
  geom_point(size = 0.5) +
  facet_wrap(~reg_no) +
  labs(x = NULL) 
ggsave(p, filename = file.path(sensors_dir, "Outputs", "n_violations_daily_allvehicles.png"),
       height = 7, width = 11)

# Weekly Violations -------------------------------------------------------------
## Total number of violations per week per vehicle
echo_sum_df <- echo_df %>%
  dplyr::mutate(violation = violation %>% tolower) %>%
  dplyr::filter(violation %>% str_detect("accel|brake|turn"),
                value_g > 0.3) %>%
  distinct(begin_datetime, reg_no) %>%
  dplyr::mutate(date = begin_datetime %>% week()) %>%
  group_by(date, reg_no) %>%
  dplyr::summarise(N = n()) %>%
  
  # Total number of violations
  ungroup() %>%
  group_by(reg_no) %>%
  dplyr::mutate(N_total = sum(N)) %>%
  
  # Add total to name
  dplyr::mutate(reg_no = paste0(toupper(reg_no), "\nTotal = ", N_total)) 

# Order factor by total violations (for some reason doesnt work in mutate)
echo_sum_df$reg_no <- fct_reorder(echo_sum_df$reg_no, echo_sum_df$N_total, .desc = T)

p <- echo_sum_df %>%
  ggplot(aes(x = date, y = N)) +
  geom_line() +
  geom_point(size = 0.5) +
  facet_wrap(~reg_no) +
  labs(x = NULL) 
ggsave(p, filename = file.path(sensors_dir, "Outputs", "n_violations_weekly_allvehicles.png"),
       height = 7, width = 11)
