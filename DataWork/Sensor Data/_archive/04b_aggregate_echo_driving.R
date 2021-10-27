# Aggregate Echo Driving to Hourly

# Load data --------------------------------------------------------------------
ed_df <- readRDS(file.path(sensors_dir, "FinalData", "echodriving.Rds")) 

# Clean variables --------------------------------------------------------------
ed_sum_df <- ed_df %>%
  dplyr::mutate(datetime_eat = begin_datetime_eat %>% floor_date(unit = "hour")) %>%
  dplyr::group_by(reg_no_id, reg_no, violation, datetime_eat) %>%
  dplyr::summarise(N_violation = n(),
                   N_valueg_above0_1 = sum(value_g > 0.1),
                   N_valueg_above0_2 = sum(value_g > 0.2),
                   N_valueg_above0_3 = sum(value_g > 0.3),
                   N_valueg_above0_4 = sum(value_g > 0.4),
                   N_valueg_above0_5 = sum(value_g > 0.5),
                   N_valueg_above1_0 = sum(value_g > 1.0)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(reg_no_id, reg_no, datetime_eat),
              values_from = c(N_violation,
                              N_valueg_above0_1,
                              N_valueg_above0_2,
                              N_valueg_above0_3,
                              N_valueg_above0_4,
                              N_valueg_above0_5,
                              N_valueg_above1_0),
              names_from = violation) %>%
  dplyr::mutate_at(vars(contains("N_violation")), ~ replace_na(., 0)) %>%
  dplyr::mutate(N_violation = N_violation_acceleration +
                  N_violation_brake +
                  N_violation_turn,
                
                N_valueg_above0_1 = N_valueg_above0_1_acceleration + 
                  N_valueg_above0_1_brake + 
                  N_valueg_above0_1_turn,
                
                N_valueg_above0_2 = N_valueg_above0_2_acceleration + 
                  N_valueg_above0_2_brake + 
                  N_valueg_above0_2_turn,
                
                N_valueg_above0_3 = N_valueg_above0_3_acceleration + 
                  N_valueg_above0_3_brake + 
                  N_valueg_above0_3_turn,
                
                N_valueg_above0_4 = N_valueg_above0_4_acceleration + 
                  N_valueg_above0_4_brake + 
                  N_valueg_above0_4_turn,
                
                N_valueg_above0_5 = N_valueg_above0_5_acceleration + 
                  N_valueg_above0_5_brake + 
                  N_valueg_above0_5_turn,
                
                N_valueg_above1_0 = N_valueg_above1_0_acceleration + 
                  N_valueg_above1_0_brake + 
                  N_valueg_above1_0_turn
  )


