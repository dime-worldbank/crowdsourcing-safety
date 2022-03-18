# Check Server Usage

# Load data --------------------------------------------------------------------
echo_df <- readRDS(file.path(sensors_dir, "FinalData", "echodriving_dayhr.Rds"))

echo_sum_df <- echo_df %>%
  # Variable for any violation and filter to observations with a violation
  dplyr::mutate(viol_any = (N_violation_acceleration > 0) |
                  (N_violation_acceleration > 0) |
                  (N_violation_acceleration > 0)) %>%
  dplyr::filter(viol_any %in% T) %>%
  # Keep first observation with violation
  group_by(reg_no_id) %>%
  dplyr::summarise(datetime_eat = min(datetime_eat)) %>%
  # Make date as first of month
  dplyr::mutate(date_first = datetime_eat %>% 
                  as.Date() %>%
                  substr(1,7) %>%
                  paste0("-01") %>%
                  as.Date()) 

## Calculate number of months used
n_months <- echo_sum_df %>%
  group_by(date_first) %>%
  dplyr::summarise(N = n())

n_months

n_months$N %>% sum()
