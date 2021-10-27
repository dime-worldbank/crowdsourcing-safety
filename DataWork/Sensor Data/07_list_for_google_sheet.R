# Create list of PSVs for Google Sheet

# https://docs.google.com/spreadsheets/d/1uaAEIMOV091p9TyL_xEmdoLAwalzVFwKryV-N7jXj5k/edit?usp=sharing

# Load Data --------------------------------------------------------------------
ed <- read_parquet(file.path(sensors_dir, "FinalData", "echodriving_clean.gz.parquet"))
#ed <- read_parquet(file.path(sensors_dir, "FinalData", "sensortracing_clean.gz.parquet"))

# First Observation ------------------------------------------------------------
ed_unique <- ed %>%
  dplyr::filter(!is.na(begin_datetime_eat)) %>%
  arrange(begin_datetime_eat, regno_clean) %>%
  distinct(regno_clean, route, sacco, .keep_all = T) %>%
  dplyr::select(regno_clean, route, sacco, begin_datetime_eat) %>%
  dplyr::mutate(begin_datetime_eat = paste0("time_", begin_datetime_eat))

write.csv(ed_unique, "~/Desktop/regno.csv", row.names = F)
