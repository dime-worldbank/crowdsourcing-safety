# Check Data Issues

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))
wialon_df <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "user_ids.csv"))

## Restrict to ones with current ID
sensor_df <- sensor_df[sensor_df$reg_no_id %in% wialon_df$id,]
sensor_df$reg_no_id <- sensor_df$reg_no_id %>% as.numeric()

sensor_df$regno <- NULL
sensor_df <- left_join(sensor_df, 
                       wialon_df %>% 
                         dplyr::select(id, nm),
                       by = c("reg_no_id" = "id")) %>%
  dplyr::rename(regno = nm)
  
sensor_df$regno %>% unique() %>% length() # should be 150!

# Checks -----------------------------------------------------------------------
sensor_latest_date$date %>% max()
sensor_latest_date <- sensor_df %>%
  dplyr::filter(date %in% ymd("2022-04-10"):ymd("2022-04-24")) %>%
  group_by(reg_no) %>%
  dplyr::summarise(N_obs_speed = sum(N_obs_speed)) %>%
  ungroup()

nrow(sensor_latest_date)

sensor_latest_date

sensor_latest_date$N_obs_speed %>% summary()

ymd("2020")


