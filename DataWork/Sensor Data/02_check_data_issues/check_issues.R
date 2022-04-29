# Check Data Issues

# KCR 613W

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))
wialon_df <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "user_ids.csv"))

scania kbj 650w // 22648424 // 2021-11-21 03:00:00

read_parquet("/Users/robmarty/Dropbox/World Bank/IEs/PSV Rider Feedback/Data/Sensor Data/RawData/sensor_tracing_individual_data/2021-11-21/sensortracing_22648424_2021-11-21.gz.parquet")

25260902

st_df <- load_st_raw("2021-11-21", "kbj650w")
st_df
st_df$time_str %>% substring(1,10) %>% table()
25260902

st_df <- st_df[st_df$time_str %>% str_detect("2021-11-21"),]

22648424 	
2021-11-21

wialon_df[wialon_df$nm %>% str_detect("KCR 613W"),]
KCR 613W
2022-04-24
25260902
# raw_sensor_files

raw_sensor_files %>% str_subset("25260902")

a <- sensor_df[is.na(sensor_df$N_obs_speed),]

sensor_df$N_obs_speed %>% table()


sensor_df$regno_clean %>% is.na %>% table()

sensor_df <- sensor_df[sensor_df$regno_clean %in% "KBJ 650W",]
sensor_df[,c("date", "N_obs_speed")] %>% View()
sensor_df %>%
  ggplot() +
  geom_line(aes(x = date,
                y = N_obs_speed))


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


