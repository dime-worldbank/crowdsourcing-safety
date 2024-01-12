# Checks

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(data_dir, "FinalData", "sensor_sticker_ie_data.Rds"))

#### Vehicle information
veh_data_df <- readRDS(file.path(data_dir, "RawData", "gps_vehicle_info.Rds"))

#### Sticker Installation Survey
st_insll_df <- readRDS(file.path(data_dir, "RawData", "gps_vehicle_sticker_install_survey.Rds"))

# Check n sticker installed vs expected ----------------------------------------
veh_data_df$drvr_feedback_treat %>% table()
veh_data_df$drvr_feedback_treat[veh_data_df$regno %in% st_insll_df$regno] %>% table()

veh_data_df$drvr_feedback_treat_sticker %>% table()
veh_data_df$drvr_feedback_treat_sticker[veh_data_df$regno %in% st_insll_df$regno] %>% table()
25/58

# Cleanup ----------------------------------------------------------------------
# Last sensor day: 2023-01-16
sensor_df <- sensor_df %>%
  dplyr::filter( (drvr_feedback_treat_sticker == 0) | (!is.na(sticker_install_date))) %>%
  # dplyr::filter(date >= ymd("2022-08-01"),
  #               date <= ymd("2022-12-31")) %>%
  dplyr::filter(distance_minmax_latlon_daily_km >= 50)

sensor_df %>%
  distinct(regno, .keep_all = T) %>%
  pull(sticker_install_date) %>%
  table()

difftime(ymd("2022-11-30"), ymd("2023-01-16"))

sensor_df <- sensor_df %>%
  dplyr::filter(date >= "2022-08-01",
                date <= "2023-01-16") %>%
  filter(!is.na(speed_mean)) 

sensor_df %>%
  group_by(date) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  arrange(date)

sensor_df <- sensor_df %>%
  mutate(date_week = date %>% round_date(unit = "week"),
         sticker_install_date_week = sticker_install_date %>% round_date(unit = "week"))

sensor_week_df <- sensor_df %>%
  dplyr::filter(date_week <= ymd("2023-01-16"),
                date_week >= ymd("2022-08-15")) %>%
  
  group_by(regno, date_week, sticker_install_date_week) %>%
  dplyr::summarise(prop_time_over_90kph_base_10kph = mean(prop_time_over_90kph_base_10kph, na.rm = T),
                   prop_time_over_90kph_base_80kph = mean(prop_time_over_90kph_base_80kph, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(regno) %>%
  dplyr::mutate(n_week_regno = n()) %>%
  ungroup()

sensor_week_df$prop_time_over_90kph_base_10kph[sensor_week_df$prop_time_over_90kph_base_10kph == -Inf] <- NA
sensor_week_df$prop_time_over_90kph_base_80kph[sensor_week_df$prop_time_over_90kph_base_80kph == -Inf] <- NA

n_week <- sensor_week_df$date_week %>% unique() %>% length()

# sensor_week_df_full <- sensor_week_df %>%
#   dplyr::filter(n_week_regno >= n_week)

sensor_week_df_full$regno %>% unique() %>% length()

sensor_df_full <- sensor_df[sensor_df$regno %in% sensor_week_df_full$regno,]

sensor_week_df_full %>%
  distinct(regno, .keep_all = T) %>%
  pull(sticker_install_date_week) %>%
  table()

sensor_week_df_full <- sensor_week_df_full %>%
  mutate(sticker_install_date_week_num = sticker_install_date_week %>% as.numeric(),
         date_week_num = date_week %>% as.numeric(),
         regno_num = regno %>% as.factor() %>% as.numeric()) %>%
  mutate(sticker_install_date_week_num = 
           replace_na(sticker_install_date_week_num, 0))

sensor_df_full <- sensor_df_full %>%
  mutate(sticker_install_date_num = sticker_install_date %>% as.numeric(),
         date_num = date %>% as.numeric(),
         regno_num = regno %>% as.factor() %>% as.numeric()) %>%
  mutate(sticker_install_date_num = 
           replace_na(sticker_install_date_num, 0))

sensor_df_full$dow = sensor_df_full$date %>% wday() 
 
####
out_attgt <- att_gt(yname = "prop_time_over_90kph_base_80kph",
                    tname = "date_num",
                    idname = "regno_num",
                    gname = "sticker_install_date_num",
                    control_group = "nevertreated",
                    data = sensor_df_full,
                    allow_unbalanced_panel = T
)

agg_simple <- aggte(out_attgt, type = "dynamic", na.rm = TRUE)

ggdid(agg_simple)

####
out_attgt <- att_gt(yname = "prop_time_over_90kph_base_10kph",
                    tname = "date_week_num",
                    idname = "regno_num",
                    gname = "sticker_install_date_week_num",
                    control_group = "nevertreated",
                    data = sensor_week_df_full,
                    allow_unbalanced_panel = T
)

agg_simple <- aggte(out_attgt, type = "dynamic", na.rm = TRUE)

ggdid(agg_simple)

sensor_week_df$n_week_regno

sensor_week_df$date_week %>%
  unique() %>%
  length()

sensor_week_df$regno %>%
  table()

sensor_week_df %>%
  group_by(date_week) %>%
  dplyr::summarise(n = n()) %>%
  ungroup()

sensor_week_df$regno %>%
  table()

sensor_df %>%
  distinct(date_week, regno) %>%
  group_by(date_week) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  arrange(date_week)
  


