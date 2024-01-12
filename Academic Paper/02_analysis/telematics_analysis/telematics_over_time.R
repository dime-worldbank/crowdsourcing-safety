# Telematis Over Time: Persistence

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(data_dir, "FinalData", "sensor_day_days_when_traveled.Rds"))

# Remove days not traveling
# Std dev

# Cleanup ----------------------------------------------------------------------
# Remove days not travelling

# sensor_df <- sensor_df %>%
#   filter()

veh_df <- sensor_df %>%
  group_by(regno, route) %>%
  summarize(across(everything(), list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  ungroup()

route_df <- veh_df %>%
  group_by(route) %>%
  summarize(across(everything(), sd, na.rm=T)) %>%
  ungroup()

var <- "rate_N_valueg_above0_5_brake_base_10kph"

veh_df[[paste0(var, "_mean")]] %>% sd()
route_df[[paste0(var, "_mean")]] %>% quantile(0.50) %>% as.numeric()
veh_df[[paste0(var, "_sd")]] %>% quantile(0.50) %>% as.numeric()

veh_df[[paste0(var, "_sd")]] %>% quantile(0.25) %>% as.numeric()
veh_df[[paste0(var, "_sd")]] %>% quantile(0.75) %>% as.numeric()


veh_df <- sensor_df %>%
  group_by(regno, route) %>%
  dplyr::summarise(prop_time_over_80kph_base_10kph_sd = sd(prop_time_over_80kph_base_10kph, na.rm=T),
                   prop_time_over_80kph_base_10kph_mean = mean(prop_time_over_80kph_base_10kph, na.rm=T)) %>%
  ungroup()

veh_df$prop_time_over_80kph_base_10kph_sd %>% hist(breaks=30)
veh_df$prop_time_over_80kph_base_10kph_mean %>% sd(na.rm=T)

veh_df %>%
  group_by(route) %>%
  dplyr::summarise(sd = mean(prop_time_over_80kph_base_10kph_sd),
                   n = n())

# Analysis ---------------------------------------------------------------------
p_list <- lapply(c("prop_time_over_80kph_base_10kph",
                   "prop_time_over_90kph_base_80kph",
                   "rate_N_valueg_above0_5_base_10kph"), function(var){
                     
                     sensor_df$var <- sensor_df[[var]]
                     
                     sensor_df <- sensor_df %>%
                       group_by(regno) %>%
                       mutate(var_median = median(var)) %>%
                       ungroup() 
                     
                     veh_df <- sensor_df %>%
                       distinct(regno, var_median) %>%
                       arrange(-var_median) 
                     
                     sensor_df %>%
                       filter(regno %in% veh_df$regno[1:10]) %>%
                       ggplot(aes(x = date, y = var)) +
                       geom_col() +
                       facet_wrap(~regno, 
                                  nrow = 1,
                                  scales = "free_x") +
                       labs(x = NULL,
                            y = NULL,
                            title = var) +
                       theme_classic2() +
                       theme(strip.background = element_blank(),
                             strip.text = element_text(size = 8),
                             plot.title = element_text(face = "bold", size = 10))
                     
                   })

ggarrange(p_list[[1]],
          p_list[[2]],
          p_list[[3]],
          ncol = 1)


