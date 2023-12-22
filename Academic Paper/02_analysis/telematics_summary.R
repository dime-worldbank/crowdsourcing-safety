# Telematics Summary

# Load data --------------------------------------------------------------------
veh_df    <- readRDS(file.path(data_dir, "FinalData", "vehicle_level.Rds"))
#sensor_df <- readRDS(file.path(data_dir, "RawData", "sensor_day.Rds"))

# Prep data --------------------------------------------------------------------
sum_df <- veh_df %>%
  dplyr::select(
    regno,
    
    prop_time_over_80kph_base_10kph,
    prop_time_over_90kph_base_10kph,
    prop_time_over_100kph_base_10kph,
    prop_time_over_110kph_base_10kph,
    prop_time_over_120kph_base_10kph,
    
    rate_N_valueg_above0_5_base_10kph,
    rate_N_valueg_above0_5_acceleration_base_10kph,
    rate_N_valueg_above0_5_brake_base_10kph,
    rate_N_valueg_above0_5_turn_base_10kph,
    
    rate_N_valueg_above1_0_base_10kph,
    rate_N_valueg_above1_0_acceleration_base_10kph,
    rate_N_valueg_above1_0_brake_base_10kph,
    rate_N_valueg_above1_0_turn_base_10kph
  ) %>%
  pivot_longer(cols = -regno) %>%
  filter(!is.na(value)) %>%
  mutate(value = case_when(
    str_detect(name, "prop_time") ~ value * 100,
    TRUE ~ value
  )) %>%
  group_by(name) %>%
  dplyr::summarise(min = min(value),
                   p25 = quantile(value, 0.25),
                   mean = mean(value),
                   p50 = quantile(value, 0.50),
                   p75 = quantile(value, 0.75),
                   max = max(value)
                   ) %>%
  ungroup() %>%
  dplyr::mutate(across(where(is.numeric), ~round(.,2))) %>%
  mutate(speed_viol = ifelse(str_detect(name, "prop_time"), "speed", "violation"),
         speed_name = name %>%
           str_replace_all("prop_time_over_", "") %>%
           str_replace_all("kph_base_10kph", " km/h"),
         viol_name = name %>%
           str_replace_all("rate_N_valueg_above", "") %>%
           str_replace_all("0_5_base", "0_5_Any Type") %>%
           str_replace_all("1_0_base", "1_0_Any Type") %>%
           str_replace_all("_base_10kph", "") %>%
           str_replace_all("10kph", "") %>%
           str_replace_all("0_5", "$>$0.5g") %>%
           str_replace_all("1_0", "$>$1g") %>%
           str_replace_all("_", " ") %>%
           str_squish() %>%
           str_replace_all("acceleration", "Forward") %>%
           str_replace_all("brake", "Backward") %>%
           str_replace_all("turn", "Lateral")
         ) %>%
  mutate(name_full = case_when(
    speed_viol == "speed" ~ speed_name,
    speed_viol == "violation" ~ viol_name
  )) %>%
  mutate(tex = paste(name_full, min, p25, mean, p50, p75, max, sep = " & ") %>%
           paste("\\\\ \n"))

speed_df <- sum_df %>% 
  filter(speed_viol == "speed") %>%
  mutate(speed = speed_name %>%
           str_replace_all(" km/h", "") %>%
           as.numeric()) %>%
  arrange(speed)

viol_df  <- sum_df %>% 
  filter(speed_viol == "violation") %>%
  arrange(viol_name)

# Table ------------------------------------------------------------------------
sink(file.path(tables_dir, "telematics_sum_stat.tex"))

cat("\\begin{tabular}{lllllll} ")
cat("\\hline ")
cat("Variable & Min & 25th Perc. & Median & Mean & 75th Perc. & Max \\\\ \n")

cat("\\hline ")
cat("\\multicolumn{7}{l}{\\textbf{Percent of time vehicle travels over speed}} \\\\ \n")
speed_df$tex %>% 
  paste(collapse = " ") %>%
  cat()

cat("\\hline ")
cat("\\multicolumn{7}{l}{\\textbf{N violations per hour}} \\\\ \n")
viol_df$tex %>% 
  paste(collapse = " ") %>%
  cat()

cat("\\hline ")
cat("\\end{tabular} ")

sink()



  

# Speeding Figures -------------------------------------------------------------
# prep_prop <- function(var, veh_df){
#   
#   veh_df$var <- veh_df[[var]]
#   
#   out_df <- veh_df %>%
#     dplyr::filter(!is.na(var)) %>%
#     arrange(var) %>%
#     mutate(n = 1:n(),
#            prop = n/max(n)) %>%
#     dplyr::select(n, prop, var)
#   
#   out_df$var_name <- var
#   
#   return(out_df)
# }
# 
# speed_df <- map_df(c("prop_time_over_80kph_base_10kph",
#                      "prop_time_over_90kph_base_10kph",
#                      "prop_time_over_100kph_base_10kph",
#                      "prop_time_over_110kph_base_10kph",
#                      "prop_time_over_120kph_base_10kph"),
#        prep_prop, veh_df) %>%
#   mutate(var_name = var_name %>%
#            str_replace_all("prop_time_over_", "") %>%
#            str_replace_all("_base_10kph", "") %>%
#            str_replace_all("_"," ") %>%
#            str_replace_all("kph", "") %>%
#            factor(levels = c("80",
#                              "90",
#                              "100",
#                              "110",
#                              "120")))
# 
# speed_df %>%
#   ggplot(aes(x = var,
#              y = prop,
#              color = var_name)) +
#   geom_line(linewidth = 0.75) +
#   labs(y = "Proportion\nof Vehicles",
#        x = "Proportion\nof Time\nTraveling\nAbove Speed") +
#   labs(color = "Speed\n(km/h)") +
#   theme_classic() +
#   theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
#         panel.grid.major = element_line(color = "gray90", linetype = "dashed"))

# veh_df %>%
#   dplyr::select(regno, 
#                 prop_time_over_80kph_base_10kph,
#                 prop_time_over_90kph_base_10kph,
#                 prop_time_over_100kph_base_10kph,
#                 prop_time_over_110kph_base_10kph,
#                 prop_time_over_120kph_base_10kph,
#                 
#                 prop_time_over_80kph_base_50kph,
#                 prop_time_over_90kph_base_50kph,
#                 prop_time_over_100kph_base_50kph,
#                 prop_time_over_110kph_base_50kph,
#                 prop_time_over_120kph_base_50kph) %>%
#   pivot_longer(cols = -regno) %>%
#   mutate(base = ifelse(str_detect(name, "_base_10"),
#                        "10",
#                        "50")) %>%
#   mutate(name = name %>%
#            str_replace_all("prop_time_over_", "") %>%
#            str_replace_all("_base_10kph", "") %>%
#            str_replace_all("_base_50kph", "") %>%
#            str_replace_all("_"," ") %>%
#            str_replace_all("kph", " km/h") %>%
#            factor(levels = c("80 km/h",
#                              "90 km/h",
#                              "100 km/h",
#                              "110 km/h",
#                              "120 km/h"))) %>%
#   ggplot() +
#   geom_boxplot(aes(x = value,
#                    y = name,
#                    fill = base)) +
#   labs(x = "Proportion of Time Traveling Above Speed",
#        y = NULL) +
#   theme_classic2()
# 
# # Harsh Driving ----------------------------------------------------------------
# #veh_df$rate_N_valueg_above0_5_base_10kph[veh_df$rate_N_valueg_above0_5_base_10kph >= 3] <- 3
# # veh_df %>%
# #   ggplot() +
# #   geom_boxplot(aes(x = rate_N_valueg_above1_0_base_50kph))
# 
# # Correlation ------------------------------------------------------------------
# veh_df %>%
#   dplyr::select(regno,
#                 rate_N_valueg_above0_5_base_10kph,
#                 rate_N_valueg_above0_5_acceleration_base_10kph,
#                 rate_N_valueg_above0_5_brake_base_10kph,
#                 rate_N_valueg_above0_5_turn_base_10kph,
#                 
#                 rate_N_valueg_above1_0_base_10kph,
#                 rate_N_valueg_above1_0_acceleration_base_10kph,
#                 rate_N_valueg_above1_0_brake_base_10kph,
#                 rate_N_valueg_above1_0_turn_base_10kph) %>%
#   pivot_longer(cols = -regno) %>%
#   # dplyr::mutate(value = case_when(
#   #   value >= 1 ~ 1,
#   #   TRUE ~ value
#   # )) %>%
#   ggplot() +
#   geom_boxplot(aes(x = value,
#                    y = name))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Prop above 80 km/h -----------------------------------------------------------
# veh_df %>%
#   dplyr::mutate(prop_time_over_80kph_base_10kph = floor(prop_time_over_80kph_base_10kph * 10) / 10) %>%
#   group_by(prop_time_over_80kph_base_10kph) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   
#   ggplot() + 
#   geom_col(aes(x = prop_time_over_80kph_base_10kph,
#                y = n),
#            width = 0.075,
#            color = "black",
#            fill = "dodgerblue") +
#   geom_text(aes(x = prop_time_over_80kph_base_10kph,
#                 y = n + 5,
#                 label = n)) +
#   scale_x_continuous(#limits = c(-0.05, 0.55),
#     breaks = seq(0, 0.5 , 0.1),
#     labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%")) +
#   labs(x = "Percent of driving time",
#        y = "N Vehicles",
#        title = "Percent of time vehicle traveling above 80 km/h",
#        caption = "Percent calculated as: (time traveling above 80 km/h) / (time traveling above 10 km/h) * 100") +
#   theme_classic2() +
#   theme(plot.title = element_text(size = 9),
#         plot.subtitle = element_text(size = 8, face = "bold"),
#         plot.caption = element_text(size = 6),
#         axis.title = element_text(size = 8),
#         axis.text = element_text(size = 8, color = "black")) 
# 
# ggsave(filename = file.path(brief_figures_dir, "prop_over_80kmh.png"),
#        height = 2.25, width = 4)
# 
# # Harsh driving ----------------------------------------------------------------
# veh_df %>%
#   dplyr::mutate(rate_N_valueg_above0_5_base_10kph = rate_N_valueg_above0_5_base_10kph * 60 * 60) %>%
#   dplyr::mutate(rate_N_valueg_above0_5_base_10kph = case_when(
#     rate_N_valueg_above0_5_base_10kph >= 3 ~ 3,
#     TRUE ~ rate_N_valueg_above0_5_base_10kph
#   )) %>%
#   dplyr::mutate(rate_N_valueg_above0_5_base_10kph = round(rate_N_valueg_above0_5_base_10kph * 5) / 5) %>%
#   group_by(rate_N_valueg_above0_5_base_10kph) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   
#   ggplot() + 
#   geom_col(aes(x = rate_N_valueg_above0_5_base_10kph,
#                y = n),
#            width = 0.15,
#            color = "black",
#            fill = "dodgerblue") +
#   geom_text(aes(x = rate_N_valueg_above0_5_base_10kph,
#                 y = n + 5,
#                 label = n)) +
#   scale_x_continuous(#limits = c(-0.1, 3.1),
#     breaks = c(0, 1, 2, 3),
#     labels = c("0", "1", "2", ">3")) +
#   labs(x = "N Harsh Driving Events per Driving Hour",
#        y = "N Vehicles",
#        title = "Number of harsh driving events per driving hour, on average",
#        subtitle = "Harsh breaking events are instances where acceleration is >0.5g",
#        caption = "Calculating as: (N harsh breaking events) / (time traveling above 10 km/h)") +
#   theme_classic2() +
#   theme(plot.title = element_text(size = 9),
#         plot.subtitle = element_text(size = 8, face = "bold"),
#         plot.caption = element_text(size = 6),
#         axis.title = element_text(size = 8)) 
# 
# ggsave(filename = file.path(brief_figures_dir, "harsh_breaking_distribution.png"),
#        height = 2.25, width = 4)