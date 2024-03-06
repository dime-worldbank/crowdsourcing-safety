# Sticker Distribution within 150 Distribution

library(fixest)
library(modelsummary)

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", 
                            "vehicle_level_all202_cmntfilterFALSE_dstnctpassTRUE.Rds"))

veh_df <- veh_df %>%
  dplyr::mutate(sticker = !is.na(shortcode_on_sticker)) %>%
  dplyr::filter(!is.na(speed_mean)) 

veh_df$route[is.na(veh_df$route)] <- "Other"

table(veh_df$route)
veh_df$route %>% unique() %>% length()

run_reg <- function(var){
  veh_df$var <- veh_df[[var]]
  
  bind_rows(
    feols(var ~ sticker | route, data = veh_df) %>% 
      confint() %>% 
      clean_names() %>%
      tail(1) %>%
      mutate(fe = "Yes")
  ) %>%
    mutate(variable = var)
}

lm_df <- map_df(c("prop_time_over_80kph_base_10kph",
                  "prop_time_over_90kph_base_10kph",
                  "prop_time_over_100kph_base_10kph",
                  "prop_time_over_90kph_base_80kph",
                  "prop_time_over_100kph_base_80kph",
                  "rate_N_valueg_above0_5_base_10kph"),
                run_reg) %>%
  mutate(b = (x2_5_percent + x97_5_percent)/2) %>%
  mutate(variable_clean = case_when(
    variable == "prop_time_over_80kph_base_10kph" ~ "Prop. Time over 80km/h",
    variable == "prop_time_over_90kph_base_10kph" ~ "Prop. Time over 90km/h",
    variable == "prop_time_over_100kph_base_10kph" ~ "Prop. Time over 100km/h",
    variable == "prop_time_over_90kph_base_80kph" ~ "Prop. Time over 90km/h,\nwhen >80 km/h",
    variable == "prop_time_over_100kph_base_80kph" ~ "Prop. Time over 100km/h,\nwhen >80 km/h",
    variable == "rate_N_valueg_above0_5_base_10kph" ~ "N Harsh Violations\nper Hour"
  ) %>%
    factor(levels = rev(c("Prop. Time over 80km/h",
           "Prop. Time over 90km/h",
           "Prop. Time over 100km/h",
           "Prop. Time over 90km/h,\nwhen >80 km/h",
           "Prop. Time over 100km/h,\nwhen >80 km/h",
           "N Harsh Violations\nper Hour"))))

lm_df %>%
  ggplot(aes(xmin = x2_5_percent,
             xmax = x97_5_percent,
             x = b,
             y = variable_clean)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_linerange() +
  geom_point() +
  labs(x = "Coef (+/- 95% CI)",
       y = NULL) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        #axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.position = "none") 

ggsave(filename = file.path(figures_dir, "reg_sticker_150.png"),
       height = 4, width = 4)



