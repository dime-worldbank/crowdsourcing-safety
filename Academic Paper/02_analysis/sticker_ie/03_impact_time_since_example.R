# Sticker IE

# Load data --------------------------------------------------------------------
sensor_df <- readRDS(file.path(data_dir, "FinalData", "sensor_sticker_ie_data.Rds"))

# Estimate ---------------------------------------------------------------------
es_att <- feols(prop_time_over_90kph_base_80kph ~ days_since_stk_install_fct | date + regno,
                vcov = ~regno+date,
                data = sensor_df %>% filter(stickers_installed == TRUE))

es_att_p_df <- summary(es_att)$coeftable %>% 
  as.data.frame() %>%
  clean_names()

es_att_df <- es_att %>%
  confint() %>%
  as.data.frame() %>%
  clean_names() %>%
  rownames_to_column(var = "days_since_treat") %>%
  dplyr::mutate(days_since_treat = days_since_treat %>%
                  str_replace_all("days_since_stk_install_fct", "") %>%
                  as.numeric()) %>%
  mutate(beta = (x2_5_percent + x97_5_percent)/2)

es_att_df$p_value <- es_att_p_df$pr_t

es_att_df <- es_att_df %>%
  mutate(p_value_fct = case_when(
    p_value <= 0.01 ~ "p < 0.01",
    p_value <= 0.05 ~ "p < 0.05",
    p_value <= 0.1  ~ "p < 0.1",
    p_value > 0.1 ~ "p > 0.1"
  ))

# Figure -----------------------------------------------------------------------
p <- es_att_df %>%
  filter(abs(days_since_treat) <= 30) %>%
  ggplot(aes(x = days_since_treat,
             y = beta,
             ymin = x2_5_percent,
             ymax = x97_5_percent,
             color = p_value_fct)) +
  geom_vline(xintercept = 0, color = "black", alpha = 0.7, linetype = "dotted") +
  geom_hline(yintercept = 0, color = "black", alpha = 0.7, linetype = "dotted") +
  geom_linerange() +
  geom_point() +
  labs(x = "Days Since Sticker Installation",
       y = "Coef\n+/- 95% CI",
       color = "Significance",
       title = "Dependent Variable: Proportion of time > 90 km/h, when traveling above 80 km/h") +
  scale_color_manual(values = c("firebrick2", "black")) +
  theme_classic2() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(face = "bold", size = 12))
p
ggsave(p,
       filename = file.path(figures_dir, 
                            "sticker_ie_days_since_speed_example.png"),
       height = 3, width = 10)
