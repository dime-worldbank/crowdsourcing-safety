# Tom Harris
# Event study analysis 1

# n.b. Before any new scripts, run psv_feedback_master.R

# Installing packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyr,
  dplyr,
  plyr,
  ggpmisc,
  wesanderson,
  plotly,
  ggplot2,
  tidymodels,
  data.table,
  tidyverse,
  haven,
  lfe
)

# Loading data
sensor_data <-
  readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))


# Creating data for analysis

# Creating data for plot
sensor_data_clean <- sensor_data %>%
  
  # Only consider vehicles with sensor installed
  dplyr::filter(sticker_installed %in% T) %>%
  
  # Days since installation
  dplyr::mutate(days_since_sticker = as.numeric(date - sticker_install_date)) %>%
  
  # Only look 30 days before/after installed
  dplyr::filter(abs(days_since_sticker) <= 30)


# Need a variable which measures number of speed violations per km, otherwise
# biased by buses which drive longer routes.
sensor_data_clean$over_80_by_km <-
  sensor_data_clean$N_speed_over_80 / sensor_data_clean$distance_km

# Generating variable to capture all g-force related violations
# should this also be 'per km'?
sensor_data_clean$total_g_violations <-
  sensor_data_clean$N_violation_acceleration +
  sensor_data_clean$N_violation_brake +
  sensor_data_clean$N_violation_turn

###

analysis_data <- sensor_data_clean %>%
  group_by(regno_clean) %>%
  mutate(
    days_since_sticker = as.numeric(date - sticker_install_date),
    after = ifelse(days_since_sticker > 0, TRUE, FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    days_since_sticker = relevel(as.factor(days_since_sticker), ref = "-1"),
    regno_clean = as.factor(regno_clean)
  )



# Regression results

res_es <-
  felm(over_80_by_km ~ factor(days_since_sticker) |
         regno_clean | 0 | regno_clean,
       data = analysis_data)
res_es

res_dind <-
  felm(over_80_by_km ~ after |
         regno_clean | 0 | regno_clean, data = analysis_data)
res_dind

# Variable label
labels <- c()
var_list <- c()
for (num in seq(-30, 30)) {
  if (num != -1) {
    labels <- c(labels, num)
    var_list <-
      c(var_list,
        paste0("factor(days_since_sticker)", as.character(num)))
  }
}

# Figure data
fig_data <- tibble(
  label = labels,
  es_coef = summary(res_es)$coef[var_list, "Estimate"] * 100,
  es_se = summary(res_es)$coef[var_list, "Cluster s.e."] * 100,
  dind_coef = summary(res_dind)$coef["afterTRUE", "Estimate"] * 100,
  dind_se = summary(res_dind)$coef["afterTRUE", "Cluster s.e."] * 100
) %>%
  add_row(label = -1,
          es_coef = 0,
          es_se = 0) %>%
  mutate(
    dind_coef = ifelse(label >= 0, dind_coef, 0),
    dind_se = ifelse(label >= 0, dind_se, 0)
  )

# Figure
ggplot(fig_data, aes(x = label, y = es_coef)) +
  geom_pointrange(aes(ymin = es_coef - 1.96 * es_se, ymax = es_coef + 1.96 * es_se),
                  alpha = 0.7) +
  geom_vline(
    xintercept = -0.5,
    alpha = 0.3,
    linetype = "dashed",
    size = 0.3
  ) +
  geom_line(
    data = fig_data,
    aes(
      x = label,
      y = dind_coef,
      group = (label >= 0)
    ),
    color = "red",
    alpha = 0.7
  ) +
  geom_line(
    data = fig_data,
    aes(
      x = label,
      y = dind_coef + 1.96 * dind_se,
      group = (label >= 0)
    ),
    linetype = "dashed",
    color = "red",
    alpha = 0.7
  ) +
  geom_line(
    data = fig_data,
    aes(
      x = label,
      y = dind_coef - 1.96 * dind_se,
      group = (label >= 0)
    ),
    linetype = "dashed",
    color = "red",
    alpha = 0.7
  ) +
  theme_classic() +
  geom_hline(yintercept = 0,
             alpha = 0.5,
             size = 0.5) +
  ylab("Coefficient estimates & 95% CI") +
  xlab("Days relative to installation of sticker") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
