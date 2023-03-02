# Tom Harris
# Event study analysis 2
# Running analysis using time spent over 80 km/h as outcome variable

# n.b. Before any new scripts, run psv_feedback_master.R

# Installing further required packages
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
  stargazer,
  tidymodels,
  data.table,
  tidyverse,
  haven,
  lfe
)

#### Loading and joining data ####

# Loading sensor data
sensor_data <-
  readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

# Convert the 'reg_no' column to lower case
sensor_data <- sensor_data %>% mutate(reg_no = tolower(reg_no))

# Loading sensor data with time spent over xkm/h
time_data <-
  read_parquet(file.path(sensors_dir, "FinalData", "time_moving_day.gz.parquet"))

# Remove double spaces from 'reg_no' column
time_data <- time_data %>% mutate(reg_no = gsub("\\s+", " ", reg_no))

# subset to journeys longer than 10 minutes
time_data <- subset(time_data, time_data$time_mov_s > 600)

# Join data by reg_no and date
joined_data <- left_join(time_data, sensor_data, by = c("reg_no", "date"))

# Generating variable for proportion of time spent over 80km/h
joined_data$prop_80 <- joined_data$time_spd80_s / joined_data$time_mov_s

# Generating variable for proportion of time spent over 100km/h
joined_data$prop_100 <- joined_data$time_spd100_s / joined_data$time_mov_s

# Creating data for analysis
sensor_data_clean <- joined_data %>%
  # Only consider vehicles with sensor installed
  dplyr::filter(sticker_installed %in% T) %>%
  # Days since installation
  dplyr::mutate(days_since_sticker = as.numeric(date - sticker_install_date)) %>%
  # Only look 30 days before/after installed
  dplyr::filter(abs(days_since_sticker) <= 30)

# # change so that we only have observations where time driving > 0
# sensor_data_clean <- subset(sensor_data_clean, sensor_data_clean$time_mov_s > 120) # 2 mins

# Generating variable to capture all g-force related violations
# Q. should this also be 'per km'?
sensor_data_clean$total_g_violations <-
  (sensor_data_clean$N_violation_acceleration +
    sensor_data_clean$N_violation_brake +
    sensor_data_clean$N_violation_turn)

# this is now per hour moving
sensor_data_clean$total_g_violations_per_hour <-
  sensor_data_clean$total_g_violations / (sensor_data_clean$time_mov_s / 3600)

# renaming to this for ease ... temporary
sensor_data_clean$total_g_violations <- sensor_data_clean$total_g_violations_per_hour
  

# Preparing data for event study analysis
analysis_data <- sensor_data_clean %>%
  group_by(regno_clean) %>%
  mutate(
    days_since_sticker = as.numeric(date - sticker_install_date),
    after = ifelse(days_since_sticker > 0, TRUE, FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    days_since_sticker = relevel(as.factor(days_since_sticker), ref = "-1"),
    regno_clean = as.factor(regno_clean),
    date = as.factor(date)
  )


#### Event Study Analysis ####

#### Speed Event Study ####

# Event study regression with FE for mutatu and date
res_es <-
  felm(
    prop_80 ~ factor(days_since_sticker) |
      regno_clean + date | 0 | regno_clean,
    data = analysis_data
  )

res_es
stargazer(res_es, type = "text") # visualising results

# Regression of outcome on 'after' variable for overall effect over the entire post period
res_dind <-
  felm(
    prop_80 ~ after |
      regno_clean + date | 0 | regno_clean,
    data = analysis_data
  )

res_dind
stargazer(res_dind, type = "text")


#### Plotting event study ####

# Variable label
labels <- c()
var_list <- c()
for (num in seq(-30, 30)) {
  if (num != -1) {
    labels <- c(labels, num)
    var_list <-
      c(
        var_list,
        paste0("factor(days_since_sticker)", as.character(num))
      )
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
  add_row(
    label = -1,
    es_coef = 0,
    es_se = 0
  ) %>%
  mutate(
    dind_coef = ifelse(label >= 0, dind_coef, 0),
    dind_se = ifelse(label >= 0, dind_se, 0)
  )

# Figure 1

plot_1 <- ggplot(fig_data, aes(x = label, y = es_coef)) +
  geom_pointrange(
    aes(ymin = es_coef - 1.96 * es_se, ymax = es_coef + 1.96 * es_se),
    alpha = 0.7,
    color = "navy"
  ) +
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
  geom_hline(
    yintercept = 0,
    alpha = 0.5,
    size = 0.5
  ) +
  ylab("Coefficient estimates & 95% CI") +
  xlab("Days relative to installation of sticker") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  ggtitle("Event study, Proportion of time >80km/h")

plot_1
ggplotly(plot_1) # interactive version

# Figure 2

fig_data <- tibble(
  label = labels,
  coef = summary(res_es)$coef[var_list, "Estimate"] * 100,
  se = summary(res_es)$coef[var_list, "Cluster s.e."] * 100
) %>%
  add_row(label = -1, coef = 0, se = 0)

plot_2 <- ggplot(fig_data, aes(x = label, y = coef)) +
  geom_point() +
  geom_ribbon(
    aes(
      ymin = coef - 1.645 * se,
      ymax = coef + 1.645 * se,
      fill = "90%"
    ),
    alpha = 0.3
  ) +
  geom_ribbon(aes(
    ymin = coef - 1.96 * se,
    ymax = coef + 1.96 * se,
    fill = "95%"
  ), alpha = 0.2) +
  geom_vline(
    xintercept = -0.5,
    alpha = 0.3,
    linetype = "dashed",
    size = 0.3
  ) +
  theme_classic() +
  geom_hline(
    yintercept = 0,
    alpha = 0.5,
    size = 0.5
  ) +
  scale_fill_manual(
    name = "Confidence Intervals",
    values = c("90%" = "navy", "95%" = "blue")
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = c(0.3, 0.2)))) +
  ylab("Coefficient estimates & CIs") +
  xlab("Days relative to sticker installation") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  ggtitle("Event study, Proportion of time >80km/h")

plot_2
ggplotly(plot_2) # not visualising correctly in my R

#### G-force violations event study ####

# Regression results
res_es <-
  felm(
    total_g_violations ~ factor(days_since_sticker) |
      regno_clean + date | 0 | regno_clean,
    data = analysis_data
  )
res_es

stargazer(res_es, type = "text")

res_dind <-
  felm(
    total_g_violations ~ after |
      regno_clean + date | 0 | regno_clean,
    data = analysis_data
  )
res_dind

stargazer(res_dind, type = "text")


# Variable label
labels <- c()
var_list <- c()
for (num in seq(-30, 30)) {
  if (num != -1) {
    labels <- c(labels, num)
    var_list <-
      c(
        var_list,
        paste0("factor(days_since_sticker)", as.character(num))
      )
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
  add_row(
    label = -1,
    es_coef = 0,
    es_se = 0
  ) %>%
  mutate(
    dind_coef = ifelse(label >= 0, dind_coef, 0),
    dind_se = ifelse(label >= 0, dind_se, 0)
  )


# Figure 1

ggplot(fig_data, aes(x = label, y = es_coef)) +
  geom_pointrange(
    aes(ymin = es_coef - 1.96 * es_se, ymax = es_coef + 1.96 * es_se),
    alpha = 0.7,
    color = "navy"
  ) +
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
  geom_hline(
    yintercept = 0,
    alpha = 0.5,
    size = 0.5
  ) +
  ylab("Coefficient estimates & 95% CI") +
  xlab("Days relative to installation of sticker") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  ggtitle("Event study, G force violations")

# Figure 2

fig_data <- tibble(
  label = labels,
  coef = summary(res_es)$coef[var_list, "Estimate"] * 100,
  se = summary(res_es)$coef[var_list, "Cluster s.e."] * 100
) %>%
  add_row(label = -1, coef = 0, se = 0)

ggplot(fig_data, aes(x = label, y = coef)) +
  geom_point() +
  geom_ribbon(
    aes(
      ymin = coef - 1.645 * se,
      ymax = coef + 1.645 * se,
      fill = "90%"
    ),
    alpha = 0.3
  ) +
  geom_ribbon(aes(
    ymin = coef - 1.96 * se,
    ymax = coef + 1.96 * se,
    fill = "95%"
  ), alpha = 0.2) +
  geom_vline(
    xintercept = -0.5,
    alpha = 0.3,
    linetype = "dashed",
    size = 0.3
  ) +
  theme_classic() +
  geom_hline(
    yintercept = 0,
    alpha = 0.5,
    size = 0.5
  ) +
  scale_fill_manual(
    name = "Confidence Intervals",
    values = c("90%" = "navy", "95%" = "blue")
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = c(0.3, 0.2)))) +
  ylab("Coefficient estimates & CIs") +
  xlab("Days relative to sticker installation") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  ggtitle("Event study, G force violations")
