# Tom Harris
# Before and after analysis

# n.b. Before any new scripts, run psv_feedback_master.R

# Installing packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyr,
               dplyr,
               plyr,
               ggpmisc,
               wesanderson,
               plotly,
               ggplot2,
               data.table)

# Loading data
sensor_data <-
  readRDS(file.path(sensors_dir, "FinalData", "sensor_day.Rds"))

# sensor_data$sacco <- sensor_data$sacco.x

feedback_data <-
  readRDS(file.path(rider_feedback_dir, "FinalData", "rider_feedback.Rds"))

## Looks like the translated data isn't there anymore...?
# feedback_trans_data <- readRDS(file.path(
#   rider_feedback_dir,
#   "FinalData",
#   "rider_feedback_translated.Rds"
# ))


### Grouping the feedback data

#### Simple Average method (I'm not sure about this...) ####

# Load sample data

# First, converting the safety and speed factor variables into numeric variables
# which can be gruoped and averaged by regno_clean

feedback_data$safety_numeric <- feedback_data$safety_label_en

feedback_data$safety_numeric <-
  mapvalues(
    feedback_data$safety_numeric,
    from = c("Not Very Safe",
             "Not Safe",
             "Safe",
             "Very Safe"),
    #I'm not sure here if 'not very safe' is better than 'not safe'?
    to = c(4, 3, 2, 1)
  )

feedback_data$safety_numeric <-
  as.numeric(feedback_data$safety_numeric)

# converting speed feedback to numeric

feedback_data$speed_numeric <- feedback_data$speed_label_en

feedback_data$speed_numeric <-
  mapvalues(
    feedback_data$speed_numeric,
    from = c(
      "Very slow [0-10 km/h]",
      "Slow [10-30]",
      "Average [30-50]",
      "Fast [50-80]",
      "Very fast [80+]"
    ),
    to = c(1, 2, 3, 4, 5)
  )

feedback_data$speed_numeric <-
  as.numeric(feedback_data$speed_numeric)

#### Before / after analysis with feedback data ####
# Generating variable for 'number of days since sticker installed'...

feedback_data$days_since_sticker <-
  as.numeric(
    difftime(
      feedback_data$completion_date,
      feedback_data$sticker_install_date,
      units = "days"
    )
  )

# an issue here is there are no points 'before', i.e. with a negative number of days
# that's because for observations where the sticker hasn't been installed yet, the date is NA


## Plotting
ggplot(feedback_data) +
  aes(x = days_since_sticker, y = safety_numeric) +
  geom_point(shape = "circle",
             size = 1.5,
             colour = "#112446") +
  theme_minimal()



#### Before / after analysis with sensor data ####

#Rob's code
df_clean <- sensor_data %>%
  
  # Only consider vehicles with sensor installed
  dplyr::filter(sticker_installed %in% T) %>%
  
  # Days since installation
  dplyr::mutate(days_since_sticker = as.numeric(date - sticker_install_date)) %>%
  
  # Only look 30 days before/after installed
  dplyr::filter(abs(days_since_sticker) <= 30) %>%
  
  # Make facet title
  dplyr::mutate(
    ftitle = paste0(
      regno_clean,
      "\n",
      "NS: ",
      n_stickers_installed,
      "; ",
      "NF: ",
      n_rider_feedback_total
    )
  )
df_clean %>%
  ggplot() +
  geom_vline(aes(xintercept = 0),
             color = "red") +
  geom_line(aes(x = days_since_sticker,
                y = speed_max)) +
  facet_wrap( ~ ftitle) +
  labs(x = "Days Since Sticker Installed",
       note = "NS = Number of stickers installed; NF = number of passenger feedback surveys") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))


## adsf

sensor_plot_data <- sensor_data %>%
  
  # Only consider vehicles with sensor installed
  dplyr::filter(sticker_installed %in% T) %>%
  
  # Days since installation
  dplyr::mutate(days_since_sticker = as.numeric(date - sticker_install_date)) %>%
  
  # Only look 30 days before/after installed
  dplyr::filter(abs(days_since_sticker) <= 50)


# Need a variable which measures number of violations per km, otherwise
# biased by buses which drive longer routes.
sensor_plot_data$over_80_by_km <-
  sensor_plot_data$N_speed_over_80 / sensor_plot_data$distance_km


# Generating variable to capture all g-force related violations
sensor_plot_data$total_g_violations <-
  sensor_plot_data$N_violation_acceleration +
  sensor_plot_data$N_violation_brake +
  sensor_plot_data$N_violation_turn


plot_1 <-
  ggplot(
    sensor_plot_data,
    aes(
      x = days_since_sticker,
      y = over_80_by_km,
      color = regno_clean,
      group = 1
    )
  ) +
  geom_vline(aes(xintercept = 0),
             color = "red") +
  geom_point(alpha = 0.2) +
  geom_smooth(method = loess, formula = y ~ x) +
  labs(x = "Days Since Sticker Installed", y = ">80km/h violations per km") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

plot_1

ggplotly(plot_1)


plot_2 <-
  ggplot(
    sensor_plot_data,
    aes(
      x = days_since_sticker,
      y = sensor_plot_data$N_speed_over_100,
      color = regno_clean,
      group = 1
    )
  ) +
  geom_vline(aes(xintercept = 0),
             color = "red") +
  geom_point(alpha = 0.2) +
  geom_smooth(method = loess, formula = y ~ x) +
  labs(x = "Days Since Sticker Installed", y = "Number of readings >100km/h") +
  theme_minimal() + ylim(0, 50) +
  theme(strip.text = element_text(face = "bold"))

plot_2

ggplotly(plot_1)


plot_3 <-
  ggplot(
    sensor_plot_data,
    aes(
      x = days_since_sticker,
      y = sensor_plot_data$total_g_violations,
      color = regno_clean,
      group = 1
    )
  ) +
  geom_vline(aes(xintercept = 0),
             color = "red") +
  geom_point(alpha = 0.2) +
  geom_smooth(method = loess, formula = y ~ x) +
  labs(x = "Days Since Sticker Installed", y = "Number of G force violations") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

plot_3

ggplotly(plot_3)


#### Now looking at date since first rider feedback ####

#Generating a variable for days since first feedback
sensor_plot_data$days_since_first_feedback <-
  as.numeric(sensor_plot_data$date - sensor_plot_data$date_first_rider_feedback)

sensor_plot_data <- sensor_plot_data %>%
  filter(abs(sensor_plot_data$days_since_first_feedback) <= 50)

sensor_feedback_plot_data <-
  subset(sensor_plot_data,
         sensor_plot_data$drvr_feedback_treat_feedback == 1)


plot_4 <-
  ggplot(
    sensor_feedback_plot_data,
    aes(
      x = sensor_feedback_plot_data$days_since_first_feedback,
      y = over_80_by_km,
      color = regno_clean,
      group = 1
    )
  ) +
  geom_vline(aes(xintercept = 0),
             color = "red") +
  geom_point(alpha = 0.2) +
  geom_smooth(method = loess, formula = y ~ x) +
  labs(x = "Days Since First Feedback Received", y = ">80km/h violations per km") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

plot_4

ggplotly(plot_4)
## issue here seems to be that very few vehicles have started receiving feedback and the
# ones which have started receiving feedback differ in certain ways.


plot_5 <- 
  ggplot(
    sensor_feedback_plot_data,
    aes(
      x = sensor_feedback_plot_data$days_since_first_feedback,
      y = sensor_feedback_plot_data$total_g_violations,
      color = regno_clean,
      group = 1
    )
  ) +
  geom_vline(aes(xintercept = 0),
             color = "red") +
  geom_point(alpha = 0.2) +
  geom_smooth(method = loess, formula = y ~ x) +
  labs(x = "Days Since First Feedback Received", y = "G violations") +
  theme_minimal() + ylim(0, 150) +
  theme(strip.text = element_text(face = "bold"))

plot_5

ggplotly(plot_5)
