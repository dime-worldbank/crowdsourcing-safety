# PSV Rider Feedback: Figures of Main Results

# Load Data --------------------------------------------------------------------
feedback_df <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "All Data", "FinalData", 
                                 "rider_feedback.Rds"))

# Prep Data --------------------------------------------------------------------
data_long <- data %>%
  dplyr::select(reg_no, 
                driver_rating, speed_rating, occupancy, covid_measures) %>%
  pivot_longer(-reg_no) %>%
  
  filter(value != "",
         !is.na(value)) %>%
  
  # Number of responses per question, per vehicle
  group_by(reg_no, name) %>%
  mutate(N_veh = n()) %>%
  ungroup() %>%
  
  # Aggregate
  group_by(name, value, reg_no, N_veh) %>%
  summarise(N = n()) %>%
  ungroup()  %>%
  
  # Clean variables
  mutate(reg_no_N_veh = paste0(reg_no, " - ", N_veh),
         N = N/N_veh,
         reg_no = reg_no %>% toupper())

data_long$name[data_long$name %in% "driver_rating"] <- "How would you rate your Matatu driver?"
data_long$name[data_long$name %in% "covid_measures"] <- "Were measures taken to prevent spread of COVID-19?"
data_long$name[data_long$name %in% "occupancy"] <- "On the Matatu, are there"
data_long$name[data_long$name %in% "speed_rating"] <- "How would you describe your Matatu driver's speed?"

data_long$value <- data_long$value %>% str_squish() %>% tolower()

data_long$value <- data_long$value %>% tools::toTitleCase()

data_long$value <- factor(data_long$value, levels = c("No",
                                                      "Yes, but Seemed Limited",
                                                      "Yes, Fffective",
                                                      
                                                      "Very Unsafe",
                                                      "Unsafe",
                                                      "Safe",
                                                      "Very Safe",
                                                      
                                                      "Less People than Seats",
                                                      "Same Number of People as Seats",
                                                      "More People than Seats",
                                                      "More People than can Fit",
                                                      
                                                      "Too slow",
                                                      "Okay",
                                                      "Fast",
                                                      "Dangerously Fast"))



data_long %>%
  filter(name %in% "How would you rate your Matatu driver?") %>%
  
  group_by(reg_no) %>%
  mutate(ordernum = N[value %in% "Very Safe"][1]) %>%
  ungroup() %>%
  
  filter(N_veh >= 10,
         reg_no != "UNKNOWN") %>%
  ggplot(aes(x = reorder(reg_no, ordernum), # 
             y = N, group=value, fill=value)) +
  geom_col(color="black",
           size = .1) +
  labs(x="", y="",
       title = "How would you rate your Matatu driver?") +
  scale_fill_manual(values = c("firebrick4", "firebrick1",
                               "palegreen3", "palegreen"),
                    guide = guide_legend(reverse = TRUE) ) +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum() +
  theme(plot.title = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  coord_flip() +
  labs(x = "Matatu", y = "Percent of Ratings", fill = "") 



"asdadsasd" %>% str_replace_all("{4}\\1", " ")


