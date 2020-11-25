# Append Data for Picking Winners

data <- read_xlsx(file.path(sensors_pilot_dir, "RawData", 
                            "Matt_KBA_970C_Lions-_Trip_and_Eco-driving_detalization_19.11.2020_23-45-06.xlsx"),5)

data <- data %>%
  separate(col =Coordinates,
           into = c("lat", "lon"),
           sep = ",") %>%
  mutate(lat = lat %>% as.numeric(),
         lon = lon %>% as.numeric()) %>%
  mutate(Time = Time %>% dmy_hms()) %>%
  mutate(date = Time %>% as.Date())

# Map --------------------------------------------------------------------------
expand <- 1.5/111.12
basemap <- get_stamenmap(bbox = c(left   = min(data$lon, na.rm=T) - expand, 
                                  bottom = min(data$lat, na.rm=T) - expand, 
                                  right  = max(data$lon, na.rm=T) + expand, 
                                  top    = max(data$lat, na.rm=T) + expand),
                         zoom = 12)


ggmap(basemap) +
  geom_point(data = data %>%
               filter(date %in% as.Date(c("2020-10-24",
                                          "2020-10-25",
                                          "2020-10-26",
                                          "2020-10-27"))) %>%
               arrange(Speed),
             aes(x = lon, y=lat),
             size = 1.2,
             color = "black") +
  geom_point(data = data %>%
               filter(date %in% as.Date(c("2020-10-24",
                                        "2020-10-25",
                                        "2020-10-26",
                                        "2020-10-27"))) %>%
               arrange(Speed),
             aes(x = lon, y=lat, color = Speed),
             size = 1) +

  labs(title = "Routes and Speeds Over Example Four Day Period\nfor Matatu KBA 970C\n",
       color = "Speed\n(km/hr)") +
  scale_color_gradientn(colours = rev(brewer.pal(n = 7, name = "Spectral"))) + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 10),
        legend.position = "bottom") +
  facet_wrap(~date) +
  ggsave(filename = file.path(sensors_pilot_dir, 
                              "Outputs",
                              "example_speed_map.png"),
         height = 3.5, width = 4.5)



  





