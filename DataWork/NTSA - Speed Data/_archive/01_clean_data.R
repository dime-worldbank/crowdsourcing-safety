# Clean NTSA Speed Data

# Load data --------------------------------------------------------------------
ntsa_df <- read_csv(file.path(ntsa_speed_dir, "RawData", "Speed limiter transmission data 20220907.csv"))

ntsa_df <- ntsa_df %>%
  clean_names() %>%
  dplyr::rename(datetime = date_17,
                regno = car_plate_registration_number) %>%
  dplyr::mutate(regno = regno %>% str_replace_all(" ", "") %>% str_squish() %>% tolower())

ntsa_df$datetime
ntsa_df$regno

a <- load_st_raw(dates = c("2021-02-21"),
                 vehicles = c("kbl729y"))

load_st_raw <- function(dates, 
                        vehicles){
