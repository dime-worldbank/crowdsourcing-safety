# PSV Rider Feedback: Clean Data

# Load Data --------------------------------------------------------------------
feedback_df <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "All Data", "FinalData", 
                          "rider_feedback.Rds"))

matatu_df <- read_excel(file.path(dropbox_file_path, "Data", "Matatu Data", "Characteristics", "RawData", 
                                 "vehicle_basic_info.xlsx"))

# Stats ------------------------------------------------------------------------

nrow(feedback_df)
feedback_df$reg_no  %>% unique() %>% length()
feedback_df$date %>% min()
feedback_df$date %>% max()

feedback_df %>% 
  filter(reg_no != "UNKNOWN") %>%
  group_by(reg_no) %>%
  summarise(N = n()) %>%
  pull(N) %>%
  max()

## Number of completed reports
sum(!is.na(feedback_df$complete_date))

## Number of Matatus
matatu_df %>%
  filter(pilot_number %in% 1:2) %>%
  nrow()

