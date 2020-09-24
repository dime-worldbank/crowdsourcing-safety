# PSV Rider Feedback: Clean Data

# Load Data --------------------------------------------------------------------
feedback_df <- readRDS(file.path(dropbox_file_path, "Data", "Rider Feedback", "Echo Mobile Data", "FinalData", 
                          "echo_data.Rds"))

matatu_df <- read_excel(file.path(dropbox_file_path, "Data", "Matatu Data", "Characteristics", "RawData", 
                                 "vehicle_basic_info.xlsx"))

# Stats ------------------------------------------------------------------------

## Number of completed reports
sum(!is.na(feedback_df$complete_date))

## Number of Matatus
matatu_df %>%
  filter(pilot_number %in% 1:2) %>%
  nrow()

