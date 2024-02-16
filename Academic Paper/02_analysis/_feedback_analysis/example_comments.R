# Example comments

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))


fb_df %>%
  filter(q_comment_nchar >= 10) %>%
  pull(q_comment) %>%
  tolower() %>%
  str_subset("nice")

