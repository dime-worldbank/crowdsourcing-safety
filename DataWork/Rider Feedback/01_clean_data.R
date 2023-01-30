# Clean Rider Feedback Data

# Load data --------------------------------------------------------------------
df <- read_csv(file.path(data_pii_dir, "Rider Feedback", "RawData - PII", "rider_feedback.csv"))
valid_reg_df <- read_csv(file.path(data_dir, "Valid Reg Numbers", "valid_psv_nums.csv"))

# Clean data -------------------------------------------------------------------
df <- df %>%
  clean_names() %>%
  mutate(phone_hash = phone %>% as.character() %>% md5()) %>%
  dplyr::select(-c(name, phone)) %>%
  dplyr::mutate(comments_label_nchar = comments_label %>% nchar())

df$valid_psvnum <- df$psvnum_label %in% as.character(valid_reg_df$psv_num)

# Export -----------------------------------------------------------------------
write_csv(df, file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback.csv"))
saveRDS(df,   file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback.Rds"))

