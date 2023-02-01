# Merge translations with main data

# Load data --------------------------------------------------------------------
comments_en_df <- file.path(data_dir, "Rider Feedback", "FinalData",
                                 "comments_translated", "daily") %>%
  list.files(full.names = T) %>%
  map_df(readRDS)

saveRDS(comments_en_df, file.path(data_dir, "Rider Feedback", "FinalData", 
                                  "comments_translated",
                                  "comment_english.Rds"))
