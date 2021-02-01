# Append Randomization Files

# 1_randomize_sacco.R creates a separate .csv file for each SACCO. This script
# appends those files together and saves a separate CSV

file.path(dropbox_file_path, 
          "Data", "Matatu Data", "Listing_202101", "randomization", "file_per_sacco") %>%
  list.files(pattern = ".csv", full.names = T) %>%
  lapply(read.csv) %>%
  bind_rows() %>%
  write.csv(file.path(dropbox_file_path, 
                      "Data", "Matatu Data", "Listing_202101", "randomization",
                      "sacco_randomization_appended.csv"),
            row.names = F)
