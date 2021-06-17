# Sacco Listing Randomization

# There is an excel file for each SACCO. This script imports the file, randomizes
# the order, and saves a clean .csv file with the listing

# Setup path to data
sacco_listing_dir <- file.path(dropbox_file_path, "Data", "Matatu Data", 
                               "Listing_202102", "sacco_listing_digitized")
output_dir        <- file.path(dropbox_file_path, "Data", "Matatu Data", 
                               "Listing_202102", "sacco_listing_digitized_randomized", 
                               "file_per_sacco")

# Load and append all the sacco files
sacco_file_names <- sacco_listing_dir %>%
  list.files(pattern = "*.xlsx", full.names= F) 

for(file_name in sacco_file_names){
  set.seed(42)
  
  ## Load Data
  sacco_df <- read_xlsx(file.path(sacco_listing_dir, file_name))
  
  ## Randomly Sort
  sacco_df <- sacco_df[order(runif(nrow(sacco_df))),]
  
  ## Cleanup
  sacco_df <- sacco_df %>%
    dplyr::mutate(order = 1:n()) %>%
    dplyr::select(order, everything())
  
  ## Save
  write.csv(sacco_df, 
            file.path(output_dir, 
                      file_name %>% str_replace_all(".xlsx$", ".csv")),
            row.names = F
  )
}


