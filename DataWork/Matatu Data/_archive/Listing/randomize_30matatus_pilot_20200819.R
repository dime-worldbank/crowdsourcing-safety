# PSV Rider Feedback

set.seed(42)

# Load Data --------------------------------------------------------------------
matatu_df <- readRDS(file.path(dropbox_file_path, "Data", "Matatu Data", "Listing", "FinalData", 
                               "listing.Rds"))
matatu_df <- matatu_df[matatu_df$listing_provided %in% as.Date("2020-07-16"),]

matatu_df <- matatu_df %>% 
  arrange(reg_number)

# Remove past choosen matatus --------------------------------------------------
#### File list
randomiz_files <- list.files(file.path(dropbox_file_path, "Data", "Matatu Data", "Listing", "FinalData", 
                                      "randomization"), pattern = "*.csv",
                            full.names = T)

#### Only keep randomization files from previous randomizations
randomiz_dates <- randomiz_files %>% 
  str_replace_all(".*randomization_", "") %>% 
  str_replace_all(".csv", "")

randomiz_dates_tf <- randomiz_dates < "20200819" 
randomiz_files <- randomiz_files[randomiz_dates_tf]

#### Remove
past_randomiz_df <- randomiz_files %>%
  lapply(read.csv) %>%
  bind_rows()

matatu_df <- matatu_df[!(matatu_df$reg_number %in% past_randomiz_df$reg_number),]

# Randomization ----------------------------------------------------------------
matatu_df$rand_num <- runif(nrow(matatu_df))
matatu_df <- matatu_df %>%
  arrange(rand_num)

matatu_rand <- bind_rows(
  matatu_df %>%
    filter(sacco %in% "kigumo") %>%
    head(6),
  
  matatu_df %>%
    filter(sacco %in% "mtn") %>%
    head(8),
  
  matatu_df %>%
    filter(sacco %in% "ntk") %>%
    head(6)
) %>%
  dplyr::select(reg_number, sacco) 

matatu_rand$sticker_type <- rep(c("shortcode_qr", "qr"), 10)

# Export -----------------------------------------------------------------------
write.csv(matatu_rand, file.path(dropbox_file_path, "Data", "Matatu Data", "Listing", "FinalData", 
                               "randomization",
                               "randomization_20200819.csv"), row.names = F)



