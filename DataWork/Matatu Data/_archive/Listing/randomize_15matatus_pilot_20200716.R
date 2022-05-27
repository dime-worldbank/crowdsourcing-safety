# PSV Rider Feedback

set.seed(42)

# Load Data --------------------------------------------------------------------
matatu_df <- readRDS(file.path(dropbox_file_path, "Data", "Matatu Data", "Listing", "FinalData", 
                             "listing.Rds"))
matatu_df <- matatu_df[matatu_df$listing_provided %in% as.Date("2020-07-16"),]

matatu_df$rand_num <- runif(nrow(matatu_df))
matatu_df <- matatu_df %>%
  arrange(rand_num)

matatu_15 <- bind_rows(
  matatu_df %>%
    filter(sacco %in% "kigumo") %>%
    head(5),
  
  matatu_df %>%
    filter(sacco %in% "mtn") %>%
    head(5),
  
  matatu_df %>%
    filter(sacco %in% "ntk") %>%
    head(5)
) %>%
  dplyr::select(reg_number, sacco) 

matatu_15$sticker_type <- rep(c("get_airtime", "get_airtime", "win_airtime", "win_airtime", "qr_only"), 3)

# Reg Number variations --------------------------------------------------------
matatu_15$reg_number_lwr <- matatu_15$reg_number %>% tolower()
matatu_15$reg_number_nospc <- matatu_15$reg_number %>% str_replace_all(" ", "")
matatu_15$reg_number_lwr_nospc <- matatu_15$reg_number_lwr %>% str_replace_all(" ", "")

# Export -----------------------------------------------------------------------
write.csv(matatu_15, file.path(dropbox_file_path, "Data", "Matatu Data", "Listing", "FinalData", 
                               "randomization",
                          "randomization_20200716.csv"), 
          row.names = F)



