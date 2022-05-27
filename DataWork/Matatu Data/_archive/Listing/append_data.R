# PSV Rider Feedback

# Read Data --------------------------------------------------------------------
listing_20200716_path <- file.path(dropbox_file_path, "Data", "Matatu Data", 
                                   "Listing", "RawData", "vehicle_list_provided_20200716")

kigumo_df <- read_excel(file.path(listing_20200716_path, "Kigumo Vehicles.xlsx")) %>%
  mutate(sacco = "kigumo")

mtn_df <- read_excel(file.path(listing_20200716_path, "MTN Vehicles.xlsx")) %>%
  mutate(sacco = "mtn")

ntk_df <- read_excel(file.path(listing_20200716_path, "NTK Vehicles.xlsx")) %>%
  mutate(sacco = "ntk")

matatu_df <- bind_rows(kigumo_df,
                       mtn_df,
                       ntk_df) %>%
  dplyr::rename(reg_number = "Reg Number") %>%
  dplyr::select(reg_number, sacco) %>%
  dplyr::mutate(listing_provided = as.Date("2020-07-16"),
                reg_number = reg_number %>% 
                  str_replace_all(" ", "") %>% 
                  toupper() %>% 
                  str_replace_all('^(.{3})(.*)$', '\\1 \\2')) %>%
  distinct()

matatu_df <- matatu_df[nchar(matatu_df$reg_number) != 3,]

## Check for duplicated plates
matatu_df <- matatu_df %>%
  dplyr::group_by(reg_number) %>%
  dplyr::mutate(N = n())

## Only keep if unique
matatu_df <- matatu_df[matatu_df$N %in% 1,]

# Export -----------------------------------------------------------------------
write.csv(matatu_df, file.path(dropbox_file_path, "Data", "Matatu Data", "Listing", "FinalData", 
                          "listing.csv"), row.names = F)
saveRDS(matatu_df, file.path(dropbox_file_path, "Data", "Matatu Data", "Listing", "FinalData", 
                        "listing.Rds"))



