# Randomization

# R Version: 4.1.3 (ARM)
set.seed(42)

# Load data --------------------------------------------------------------------
veh_df <- read_csv(file.path(matatu_data_dir, "RawData", "vehicle_info.csv"))

veh_df <- veh_df %>%
  arrange(id)

# Assign PSV number ------------------------------------------------------------
psv_nums <- seq(from = 1,
                to = 9999,
                by = 3)

# Must have 3 digits
psv_nums <- psv_nums[psv_nums >= 100]

# Remove across, down, diagonal
psv_nums <- psv_nums[!(psv_nums %in% c(123,321,
                                       456,654,
                                       789,987,
                                       
                                       147,741,
                                       258,852,
                                       369,963,
                                       
                                       159,951,
                                       357,753))]

# Must have 3 unique digits
psv_nums <- lapply(psv_nums, function(x){
  
  x_digits <- x %>% str_split("") %>% unlist() %>% unique()
  if(length(x_digits) %in% 3){
    out <- x
  } else{
    out <- NULL
  }
  
  return(out)
}) %>%
  unlist()

veh_df$psv_num <- psv_nums[1:nrow(veh_df)]

# Export -----------------------------------------------------------------------
veh_df <- veh_df %>%
  dplyr::select(id, psv_num)

saveRDS(veh_df, file.path(matatu_data_dir, "FinalData", "individual_files", "psv_num.Rds"))


