# Make List of Valid Reg Numbers

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(matatu_data_dir, "FinalData", "individual_files", "psv_num.Rds"))
veh_df <- readRDS(file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))

sticker_df <- read_csv(file.path(data_dir, "Sticker Installation Survey", "RawData", "psv_sticker_installation_WIDE.csv"))

veh_df <- veh_df %>%
  mutate(reg_no = reg_no %>% tolower() %>% str_replace_all(" ", ""))

sticker_df <- sticker_df %>%
  mutate(reg_no = matatu_regno %>% tolower() %>% str_replace_all(" ", ""))

veh_df <- veh_df[veh_df$reg_no %in% sticker_df$reg_no,]

veh_df <- veh_df %>%
  dplyr::select(psv_num, reg_no)

write_csv(veh_df, file.path(data_dir, "Valid Reg Numbers", "valid_psv_nums.csv"))




# veh_df <- veh_df %>%
#   dplyr::filter(drvr_feedback_treat_sticker %in% 1) %>%
#   dplyr::select(psv_num, reg_no)
# 
# write_csv(veh_df, file.path(data_dir, "Valid Reg Numbers", "valid_psv_nums.csv"))

