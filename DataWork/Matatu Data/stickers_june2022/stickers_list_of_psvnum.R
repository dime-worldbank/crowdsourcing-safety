
## Load data
veh_df <- readRDS(file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))

## Prep data
veh_df <- veh_df %>%
  dplyr::filter(drvr_feedback_treat_sticker %in% 1,
                phase %in% "june2022-sensor-driverfeedback") 

veh_df <- veh_df %>%
  dplyr::select(reg_no, psv_num)

## Export as csv [For requiring response]
write_csv(veh_df, file.path(matatu_data_dir, "Outputs", "june22_psvnums.csv"))

## Comma separated list [For logic handling]
# 3793170 for testing
c(veh_df$psv_num, 3793170) %>% paste(collapse = ",")
