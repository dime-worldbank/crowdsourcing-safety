# Distance Travelled

veh_df <- readRDS(file.path(data_dir, "FinalData", 
                  paste0("vehicle_level_stickers_telematics_",
                         "cmntfilter",
                         FALSE,
                         "_",
                         "dstnctpass",
                         TRUE,".Rds")))

veh_df$distance_minmax_latlon_daily_km %>% summary()

