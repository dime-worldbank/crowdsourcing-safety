# Get IDs needed for Wialon API request

# Saves datasets with IDs for
# (1) User IDs (vehicle IDs)
# (2) Resource ID (resource associated with lions auto account)
# (3) Report IDs (for sensor tracing, echo driving, etc reports)

# Wialon Token/Params ----------------------------------------------------------
wailon_token <- read.table(file.path(sensors_dir, "wialon_token", "wialon_token.txt"), stringsAsFactors = F)$V1
SID <- get_sid(wailon_token)

# User IDs ---------------------------------------------------------------------
users_df <- get_ids(SID, "avl_unit", 1)
write.csv(users_df, file.path(sensors_dir, "RawData", "wialon_ids", "user_ids.csv"), row.names = F)

# Resource and reports ---------------------------------------------------------
res_rep_df <- get_ids(SID, "avl_resource", 8193)

## Resource
resource_df <- res_rep_df
resource_df$rep <- NULL
write.csv(resource_df, file.path(sensors_dir, "RawData", "wialon_ids", "resource_ids.csv"), row.names = F)

## Report
report_df <- res_rep_df$rep %>% map_df(function(i){i})
write.csv(report_df, file.path(sensors_dir, "RawData", "wialon_ids", "report_ids.csv"), row.names = F)




