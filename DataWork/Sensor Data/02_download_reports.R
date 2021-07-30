# Get IDs needed for Wialon API request

# Saves datasets with IDs for
# (1) User IDs (vehicle IDs)
# (2) Resource ID (resource associated with lions auto account)
# (3) Report IDs (for sensor tracing, echo driving, etc reports)

# Load Data --------------------------------------------------------------------
wailon_token <- read.table(file.path(sensors_dir, "wialon_token", "wialon_token.txt"), stringsAsFactors = F)$V1

users_df <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "user_ids.csv"))
resource_df <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "resource_ids.csv"))
report_df <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "report_ids.csv"))

##
user_id <- users_df$id[users_df$nm %in% "Noah KCR 318T"]
report_id <- report_df$id[report_df$n %in% "Eco Driving Report"]
resource_id <- resource_df$id
datetime_begin <- "2021-07-20 01:01:01" %>% ymd_hms() %>% as.numeric()
datetime_end <- "2021-07-29 01:01:01" %>% ymd_hms() %>% as.numeric()

a <- get_report(user_id = users_df$id[users_df$nm %in% "Noah KCR 318T"], 
                report_id = report_df$id[report_df$n %in% "Sensor Tracing"], 
                resource_id = resource_id, 
                datetime_begin = datetime_begin, 
                datetime_end = datetime_end,
                wailon_token)



