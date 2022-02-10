# Load raw data from sensor tracing data for specific time interval and/or vehicle

vehicles <- c("kck139y", "kcq012w")
dates <- c("2021-12-06", "2021-12-07", "2021-12-08", "2021-07-04")

load_st_raw <- function(dates, 
                        vehicles){
  
  ## Grab all files
  print("Grabing files...")
  files <- file.path(sensors_dir, "RawData", "sensor_tracing_individual_data") %>% 
    list.files(recursive = T, full.names = T,
               pattern = ".gz.parquet") 
  
  print("Subsetting...")
  ## Subset vehicles
  if(!("all" %in% vehicles)){
    
    users_df <- read.csv(file.path(sensors_dir, "RawData", "wialon_ids", "user_ids.csv"))
    users_df$regno_clean <- users_df$nm %>% 
      str_sub(-9,-1) %>%
      str_replace_all(" ", "") %>%
      str_squish() %>%
      tolower()
    
    veh_ids <- users_df$id[users_df$regno_clean %in% vehicles] %>%
      paste(collapse = "|")
    
    files <- files %>%
      str_subset(veh_ids)
  }
  
  ## Subset Dates
  if(!("all" %in% dates)){
    
    dates_rx <- dates %>% paste(collapse = "|")
    
    files <- files %>%
      str_subset(dates_rx)
  }
  
  print(paste0("Appending ", length(files), " files..."))
  
  df <- map_df(files, function(file_i){
    df_i <- read_parquet(file_i) 
    if(nrow(df_i) %in% 0){
      out <- NULL 
    } else{
      out <- df_i %>%
        dplyr::mutate(reg_no_id = reg_no_id %>% as.character())
    }
  
    return(out)
  })
  
  return(df)
}
