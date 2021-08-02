get_sid <- function(wialon_token){
  # Get SID. The session ID doesn't not stay the same; need to generate
  # the current SID whenever querying data.
  # ARGS
  # -- wialon_token: Wialon token (unique for this account)
  
  req <- paste0("https://hst-api.wialon.com/wialon/ajax.html?svc=token/login&
    params={
      %22token%22:%22",wialon_token,"%22
    }") %>%
    str_replace_all('[\r\n]' , '') %>% 
    str_replace_all(' ', '') 
  
  res = GET(req)
  raw_json <- rawToChar(res$content)
  df <- jsonlite::fromJSON(raw_json)
  
  return(df$eid)
}

get_ids <- function(sid, 
                    items_type, 
                    flag){
  # Get IDs. Function to return IDs, given items type and flag. For example,
  # to get unique IDs of users (vehicles), items_type=avl_unit; flag=1; to
  # get unique IDs of resource and report types, items_type=avl_resource; flag=8193
  # ARGS
  # -- sid: Unique sid (session id)
  # -- items_type: (eg, "avl_unit", "avl_resource")
  # -- flag: Numeric value for flag
  
  
  req <- paste0("
https://hst-api.wialon.com/wialon/ajax.html?svc=core/search_items&
  params={
    %22spec%22:{
      %22itemsType%22:%22",items_type,"%22,
      %22propName%22:%22*%22,
      %22propValueMask%22:%22*%22,
      %22sortType%22:%22*%22
    },
    %22force%22:1,
    %22flags%22:",flag,",
    %22from%22:0,
    %22to%22:0
  }&sid=",sid,"
  ") %>%
    str_replace_all('[\r\n]' , '') %>% 
    str_replace_all(' ', '') 
  
  res = GET(req)
  raw_json <- rawToChar(res$content)
  df <- jsonlite::fromJSON(raw_json)$items
  
  return(df)
}

get_report <- function(user_id, 
                       report_id, 
                       resource_id, 
                       datetime_begin, 
                       datetime_end, 
                       wialon_token,
                       users_df,
                       show_progress = T){
  # Get data from a report.
  # ARGS
  # -- user_id: Unique vehicle ID
  # -- report_id: Unique ID for the report type (eg, id for sensor tracing, echo driving, etc)
  # -- resource_id: Unique ID for the resource (ID associated with the account)
  # -- datetime_begin: Date/time begin (numeric unix time value in UTC)
  # -- datetime_end: Date/time end (numeric unix time value in UTC)
  # -- wialon_token: Wialon token value (string)
  # -- user_df: Dataframe with user IDs and names. Used to add the reg_no
  #             to the dataset.
  # RESOURCES
  # https://sdk.wialon.com/wiki/en/sidebar/remoteapi/codesamples/reports
  
  Sys.sleep(10) # For API limits; Set large until figure out limits.
  
  # API Calls ------------------------------------------------------------------
  # Make calls to the API to get the raw data
  SID <- get_sid(wialon_token)
  
  #### Initial Call
  req1 <- paste0("https://hst-api.wialon.com/wialon/ajax.html?svc=report/exec_report&
    params={
      %22reportResourceId%22:",resource_id,",
      %22reportTemplateId%22:",report_id,",
      %22reportObjectId%22:",user_id,",
      %22reportObjectSecId%22:0,
      %22interval%22:{
        %22from%22:",datetime_begin,",
        %22to%22:",datetime_end,",
        %22flags%22:0
      }
    }&sid=", SID) %>%
    str_replace_all('[\r\n]' , '') %>% 
    str_replace_all(' ', '') 
  
  res1 = GET(req1)
  raw_json1 <- rawToChar(res1$content)
  df1 <- jsonlite::fromJSON(raw_json1)
  
  #### Get Data
  req <- paste0("https://hst-api.wialon.com/wialon/ajax.html?svc=report/select_result_rows&
    params={
      %22tableIndex%22:0,
      %22config%22:{
        %22type%22:%22range%22,
        %22data%22:{
          %22from%22:0,
          %22to%22:0,
          %22level%22:2
        }
      }
    }&sid=", SID) %>%
    str_replace_all('[\r\n]' , '') %>%
    str_replace_all(' ', '')
  
  res = GET(req)
  raw_json <- rawToChar(res$content)
  df <- jsonlite::fromJSON(raw_json)
  
  # Data to dataframe ----------------------------------------------------------
  # Convert data from the raw json to a dataframe
  
  #### Set column names
  # Data originally doesn't contain sensible column names, so need to define here
  
  # Echo Driving Report
  if(report_id %in% 1){
    col_names <- c("no", 
                   "grouping",
                   "violation",
                   "begin_datetime_str",
                   "begin_datetime_int",
                   "latitude_begin",
                   "longitude_begin",
                   "location_begin",
                   "latitude1_begin",
                   "longitude1_begin",
                   
                   "end_datetime_str",
                   "end_datetime_int",
                   "latitude_end",
                   "longitude_end",
                   "location_end",
                   "latitude1_end",
                   "longitude1_end",
                   
                   "value",
                   
                   "latitude2_begin",
                   "longitude2_begin",
                   
                   "max_speed",
                   "time_interval",
                   "distance",
                   "count",
                   "driver")
    
  }
  
  # Sensor Tracing
  if(report_id %in% 2){
    # col_names <- c("no", 
    #                "grouping",
    #                "speed",
    #                "coordinates",
    #                "latitude",
    #                "longitude",
    #                "location",
    #                "latitude1",
    #                "longitude1",
    #                "sensor",
    #                "time_str",
    #                "time_int",
    #                "latitude2",
    #                "longitude2",
    #                "sensor_value",
    #                "formatted_value",
    #                "driver")
    
    col_names <- c("no",
                   "grouping",
                   "speed",
                   "coordinates",
                   "latitude",
                   "longitude",
                   "location",
                   "latitude1",
                   "longitude1",
                   "time_str",
                   "time_int",
                   "latitude2",
                   "longitude2")
    
    # Sometimes there's only
    col_names8 <- c("no",
                    "group",
                    "speed",
                    "blank",
                    "location",
                    "latitude",
                    "longitude",
                    "time_str")
    
  }
  
  #### Grab list of data
  results_list <- df$r[[1]]$c
  
  ####  List to dataframe
  if(show_progress) print(paste0("--", user_id, "; N Obs: ", length(results_list)))
  
  if(length(results_list) > 0){
    df_out <- map_df(1:length(results_list), function(i){
      if(show_progress & ((i %% 1000) %in% 0)) print(paste0(i, "/", length(results_list)))
      #print(i)
      df_i <- results_list[i] %>% as.data.frame()

      # In some cases, a dataframe will have less variables than typical; here, we use
      # a different dataframe
      if(report_id %in% 2 & ncol(df_i) %in% 8){
        names(df_i) <- col_names8
        df_i$diff_num_vars_rawdata <- T # flag to check data later
      } else{
        names(df_i) <- col_names
        df_i$diff_num_vars_rawdata <- F
      }
      
      ## Cleanup
      # Do some basic clean-up. Main clean-up involes removing unnessary data in
      # order to minimize the size of the data.
      # Echo Driving Report
      if(report_id %in% 1){
        df_i <- df_i %>%
          dplyr::select(violation, 
                        begin_datetime_str,
                        latitude_begin,
                        longitude_begin,
                        end_datetime_str,
                        latitude_end,
                        longitude_end,
                        value,
                        max_speed,
                        time_interval,
                        distance) 
      }
      
      # Sensor Tracing
      if(report_id %in% 2){
        df_i <- df_i %>%
          dplyr::select(speed,
                        latitude,
                        longitude,
                        location,
                        #sensor, ##
                        #sensor_value, ##
                        #formatted_value, ##
                        time_str,
                        diff_num_vars_rawdata) 
        
      }
      
      return(df_i)
    })
    
    # Cleanup --------------------------------------------------------------------
    # Add reg no and vehicle id
    reg_no <- users_df$nm[users_df$id %in% user_id] %>% tolower()
    
    df_out <- df_out %>%
      dplyr::mutate(reg_no_id = user_id,
                    reg_no = reg_no) %>%
      distinct()
    
  } else{
    df_out <- data.frame(NULL)
  }
  
  # Clear Report ---------------------------------------------------------------
  # After accessing a report, need to clear it
  Sys.sleep(0.1)
  req_clear <- paste0("https://hst-api.wialon.com/wialon/ajax.html?svc=report/cleanup_result&params={}&sid=",SID)
  res_clear = GET(req_clear)
  raw_json <- rawToChar(res_clear$content)
  Sys.sleep(0.1)
  
  return(df_out)
}

