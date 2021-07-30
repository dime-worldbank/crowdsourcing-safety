get_sid <- function(wailon_token){
  # Get SID
  
  req <- paste0("https://hst-api.wialon.com/wialon/ajax.html?svc=token/login&
    params={
      %22token%22:%22",wailon_token,"%22
    }") %>%
    str_replace_all('[\r\n]' , '') %>% 
    str_replace_all(' ', '') 
  
  res = GET(req)
  raw_json <- rawToChar(res$content)
  df <- jsonlite::fromJSON(raw_json)
  
  return(df$eid)
}

get_ids <- function(sid, items_type, flag){
  # Get user IDs (id for each vehicle)
  
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

get_report <- function(user_id, report_id, resource_id, datetime_begin, datetime_end, wailon_token){
  # https://sdk.wialon.com/wiki/en/sidebar/remoteapi/codesamples/reports
  
  SID <- get_sid(wailon_token)
  
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
  
  ## Sensor Tracing
  if(report_id %in% 6){
    results_list <- df$r[[1]]$c
    
    df_out <- map_df(1:length(results_list), function(i){
      
      df_i <- results_list[i] %>% as.data.frame()
      names(df_i) <- c("no", 
                       "grouping",
                       "speed",
                       "coordinates",
                       "latitude",
                       "longitude",
                       "location",
                       "latitude1",
                       "longitude1",
                       "harsh_driving_type",
                       "time_str",
                       "time_int",
                       "latitude2",
                       "longitude2",
                       "sensor_value",
                       "driver",
                       "formatted_value")
      
      return(df_i)
    })
  }
  
  ## Echo Driving
  
  #### Clear Report
  req_clear <- paste0("https://hst-api.wialon.com/wialon/ajax.html?svc=report/cleanup_result&params={}&sid=",SID)
  res_clear = GET(req_clear)
  raw_json <- rawToChar(res_clear$content)
  
  return(df_out)
}

