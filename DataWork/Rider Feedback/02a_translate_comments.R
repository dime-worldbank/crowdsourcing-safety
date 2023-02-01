# Translate Comments
# Only translate comments where PSV number is valid

TRANSLATE <- F

# Load data --------------------------------------------------------------------
if(TRANSLATE){
  api_key <- read_csv(file.path("~", "Dropbox", "World Bank", "Webscraping", 
                                "Files for Server", 
                                "api_keys.csv")) %>%
    filter(Account %in% "robertandrew311@gmail.com",
           Service %in% "Google Directions API") %>%
    pull(Key)
  
  
  df <- readRDS(file.path(data_dir, "Rider Feedback", "FinalData", 
                          "rider_feedback.Rds"))
  
  # Subset data ------------------------------------------------------------------
  
  df <- df %>%
    dplyr::filter(!is.na(completion_date),
                  !is.na(comments_label_nchar)) %>%
    dplyr::filter(comments_label_nchar >= 2)
  
  # Translate daily --------------------------------------------------------------
  
  #### Dates to translate
  dates_to_trans <- df$completion_date %>% unique() %>% sort()
  
  ## Remove last day, as may not be complete
  dates_to_trans <- dates_to_trans[-length(dates_to_trans)]
  
  for(date_i in dates_to_trans){
    
    PATH_OUT <- file.path(data_dir, "Rider Feedback", "FinalData", 
                          "comments_translated", "daily", 
                          paste0("comment_english_", date_i, ".Rds"))
    
    if(!file.exists(PATH_OUT)){
      
      df_i <- df[df$completion_date %in% date_i,]
      
      comments_label_english_l <- lapply(df_i$comments_label, function(comment){
        r_google_translate_vec(comment,
                               target = "en",
                               format = "text",
                               source = "sw",
                               model = "nmt", 
                               key = api_key,
                               sleep = 0)
      })
      
      comments_label_english_l_na <- lapply(comments_label_english_l, function(x){
        if(is.null(x)){
          return(NA)
        } else{
          return(x)
        }
      })
      
      df_i$comments_label_english <- comments_label_english_l_na %>% unlist()
      
      df_i <- df_i %>%
        dplyr::select(case_key, comments_label_english)
      
      saveRDS(df_i, PATH_OUT)
      
    }
    
  }
  
}