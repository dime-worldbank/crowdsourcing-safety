# Create Comments to Code

if(T){
  df <- readRDS(file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_clean.Rds"))
  
  df <- df %>%
    dplyr::filter(valid_psvnum %in% T,
                  comment_coded %in% F,
                  !is.na(comments_label)) %>%
    dplyr::filter(comments_label_nchar >= 4) %>%
    dplyr::filter(completion_date >= ymd("2023-01-16") )
  
  df <- df %>%
    dplyr::mutate(needs_crosschecking = "",
                  comments_checking_relevance = "",
                  coding_relevant_comments = "",
                  additional_notes = "") %>%
    dplyr::select(-comment_coded)
  
  date_i <- Sys.Date() %>% as.character() %>% str_replace_all("-", "")
  
  #### Further Subset
  df <- df %>%
    distinct(comments_label_english, .keep_all = T) %>%
    arrange(-comments_label_nchar)
  
  unsafe_ids <- df %>%
    dplyr::filter(safety_label %>% str_detect("Si Salama") | 
                    speed_label %in% "Haraka sana [80+]") %>%
    pull(case_key)
  
  safe_ids <- df %>%
    dplyr::filter(safety_label %in% c("Salama", "Salama Sana")) %>%
    pull(case_key) %>%
    head(242)
  
  df <- df[df$case_key %in% c(safe_ids, unsafe_ids),]
  
  nrow(df)
  
  
  #### Cleanup
  df <- df %>%
    dplyr::select(case_key, 
                  comments_label, comments_label_english,
                  comments_checking_relevance, coding_relevant_comments,
                  needs_crosschecking, additional_notes) 
  
  write_csv(df, 
            file.path(data_dir, "Rider Feedback", "FinalData", "comments_coded",
                      "to_code",
                      paste0("comments_to_code_", date_i, ".csv")))
  
}


library(leaflet)
library(leaflet.extras)
library(leaflet.providers)

locs_df <- data.frame(latitude = c(38.912809166821376, 38.91581443236016),
                      longitude = c(-77.04265763680785, -77.03819444111005),
                      id = 1:2)

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircles(data = locs_df,
             color = "red",
             opacity = 1)

