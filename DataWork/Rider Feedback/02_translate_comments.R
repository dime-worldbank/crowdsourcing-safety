# Translate Comments
# Only translate comments where PSV number is valid

source("https://raw.githubusercontent.com/ramarty/r_google_translate/main/r_google_translate.R")

# Load data --------------------------------------------------------------------
if(F){
  api_key <- read_csv(file.path("~", "Dropbox", "World Bank", "Webscraping", 
                                "Files for Server", 
                                "api_keys.csv")) %>%
    filter(Account %in% "robertandrew311@gmail.com",
           Service %in% "Google Directions API") %>%
    pull(Key)
}

df <- readRDS(file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback.Rds"))

df <- df %>%
  dplyr::filter((valid_psvnum %in% T) | comments_label_nchar >= 20) %>%
  dplyr::filter(comments_label_nchar > 1) %>%
  dplyr::filter(!is.na(comments_label)) 

r_google_translate_vec(df$comments_label[df$comments_label_nchar == 159],
                       target = "en",
                       format = "text",
                       source = "sw",
                       model = "nmt", 
                       key = api_key,
                       sleep = 0)

comments_label_english_l <- lapply(df$comments_label, function(comment){
  print(comment)
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

comments_label_english <- comments_label_english_l_na %>% unlist()

df$comments_label_english <- comments_label_english

# df$comments_label_english <- r_google_translate_vec(df$comments_label,
#                                                     target = "en",
#                                                     format = "text",
#                                                     source = "sw",
#                                                     model = "nmt", 
#                                                     key = api_key,
#                                                     sleep = 0)

df <- df %>%
  dplyr::select(everything(), 
                -comments_label, -comments_label_english,
                comments_label, comments_label_english)

saveRDS(df, file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_translated.Rds"))
write_csv(df, file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_translated.csv"))

write_csv(df[df$valid_psvnum %in% T,], file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_translated_valid.csv"))
write_csv(df[df$valid_psvnum %in% F,], file.path(data_dir, "Rider Feedback", "FinalData", "rider_feedback_translated_not_valid.csv"))
