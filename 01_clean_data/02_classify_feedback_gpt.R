# Prep data for classifying from chatGPT

# https://platform.openai.com/

# "gpt-3.5-turbo"
# "gpt-4o"
CHATGPT_RUN_NUM <- 10
CHATGPT_MODEL <- "gpt-4o"

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", 
                           paste0("passenger_feedback_valid_",
                                  "main", "_",
                                  "cmntfilter",
                                  FALSE,
                                  "_",
                                  "dstnctpass",
                                  TRUE,
                                  ".Rds")))

CHATGPT_RUN <- paste0(CHATGPT_RUN_NUM, "_", CHATGPT_MODEL)

# Prep dataset -----------------------------------------------------------------
## Cleanup comment variable
fb_df <- fb_df %>%
  dplyr::mutate(q_comment_nchar = nchar(q_comment)) %>%
  dplyr::filter(!is.na(q_comment)) %>%
  dplyr::filter(q_comment != "") %>%
  dplyr::filter(q_comment_nchar >= 3) %>%
  dplyr::mutate(q_comment = q_comment %>%
                  str_replace_all("[:punct:]", " ") %>%
                  str_squish() %>%
                  tolower()) %>%
  dplyr::arrange(-q_comment_nchar)

## Make unique ID
fb_df <- fb_df %>%
  dplyr::mutate(q_comment = q_comment %>%
                  tolower()) %>%
  dplyr::filter(!is.na(q_comment)) %>%
  dplyr::mutate(q_comment_id = q_comment %>% 
                  as.factor() %>%
                  as.numeric()) %>%
  dplyr::select(uid, q_comment_id,  q_comment) 

## Make unique dataframe
fb_unique_df <- fb_df %>%
  distinct(q_comment_id, .keep_all = T) %>%
  dplyr::select(uid, q_comment_id,  q_comment)

# Classify ---------------------------------------------------------------------
# "gpt-4o"
# gpt-3.5-turbo
Sys.setenv("OPENAI_MODEL" = CHATGPT_MODEL)

prompt <- "The following comments are from bus passengers giving feedback about the bus ride. Indicate if the comment describes the driving of the vehicle the passenger is on as: safe, unsafe, neutral, or not related to the safety of driving. Return a “1” for safe”, “2” for unsafe”, and “3” for other (unclear, not related, etc). ONLY return the number, not any text. Here is more information about each of the codes. 1 = A compliment; describes the driving of the vehicle the passenger is on positively. 2 = Negative; describes the driving of the vehicle the passenger is on as negative. 3 = These could include very general comments about driving. However, general comments that may imply something about the current driving experience are OK. For example: “Drivers should be careful when driving. Stop drinking alcohol” may imply that the driver was not driving carefully and drinking alcohol, so this should be coded as “2” (unsafe). If a comment includes a statement about the driving of the specific vehicle and a general statement about driving not related to the specific vehicle, ignore the general statement. Does this make sense?"
cat(ask_chatgpt(prompt))

dir.create(file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", "classification", CHATGPT_RUN))

for(i in 1:nrow(fb_unique_df)){
  print(i)
  fb_unique_df_i <- fb_unique_df[i,]
  
  OUT_NAME <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", "classification", CHATGPT_RUN, 
                        paste0("comment_id", fb_unique_df_i$q_comment_id, ".Rds"))
  
  if(!file.exists(OUT_NAME)){
    retry <- TRUE
    while(retry){
      try({
        fb_unique_df_i$q_comment_rating <- ask_chatgpt(fb_unique_df_i$q_comment)
        print(fb_unique_df_i$q_comment_rating)
        saveRDS(fb_unique_df_i, OUT_NAME)
        Sys.sleep(2)
        retry <- FALSE
      }, silent = TRUE)
      if(retry){
        print("Retry")
        Sys.sleep(61)
      }
    }
  }
}

chatgpt_all_df <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", "classification", CHATGPT_RUN) %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS)

# Export -----------------------------------------------------------------------
OUT_PATH_1 <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "inputs", "comment_id.csv")
write_csv(fb_df, OUT_PATH_1)

OUT_PATH_2 <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "inputs", "unique_comments.csv")
write_csv(fb_unique_df, OUT_PATH_2)

saveRDS(chatgpt_all_df, file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", 
                                  paste0("chatgpt_classification_",CHATGPT_RUN, ".Rds")))



# a <- chatgpt_all_df[chatgpt_all_df$q_comment_rating %>% nchar() > 1,]
# 
# 
# for(i in a$q_comment_id){
#   OUT_NAME <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", "classification", CHATGPT_RUN,
#                         paste0("comment_id", i, ".Rds"))
#   file.remove(OUT_NAME)
# }
