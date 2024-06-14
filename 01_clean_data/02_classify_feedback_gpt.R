# Prep data for classifying from chatGPT

# https://platform.openai.com/

# Load data --------------------------------------------------------------------
<<<<<<< HEAD
df <- readRDS(file.path(data_dir, "FinalData", 
                        paste0("passenger_feedback_valid_",
                               "main", "_",
                               "cmntfilter",
                               FALSE,
                               "_",
                               "dstnctpass",
                               TRUE,
                               ".Rds")))
=======
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_valid_class_main_cmntfilterFALSE_dstnctpassTRUE.Rds"))
>>>>>>> 1c3ec44b8a3c5979b8fee116e8ca792acfdefc3e

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
Sys.setenv("OPENAI_MODEL" = "gpt-4o")

prompt <- "The following comments are from bus passengers giving feedback about the bus ride. Indicate if the comment describes the driving of the vehicle the passenger is on as: safe, unsafe, neutral, or not related to the safety of driving. Return a “1” for safe”, “2” for unsafe”, and “3” for other (unclear, not related, etc). ONLY return the number, not any text. Here is more information about each of the codes. 1 = A compliment; describes the driving of the vehicle the passenger is on positively. 2 = Negative; describes the driving of the vehicle the passenger is on as negative. 3 = These could include very general comments about driving. However, general comments that may imply something about the current driving experience are OK. For example: “Drivers should be careful when driving. Stop drinking alcohol” may imply that the driver was not driving carefully and drinking alcohol, so this should be coded as “2” (unsafe). If a comment includes a statement about the driving of the specific vehicle and a general statement about driving not related to the specific vehicle, ignore the general statement. Does this make sense?"
cat(ask_chatgpt(prompt))

CHATGPT_RUN <- 1

dir.create(file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", CHATGPT_RUN))

for(i in 1:nrow(fb_unique_df)){
  print(i)
  fb_unique_df_i <- fb_unique_df[i,]
  
  OUT_NAME <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", CHATGPT_RUN, 
                        paste0("comment_id", fb_unique_df_i$q_comment_id, ".Rds"))
  
  if(!file.exists(OUT_NAME)){
    retry <- TRUE
    while(retry){
      try({
        fb_unique_df_i$q_comment_rating <- ask_chatgpt(fb_unique_df_i$q_comment)
        saveRDS(fb_unique_df_i, OUT_NAME)
        Sys.sleep(1)
        retry <- FALSE
      }, silent = TRUE)
      if(retry){
        print("Retry")
        Sys.sleep(60)
      }
    }
  }
}

chatgpt_all_df <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", CHATGPT_RUN) %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS)

# Export -----------------------------------------------------------------------
OUT_PATH_1 <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "inputs", "comment_id.csv")
if(!file.exists(OUT_PATH_1)) write_csv(fb_df, OUT_PATH_1)

OUT_PATH_2 <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "inputs", "unique_comments.csv")
if(!file.exists(OUT_PATH_2)) write_csv(fb_unique_df, OUT_PATH_2)

saveRDS(chatgpt_all_df, file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", "chatgpt_classification.Rds"))
