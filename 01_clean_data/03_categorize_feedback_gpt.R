# Prep data for classifying from chatGPT

# https://platform.openai.com/

# "gpt-3.5-turbo"
# "gpt-4o"
CHATGPT_RUN_NUM <- 2
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

prompt <- "The following are comments from bus passengers. Categorize each comment into the following categories: 1 = Driving and safety; 2 = Adherence to COVID-19 measures; 3 = Service quality and passenger experience; 4 = Vehicle condition; 5 = Other. After categorizing, indicating if the passenger gave a positive comment, negative comment, or made a suggestion or recommendation for improvement if the category was 1, 2, 3 or 4. Positive indicators praise, complying with rules and regulations, and/or a safe ride. Negative indicates complaints, not complying with rules and regulations, and/or an unsafe ride. Suggestions or recommendations indicate a passenger is providing a recommendation for improvement. Indicate “p” for a positive comment, “n” for a negative comment, and “r” for comment with a suggestion for improvement. For example, if a passenger indicated safe driving, indicate “1p”. Multiple categories from an individual comment are allowed. For example, for the following comment: “the driving was safe but COVID-19 measures were not adhered to”, the response should be: “1p;2n”. As another example, for the following comment: “the driving was safe but COVID-19 measures were not adhered to, additional sanitizer should be provided for passengers”, the response should be: “1p;2n;2r”. Only output the numbers and letters; do not include any explanation. Does this make sense?"
cat(ask_chatgpt(prompt))

dir.create(file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", "category", CHATGPT_RUN))

for(i in 1:nrow(fb_unique_df)){
  print(i)
  fb_unique_df_i <- fb_unique_df[i,]
  
  OUT_NAME <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", "category", CHATGPT_RUN, 
                        paste0("comment_id", fb_unique_df_i$q_comment_id, ".Rds"))
  
  if(!file.exists(OUT_NAME)){
    retry <- TRUE
    while(retry){
      try({
        fb_unique_df_i$q_comment_rating <- ask_chatgpt(fb_unique_df_i$q_comment)
        saveRDS(fb_unique_df_i, OUT_NAME)
        Sys.sleep(5)
        retry <- FALSE
      }, silent = TRUE)
      if(retry){
        print("Retry")
        Sys.sleep(70)
      }
    }
  }
}

chatgpt_all_df <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", "category", CHATGPT_RUN) %>%
  list.files(pattern = "*.Rds",
             full.names = T) %>%
  map_df(readRDS)

# Export -----------------------------------------------------------------------
#OUT_PATH_1 <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "inputs", "comment_id.csv")
#write_csv(fb_df, OUT_PATH_1)

#OUT_PATH_2 <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "inputs", "unique_comments.csv")
#write_csv(fb_unique_df, OUT_PATH_2)

saveRDS(chatgpt_all_df, file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", 
                                  paste0("chatgpt_category_",CHATGPT_RUN, ".Rds")))

