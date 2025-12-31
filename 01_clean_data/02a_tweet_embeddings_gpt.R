# Tweet Embeddings

#Sys.setenv(OPENAI_API_KEY = "YOUR_API_KEY")

text_df <- readRDS(file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", "chatgpt_category_1_gpt-4o.Rds"))

text_df <- text_df %>%
  dplyr::mutate(q_comment = q_comment %>%
                  str_replace_all("[:punct:]", " ") %>%
                  str_replace_all("[:digit:]", " ") %>%
                  str_squish() %>%
                  tolower()) %>%
  dplyr::filter(!is.na(q_comment)) %>%
  dplyr::filter(q_comment != "") %>%
  dplyr::mutate(q_comment_nchar = nchar(q_comment)) %>%
  dplyr::filter(q_comment_nchar >= 3) %>%
  dplyr::arrange(-q_comment_nchar)

# Create Embeddings ------------------------------------------------------------
for(uid in text_df$uid){
  
  OUT_PATH <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", "tweet_embeddings",
                        paste0(uid, ".Rds"))
  
  if(!file.exists(OUT_PATH)){
    
    text_df_i <- text_df[text_df$uid %in% uid,]
    
    res <- create_embedding(
      model = "text-embedding-3-small",
      input = text_df_i$q_comment[1]
    )
    
    df_out <- as.data.frame(t(res$data$embedding[[1]]))
    df_out$uid <- uid
    
    saveRDS(df_out, OUT_PATH)
  }
  
}

# Append + Export --------------------------------------------------------------
tweet_embed_df <- file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", "tweet_embeddings") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  map_df(readRDS)

saveRDS(tweet_embed_df, file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs",
                               "fb_comments_with_embeddings.Rds"))
