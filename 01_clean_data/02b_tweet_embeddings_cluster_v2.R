# Cluster Embeddings

library(stats)
library(uwot)
library(dbscan)
library(httr)
library(jsonlite)
library(purrr)
library(stringr)

n_neighbors <- 10

# Load data --------------------------------------------------------------------
fb_df_embed <- readRDS(file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs",
                                 "fb_comments_with_embeddings.Rds"))

# Determine number of tweets per comment ---------------------------------------
text_df <- readRDS(file.path(data_dir, "FinalData", 
                           paste0("passenger_feedback_valid_class_",
                                  "main", "_",
                                  "cmntfilter",
                                  FALSE,
                                  "_",
                                  "dstnctpass",
                                  TRUE,
                                  ".Rds"))) %>%
  dplyr::mutate(q_comment_rating = chatgpt_4o_cat) %>%
  dplyr::filter(!is.na(chatgpt_4o_cat))

# ## Cleanup comment variable
# fb_df <- fb_df %>%
#   dplyr::mutate(q_comment_nchar = nchar(q_comment)) %>%
#   dplyr::filter(!is.na(q_comment)) %>%
#   dplyr::filter(q_comment != "") %>%
#   dplyr::filter(q_comment_nchar >= 3) %>%
#   dplyr::mutate(q_comment = q_comment %>%
#                   str_replace_all("[:punct:]", " ") %>%
#                   str_squish() %>%
#                   tolower()) %>%
#   dplyr::arrange(-q_comment_nchar)
# 
# ## Make unique ID
# fb_df <- fb_df %>%
#   dplyr::mutate(q_comment = q_comment %>%
#                   tolower()) %>%
#   dplyr::filter(!is.na(q_comment)) %>%
#   dplyr::mutate(q_comment_id = q_comment %>% 
#                   as.factor() %>%
#                   as.numeric()) %>%
#   dplyr::select(uid, q_comment_id,  q_comment) 
# 
# ## Make unique dataframe
# fb_unique_df <- fb_df %>%
#   group_by(q_comment) %>%
#   dplyr::mutate(n_alltweets = n()) %>%
#   ungroup() %>%
#   distinct(uid, q_comment)
# 
# head(fb_unique_df)
# 
# 
# fb_df_embed

# Prepare long dataframe for clustering ----------------------------------------
text_long_df <- map_df(1:nrow(text_df), function(i){
  text_df_i <- text_df[i,]
  rating_stacked <- text_df_i$q_comment_rating %>%
    str_split(";") %>%
    unlist() %>%
    as.data.frame() %>%
    dplyr::rename(q_comment_rating = ".")
  
  text_df_i$q_comment_rating <- NULL
  
  text_df_i <- bind_cols(text_df_i, rating_stacked)
  
  return(text_df_i)
}) %>%
  dplyr::filter(nchar(q_comment_rating) <= 2)

text_long_df <- text_long_df %>%
  group_by(q_comment, q_comment_rating) %>%
  dplyr::mutate(n_alltweets = n()) %>%
  ungroup() %>%
  distinct(q_comment, q_comment_rating, n_alltweets, .keep_all = T) %>%
  dplyr::select(uid, q_comment, q_comment_rating, n_alltweets)
# 
# text_long_df %>%
#   group_by(q_comment_rating) %>%
#   dplyr::summarise(n_alltweets = sum(n_alltweets)) %>%
#   ungroup()

text_long_df <- text_long_df %>%
  left_join(fb_df_embed, by = "uid")

text_long_df <- text_long_df %>%
  dplyr::filter(q_comment_rating != "") %>%
  group_by(q_comment_rating) %>%
  dplyr::mutate(q_comment_rating_n = n()) %>%
  ungroup() %>%
  dplyr::filter(!is.na(V1)) %>%
  dplyr::filter(q_comment_rating_n >= 15) 

cluster_all_df <- map_df(unique(text_long_df$q_comment_rating), function(rating_i){
  message(rating_i)
  
  fb_df_embed <- text_long_df[text_long_df$q_comment_rating %in% rating_i,]

  fb_df_embed_noid <- fb_df_embed %>%
    dplyr::select(contains("V"))
  
  # Determine clusters -----------------------------------------------------------
  
  #### PCA
  X <- as.matrix(fb_df_embed_noid)
  
  pca <- prcomp(X, center = TRUE, scale. = FALSE)
  
  n_col <- ncol(pca$x)
  
  X_pca <- pca$x[, 1:min(n_col, 50)]   # 30–100 is common
  
  #### UMAP
  set.seed(123)
  X_umap <- umap(
    X_pca,
    n_neighbors = n_neighbors,
    min_dist = 0.0,
    n_components = 5,   # key difference vs 2D
    metric = "cosine"
  )
  
  #### HDBSCAN on UMAP space -> determine clusters
  hdb <- hdbscan(X_umap, minPts = n_neighbors)
  
  fb_df_embed$cluster <- hdb$cluster
  fb_df_embed$prob    <- hdb$membership_prob
  
  #### 2D for plotting
  X_umap_2d <- umap(
    X_pca,
    n_neighbors = n_neighbors,
    min_dist = 0.1,
    n_components = 2,
    metric = "cosine"
  )
  
  fb_df_embed$u1 <- X_umap_2d[,1]
  fb_df_embed$u2 <- X_umap_2d[,2]
  
  #### Cleanup
  fb_df_embed <- fb_df_embed %>%
    dplyr::select(-contains("V"))

  # Aggregate clusters -----------------------------------------------------------
  
  #### Aggregate to cluster
  cluster_df <- fb_df_embed %>%
    dplyr::filter(cluster != 0) %>%
    group_by(cluster) %>%
    dplyr::summarise(n = n(),
                     n_alltweets = sum(n_alltweets),
                     u1 = mean(u1),
                     u2 = mean(u2),
                     q_comment = paste0(q_comment, collapse = "; ")) %>%
    ungroup()
  
  #### Summarize clusters --------------------------------------------------------
  OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
  
  summarize_comments <- function(text) {
    
    prompt <- paste0(
      "Summarize the following comments in 3–5 words. ",
      "Be descriptive, neutral, and concise.\n\n",
      text
    )
    
    res <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(
        Authorization = paste("Bearer", OPENAI_API_KEY),
        `Content-Type` = "application/json"
      ),
      body = toJSON(list(
        model = "gpt-4.1-mini",
        messages = list(
          list(role = "system", content = "You are a concise research assistant."),
          list(role = "user", content = prompt)
        ),
        temperature = 0.2
      ), auto_unbox = TRUE)
    )
    
    out <- content(res, as = "parsed")
    
    out$choices[[1]]$message$content %>%
      str_squish()
  }
  
  cluster_df$q_comment_chatgpt_summary <- map_chr(
    cluster_df$q_comment,
    summarize_comments
  )
  
  cluster_df$q_comment_rating <- rating_i
  
  return(cluster_df)
  
})

# Export -----------------------------------------------------------------------
saveRDS(cluster_all_df, 
        file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs",
                  paste0("fb_comments_with_embeddings_cluster_nneighbor",n_neighbors,".Rds")))

