# Cluster Embeddings

library(stats)
library(uwot)
library(dbscan)
library(httr)
library(jsonlite)
library(purrr)
library(stringr)

n_neighbors <- 6

# Load data --------------------------------------------------------------------
text_df <- readRDS(file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs", 
                             "chatgpt_category_1_gpt-4o.Rds"))

fb_df_embed <- readRDS(file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs",
                                 "fb_comments_with_embeddings.Rds"))


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
})

text_long_df <- text_long_df %>%
  dplyr::filter(q_comment_rating != "")

text_long_df <- text_long_df %>%
  left_join(fb_df_embed, by = "uid")

# Determine clusters -----------------------------------------------------------

#### PCA
X <- as.matrix(fb_df_embed_noid)

pca <- prcomp(X, center = TRUE, scale. = FALSE)
X_pca <- pca$x[, 1:50]   # 30–100 is common

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

fb_df_embed <- fb_df_embed %>%
  left_join(text_df, by = "uid")

# Aggregate clusters -----------------------------------------------------------

#### Aggregate to cluster
cluster_df <- fb_df_embed %>%
  dplyr::filter(cluster != 0) %>%
  group_by(cluster) %>%
  dplyr::summarise(n = n(),
                   u1 = mean(u1),
                   u2 = mean(u2),
                   q_comment_rating = paste0(q_comment_rating, collapse = ";"),
                   q_comment = paste0(q_comment, collapse = "; ")) %>%
  ungroup()

cluster_df <- cluster_df %>%
  dplyr::mutate(q_comment_rating_no5 = q_comment_rating %>% str_replace_all("5;", ""))

cluster_df$q_comment_rating_summary <- cluster_df$q_comment_rating %>%
  strsplit(";") %>%
  lapply(function(x){
    tab <- table(x)
    paste0(names(tab)[which.max(tab)])
  }) %>%
  unlist()

cluster_df$q_comment_rating_summary_no5 <- cluster_df$q_comment_rating_no5 %>%
  strsplit(";") %>%
  lapply(function(x){
    tab <- table(x)
    paste0(names(tab)[which.max(tab)])
  }) %>%
  unlist()

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

# Export -----------------------------------------------------------------------
saveRDS(cluster_df, file.path(data_dir, "FinalData ChatGPT Comment Codes", "outputs",
                                 paste0("fb_comments_with_embeddings_cluster_nneighbor",n_neighbors,".Rds")))

