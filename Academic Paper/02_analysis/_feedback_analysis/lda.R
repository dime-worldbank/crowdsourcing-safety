# Topic Classification

library(topicmodels)
library(seededlda)
library(quanteda)

# https://koheiw.github.io/seededlda/articles/pkgdown/seeded.html

# Load data --------------------------------------------------------------------
fb_df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

fb_df <- fb_df %>%
  filter(!is.na(q_comment)) %>%
  mutate(q_comment = q_comment %>% tolower())

toks <- fb_df$q_comment %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE)

dfmt <- dfm(toks) |> 
  dfm_remove(stopwords("en")) |>
  dfm_remove("*@*") |>
  dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")



dict <- dictionary(list(drivingsafety = c("drives", "safe", "safely", "tired", "drunk"),
                        covid = c("covid*", "mask", "sanitizer", "sanitiser"),
                        amenities = c("music", "dance")))

lda_seed <- textmodel_seededlda(dfmt, 
                                dict, 
                                batch_size = 0.01, 
                                auto_iter = TRUE,
                                verbose = TRUE,
                                residual = 2)

terms(lda_seed)
knitr::kable(terms(lda_seed))
