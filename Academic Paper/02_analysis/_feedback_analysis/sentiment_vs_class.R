# Sentiment vs Classification

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "FinalData", "passenger_feedback_clean_class.Rds"))

# Classification ---------------------------------------------------------------
df %>%
  dplyr::filter(!is.na(comment_driver_sntmt_code)) %>%
  dplyr::mutate(comment_driver_sntmt_code_str = case_when(
    comment_driver_sntmt_code == 1 ~ "Compliment",
    comment_driver_sntmt_code == 2 ~ "Negative",
    comment_driver_sntmt_code == 3 ~ "Neutral",
    comment_driver_sntmt_code == 4 ~ "Unclear",
  )) %>%
  group_by(comment_driver_sntmt_code) %>%
  dplyr::mutate(n_code = n()) %>%
  ungroup() %>%
  dplyr::mutate(comment_driver_sntmt_code_str = 
                  paste0(comment_driver_sntmt_code_str,
                         "\n[N = ",n_code,"]")) %>%
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_boxplot(aes(x = sentiment_snmtr,
                   y = comment_driver_sntmt_code_str)) +
  theme_classic2()

df %>%
  dplyr::filter(!is.na(comment_driver_sntmt_code),
                q_comment_nchar >= 20) %>%
  dplyr::mutate(comment_driver_sntmt_code_str = case_when(
    comment_driver_sntmt_code == 1 ~ "Compliment",
    comment_driver_sntmt_code == 2 ~ "Negative",
    comment_driver_sntmt_code == 3 ~ "Neutral",
    comment_driver_sntmt_code == 4 ~ "Unclear",
  )) %>%
  group_by(comment_driver_sntmt_code) %>%
  dplyr::mutate(n_code = n()) %>%
  ungroup() %>%
  dplyr::mutate(comment_driver_sntmt_code_str = 
                  paste0(comment_driver_sntmt_code_str,
                         "\n[N = ",n_code,"]")) %>%
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_boxplot(aes(x = sentiment_snmtr,
                   y = comment_driver_sntmt_code_str)) +
  theme_classic2()

df %>%
  dplyr::filter(!is.na(comment_driver_sntmt_relev),
                q_comment_nchar >= 20) %>%
  dplyr::mutate(comment_driver_sntmt_relev_str = case_when(
    comment_driver_sntmt_relev == 1 ~ "Yes",
    comment_driver_sntmt_relev == 0 ~ "No",
  )) %>%
  group_by(comment_driver_sntmt_relev_str) %>%
  dplyr::mutate(n_code = n()) %>%
  ungroup() %>%
  dplyr::mutate(comment_driver_sntmt_relev_str = 
                  paste0(comment_driver_sntmt_relev_str,
                         "\n[N = ",n_code,"]")) %>%
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_boxplot(aes(x = sentiment_snmtr,
                   y = comment_driver_sntmt_relev_str)) +
  theme_classic2()




