# Figures

# Load data --------------------------------------------------------------------
fb_df  <- readRDS(file.path(ap_data_dir, "FinalData", "passenger_feedback_valid_class_main_cmntfilterFALSE_dstnctpassTRUE.Rds"))

fb_df$comment_driver_sntmt_code_str <- as.character(fb_df$comment_driver_sntmt_code_str)
fb_df$comment_driver_sntmt_code_str[fb_df$comment_driver_sntmt_relev %in% 0] <- "Not relevant"

fb_df <- fb_df %>%
  dplyr::filter(comment_driver_sntmt_code_str %in% c("Positive",
                                                     "Negative",
                                                     "Not relevant"))

table(fb_df$comment_driver_sntmt_code_str)
table(fb_df$comment_driver_sntmt_code_str) / nrow(fb_df)

2051 / (2051 + 149)

fb_pn_df <- fb_df %>%
  dplyr::filter(comment_driver_sntmt_code_str %in% c("Positive",
                                                     "Negative"))



fb_pn_df$comment_driver_sntmt_code <- 2 - fb_pn_df$comment_driver_sntmt_code

fb_pn_df %>%
  filter(!is.na(q_safety_rating)) %>%
  group_by(q_safety_rating) %>%
  dplyr::summarise(prop_pos = mean(comment_driver_sntmt_code),
                   n = n()) %>%
  ungroup()

sentiment("fast")
sentiment("The driver drove very fast")


sentiment("The driver drove the matatu very fast")
sentiment("The driver drives the car at high speed")
sentiment("The driver is tired")
sentiment("The conductor is the most polite conductor ever in the industry")
sentiment("The driver is observant and also drives safely")
