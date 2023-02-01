# Clean Rider Feedback Data

# Load data --------------------------------------------------------------------
## Main Data
df <- read_csv(file.path(rider_feedback_pii_dir, "RawData - PII", "rider_feedback.csv"))

## Data to Merge Into
valid_reg_df <- read_csv(file.path(data_dir, "Valid Reg Numbers", "valid_psv_nums.csv"))


# Clean var names --------------------------------------------------------------
df <- df %>%
  clean_names()

# Fix issue with variable switches ---------------------------------------------
# After the SMS survey was implemented, we re-ordered the questions. In this
# process, for a time the variable names were not changed. Specifically:
# -- consent_label was recording the PSV number (psvnum_label)
# -- psvnum_label was recording the consent (psvnum_label)
# This code fixes this issue, so that information is obtained in the correct
# variable

df <- df %>%
  # Issue if:
  # -- psvnum_label is "Ndiyo"
  # -- consent_label is (1) not NA and (2) not "Ndiyo"
  dplyr::mutate(var_issue = (psvnum_label %in% "Ndiyo") |  
                  (!is.na(df$consent_label) & !(df$consent_label %in% "Ndiyo")))

df_1 <- df %>%
  dplyr::filter(var_issue %in% T) %>%
  dplyr::rename(consent_label_tmp = psvnum_label,
                psvnum_label_tmp  = consent_label) %>%
  dplyr::rename(consent_label = consent_label_tmp,
                psvnum_label  = psvnum_label_tmp)

df_2 <- df %>%
  dplyr::filter(var_issue %in% F)

df <- bind_rows(df_1, df_2)

# Basic cleaning ---------------------------------------------------------------
df <- df %>%
  clean_names() %>%
  dplyr::rename(invite_datetime     = invite_date,
                start_datetime      = start_date,
                completion_datetime = completion_date) %>%
  dplyr::mutate(phone_hash           = phone %>% as.character() %>% md5(),
                comments_label_nchar = comments_label %>% nchar(),
                invite_date     = invite_datetime     %>% date(),
                start_date      = start_datetime      %>% date(),
                completion_date = completion_datetime %>% date()) 

## Indicate if valid PSV number
df$valid_psvnum <- df$psvnum_label %in% as.character(valid_reg_df$psv_num)

# English and order factors ----------------------------------------------------
## Order factors so that low is unsafe, and high is safe

df <- df %>%
  
  ## English version
  dplyr::mutate(
    consent_label_en = case_when(
      consent_label == "Ndiyo" ~ "Yes"
    ),
    safety_label_en = case_when(
      safety_label == "Si Salama Sana" ~ "Not Very Safe",
      safety_label == "Si Salama"      ~ "Not Safe",
      safety_label == "Salama"         ~ "Safe",
      safety_label == "Salama Sana"    ~ "Very Safe"
    ),
    speed_label_en = case_when(
      speed_label == "Polepole sana [0-10 km/h]" ~ "Very slow [0-10 km/h]",
      speed_label == "Polepole [10-30]"          ~ "Slow [10-30]",
      speed_label == "Wastani [30-50]"           ~ "Average [30-50]",
      speed_label == "Haraka [50-80]"            ~ "Fast [50-80]",
      speed_label == "Haraka sana [80+]"         ~ "Very fast [80+]"
    )) %>%
  
  ## Order factors
  dplyr::mutate(
    safety_label = safety_label %>% factor(levels = c("Si Salama Sana",
                                                      "Si Salama",
                                                      "Salama",
                                                      "Salama Sana")),
    safety_label_en = safety_label_en %>% factor(levels = c("Not Very Safe",
                                                            "Not Safe",
                                                            "Safe",
                                                            "Very Safe")),
    speed_label = speed_label %>% factor(levels = c("Polepole sana [0-10 km/h]",
                                                    "Polepole [10-30]",
                                                    "Wastani [30-50]",
                                                    "Haraka [50-80]",
                                                    "Haraka sana [80+]")),
    speed_label_en = speed_label_en %>% factor(levels = c("Very slow [0-10 km/h]",
                                                          "Slow [10-30]",
                                                          "Average [30-50]",
                                                          "Fast [50-80]",
                                                          "Very fast [80+]"))
  )


# Remove PII -------------------------------------------------------------------
df_nonpii <- df %>%
  dplyr::select(-c(name, phone)) 

# Export -----------------------------------------------------------------------
saveRDS(df_nonpii,   file.path(data_dir, "Rider Feedback", "FinalData",
                               "rider_feedback.Rds"))

saveRDS(df,   file.path(rider_feedback_pii_dir, "FinalData - PII", 
                        "rider_feedback.Rds"))

