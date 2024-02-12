# Analyse Pilots

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers.Rds"))

# Prep data --------------------------------------------------------------------
# veh_clean_df <- veh_df %>%
#   dplyr::filter(pilot_number %in% 1:5,
#                 #shortcode_on_sticker = "yes",
#                 !is.na(award_type)) %>%
#   dplyr::mutate(award = paste0(pilot_number, ": ", award_type, ": KES", award_amount)) %>%
#   dplyr::mutate(n_feedback_daily = n_feedback_1wk)

pilot_df <- veh_df %>% 
  
  mutate(n_feedback_daily = n_feedback_1wk) %>%
  
  dplyr::filter(pilot_number %in% 1:5,
                !is.na(award_type)) %>%
  group_by(pilot_number, award_type, award_amount, shortcode_on_sticker) %>%
  dplyr::summarise(n_vehicle = n(),
                   n_min = min(n_feedback_daily),
                   n_median = median(n_feedback_daily),
                   
                   n_mean = mean(n_feedback_daily),
                   n_max = max(n_feedback_daily)) %>%
  arrange(pilot_number, award_type, award_amount) %>%
  mutate(award_type = award_type %>% tools::toTitleCase(),
         award_amount = paste0("KES ", award_amount),
         qr_sms = ifelse(shortcode_on_sticker == "yes", "SMS \\& QR", "QR"),
         n_mean = round(n_mean, 2)) %>%
  mutate(tex = paste(award_type,
                     award_amount,
                     qr_sms,
                     n_vehicle,
                     n_min,
                     n_median,
                     n_mean,
                     n_max,
                     sep = " & ") %>%
           paste0(" \\\\ \n"))

# Make table -------------------------------------------------------------------
sink(file.path(tables_dir, "pilot_testing.tex"))

cat("\\begin{tabular}{l | l | l | l | l | l | l | l} \n")
cat("\\hline \n")
cat(" & & & & \\multicolumn{4}{c}{N Surveys Completed 1 Week} \\\\ \n")
cat(" & & & & \\multicolumn{4}{c}{After Stickers Installed} \\\\ \n")
cat("\\hline \n")
cat("Award   & Award  & Survey & N        & Min & Median & Mean & Max \\\\ \n")
cat("Get/Win & Amount & Method & Vehicles &     &        &      &     \\\\ \n ")
cat("\\hline \n")
cat("\\multicolumn{8}{c}{} \\\\ \n")
cat("\\multicolumn{8}{l}{ \\textbf{Test 1:} Initial sticker test with no awards} \\\\ \n")

cat("\\hline \n")
pilot_df %>% filter(pilot_number == 1) %>% pull(tex) %>% cat()
cat("\\hline \n")

cat("\\multicolumn{8}{c}{} \\\\ \n")
cat("\\multicolumn{8}{l}{ \\textbf{Test 2:} Award all respondents KES 100 vs 200} \\\\ \n")
cat("\\hline \n")
pilot_df %>% filter(pilot_number == 2) %>% pull(tex) %>% cat()
cat("\\hline \n")

cat("\\multicolumn{8}{c}{} \\\\ \n")
cat("\\multicolumn{8}{l}{ \\textbf{Test 3:} Respondents either get award or enter lotter for KES 50} \\\\ \n")
cat("\\hline \n")
pilot_df %>% filter(pilot_number == 3) %>% pull(tex) %>% cat()
cat("\\hline \n")

cat("\\multicolumn{8}{c}{} \\\\ \n")
cat("\\multicolumn{8}{l}{ \\textbf{Test 4:} Respondents enter lottery for KES 100, testing QR vs QR \\& SMS} \\\\ \n")
cat("\\hline \n")
pilot_df %>% filter(pilot_number == 4) %>% pull(tex) %>% cat()
cat("\\hline \n")

cat("\\multicolumn{8}{c}{} \\\\ \n")
cat("\\multicolumn{8}{l}{ \\textbf{Test 5:} Respondents get KES 20} \\\\ \n")
cat("\\hline \n")
pilot_df %>% filter(pilot_number == 5) %>% pull(tex) %>% cat()
cat("\\hline \n")

cat("\\end{tabular}")

sink()


