# Route / summary SACCO

# Load data --------------------------------------------------------------------
veh_df <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_all202_cmntfilterFALSE_dstnctpassTRUE.Rds"))

veh_df$sticker <- !is.na(veh_df$shortcode_on_sticker)

route_df <- veh_df %>%
  dplyr::filter(!is.na(speed_max)) %>%
  group_by(route, sticker) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = route,
              names_from = sticker,
              values_from = n) %>%
  clean_names() %>%
  dplyr::rename(sticker = true,
                no_sticker = false) %>%
  dplyr::mutate(route      = replace_na(route, "Other"),
                no_sticker = replace_na(no_sticker, 0),
                sticker    = replace_na(sticker, 0)) %>%
  dplyr::mutate(sticker_total = sum(sticker),
                no_sticker_total = sum(no_sticker)) %>%
  dplyr::mutate(per_sticker = round(sticker / sticker_total * 100,1) %>% paste0("\\%"),
                per_no_sticker = round(no_sticker / no_sticker_total * 100,1) %>% paste0("\\%")) %>%
  mutate(tex = paste0(route, " & ", sticker, " (", per_sticker, ") & ", no_sticker, " (", per_no_sticker, ") \\\\ \n")) %>%
  arrange(factor(route))

sink(file.path(tables_dir, "route_stickers.tex"))
cat("\\begin{tabular}{lcc} \n")
cat("\\hline \n")
cat("      & Number (Percent) of Vehicles         & Number (Percent) of Vehicles \\\\ \n")
cat("Route & Stickers Installed & No Stickers Installed \\\\ \n")
cat("\\hline \n")
route_df$tex %>% cat()
cat("\\hline \n")
cat("Total & 25 (100\\%) & 125 (100\\%) \\\\ \n")
cat("\\hline \n")
cat("\\end{tabular}")
sink()

