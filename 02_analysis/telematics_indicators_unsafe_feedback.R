# Telematics indicators for vehicles determined unsafe by crowdsourcing

# Load data --------------------------------------------------------------------
comment_filter <- FALSE
distinct_pass  <- TRUE

veh_df <- readRDS(file.path(data_dir, "FinalData", 
                            paste0("vehicle_level_stickers_telematics_suff_feedback_cmntfilter",
                                   comment_filter,
                                   "_dstnctpass",distinct_pass,".Rds")))

veh_all_df <- readRDS(file.path(data_dir, "FinalData", 
                                paste0("vehicle_level_stickers_telematics_cmntfilter",
                                       comment_filter,
                                       "_dstnctpass",distinct_pass,".Rds")))

# Table ------------------------------------------------------------------------
rank_df <- bind_rows(
  veh_df %>%
    arrange(-q_safety_prop_unsafe) %>%
    head(2),
  
  veh_df %>%
    arrange(-q_speed_rating_v2_vfast) %>%
    head(2),
  
  veh_df %>%
    arrange(comment_driver_sntmt_code_avg) %>%
    head(2)
) %>%
  distinct(regno, .keep_all = T) %>%
  select(regno, n_feedback,
         
         q_safety_prop_unsafe, q_speed_rating_v2_vfast, comment_driver_sntmt_code_avg,
         
         prop_time_over_80kph_base_10kph,
         prop_time_over_100kph_base_10kph,
         
         rate_N_valueg_above0_5_base_10kph) 

veh_all_rank_df <- veh_all_df %>%
  select(regno,
         
         prop_time_over_80kph_base_10kph,
         prop_time_over_100kph_base_10kph,
         
         rate_N_valueg_above0_5_base_10kph) %>%
  mutate(across(where(is.numeric), ~ rank(.x))) %>%
  rename_with(~ paste0(.x, "_rank"), where(is.numeric))

veh_rank_df <- veh_df %>%
  select(regno,
         
         q_safety_prop_unsafe, 
         q_speed_rating_v2_vfast, 
         comment_driver_sntmt_code_avg) %>%
  mutate(across(where(is.numeric), ~ rank(.x))) %>%
  rename_with(~ paste0(.x, "_rank"), where(is.numeric))

rank_df <- rank_df %>%
  left_join(veh_rank_df, by = "regno") %>%
  left_join(veh_all_rank_df, by = "regno") %>%
  mutate(comment_driver_sntmt_code_avg_rank = 
           13 - comment_driver_sntmt_code_avg_rank + 1)

clean_var <- function(df, var, n){
  avg_val  <- df[[var]] %>% round(2)
  rank_val <- n - df[[paste0(var, "_rank")]] + 1
  
  df[[paste0(var, "_table")]] <- paste0(avg_val, " (", rank_val, "/", n, ")")
  
  return(df)
}

rank_df <- rank_df %>%
  clean_var("q_safety_prop_unsafe", 13) %>%
  clean_var("q_speed_rating_v2_vfast", 13) %>%
  clean_var("comment_driver_sntmt_code_avg", 13) %>%
  
  clean_var("prop_time_over_80kph_base_10kph", 25) %>%
  clean_var("prop_time_over_100kph_base_10kph", 25) %>%
  clean_var("rate_N_valueg_above0_5_base_10kph", 25) #%>%
# clean_var("rate_N_valueg_above0_5_acceleration_base_10kph", 25) %>%
# clean_var("rate_N_valueg_above0_5_brake_base_10kph", 25) %>%
# clean_var("rate_N_valueg_above0_5_turn_base_10kph", 25)

CELL_ALPHA <- 1

p <- rank_df %>%
  gt(rowname_col = "regno",
     auto_align = F) %>%
  cols_hide(c(q_safety_prop_unsafe, 
              q_speed_rating_v2_vfast, 
              comment_driver_sntmt_code_avg,
              
              q_safety_prop_unsafe_rank, 
              q_speed_rating_v2_vfast_rank, 
              comment_driver_sntmt_code_avg_rank,
              
              prop_time_over_80kph_base_10kph,
              prop_time_over_100kph_base_10kph,
              rate_N_valueg_above0_5_base_10kph,
              
              prop_time_over_80kph_base_10kph_rank,
              prop_time_over_100kph_base_10kph_rank,
              rate_N_valueg_above0_5_base_10kph_rank)) %>%
  data_color(columns = c("q_safety_prop_unsafe_rank",
                         "q_speed_rating_v2_vfast_rank",
                         "comment_driver_sntmt_code_avg_rank"), 
             target_columns = c("q_safety_prop_unsafe_table",
                                "q_speed_rating_v2_vfast_table",
                                "comment_driver_sntmt_code_avg_table"),
             palette = "RdYlBu",
             domain = c(1,13),
             reverse = T,
             alpha = CELL_ALPHA) %>%
  data_color(columns = c("prop_time_over_80kph_base_10kph_rank",
                         "prop_time_over_100kph_base_10kph_rank",
                         "rate_N_valueg_above0_5_base_10kph_rank"), 
             target_columns = c("prop_time_over_80kph_base_10kph_table",
                                "prop_time_over_100kph_base_10kph_table",
                                "rate_N_valueg_above0_5_base_10kph_table"),
             palette = "RdYlBu",
             domain = c(1,25),
             reverse = T,
             alpha = CELL_ALPHA) %>%
  cols_label(n_feedback = "N Feedback",
             q_safety_prop_unsafe_table = "Percent Rate Unsafe",
             q_speed_rating_v2_vfast_table = "Percent Rate Very Fast",
             comment_driver_sntmt_code_avg_table = "Driving Sentiment",
             prop_time_over_80kph_base_10kph_table = "Percent time > 80km/h",
             prop_time_over_100kph_base_10kph_table = "Percent time > 100km/h",
             rate_N_valueg_above0_5_base_10kph_table = "Harsh driving rate") %>%
  tab_source_note(html('<pre><span style="background-color: #a50026;"
                       >        </span> = Lowest Rank (Most Unsafe)\n<span style="background-color: #ffffbf;"
                       >        </span> = Medium Rank\n<span style="background-color: #313695;"
                       >        </span> = Highest Rank (Most Safe)</pre>')) %>% 
  tab_spanner(
    label = "Passenger Feedback: Value (Ranking)",
    columns = vars(q_safety_prop_unsafe_table,
                   q_speed_rating_v2_vfast_table,
                   comment_driver_sntmt_code_avg_table)) %>% 
  tab_spanner(
    label = "Telematics: Value (Ranking)",
    columns = vars(prop_time_over_80kph_base_10kph_table,
                   prop_time_over_100kph_base_10kph_table,
                   rate_N_valueg_above0_5_base_10kph_table))

gtsave(p, filename = file.path(figures_dir, "unsafe_vehicles_table.png"))

# Histograms -------------------------------------------------------------------
make_hist <- function(df,
                      var,
                      name_str){
  
  df$var <- df[[var]]
  rank_df$var <- rank_df[[var]]
  
  rank_df <- bind_rows(
    rank_df,
    data.frame(var = median(df$var),
               regno = "Median")
  )

  df %>%
    ggplot(aes(x = var)) +
    geom_histogram(fill = "gray80",
                   color = "black") +
    geom_vline(data = rank_df,
               aes(xintercept = var,
                   color = regno),
               linewidth = 0.75) +
    labs(color = NULL,
         y = "N Vehicles",
         x = name_str,
         title = name_str) +
    scale_color_manual(values = c("black", "red", "green2", "deepskyblue")) +
    theme_classic2() +
    theme(plot.title = element_text(size = 12, face = "bold"))
}

p1 <- make_hist(veh_all_df,
          "prop_time_over_80kph_base_10kph",
          "Proportion of time exceed 80 km/h")

p2 <- make_hist(veh_all_df,
                "prop_time_over_100kph_base_10kph",
                "Proportion of time exceed 100 km/h")

p3 <- make_hist(veh_all_df,
                "rate_N_valueg_above0_5_base_10kph",
                "N Harsh Violations per Hour")

p <- ggarrange(p1, p2, p3, nrow = 1, common.legend = T, legend = "top")

ggsave(p, filename = file.path(figures_dir, "unsafe_vehicles_hist.png"),
       height = 3, width = 10)
