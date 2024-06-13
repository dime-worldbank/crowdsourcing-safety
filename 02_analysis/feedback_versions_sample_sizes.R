

# Vehicle Sample Sizes ---------------------------------------------------------
## Original
df_v1 <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_cmntfilterFALSE_dstnctpassTRUE.Rds"))
table(df_v1$n_feedback >= 10)

df_v1 <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_telematics_cmntfilterFALSE_dstnctpassTRUE.Rds"))
table(df_v1$n_feedback >= 10)

## Keep duplicate passengers
df_v2 <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_telematics_cmntfilterFALSE_dstnctpassFALSE.Rds"))
table(df_v2$n_feedback >= 10)

## Filter comments
df_v3 <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_telematics_cmntfilterTRUE_dstnctpassTRUE.Rds"))
table(df_v3$n_feedback >= 10)

## Keep duplicate passengers and filter comments
df_v4 <- readRDS(file.path(data_dir, "FinalData", "vehicle_level_stickers_telematics_cmntfilterTRUE_dstnctpassFALSE.Rds"))
table(df_v4$n_feedback >= 10)


cmntfilter <- FALSE
dstnctpass <- TRUE

make_tex <- function(dstnctpass, cmntfilter){
  
  df <- readRDS(file.path(data_dir, "FinalData", paste0("vehicle_level_stickers_cmntfilter",cmntfilter,"_dstnctpass",dstnctpass,".Rds")))
  df_both <- readRDS(file.path(data_dir, "FinalData", paste0("vehicle_level_stickers_telematics_cmntfilter",cmntfilter,"_dstnctpass",dstnctpass,".Rds")))
  
  cat(paste0(
    ifelse(dstnctpass, "No", "Yes"), " & ",
    ifelse(cmntfilter, "Yes", "No"), " & ",
    sum(df$n_feedback), " & ",
    sum(df$n_feedback >= 10), " & ",
    sum(df_both$n_feedback >= 10), " \\\\ \n"
  ))
  
}

sink(file.path(tables_dir, "feedback_versions_samples.tex"))
cat("\\begin{tabular}{ccccc} \n")
cat("\\hline \n")
cat("Keep duplicate            & Filter by & N         & N vehicles with & N vehicles with $\\geq 10$ responses \\\\ \n")
cat("vehicle-passenger entries & comments  & responses & $\\geq 10$ responses & of 25 vehicles with telematics devices \\\\ \n")
cat("\\hline \n")
make_tex(dstnctpass = TRUE, cmntfilter = FALSE)
make_tex(dstnctpass = FALSE, cmntfilter = FALSE)
make_tex(dstnctpass = TRUE, cmntfilter = TRUE)
make_tex(dstnctpass = FALSE, cmntfilter = TRUE)
cat("\\hline \n")
cat("\\end{tabular} \n")
sink()

