# Make Stickers - SMS Only

# Load data --------------------------------------------------------------------
veh_df           <- readRDS(file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))
blank_sticker_es <- brick(file.path(matatu_data_dir, "Stickers", "stickers_june2022", "sticker_blank_smsonly_english.png"))
blank_sticker_sw <- brick(file.path(matatu_data_dir, "Stickers", "stickers_june2022", "sticker_blank_smsonly_swahili.png"))

# Subset to vehicles to make sticker for ---------------------------------------
veh_df <- veh_df %>%
  dplyr::filter(drvr_feedback_treat_sticker %in% 1,
                phase %in% "june2022-sensor-driverfeedback") 

# Make sticker: English --------------------------------------------------------
for(i in 1:nrow(veh_df)){
  print(i)
  
  ## Grab Reg No & PSV Number
  reg_no_i  <- veh_df$reg_no[i]
  psv_num_i <- veh_df$psv_num[i]
  
  ## Plot
  png(file.path(matatu_data_dir, "Stickers", "stickers_june2022", "psv_stickers_smsonly", "english", paste0(reg_no_i, ".png")),
      10240,10240)
  plotRGB(blank_sticker_es, maxpixels=10240*10240, margins = F)
  text(x = 1760,
       y = 875,
       labels = psv_num_i,
       font = 2,
       col = "#377e40",
       cex = 135)
  text(x = 3350,
       y = 40,
       labels = reg_no_i,
       font = 2,
       col = "black",
       cex = 15)
  dev.off()
  
}

# Make sticker: Swahili --------------------------------------------------------
for(i in 1:nrow(veh_df)){
  print(i)
  
  ## Grab Reg No & PSV Number
  reg_no_i  <- veh_df$reg_no[i]
  psv_num_i <- veh_df$psv_num[i]
  
  ## Plot
  png(file.path(matatu_data_dir, "Stickers", "stickers_june2022", "psv_stickers_smsonly", "swahili", paste0(reg_no_i, ".png")),
      10240,10240)
  plotRGB(blank_sticker_sw, maxpixels=10240*10240, margins = F)
  text(x = 1760,
       y = 875,
       labels = psv_num_i,
       font = 2,
       col = "#377e40",
       cex = 135)
  text(x = 3350,
       y = 40,
       labels = reg_no_i,
       font = 2,
       col = "black",
       cex = 15)
  dev.off()
  
}

# Append images ----------------------------------------------------------------
## English
images_en = list.files(file.path(matatu_data_dir, "Stickers", "stickers_june2022", "psv_stickers_smsonly", "english"), 
                       full.names = TRUE, 
                       pattern = '.png')

image_write(image_read(images_en), 
            format = "pdf", 
            file.path(matatu_data_dir, "Stickers", "stickers_june2022", "psv_stickers_smsonly", "stickers_english.pdf"))

## Swahili
images_sw = list.files(file.path(matatu_data_dir, "Stickers", "stickers_june2022", "psv_stickers_smsonly", "swahili"), 
                       full.names = TRUE, 
                       pattern = '.png')

image_write(image_read(images_sw), 
            format = "pdf", 
            file.path(matatu_data_dir, "Stickers", "stickers_june2022", "psv_stickers_smsonly", "stickers_swahili.pdf"))





