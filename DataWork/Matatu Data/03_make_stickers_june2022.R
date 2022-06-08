# Make Stickers

# Load data --------------------------------------------------------------------
veh_df        <- readRDS(file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))
blank_sticker_es <- brick(file.path(matatu_data_dir, "Stickers", "stickers_june2022", "sticker_blank_english.png"))
blank_sticker_sw <- brick(file.path(matatu_data_dir, "Stickers", "stickers_june2022", "sticker_blank_swahili.png"))

# Subset to vehicles to make sticker for ---------------------------------------
veh_df <- veh_df %>%
  dplyr::filter(drvr_feedback_treat_sticker %in% 1) 

# Make sticker: English --------------------------------------------------------
for(i in 1:nrow(veh_df)){
  print(i)
  
  ## Grab Reg No & PSV Number
  reg_no_i  <- veh_df$reg_no[i]
  psv_num_i <- veh_df$psv_num[i]
  
  ## Load QR Code
  qr_code <- brick(file.path(matatu_data_dir, "QR Codes", paste0(reg_no_i, ".png")))

  ## Crop QR Code - Remove White Space
  crop_amount <- 10
  qr_code <- crop(qr_code, extent(crop_amount,
                                  200 - crop_amount,
                                  crop_amount,
                                  200 - crop_amount))
  
  ## Change QR Code Extent; Position Within Sticker
  extent(qr_code) <- c(620, 1170, 100, 650)
  
  ## Plot
  png(file.path(matatu_data_dir, "Stickers", "stickers_june2022", "psv_stickers", "english", paste0(reg_no_i, ".png")),
      10240,10240)
  plotRGB(blank_sticker_es, maxpixels=10240*10240, margins = F)
  plotRGB(qr_code, maxpixels=10240*10240, add = T, margins = F)
  text(x = 260,
       y = 295,
       labels = psv_num_i,
       font = 2,
       col = "#377e40",
       cex = 100)
  text(x = 1115,
       y = 15,
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
  
  ## Load QR Code
  qr_code <- brick(file.path(matatu_data_dir, "QR Codes", paste0(reg_no_i, ".png")))
  
  ## Crop QR Code - Remove White Space
  crop_amount <- 10
  qr_code <- crop(qr_code, extent(crop_amount,
                                  200 - crop_amount,
                                  crop_amount,
                                  200 - crop_amount))
  
  ## Change QR Code Extent; Position Within Sticker
  #extent(qr_code) <- c(630, 1170, 100, 650)
  extent(qr_code) <- c(625, 1170, 100, 630)
  
  ## Plot
  png(file.path(matatu_data_dir, "Stickers", "stickers_june2022", "psv_stickers", "swahili", paste0(reg_no_i, ".png")),
      10240,10240)
  plotRGB(blank_sticker_sw, maxpixels=10240*10240, margins = F)
  plotRGB(qr_code, maxpixels=10240*10240, add = T, margins = F)
  text(x = 260,
       y = 295,
       labels = psv_num_i,
       font = 2,
       col = "#377e40",
       cex = 100)
  text(x = 1115,
       y = 15,
       labels = reg_no_i,
       font = 2,
       col = "black",
       cex = 15)
  dev.off()
  
}

