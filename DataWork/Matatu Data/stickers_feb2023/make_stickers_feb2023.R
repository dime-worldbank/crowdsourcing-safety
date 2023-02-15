# Make Stickers

# Load data --------------------------------------------------------------------
veh_df            <- readRDS(file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))
blank_sticker_psv <- brick(file.path(matatu_data_dir, "Stickers", "stickers_feb2023", "sticker_blank.png"))

# Subset to vehicles to make sticker for ---------------------------------------
veh_reg_no_df <- veh_df %>%
  dplyr::filter(drvr_feedback_treat_sticker %in% 1,
                phase %in% "june2022-sensor-driverfeedback") 

veh_psv_no_df <- veh_df %>%
  dplyr::filter(reg_no %in% "UNASSIGNED") %>%
  head(75)

# Make sticker: Reg Number -----------------------------------------------------
for(i in 1:nrow(veh_psv_no_df)){ # 1:nrow(veh_psv_no_df)
  print(i)
  
  ## Grab Reg No & PSV Number
  psv_num_i <- veh_psv_no_df$psv_num[i]
  
  ## Load QR Code
  qr_code <- brick(file.path(matatu_data_dir, "Stickers", "stickers_feb2023", "qr.png"))
  
  ## Crop QR Code - Remove White Space
  crop_amount <- 30
  qr_code <- crop(qr_code, extent(crop_amount,
                                  1148 - crop_amount,
                                  crop_amount,
                                  1148 - crop_amount))
  
  ## Change QR Code Extent; Position Within Sticker
  extent(qr_code) <- c(990, 1740, 130, 880)
  #extent(qr_code) <- c(1850+140, 3510+140, 320-50, 1910-50)
  
  ## Plot
  png(file.path(matatu_data_dir, "Stickers", "stickers_feb2023", "stickers", paste0(psv_num_i, ".png")),
      1772,1772)
  plotRGB(blank_sticker_psv, maxpixels=1772*1772, margins = F)
  plotRGB(qr_code, maxpixels=1772*1772, add = T, margins = F)
  text(x = 430,
       y = 320,
       labels = psv_num_i,
       font = 2,
       col = "#377e40",
       cex = 28)
  # text(x = 3350,
  #      y = 40,
  #      labels = reg_no_i,
  #      font = 2,
  #      col = "black",
  #      cex = 15)
  dev.off()
  
}

# Append images ----------------------------------------------------------------
## Reg No
images_reg = list.files(file.path(matatu_data_dir, "Stickers", "stickers_feb2023", "stickers"),
                        full.names = TRUE,
                        pattern = '.png')

image_write(image_read(images_reg),
            format = "pdf",
            file.path(matatu_data_dir, "Stickers", "stickers_feb2023",
                      paste0("stickers.pdf")))


# 
# images_reg_chunks <- split(images_reg, ceiling(seq_along(images_reg) / 10))
# 
# i <- 1
# for(reg_i in images_reg_chunks){
# 
#   image_write(image_read(reg_i),
#               format = "pdf",
#               file.path(matatu_data_dir, "Stickers", "stickers_feb2023",
#                         paste0("stickers_", i, ".pdf")))
# 
#   i <- i + 1
# 
# }

