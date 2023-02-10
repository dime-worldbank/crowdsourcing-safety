# Make Stickers

# Load data --------------------------------------------------------------------
veh_df            <- readRDS(file.path(matatu_data_dir, "FinalData", "vehicle_info.Rds"))
blank_sticker_psv <- brick(file.path(matatu_data_dir, "Stickers", "stickers_feb2023_win_sms_top_comments", "sticker_blank_psv_no.png"))
blank_sticker_reg <- brick(file.path(matatu_data_dir, "Stickers", "stickers_feb2023_win_sms_top_comments", "sticker_blank_reg_no.png"))

# Subset to vehicles to make sticker for ---------------------------------------
veh_reg_no_df <- veh_df %>%
  dplyr::filter(drvr_feedback_treat_sticker %in% 1,
                phase %in% "june2022-sensor-driverfeedback") %>%
  dplyr::filter(reg_no %in% c("KCQ 450T",
                              "KCC 572Z",
                              "KCC 273J",
                              "KDC 892Q",
                              "KCU 878Z",
                              "KBW 511R",
                              "KCE 800J",
                              "KDE 255R",
                              "KDA 285E",
                              "KDB 762X",
                              "KCE 670R",
                              "KCR 613W",
                              "KCQ 580G",
                              "KDD 023J",
                              "KCH 471D"))

# Make sticker: Reg Number -----------------------------------------------------
for(i in 1:nrow(veh_reg_no_df)){ # 1:nrow(veh_df)
  print(i)
  
  ## Grab Reg No & PSV Number
  reg_no_i  <- veh_reg_no_df$reg_no[i]
  psv_num_i <- veh_reg_no_df$psv_num[i]
  
  ## Load QR Code
  qr_code <- brick(file.path(matatu_data_dir, "Stickers", "stickers_feb2023_win_sms_top_comments", 
                             "static_qr_codes", "qr_reg_no.png"))
  
  ## Crop QR Code - Remove White Space
  crop_amount <- 30
  qr_code <- crop(qr_code, extent(crop_amount,
                                  1148 - crop_amount,
                                  crop_amount,
                                  1148 - crop_amount))
  
  ## Change QR Code Extent; Position Within Sticker
  #extent(qr_code) <- c(620, 1170, 100-15, 650)
  extent(qr_code) <- c(1850+140, 3510+140, 320-50, 1910-50)
  
  ## Plot
  png(file.path(matatu_data_dir, "Stickers", "stickers_feb2023_win_sms_top_comments", "stickers_reg_no", paste0(reg_no_i, ".png")),
      10240,10240)
  plotRGB(blank_sticker_reg, maxpixels=10240*10240, margins = F)
  plotRGB(qr_code, maxpixels=10240*10240, add = T, margins = F)
  text(x = 830,
       y = 850,
       labels = reg_no_i,
       font = 2,
       col = "#377e40",
       cex = 68)
  # text(x = 3350,
  #      y = 40,
  #      labels = reg_no_i,
  #      font = 2,
  #      col = "black",
  #      cex = 15)
  dev.off()
  
}

# Make sticker: PSV Number -----------------------------------------------------
## TODO: Only do for "extras"
# for(i in 1:nrow(veh_psv_no_df)){ # 1:nrow(veh_df)
#   print(i)
#   
#   ## Grab Reg No & PSV Number
#   reg_no_i  <- veh_psv_no_df$reg_no[i]
#   psv_num_i <- veh_psv_no_df$psv_num[i]
#   
#   ## Load QR Code
#   qr_code <- brick(file.path(matatu_data_dir, "Stickers", "stickers_feb2023_win_sms", 
#                              "static_qr_codes", "qr_psv_no.png"))
#   
#   ## Crop QR Code - Remove White Space
#   crop_amount <- 30
#   qr_code <- crop(qr_code, extent(crop_amount,
#                                   1148 - crop_amount,
#                                   crop_amount,
#                                   1148 - crop_amount))
#   
#   ## Change QR Code Extent; Position Within Sticker
#   #extent(qr_code) <- c(620, 1170, 100-15, 650)
#   extent(qr_code) <- c(1850+140, 3510+140, 320-50, 1910-50)
#   
#   ## Plot
#   png(file.path(matatu_data_dir, "Stickers", "stickers_feb2023_win_sms", "stickers_psv_no", paste0(reg_no_i, ".png")),
#       10240,10240)
#   plotRGB(blank_sticker_psv, maxpixels=10240*10240, margins = F)
#   plotRGB(qr_code, maxpixels=10240*10240, add = T, margins = F)
#   text(x = 800,
#        y = 850,
#        labels = psv_num_i,
#        font = 2,
#        col = "#377e40",
#        cex = 120)
#   # text(x = 3350,
#   #      y = 40,
#   #      labels = reg_no_i,
#   #      font = 2,
#   #      col = "black",
#   #      cex = 15)
#   dev.off()
#   
# }
# 
# # Append images ----------------------------------------------------------------
## Reg No
images_reg = list.files(file.path(matatu_data_dir, "Stickers", "stickers_feb2023_win_sms_top_comments", "stickers_reg_no"),
                        full.names = TRUE,
                        pattern = '.png')

image_write(image_read(images_reg),
            format = "pdf",
            file.path(matatu_data_dir, "Stickers", "stickers_feb2023_win_sms_top_comments", "stickers_reg_no.pdf"))
# 
# ## PSV No
# images_reg = list.files(file.path(matatu_data_dir, "Stickers", "stickers_feb2023_win_sms", "stickers_psv_no"),
#                         full.names = TRUE,
#                         pattern = '.png')
# 
# image_write(image_read(images_reg),
#             format = "pdf",
#             file.path(matatu_data_dir, "Stickers", "stickers_feb2023_win_sms", "stickers_psv_no.pdf"))
# 
# 
# # 
# # ## Swahili
# # images_sw = list.files(file.path(matatu_data_dir, "Stickers", "stickers_june2022", "psv_stickers", "swahili"), 
# #                        full.names = TRUE, 
# #                        pattern = '.png')
# # 
# # image_write(image_read(images_sw), 
# #             format = "pdf", 
# #             file.path(matatu_data_dir, "Stickers", "stickers_june2022", "psv_stickers", "stickers_swahili.pdf"))
# # 
# # 
# # 
# # 
# # 
