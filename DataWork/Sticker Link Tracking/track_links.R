# Track QRs

# https://github.com/brianwdavis/quadrangle
library(quadrangle)
library(httr)
library(magick)

links_1_4_df <- map_df(1:4, function(pdf_num){
  
  if(pdf_num == 1) pdf_len <- 6
  if(pdf_num == 2) pdf_len <- 12
  if(pdf_num == 3) pdf_len <- 15
  if(pdf_num == 4) pdf_len <- 20
  if(pdf_num == 5) pdf_len <- 59
  
  links_out <- map_df(1:pdf_len, function(page_i){
    
    png_img <- image_read_pdf(file.path(dropbox_dir, "Data", 
                                        "Stickers Link Tracking", 
                                        "stickers", 
                                        paste0("Stickers - Pilot ",pdf_num,".pdf")), 
                              pages = page_i)
    
    qr_out <- qr_scan(png_img)
    init_url  <- qr_out$values$value
    redir_out <- HEAD(init_url)
    redir_url <- redir_out$url
    
    data.frame(pdf_name = paste0("Stickers - Pilot ",pdf_num,".pdf"),
               page = page_i,
               initial_url = init_url,
               redir_url = redir_url,
               time = Sys.time())
    
  })
  
  links_out
  
})

links_5_df <- map_df(1:59, function(i){
  
  png_f <- file.path(dropbox_dir, "Data", 
                     "Stickers Link Tracking", 
                     "stickers", 
                     "Pilot 5 - PNGs") %>%
    list.files()
  
  qr_out <- qr_scan(file.path(dropbox_dir, "Data", 
                              "Stickers Link Tracking", 
                              "stickers", 
                              "Pilot 5 - PNGs",
                              png_f[i]))
  init_url  <- qr_out$values$value
  redir_out <- HEAD(init_url)
  redir_url <- redir_out$url
  
  data.frame(pdf_name = paste0("Stickers - Pilot ",5,".pdf"),
             page = page_i,
             initial_url = init_url,
             redir_url = redir_url,
             time = Sys.time())
  
})

links_all_df <- bind_rows(links_1_4_df,
                          links_5_df)

tm <- Sys.time() %>% as.character() %>% str_replace_all("-|:| ", "_")
write_csv(links_all_df, 
          file.path(dropbox_dir, "Data", 
                    "Stickers Link Tracking",
                    "output",
                    paste0("Stickers - Pilot ",tm,".csv")))

saveRDS(links_all_df, 
        file.path(dropbox_dir, "Data", 
                  "Stickers Link Tracking",
                  "output",
                  paste0("Stickers - Pilot ",tm,".Rds")))

# Analysis ---------------------------------------------------------------------
links_all_df <- links_all_df %>%
  mutate(init_eq_redir = initial_url == redir_url)

links_all_df %>%
  group_by(pdf_name) %>%
  summarise(prop_init_eq_redir = mean(init_eq_redir))


#links_all_df$init_eq_redir <- links_all_df$initial_url == links_all_df$redir_url


#links_all_df$redir_url[links_all_df$init_eq_redir == F]
