# Check if Wialon token is still valid

# Check
wialon_token <- read.table(file.path(sensors_dir, "wialon_token", "wialon_token.txt"), 
                           stringsAsFactors = F)$V1 %>% as.character()
SID <- get_sid(wialon_token)

if(is.null(SID)){
  cat("Wialon token is NOT valid!\n")
  
  cat("1. Go to this website: https://hosting.wialon.com//login.html\n")
  cat("2. Enter login details and click 'authorize'\n")
  cat("3. The token will appear in the URL\n")
  cat("4. Replace token in this file: [OneDrive]/Data/Sensor Data/wialon_token/wialon_token\n\n")

} else{
  cat("Wialon token is valid!\n\n")
}



