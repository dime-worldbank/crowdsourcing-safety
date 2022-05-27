# Create list of valid plate numbers

change_ith_letter <- function(word, i, type){
  # word = string
  # i = ith letter in string
  # type = capitalize or lowercase
  
  letters <- word %>% strsplit(split = "") %>% unlist()
  if(type == "upper") letters[i] <- letters[i] %>% toupper()
  if(type == "lower") letters[i] <- letters[i] %>% tolower()
  
  word_out <- letters %>% paste(collapse = "")
  
  return(word_out)
} 

# Load Data --------------------------------------------------------------------
data <- read_excel(file.path(dropbox_file_path, "Data", "Matatu Data", 
                                   "Characteristics", "RawData", "vehicle_basic_info.xlsx"))

data <- data %>%
  filter(pilot_number %in% 2:3) %>%
  dplyr::select(plate, award_type)

data$plate <- data$plate %>% str_squish() %>% str_replace_all(" ", "")

data_allupper <- data %>% mutate(plate = plate %>% toupper())
data_alllower <- data %>% mutate(plate = plate %>% tolower())
data_firstupper <- data_alllower %>% 
  mutate(plate = plate %>% lapply(change_ith_letter, 1, "upper") %>% unlist)
data_lastlower <- data_allupper %>% 
  mutate(plate = plate %>% lapply(change_ith_letter, 7, "lower") %>% unlist)

data_nospace <- bind_rows(data_allupper,
                          data_alllower,
                          data_firstupper,
                          data_lastlower)

data_withspace <- data_nospace %>%
  mutate(plate = plate %>% str_replace_all('^(.{3})(.*)$', '\\1 \\2'))

data_all <- bind_rows(data_nospace,
                      data_withspace)


data_all$plate[data_all$award_type %in% "get"] %>%
  write.csv(file.path(dropbox_file_path, "Data", "Matatu Data", 
                      "Valid Plate Numbers",
                      "get_award.csv"), 
            row.names = F)

data_all$plate[data_all$award_type %in% "win"] %>%
  write.csv(file.path(dropbox_file_path, "Data", "Matatu Data", 
                      "Valid Plate Numbers",
                      "win_award.csv"), 
            row.names = F)



