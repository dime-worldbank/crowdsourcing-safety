# Make PDFs for GPS Installation

group <- "B"

# Define Group Information -----------------------------------------------------
# Define information about SACCO group

if(group %in% "B"){
  group_route <- "Nairobi -- Eldoret -- OTHER. (Matatu goes to Eldoret - can go further, but doesnt have to.)"
  group_stops_type <- "Frequent Stops: Matatu should make frequent stops along the route."
  instructions <- "Install GPS sensors on {\\bf \\underline{3 matatus}} for this SACCO. 
  Install sensors in the order of this list. Skip matatus if needed (for example, can't find). 
  Stop when you've installed 5 sensors."
}

if(group %in% "C"){
  group_route <- "Nairobi -- Eldoret -- OTHER. (Matatu goes to Eldoret - can go further, but doesnt have to.)"
  group_stops_type <- "Direct or minimal: These matatus should either be direct or make minimal stops"
  instructions <- "Install GPS sensors on {\\bf \\underline{3 matatus}} for this SACCO. 
  Install sensors in the order of this list and stop when you've installed 3 sensors."
}

# Load Matatu Data and Set Dir -------------------------------------------------
# Load matatu information and define directory to SACCO randomized files

matatu_df <- read.csv(file.path(dropbox_file_path, 
                                "Data", "Matatu Data", "Listing_202102", "sacco_listing_digitized_randomized",
                                "file_names_and_groups.csv"),
                      stringsAsFactors = F)

randomized_files_dir <- file.path(dropbox_file_path, 
                                  "Data", "Matatu Data", "Listing_202102", 
                                  "sacco_listing_digitized_randomized",
                                  "file_per_sacco")

# Make PDF ---------------------------------------------------------------------
sink(file.path(dropbox_file_path, "Data", "Matatu Data", "Listing_202102", "pdf_lists",
               paste0("gps_install_sacco_group_", group, ".tex")))

#### PREAMBLE

cat("\\documentclass[11pt, oneside]{article} \n")
cat("\\usepackage[top=0.35in, bottom=0.35in, left=0.5in, right=0.5in]{geometry} \n")
cat("\\geometry{letterpaper} \n")
cat("\\usepackage{graphicx} \n")
cat("\\usepackage{amssymb} \n")
cat("\\usepackage{amssymb} \n")
cat("\\title{Brief Article} \n")
cat("\\author{The Author} \n")

cat("\\begin{document} \n")

cat("\\pagenumbering{gobble}")

#### LOOP THROUGH MATATUS
matatu_df <- matatu_df[matatu_df$group %in% group,]

for(i in 1:nrow(matatu_df)){
  
  matatu_df_i <- matatu_df[i,]
  
  listing_df <- read.csv(file.path(randomized_files_dir, matatu_df_i$file_name),
                         stringsAsFactors = F)
  
  cat("\\noindent \n")
  cat("\\begin{tabular}{lp{16cm}} \n")
  cat("{\\bf SACCO:} & ", matatu_df_i$sacco_name, " \\\\ \n")
  cat("{\\bf Route:} & ", matatu_df_i$route, " (Group: ", group,   ") \\\\ \n", sep="")
  cat("{\\bf Route Type:} & ", group_route, " \\\\ \n")
  cat("{\\bf Stops Type:} & ", group_stops_type, " \\\\ \n")
  cat("{\\bf Instructions:} & ", instructions, " \\\\ \n")
  cat("\\end{tabular} \n")
  
  cat("\\\\ \n")
  cat("\\\\ \n")
  cat("\\\\ \n")
  cat("\\bigskip \n")
  cat("\\noindent \n")
  
  cat("\\begin{tabular}{l | p{16cm}}  \n")
  cat(" \\hline  \n")
  cat("Reg No & Installed GPS? (If no, why not) \\\\ \n")
  for(row in 1:21){
    cat(" \\hline  \n")
    cat(listing_df$reg_no[row], " & \\\\  \n")
    cat(" & \\\\ \n")
  }
  cat(" \\hline  \n")
  cat("\\end{tabular} \n")
  
  cat("\\\\ ")
  cat("\\scriptsize ")
  cat("{\\bf Other Matatus:} ")
  cat(paste(listing_df$reg_no[22:50], collapse = " - "))
  cat(" \n ")
  cat("\\normalsize \n")
  cat("\\newpage \n")
  
}

cat("\\end{document}")
sink()
