if(!("googledrive" %in% installed.packages())){
  install.packages("googledrive")
}
library(googledrive)

# Authenticate google
drive_auth(email = "biasoutputfiles@gmail.com")

# file list to upload
directory_files = list.files("~/Desktop/Documents/GitHub/bias assessment/13.  bias assessment results")
directory_files2 = directory_files[grepl(".RDS", directory_files)]

# Already uploaded files
uploaded = drive_ls()$name

for (i in 1:length(directory_files2)) {
  if(!(directory_files2[i] %in% uploaded)){
    drive_upload("~/Desktop/Documents/GitHub/bias assessment/13.  bias assessment results/", paste0(directory_files2[i]))
  }
}