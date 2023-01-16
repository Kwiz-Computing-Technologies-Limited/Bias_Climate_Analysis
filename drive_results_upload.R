if(!("googledrive" %in% installed.packages())){
  install.packages("googledrive")
}
library(googledrive)

# Authenticate google
drive_auth(email = "biasoutputfiles@gmail.com")

# file list to upload
directory_files = list.files("~/Desktop/Documents/GitHub/bias assessment/13.  bias assessment results")
directory_files2 = directory_files[!grepl(".Rmd", directory_files)]
directory_files2 = directory_files2[!grepl(".txt", directory_files2)]

# Already uploaded files
uploaded = drive_ls()$name

directory_files3 = directory_files2[!(directory_files2 %in% uploaded)]
# directory_files3 = directory_files3[!grepl("periods_output", directory_files3)]

if(length(directory_files3) != 0) {
  for (i in 1:length(directory_files3)) {
    if(!(directory_files3[i] %in% uploaded)){
      paste("uploading", directory_files3[i]) |>
        print()
      drive_upload(directory_files3[i])
    }
  }
}
