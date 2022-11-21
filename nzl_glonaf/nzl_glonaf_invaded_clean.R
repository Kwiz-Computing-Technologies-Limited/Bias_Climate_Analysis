
if (!"occAssess" %in% installed.packages()) devtools::install_github("https://github.com/robboyd/occAssess")
library(occAssess)
library(rgbif)
library(readr)
library(rdrop2)
library(tidyverse)



# Get nzl_glonaf invaded clean


drop_url = "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AADOzX6t5YJPH7W1Z7x03SL9a/NZL-Glonaf/NZL-Glonaf-invaded-clean.csv?dl=1"
country = "nzl_glonaf"
habitat = "Invaded-clean"

N = seq(from = 1, to = 1e9, by = 1000000)

options(timeout = 100000)

for (n in N) {
  gc()
  source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
  
  tryCatch({
    if(!("nzl_glonaf_invaded_clean" %in% dbListTables(aws_con))){
      print("fetching new")
      
      value = read.csv(file = drop_url, header = T,
                       colClasses = c("NULL", "character","NULL", "NULL", "NULL", "NULL", 
                                      "numeric", "numeric", "NULL", "NULL", "numeric", "NULL",
                                      "numeric", "numeric", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"), 
                       nrows = 1000000)
      rownames(value) = NULL
      
      source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
      
      print("uploading new")
      dbWriteTable(conn = aws_con, name = "nzl_glonaf_invaded_clean", value = value)
    } 
    
    x = dbGetQuery(aws_con, "SELECT COUNT(*) FROM nzl_glonaf_invaded_clean")$count
    source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    
    if(((x/1000000) - floor(x/1000000)) == 0) {
      
      paste("reading", 1000000, "rows from", x) |> print()
      start = Sys.time()
      
      value = read.csv(file = drop_url, header = F,
                       colClasses = c("NULL", "character","NULL", "NULL", "NULL", "NULL", 
                                      "numeric", "numeric", "NULL", "NULL", "numeric", "NULL",
                                      "numeric", "numeric", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"), 
                       nrows = 1000000, skip = x)
      
      if(!is_empty(value)) {
        rownames(value) = NULL
        
        source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
        
        db_columns = dbGetQuery(conn=aws_con, statement = "SELECT column_name 
                        FROM information_schema.columns WHERE table_name = 'nzl_glonaf_invaded_clean' 
                        ORDER BY ordinal_position")
        colnames(value) = db_columns$column_name[2:7]
        
        
        paste("Updating", x, "to", (x + nrow(value)), "to AWS DB") |> print()
        dbWriteTable(conn = aws_con, 
                     name = "nzl_glonaf_invaded_clean", 
                     value = value, 
                     overwrite = FALSE,
                     append = TRUE)
      } else {
        paste("row", n, "not found") |> print() 
      }
      
      finished = Sys.time()
      
      paste("completed in", (finished - start), "at", finished) |> print()
    }
    paste("Upload is up-to-data") |> print()
    break
  })
}

