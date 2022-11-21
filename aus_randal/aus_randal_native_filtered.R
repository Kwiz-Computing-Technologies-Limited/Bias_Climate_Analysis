
if (!"occAssess" %in% installed.packages()) devtools::install_github("https://github.com/robboyd/occAssess")
library(occAssess)
library(rgbif)
library(readr)
library(rdrop2)
library(tidyverse)

if(!("aws_con" %in% ls())) {   source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R") }


# Get aus_randal native filtered


drop_url = "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AADCW2eJp1ZWFWl5jO6ouG8-a/AUS-Randall/AUS-Randall.native-filtered.csv?dl=1"
country = "aus_randal"
habitat = "native-filtered"

N = seq(from = 1, to = 1e3, by = 1)

options(timeout = 100000)

for (n in N) {
  gc()
  
  tryCatch({
    if(!("aus_randal_native_filtered" %in% dbListTables(aws_con))){
      print("fetching new")
      
      value = read.csv(file = drop_url, header = T,
                       colClasses = c("NULL", "NULL", "NULL", "character","NULL", "NULL", "NULL", "NULL", 
                                      "numeric", "numeric", "NULL", "NULL", "numeric", "NULL",
                                      "numeric", "numeric", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"),
                       nrows = 1000000)
      rownames(value) = NULL
      
      if(!("aws_con" %in% ls())) {   source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R") }
      
      print("uploading new")
      dbWriteTable(conn = aws_con, name = "aus_randal_native_filtered", value = value)
    } 
    
    x = dbGetQuery(aws_con, "SELECT COUNT(*) FROM aus_randal_native_filtered")$count
    source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    
    if(((x/1000000) - floor(x/1000000)) == 0) {
      
      paste("reading", 1000000, "rows from", x) |> print()
      start = Sys.time()
      
      value = read.csv(file = drop_url, header = F,
                       colClasses = c("NULL", "NULL", "NULL", "character","NULL", "NULL", "NULL", "NULL", 
                                      "numeric", "numeric", "NULL", "NULL", "numeric", "NULL",
                                      "numeric", "numeric", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"),
                       nrows = 1000000, skip = x)
      if(!is_empty(value)) {
        rownames(value) = NULL
        
        if(!("aws_con" %in% ls())) {   source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R") }
        
        db_columns = dbGetQuery(conn=aws_con, statement = "SELECT column_name 
                        FROM information_schema.columns WHERE table_name = 'aus_randal_native_filtered' 
                        ORDER BY ordinal_position")
        colnames(value) = db_columns$column_name[2:7]
        paste("Updating", x, "to", (x + nrow(value)), "to AWS DB") |> print()
        dbWriteTable(conn = aws_con, 
                     name = "aus_randal_native_filtered", 
                     value = value, 
                     overwrite = FALSE,
                     append = TRUE)
      } else {
        paste("row", n, "not found") |> print() 
      }
      finished = Sys.time()
      paste("completed in", (finished - start), "at", finished) |> print()
    }
  })
}


