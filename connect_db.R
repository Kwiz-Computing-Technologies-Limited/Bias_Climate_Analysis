if(!("DBI" %in% installed.packages())){
  install.packages("DBI")
}
library(DBI)
if(!("RPostgreSQL" %in% installed.packages())){
  install.packages("RPostgreSQL")
}
library(RPostgreSQL)

AWS_POSTGRES_DRV = Sys.getenv("AWS_POSTGRES_DRV")
AWS_POSTGRES_DBNAME = Sys.getenv("AWS_POSTGRES_DBNAME")
AWS_POSTGRES_HOST = Sys.getenv("AWS_POSTGRES_HOST")
AWS_POSTGRES_PORT = Sys.getenv("AWS_POSTGRES_PORT")
AWS_POSTGRES_USER = Sys.getenv("AWS_POSTGRES_USER")
AWS_POSTGRES_PASSWORD = Sys.getenv("AWS_POSTGRES_PASSWORD")

aws_con <- dbConnect(drv = AWS_POSTGRES_DRV,
                     dbname = AWS_POSTGRES_DBNAME, 
                     host = AWS_POSTGRES_HOST,
                     port = AWS_POSTGRES_PORT, 
                     user = AWS_POSTGRES_USER, 
                     password = AWS_POSTGRES_PASSWORD)
