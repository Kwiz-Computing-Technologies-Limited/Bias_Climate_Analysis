options(timeout = max(10000, getOption("timeout")))
if (!"occAssess" %in% installed.packages()) devtools::install_github("https://github.com/robboyd/occAssess")
library(occAssess)
library(rgbif)
library(readr)
library(rdrop2)
library(tidyverse)

# connect to remote postgres from terminal using "system2() function in R
#system2(command = "psql", args = c("-h", "postgres.cl0erzvvnfux.ap-northeast-1.rds.amazonaws.com", "-p", 5432, "-d", "postgres", "-U"," kwizera_jvk", "-W"))
# <https://www.timescale.com/blog/how-to-install-psql-on-mac-ubuntu-debian-windows/>
#  psql -h "postgres.cl0erzvvnfux.ap-northeast-1.rds.amazonaws.com" -p 5432 -d postgres -U kwizera_jvk -W
# copy data directly from url to database from terminal e.g for USA native range clean data:
# CREATE TABLE usa_native_clean ("...1" INT, species VARCHAR(256), taxonKey INT, acceptedScientificName VARCHAR(256), speciesKey INT, continent VARCHAR(256), decimalLongitude DECIMAL(15, 10), decimalLatitude DECIMAL(15, 10), basisOfRecord VARCHAR(256), coordinatePrecision BOOL, coordinateUncertaintyInMeters INT, stateProvince BOOL, year INT, month INT, day INT, countryCode VARCHAR(256), gbifID DECIMAL(256, 128), occurrenceID VARCHAR(256), verbatimLocality BOOL, nc VARCHAR(256), continent_updated VARCHAR(256));
#\copy usa_native_clean FROM PROGRAM 'Curl "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAD19fTySKPG1xzpeLvC2ujza/USA/USA-native-clean.csv?dl=1"' CSV HEADER;
# \insert INTO usa_native_clean ("species", "decimalLongitude", "decimalLatitude", "month", "year", "coordinateUncertaintyInMeters") SELECT "species", "decimalLongitude", "decimalLatitude", "month", "year", "coordinateUncertaintyInMeters" FROM PROGRAM 'Curl "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAD19fTySKPG1xzpeLvC2ujza/USA/USA-native-clean.csv?dl=1"';
#quit database with \q


# get country codes list
# available_country_names = sort(unique(ggplot2::map_data("world")$region))


# connect DB
source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")

# bias analysis function
Bias_assessment_function = function(db_table, con = aws_con, periods_length = 50) {
  
  x = dbGetQuery(aws_con, paste("SELECT COUNT(*) FROM", db_table))$count
  
  if(((x/1000000) - floor(x/1000000)) == 0){
    paste("data upload for", db_table, "is Incomplete. Complete upload and retry")
  }
  
  # get habitat
  if(grepl("mdg", db_table)){
    country = "Madagascar"
  } else if(grepl("usa", db_table)) {
    country = "USA"
  } else if(grepl("gbr", db_table)){
    country = "UK"
  } else if(grepl("irl", db_table)){
    country = "Ireland"
  } else if(grepl("aus", db_table)){
    country = "Australia"
  } else if(grepl("zaf", db_table)){
    country = "South Africa"
  } else if(grepl("jpn", db_table)){
    country = "Japan"
  } else if(grepl("nzl", db_table)){
    country = "New Zealand"
  } 
  
  
  # periods to split data on
  min_period = ((dbGetQuery(aws_con, paste("SELECT MIN(year) AS min_period FROM", db_table))$min_period /10) |> floor()) * 10
  max_period = dbGetQuery(aws_con, paste("SELECT MAX(year) AS max_period FROM", db_table))$max_period
  n_periods = c(seq(min_period, max_period, periods_length), max_period)
  
  periods = list()
  for (i in 1:(length(n_periods) - 1)) {
    periods[[i]] = seq(from = n_periods[i]+1, to = n_periods[i+1], by = 1)
  }
  
  
  # add backbone record to database if not present
  if(!(paste0(db_table, "_backbone_order") %in% dbListTables(conn = aws_con))){
    
    # get the "name_backbone"/grouping variable ("Order") from GBIF
    paste("Fetching species name backbone for", db_table) |> print()
    family = sapply((dbGetQuery(aws_con, paste("SELECT DISTINCT species AS species FROM", db_table))$species |>
                       na.omit()),
                    function(x) name_backbone(name = x, kingdom = "plants")["order"], simplify = TRUE)
    paste("Fetch complete! Transforming data ...") |> print()
    
    ## Bind the rows
    aa<-family %>% bind_rows()
    aaa=as.data.frame(t(aa))
    aaa$species<-rownames(aaa)
    aaa$species<-gsub(aaa$species,pattern = ".order",replacement = "")
    rownames(aaa) <- NULL
    
    paste("uploading species name backbone for", db_table, "to DB") |> print()
    dbWriteTable(aws_con, paste0(db_table, "_backbone_order"), aaa)
    dbSendQuery(aws_con, paste('ALTER TABLE', paste0(db_table, "_backbone_order"), 'DROP COLUMN "row.names"'))
  }
  
  
  # merge backbone record to db_table 
  paste("Merging species name backbone to", db_table, "data") |> print()
  dbSendQuery(aws_con, paste('CREATE TABLE', paste0(db_table, '_backbone_merged'), 'AS SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_order'), 'USING (species)'))
  
  # get number of records in each year.
  paste("Merge complete! Fetching number of records in each year for", db_table, "...") |> print()
  nRec <- assessRecordNumber(dat = dbGetQuery(aws_con, paste('SELECT * FROM', paste0(db_table, '_backbone_merged'), 'WHERE "V1" IS NOT NULL')),
                             periods = periods,
                             species = "species",
                             y = "decimalLatitude",
                             x = "decimalLongitude",
                             year = "year", 
                             spatialUncertainty = "coordinateUncertaintyInMeters",
                             identifier = "V1",
                             normalize = FALSE)
  
  
  # get number of species recorded in each year
  paste("Fetch complete! Fetching number of species in each year from", db_table, "...") |> print()
  nSpec <- assessSpeciesNumber(dat = dbGetQuery(aws_con, paste('SELECT * FROM', paste0(db_table, '_backbone_merged'), 'WHERE "V1" IS NOT NULL')),
                               periods = periods,
                               species = "species",
                               y = "decimalLatitude",
                               x = "decimalLongitude",
                               year = "year", 
                               spatialUncertainty = "coordinateUncertaintyInMeters",
                               identifier = "V1",
                               normalize = FALSE)
  
  
  # get "the proportion (or counts) of records identified to species level over time."
  paste("Fetch complete! Fetching proportion (or counts) of records identified to species level over time from", db_table, "...") |> print()
  propID <- assessSpeciesID(dat = dbGetQuery(aws_con, paste('SELECT * FROM', paste0(db_table, '_backbone_merged'), 'WHERE "V1" IS NOT NULL')),
                            periods = periods,
                            type = "proportion",
                            species = "species",
                            y = "decimalLatitude",
                            x = "decimalLongitude",
                            year = "year", 
                            spatialUncertainty = "coordinateUncertaintyInMeters",
                            identifier = "V1")
  
  
  # get "proportionality of species observed range sizes and number of records."
  paste("Fetch complete! Fetching taxonomic bias from", db_table, "...") |> print()
  taxBias <- assessRarityBias(dat = dbGetQuery(aws_con, paste('SELECT * FROM', paste0(db_table, '_backbone_merged'), 'WHERE "V1" IS NOT NULL')),
                              periods = periods,
                              res = 0.5,
                              prevPerPeriod = FALSE,
                              species = "species",
                              y = "decimalLatitude",
                              x = "decimalLongitude",
                              year = "year", 
                              spatialUncertainty = "coordinateUncertaintyInMeters",
                              identifier = "V1")
  
  
  # grids and then maps species occurrence data
  paste("Fetch complete! mapping species occurence from", db_table, "...") |> print()
  maps <- assessSpatialCov(dat = dbGetQuery(aws_con, paste('SELECT * FROM', paste0(db_table, '_backbone_merged'), 'WHERE "V1" IS NOT NULL')),
                           periods = periods,
                           res = 0.5,
                           logCount = TRUE,
                           countries = c(country),
                           species = "species",
                           y = "decimalLatitude",
                           x = "decimalLongitude",
                           year = "year", 
                           spatialUncertainty = "coordinateUncertaintyInMeters",
                           identifier = "V1")
  
  
  # list of results objects
  paste("Fetch complete! Saving", db_table, "bias results to RDS file") |> print()
  results = list(
    periods = periods,
    dataset = db_table,
    assessRecordNumber_output = nRec,
    assessSpeciesNumber_output = nSpec,
    assessSpeciesID_output = propID,
    assessRarityBias_output = taxBias,
    assessSpatialCov_output = maps
  )
  
  saveRDS(results, file = paste(db_table, "periods_length", periods_length, "bias_output", sep = "_"))
  paste("Bias analysis for", db_table, "complete!") |> print()
  
}



