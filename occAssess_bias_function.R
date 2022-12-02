options(timeout = max(10000, getOption("timeout")))
if (!"occAssess" %in% installed.packages()) devtools::install_github("https://github.com/robboyd/occAssess")
library(occAssess)
library(rgbif)
library(readr)
library(rdrop2)
library(tidyverse)
library(geodata)
library(terra)
library(ggspatial)
library(ggplot2)

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




# bias analysis function
Bias_assessment_function = function(db_table, con = aws_con, periods_length = 10) {
  # connect DB
  source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
  
  directory_files = list.files("~/Desktop/Documents/GitHub/bias assessment/13.  bias assessment results")

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
  
  list_tables = dbListTables(conn = aws_con)
  source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
  
  periods = list()
  for (i in 1:(length(n_periods) - 1)) {
    periods[[i]] = seq(from = n_periods[i]+1, to = n_periods[i+1], by = 1)
  }
  
  # add backbone record to database if not present
  if(!(paste0(db_table, "_backbone_family") %in% list_tables)){
    
    # get the "name_backbone"/grouping variable ("family") from GBIF
    source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetching species name backbone for", db_table) |> print()
    family = sapply((dbGetQuery(aws_con, paste("SELECT DISTINCT species AS species FROM", db_table))$species |>
                       na.omit()),
                    function(x) name_backbone(name = x, kingdom = "plants"), simplify = TRUE) |> bind_rows() 
    
    aaa = family |>
      dplyr::select(species, family)
    
    paste("Fetch complete! Transforming data ...") |> print()
    
    ## Bind the rows
    rownames(aaa) <- NULL
    
    paste("uploading species name backbone for", db_table, "to DB") |> print()
    
    source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    dbWriteTable(aws_con, paste0(db_table, "_backbone_family"), aaa)
    dbSendQuery(aws_con, paste('ALTER TABLE', paste0(db_table, "_backbone_family"), 'DROP COLUMN "row.names"'))
    source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
  }
  
  
  
  if(!(paste(db_table, "periods_length", periods_length, "assessRecordNumber_output.RDS", sep = "_") %in% directory_files)){
    # get number of records in each year.
    
    source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetching number of records in each year for", db_table, "...") |> print()
    nRec <- assessRecordNumber(dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
                               periods = periods,
                               species = "species",
                               y = "decimalLatitude",
                               x = "decimalLongitude",
                               year = "year", 
                               spatialUncertainty = "coordinateUncertaintyInMeters",
                               identifier = "family",
                               normalize = FALSE)
    
   
    saveRDS(nRec, file = paste(db_table, "periods_length", periods_length, "assessRecordNumber_output.RDS", sep = "_"))
    source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
  }
  
  
  if(!(paste(db_table, "periods_length", periods_length, "assessSpeciesNumber_output.RDS", sep = "_") %in% directory_files)){
    # get number of species recorded in each year
    
    source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetch complete! Fetching number of species in each year from", db_table, "...") |> print()
    nSpec <- assessSpeciesNumber(dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
                                 periods = periods,
                                 species = "species",
                                 y = "decimalLatitude",
                                 x = "decimalLongitude",
                                 year = "year", 
                                 spatialUncertainty = "coordinateUncertaintyInMeters",
                                 identifier = "family",
                                 normalize = FALSE)
    
    saveRDS(nSpec, file = paste(db_table, "periods_length", periods_length, "assessSpeciesNumber_output.RDS", sep = "_"))
    source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
  }
  
  if(!(paste(db_table, "periods_length", periods_length, "assessRarityBias_output.RDS", sep = "_") %in% directory_files)){
    # get rarity
    
    source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetch complete! Fetching rarity from", db_table, "...") |> print()
    
    source("~/Desktop/Documents/GitHub/bias assessment/assessRarityBias_modified.R")
    taxBias <- assessRarityBias_modified(dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
                                periods = periods,
                                res = 0.5,
                                prevPerPeriod = FALSE,
                                species = "species",
                                y = "decimalLatitude",
                                x = "decimalLongitude",
                                year = "year", 
                                spatialUncertainty = "coordinateUncertaintyInMeters",
                                identifier = "family")
    
    saveRDS(taxBias, file = paste(db_table, "periods_length", periods_length, "assessRarityBias_output.RDS", sep = "_"))
    source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
  }
  
  
  if(!(paste0(db_table, "_periods_length_", periods_length, "_assessSpatialBias_output.RDS") %in% directory_files)){
    # get spatial bias
    mask = geodata::worldclim_country(country = country, level = 0, res = 10, var = "tavg",
                                             path = paste0("~/Desktop/Documents/GitHub/bias assessment/", 
                                                           substr(db_table, 1, 3)))
    mask2 = raster::brick(mask[[1]])
    
    source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetch complete! Fetching spatial bias from", db_table, "...") |> print()
    spatBias <- assessSpatialBias(dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
                                  species = "species",
                                  y = "decimalLatitude",
                                  x = "decimalLongitude",
                                  year = "year", 
                                  spatialUncertainty = "coordinateUncertaintyInMeters",
                                  identifier = "family",
                                  periods = periods,
                                  mask = mask2,
                                  nSamps = 1,
                                  degrade = TRUE)
    
    spatBias$data = spatBias$data |> 
      dplyr::mutate(Period = as.integer(Period))
    
    spatBias$data |>
      ggplot(mapping = aes(x = Period, y = mean, col = identifier)) +
      geom_line() + geom_point() + theme_bw() +
      ylab("Nearest Neighbour Index") + xlab("Period")
    
    
    saveRDS(spatBias, file = paste0(db_table, "_periods_length_", periods_length, "_assessSpatialBias_output.RDS"))
    source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
  }
  
  
  
  
  if(!(paste(db_table, "periods_length", periods_length, "assessSpatialCov_output.RDS", sep = "_") %in% directory_files)){
    # grid and map species occurrence data
    
    source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetch complete! mapping species occurence from", db_table, "...") |> print()
    maps <- assessSpatialCov(dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
                             periods = periods,
                             res = 0.5,
                             logCount = TRUE,
                             countries = c(country),
                             species = "species",
                             y = "decimalLatitude",
                             x = "decimalLongitude",
                             year = "year", 
                             spatialUncertainty = "coordinateUncertaintyInMeters",
                             identifier = "family")
    
    
    saveRDS(maps, file = paste(db_table, "periods_length", periods_length, "assessSpatialCov_output.RDS", sep = "_"))
    source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
  }
  
  
  
  if(!(paste(db_table, "periods_length", periods_length, "periods_output.RDS", sep = "_") %in% directory_files)){
    saveRDS(periods, file = paste(db_table, "periods_length", periods_length, "periods_output.RDS", sep = "_"))
  }
  
  
  
    if(!(paste0(db_table, "_periods_length_", periods_length, "_assessEnvBias_output.RDS") %in% directory_files)){
      paste("Fetch complete! Fetching environmental data for", substr(db_table, 1, 3), "...") |> print()
    # get spatial bias
    env_data = geodata::worldclim_country(country = country, res = 2.5, var = "bio",
                                      path = paste0("~/Desktop/Documents/GitHub/bias assessment/", 
                                                    substr(db_table, 1, 3))) |> raster::stack()
    
    source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetch complete! Fetching environment bias from", db_table, "...") |> print()
    envBias <- assessEnvBias(dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL AND year IS NOT NULL')),
                             species = "species",
                             y = "decimalLatitude",
                             x = "decimalLongitude",
                             year = "year", 
                             spatialUncertainty = "coordinateUncertaintyInMeters",
                             identifier = "family",
                             periods = periods,
                             envDat = terra::extract(env_data, dbGetQuery(aws_con, paste('SELECT "decimalLongitude", "decimalLatitude" FROM (SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL AND year IS NOT NULL) n1'))),
                             backgroundEnvDat = raster::sampleRandom(env_data, size = 100000, xy = F))
    
    saveRDS(envBias$plot, file = paste0(db_table, "_periods_length_", periods_length, "_assessEnvBias_output.RDS"))
    source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
  }

  paste("Bias analysis for", db_table, "complete!") |> print()
  paste("Updating", db_table, ".RDS files to GH") |> print()
  
  
  directory_files = list.files("~/Desktop/Documents/GitHub/bias assessment/13.  bias assessment results")
  directory_files2 = directory_files[-grep(".Rmd", directory_files)]
  
  # auto commit results if internet is connected 
  git2r::init()
  
  ## stage changes
  unstaged = git2r::status()$unstaged |> unlist()
  for (i in 1:length(unstaged)) {
    paste("staging", as.character(unstaged[[i]]), "for upload to GH") |> print()
    git2r::add(path = as.character(unstaged[[i]]))
  }
  
  # stage un-tracked files and uncommitted
  tracked = git2r::ls_tree()[, c("path", "name")] |>
    mutate(file_path = paste0(path, name)) |> 
    select(file_path)
  staged = git2r::status()$staged |> unlist()
  
  for (i in 1:length(directory_files2)) {
    if(!(paste0("13.  bias assessment results/", directory_files2[i]) %in% tracked) &
       !(paste0("13.  bias assessment results/", directory_files2[i]) %in% staged)){
      paste("staging", directory_files2[i], "for upload to GH") |> print()
      git2r::add(path =  paste0("13.  bias assessment results/", directory_files2[i]))
    }
  }
  
  paste(".RDs file staged for commit:") |> print()
  git2r::status()
  git2r::commit(message = "Committed result .rds files")
  git2r::push(refspec = "feature", name = git2r::remote_url())
  
  gc()
}




