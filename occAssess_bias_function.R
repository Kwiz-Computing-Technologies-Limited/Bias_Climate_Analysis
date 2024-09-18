options(timeout = getOption("timeout")^5)
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
library(tidyverse)
if (!"occAssess" %in% installed.packages()) devtools::install_github("https://github.com/robboyd/occAssess")
library(occAssess)
if (!"rgbif" %in% installed.packages()) install.packages("rgbif")
library(rgbif)
if (!"readr" %in% installed.packages()) install.packages("readr")
library(readr)
# if (!"rdrop2" %in% installed.packages()) install.packages("rdrop2")
# library(rdrop2)

if (!"geodata" %in% installed.packages()) install.packages("geodata")
library(geodata)
if (!"terra" %in% installed.packages()) install.packages("terra")
library(terra)
if (!"ggspatial" %in% installed.packages()) install.packages("ggspatial")
library(ggspatial)
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
library(ggplot2)
if (!"here" %in% installed.packages()) install.packages("here")
library(here)

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
Bias_assessment_function = function(db_table, 
                                    # con = aws_con, 
                                    periods_length = 10) {
  
  # create folder for the location's fetched environment files
  dir.create(here(substr(db_table, 1, 3)))
  
  # download occurrence files
  source(here("occurrence_download_template.R"))
  value = get_data(dataset = occ_data[occ_data$dataset == db_table, ]$dataset,
                   drop_url = occ_data[occ_data$dataset == db_table, ]$drop_url) 
  
  # get habitat
  if(grepl("mdg", db_table)){
    country = "Madagascar"
  } else if(grepl("usa", db_table)) {
    country = "US"
  } else if(grepl("gbr", db_table)){
    country = "United Kingdom"
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
  
  dataset = db_table
  paste("creating", dataset, "periods") |> print()
  # periods to split data on
  min_period = floor(min(value$year, na.rm = TRUE) / 10) * 10
  max_period = ceiling(max(value$year, na.rm = TRUE) / 10) * 10
  
  n_periods = c(seq(from = min_period, to = max_period,  by = periods_length))
  
  periods = list()
  for (i in 1:(length(n_periods) - 1)) {
    periods[[i]] = seq(from = n_periods[i]+1, to = n_periods[i+1], by = 1)
  }
  
  if(!(here("13.  bias assessment results", db_table) %in% list.files(here("13.  bias assessment results")))){
    dir.create(here("13.  bias assessment results", db_table))
  }
  
  # add backbone record to database if not present
  if(!(paste0(db_table, "_backbone_family.csv") %in% list.files(here("13.  bias assessment results", db_table)))){
    
    options(timeout = getOption("timeout")^5)
    # get the "name_backbone"/grouping variable ("family") from GBIF
    
    paste("Fetching species name backbone for", db_table) |> print()
    
    family = sapply((value$species |> unique() |> na.omit()), 
                    function(x) name_backbone(name = x, kingdom = "plants"), 
                    simplify = FALSE) |> bind_rows()
    
    aaa = family |>
      dplyr::select(species, family)
    
    paste("Fetch name backbone complete! Transforming data ...") |> print()
    
    ## Bind the rows
    rownames(aaa) <- NULL
    dir.create(here("13.  bias assessment results", db_table))
    paste("saving species name backbone for", db_table) |> print()
    readr::write_csv(x = aaa, file = here("13.  bias assessment results", db_table, paste0(db_table, "_backbone_family.csv")))
  }
  
  # merge occurrence and name backbone data sets
  aaa = readr::read_csv(file = here("13.  bias assessment results", db_table, paste0(db_table, "_backbone_family.csv")))
  dat = value |> dplyr::left_join(aaa[!is.na(aaa$family), ], by = "species") |> data.frame()
  
  # remove used R objects from environment
  rm(family, aaa)
  
  ## with family as taxonomic group
  if(!(paste(db_table, "periods_length", periods_length, "assessRecordNumber_output.csv", sep = "_") %in% list.files(here("13.  bias assessment results", db_table)))){
    # get number of records in each year.
    options(timeout = getOption("timeout")^5)
    
    # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetching number of records in each year for", db_table, "...") |> print()
    nRec <- assessRecordNumber(dat = dat[!is.na(dat$family), ],
                               
                               # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
                               periods = periods,
                               species = "species",
                               y = "decimalLatitude",
                               x = "decimalLongitude",
                               year = "year", 
                               spatialUncertainty = "coordinateUncertaintyInMeters",
                               identifier = "family",
                               normalize = FALSE)
    
    
    readr::write_csv(nRec$data, file = here("13.  bias assessment results", db_table, 
                                            paste(db_table, "periods_length", periods_length, 
                                                  "assessRecordNumber_output.csv", sep = "_")))
    # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    
    # remove used R objects from environment
    rm(nRec)
  }
  
  
  ## With species as taxonomic group
  if(!(paste(db_table, "periods_length", periods_length, "by_species_assessRecordNumber_output.csv", sep = "_") %in% list.files(here("13.  bias assessment results", db_table)))){
    # get number of records in each year.
    options(timeout = getOption("timeout")^5)
    
    # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetching number of records in each year for", db_table, "...") |> print()
    nRec2 <- assessRecordNumber(dat = dat[!is.na(dat$species), ],
                                
                                # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
                                periods = periods,
                                species = "species",
                                y = "decimalLatitude",
                                x = "decimalLongitude",
                                year = "year", 
                                spatialUncertainty = "coordinateUncertaintyInMeters",
                                identifier = "species",
                                normalize = FALSE)
    
    
    readr::write_csv(nRec2$data, file = here("13.  bias assessment results", db_table, 
                                             paste(db_table, "periods_length", periods_length, 
                                                   "by_species_assessRecordNumber_output.csv", sep = "_")))
    # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    
    # remove used R objects from environment
    rm(nRec2)
  }
  
  
  ## with family as taxonomic group
  if(!(paste(db_table, "periods_length", periods_length, "assessSpeciesNumber_output.csv", sep = "_") %in% list.files(here("13.  bias assessment results", db_table)))){
    # get number of species recorded in each year
    options(timeout = getOption("timeout")^5)
    
    # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetch number of records complete! Fetching number of species in each year from", db_table, "...") |> print()
    nSpec <- assessSpeciesNumber(dat = dat[!is.na(dat$family), ],
                                 
                                 # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
                                 periods = periods,
                                 species = "species",
                                 y = "decimalLatitude",
                                 x = "decimalLongitude",
                                 year = "year", 
                                 spatialUncertainty = "coordinateUncertaintyInMeters",
                                 identifier = "family",
                                 normalize = FALSE)
    
    readr::write_csv(nSpec$data, file = here("13.  bias assessment results", db_table, 
                                             paste(db_table, "periods_length", periods_length,
                                                   "assessSpeciesNumber_output.csv", sep = "_")))
    # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    
    # remove used R objects from environment
    rm(nSpec)
  }
  
  ## with species as taxonomic group
  if(!(paste(db_table, "periods_length", periods_length, "by_species_assessSpeciesNumber_output.csv", sep = "_") %in% list.files(here("13.  bias assessment results", db_table)))){
    # get number of species recorded in each year
    options(timeout = getOption("timeout")^5)
    
    # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetch number of records complete! Fetching number of species in each year from", db_table, "...") |> print()
    nSpec2 <- assessSpeciesNumber(dat = dat[!is.na(dat$species), ],
                                  
                                  # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
                                  periods = periods,
                                  species = "species",
                                  y = "decimalLatitude",
                                  x = "decimalLongitude",
                                  year = "year", 
                                  spatialUncertainty = "coordinateUncertaintyInMeters",
                                  identifier = "species",
                                  normalize = FALSE)
    
    readr::write_csv(nSpec2$data, file = here("13.  bias assessment results", db_table, 
                                              paste(db_table, "periods_length", periods_length,
                                                    "by_species_assessSpeciesNumber_output.csv", sep = "_")))
    # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    
    # remove used R objects from environment
    rm(nSpec2)
  }
  
  
  ## with family as taxonomic group
  if(!(paste(db_table, "periods_length", periods_length, "assessRarityBias_output.csv", sep = "_") %in% list.files(here("13.  bias assessment results", db_table)))){
    # get rarity
    options(timeout = getOption("timeout")^5)
    
    # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetch number of species complete! Fetching rarity index from", db_table, "...") |> print()
    
    source("assessRarityBias_modified.R")
    taxBias <- assessRarityBias_modified(dat = dat[!is.na(dat$family), ],
                                         
                                         # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
                                         periods = periods,
                                         res = 0.5,
                                         prevPerPeriod = FALSE,
                                         species = "species",
                                         y = "decimalLatitude",
                                         x = "decimalLongitude",
                                         year = "year", 
                                         spatialUncertainty = "coordinateUncertaintyInMeters",
                                         identifier = "family")
    
    readr::write_csv(taxBias$data, file = here("13.  bias assessment results", db_table,
                                               paste(db_table, "periods_length", periods_length,
                                                     "assessRarityBias_output.csv", sep = "_")))
    # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    
    # remove used R objects from environment
    rm(taxBias)
  }
  
  ## with species as taxonomic group
  if(!(paste(db_table, "periods_length", periods_length, "by_species_assessRarityBias_output.csv", sep = "_") %in% list.files(here("13.  bias assessment results", db_table)))){
    # get rarity
    options(timeout = getOption("timeout")^5)
    
    # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetch number of species complete! Fetching rarity index from", db_table, "...") |> print()
    
    source("assessRarityBias_modified.R")
    taxBias2 <- assessRarityBias_modified(dat = dat[!is.na(dat$species), ],
                                          
                                          # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
                                          periods = periods,
                                          res = 0.5,
                                          prevPerPeriod = FALSE,
                                          species = "species",
                                          y = "decimalLatitude",
                                          x = "decimalLongitude",
                                          year = "year", 
                                          spatialUncertainty = "coordinateUncertaintyInMeters",
                                          identifier = "species")
    
    readr::write_csv(taxBias2$data, file = here("13.  bias assessment results", db_table,
                                                paste(db_table, "periods_length", periods_length,
                                                      "by_species_assessRarityBias_output.csv", sep = "_")))
    # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    
    
    # remove used R objects from environment
    rm(taxBias2)
  }
  
  paste("Bias analysis for", db_table, "complete!") |> print()
  gc()
}


