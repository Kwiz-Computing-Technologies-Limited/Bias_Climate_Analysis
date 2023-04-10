options(timeout = getOption("timeout")^5)
if (!"occAssess" %in% installed.packages()) devtools::install_github("https://github.com/robboyd/occAssess")
library(occAssess)
if (!"rgbif" %in% installed.packages()) install.packages("rgbif")
library(rgbif)
if (!"readr" %in% installed.packages()) install.packages("readr")
library(readr)
if (!"rdrop2" %in% installed.packages()) install.packages("rdrop2")
library(rdrop2)
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
library(tidyverse)
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
  # connect DB
  # source(here("connect_db.R"))

  # if(!("googledrive" %in% installed.packages())){
  #   install.packages("googledrive")
  # }
  # library(googledrive)
  
  # Authenticate google
  # drive_auth(email = "biasoutputfiles@gmail.com")
  
  # Already uploaded files
  # uploaded = drive_ls()$name

  # bias assessment results stored locally
  # directory_files = list.files(here("13.  bias assessment results"))
  
  # x = dbGetQuery(aws_con, paste("SELECT COUNT(*) FROM", db_table))$count
  
  # if(((x/1000000) - floor(x/1000000)) == 0){
  #  paste("data upload for", db_table, "is Incomplete. Completing upload before retrying")
    
  # directory_files2 = list.files("~/Desktop/Documents/GitHub/bias assessment/")
  #  for (i in 1:length(directory_files2)) {
  #    if((directory_files2[i] == substr(db_table, 1, 3))) {
  #      path = paste0("~/Desktop/Documents/GitHub/bias assessment/", directory_files2[i],
  #                    "/", db_table, ".R")
  #    }
  #  }
    
  #  source(path)
  #  paste("data upload for", db_table, "is now complete") |> print()
  #}
  
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
  
  # min_period = ((dbGetQuery(aws_con, paste("SELECT MIN(year) AS min_period FROM", db_table))$min_period /10) |> floor()) * 10
  # max_period = dbGetQuery(aws_con, paste("SELECT MAX(year) AS max_period FROM", db_table))$max_period
  
  n_periods = c(seq(from = min_period, to = max_period,  by = periods_length))
  
  # list_tables = dbListTables(conn = aws_con)
  # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
  
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
    
    #source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetching species name backbone for", db_table) |> print()
    
    # family = sapply((dbGetQuery(aws_con, paste("SELECT DISTINCT species AS species FROM", db_table))$species |>
    #                   na.omit()),
    #                function(x) name_backbone(name = x, kingdom = "plants"), simplify = TRUE) |> bind_rows() 
    
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
    
    
    # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    # dbWriteTable(aws_con, paste0(db_table, "_backbone_family"), aaa)
    # dbSendQuery(aws_con, paste('ALTER TABLE', paste0(db_table, "_backbone_family"), 'DROP COLUMN "row.names"'))
    # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    
    
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
  
  
    ## with family as taxonomic group
  if(!(paste0(db_table, "_periods_length_", periods_length, "_assessSpatialBias_output.csv") %in% list.files(here("13.  bias assessment results", db_table)))){
    # get spatial bias
    gc()
    options(timeout = getOption("timeout")^5)
    
    paste("Fetch rarity index complete! Fetching environment mask for spatial bias for", substr(db_table, 1, 3), "...") |> print()
    mask = geodata::worldclim_country(country = country, level = 0, res = 2.5, var = "tavg",
                                      path = here(substr(db_table, 1, 3)))
    mask2 = raster::brick(mask[[1]])
    
    options(timeout = getOption("timeout")^5)
    # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    
    paste("Fetch environment mask complete! Fetching spatial bias from", db_table, "...") |> print()
    
    spatBias <- assessSpatialBias(dat = dat[!is.na(dat$family), ],
                                  # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
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
    
    # spatBias$data |>
    # ggplot(mapping = aes(x = Period, y = mean, col = identifier)) +
    # geom_line() + geom_point() + theme_bw() +
    # ylab("Nearest Neighbour Index") + xlab("Period")
    
    
    readr::write_csv(spatBias$data, file = here("13.  bias assessment results", db_table,
                                                paste0(db_table, "_periods_length_", periods_length, 
                                                       "_assessSpatialBias_output.csv")))
    # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    
    # remove used R objects from environment
    rm(spatBias, mask, mask2)
  }
    
    ## with species as the taxonomic group
    # if(!(paste0(db_table, "_periods_length_", periods_length, "_by_species__assessSpatialBias_output.csv") %in% list.files(here("13.  bias assessment results", db_table)))){
    #   # get spatial bias
    #   gc()
    #   options(timeout = getOption("timeout")^5)
    #   
    #   paste("Fetch rarity index complete! Fetching environment mask for spatial bias for", substr(db_table, 1, 3), "...") |> print()
    #   mask = geodata::worldclim_country(country = country, level = 0, res = 2.5, var = "tavg",
    #                                     path = here(substr(db_table, 1, 3)))
    #   mask2 = raster::brick(mask[[1]])
    #   
    #   options(timeout = getOption("timeout")^5)
    #   # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    #   paste("Fetch environment mask complete! Fetching spatial bias from", db_table, "...") |> print()
    #   spatBias2 <- assessSpatialBias(dat = dat[!is.na(dat$species), ],
    #                                 
    #                                 # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
    #                                 species = "species",
    #                                 y = "decimalLatitude",
    #                                 x = "decimalLongitude",
    #                                 year = "year", 
    #                                 spatialUncertainty = "coordinateUncertaintyInMeters",
    #                                 identifier = "species",
    #                                 periods = periods,
    #                                 mask = mask2,
    #                                 nSamps = 1,
    #                                 degrade = TRUE)
    #   
    #   spatBias2$data = spatBias2$data |> 
    #     dplyr::mutate(Period = as.integer(Period))
    #   
    #   # spatBias$data |>
    #   # ggplot(mapping = aes(x = Period, y = mean, col = identifier)) +
    #   # geom_line() + geom_point() + theme_bw() +
    #   # ylab("Nearest Neighbour Index") + xlab("Period")
    #   
    #   
    #   readr::write_csv(spatBias2$data, file = here("13.  bias assessment results", db_table,
    #                                               paste0(db_table, "_periods_length_", periods_length, 
    #                                                      "_by_species__assessSpatialBias_output.csv")))
    #   # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    #   
    #   
    #   # remove used R objects from environment
    #   rm(spatBias2, mask, mask2)
    # }
    
    
  ## with family as the taxonomic group
  dir.create(here("13.  bias assessment results", db_table, "spatial_cov"))  
  if(((grepl(pattern = paste(db_table, "periods_length", periods_length, "by_family_assessSpatialCov_output", sep = "_"), list.files(here("13.  bias assessment results", db_table, "spatial_cov"))) |> sum()) < 1)){
    # grid and map species occurrence data
    options(timeout = getOption("timeout")^5)
    
    # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
    paste("Fetch spatial bias complete! mapping species occurrence from", db_table, "...") |> print()
    maps <- assessSpatialCov(dat = dat[!is.na(dat$family), ],
                             
                             # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
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
    
    
    filenames = stringr::str_c(db_table, "_periods_length_", periods_length, "_by_family_assessSpatialCov_output_",
                               names(maps), ".png")
    
    dir.create(here("13.  bias assessment results", db_table, "spatial_cov"))                           
    purrr::pwalk(list(filenames, maps), ggplot2::ggsave, 
                 path = here("13.  bias assessment results", db_table, "spatial_cov"))
    
    # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
    
    # remove used R objects from environment
    rm(maps)
  }
  
  ## with species as the taxonomic group
    ### if(((grepl(pattern = paste(db_table, "periods_length", periods_length, "by_species_assessSpatialCov_output", sep = "_"), list.files(here("13.  bias assessment results", db_table, "spatial_cov"))) |> sum()) < 1)){
      # grid and map species occurrence data
  ### options(timeout = getOption("timeout")^5)
      
      # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
  ### paste("Fetch spatial bias complete! mapping species occurrence from", db_table, "...") |> print()
  ###  maps <- assessSpatialCov(dat = dat[!is.na(dat$species), ],
                               
                               # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE "family" IS NOT NULL')),
  ###                         periods = periods,
  ###                         res = 0.5,
  ###                          logCount = TRUE,
  ###                          countries = c(country),
  ###                          species = "species",
  ###                          y = "decimalLatitude",
  ###                          x = "decimalLongitude",
  ###                          year = "year", 
  ###                          spatialUncertainty = "coordinateUncertaintyInMeters",
  ###                          identifier = "species")
      
      
  ### filenames = stringr::str_c(db_table, "_periods_length_", periods_length, "_by_species_assessSpatialCov_output_",
  ###                            names(maps), ".png")
      
  ### dir.create(here("13.  bias assessment results", db_table, "spatial_cov"))                           
  ###  purrr::pwalk(list(filenames, maps), ggplot2::ggsave, 
  ###               path = here("13.  bias assessment results", db_table, "spatial_cov"))
      
      # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
      
      # remove used R objects from environment
  ### rm(maps)
  ### }
  
    
  if(!(paste(db_table, "periods_length", periods_length, "periods_output.csv", sep = "_") %in% list.files(here("13.  bias assessment results", db_table)))){
    periods[[length(periods)]] = c(periods[[length(periods)]], rep(NA, (10 - length(periods[[length(periods)]]))))
    periods = periods |> bind_cols()
    names(periods) = 1:ncol(periods)
    readr::write_csv(periods, file = here("13.  bias assessment results", db_table,
                                          paste(db_table, "periods_length", periods_length, 
                                                "periods_output.csv", sep = "_")))
  }
  
  
  # NOTE: the "bio" variable is not present for New Zealand, Japan and USA in world_clim. For that, we fetch 
  # "tmin", "tmax" and "prec" separately and use the "dismo::biovars()" function
  
    
    ## with family as taxonomic group
  if(!(country %in% c("New Zealand"))){
    if(!(paste0(db_table, "_periods_length_", periods_length, "_assessEnvBias_output.csv") %in% list.files(here("13.  bias assessment results", db_table)))){
      paste("Fetch mapping species complete! Fetching environmental data for", substr(db_table, 1, 3), "...") |> print()
      # get spatial bias
      gc()
      
      options(timeout = getOption("timeout")^10)
      paste("Fetching 'bio' data") |> print()
      env_data = geodata::worldclim_country(country = country, res = 5, var = "bio",
                                            path = here(substr(db_table, 1, 3))) |> raster::stack()
      # select the climate variables of interest
      paste("Selecting 8 'bio' variables") |> print()
      env_data2 = env_data[[c(1, 4, 10, 11, 12, 15, 16, 17)]]
      print(nrow(env_data2))
      
      # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
      dat2 = dat[!is.na(dat$family), ]
      print(nrow(dat2))
      env_data3 = terra::extract(env_data2, dat2[, c("decimalLongitude", "decimalLatitude")]
                                 # dbGetQuery(aws_con, paste('SELECT "decimalLongitude", "decimalLatitude" FROM (SELECT * FROM', db_table, 
                                 #                                      'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE 
                                 #                                    "family" IS NOT NULL AND year IS NOT NULL) n1'))
                                 )
      # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
      paste("Fetch environmental data complete! Fetching environment bias from", db_table, "...") |> print()
      print(nrow(env_data3))
      # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
      envBias <- assessEnvBias(dat = dat2,
                               # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE family IS NOT NULL AND year IS NOT NULL')),
                               species = "species",
                               y = "decimalLatitude",
                               x = "decimalLongitude",
                               year = "year", 
                               spatialUncertainty = "coordinateUncertaintyInMeters",
                               identifier = "family",
                               periods = periods,
                               envDat = (env_data3 |> as.data.frame()),
                               backgroundEnvDat = raster::sampleRandom(env_data2, size = 100000, xy = F))
      
      env_bias_data = envBias$data |>
        dplyr::select(Period, identifier, `scores.PC1`, `scores.PC2`, xVar, yVar)
      
      readr::write_csv(env_bias_data, file = here("13.  bias assessment results", db_table,
                                                  paste0(db_table, "_periods_length_", periods_length, "_assessEnvBias_output.csv")))
      # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
      
      
      # remove used R objects from environment
      rm(env_data, env_data2, env_data3, dat2, envBias, env_bias_data)
    } 
  }
    
    ## with species as taxonomic group
    if(!(country %in% c("New Zealand", "USA"))){
      if(!(paste0(db_table, "_periods_length_", periods_length, "_by_species_assessEnvBias_output.csv") %in% list.files(here("13.  bias assessment results", db_table)))){
        paste("Fetch mapping species complete! Fetching environmental data for", substr(db_table, 1, 3), "...") |> print()
        # get spatial bias
        gc()
        
        options(timeout = getOption("timeout")^10)
        paste("Fetching 'bio' data") |> print()
        env_data = geodata::worldclim_country(country = country, res = 5, var = "bio",
                                              path = here(substr(db_table, 1, 3))) |> raster::stack()
        # select the climate variables of interest
        paste("Selecting 8 'bio' variables") |> print()
        env_data2 = env_data[[c(1, 4, 10, 11, 12, 15, 16, 17)]]
        
        # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
        paste("extracting 'bio' data at occurrence coordinates") |> print()
        dat2 = dat[!is.na(dat$species), ]
        env_data3 = terra::extract(env_data2, dat2[, c("decimalLongitude", "decimalLatitude")]
                                   # dbGetQuery(aws_con, paste('SELECT "decimalLongitude", "decimalLatitude" FROM (SELECT * FROM', db_table, 
                                   #                                      'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE 
                                   #                                    "family" IS NOT NULL AND year IS NOT NULL) n1'))
        )
        # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
        paste("Fetch environmental data complete! Fetching environment bias from", db_table, "...") |> print()
        
        # source("~/Desktop/Documents/GitHub/bias assessment/connect_db.R")
        # envBias <- assessEnvBias(dat = dat2,
        #                          
        #                          # dat = dbGetQuery(aws_con, paste('SELECT * FROM', db_table, 'LEFT JOIN', paste0(db_table, '_backbone_family'), 'USING (species) WHERE family IS NOT NULL AND year IS NOT NULL')),
        #                          species = "species",
        #                          y = "decimalLatitude",
        #                          x = "decimalLongitude",
        #                          year = "year", 
        #                          spatialUncertainty = "coordinateUncertaintyInMeters",
        #                          identifier = "species",
        #                          periods = periods,
        #                          envDat = (env_data3 |> as.data.frame()),
        #                          backgroundEnvDat = raster::sampleRandom(env_data2, size = 100000, xy = F))
        
        # env_bias_data = envBias$data |>
        #   dplyr::select(Period, identifier, `scores.PC1`, `scores.PC2`, xVar, yVar)
        
        # readr::write_csv(env_bias_data, file = here("13.  bias assessment results", db_table,
        #                                             paste0(db_table, "_periods_length_", periods_length, "_by_species_assessEnvBias_output.csv")))
        # source("~/Desktop/Documents/GitHub/bias assessment/killing_DB_connections.R")
        
        # remove used R objects from environment
#  rm(env_data, env_data2, env_data3, dat2, envBias, env_bias_data)
      } 
    }
  
  paste("Bias analysis for", db_table, "complete!") |> print()
  
  # paste("Updating", db_table, "files to Drive") |> print()
  # source("~/Desktop/Documents/GitHub/bias assessment/drive_results_upload.R")
  # paste("files updated in Drive ") |> print()
  
  gc()
}


