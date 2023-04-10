# libraries
if(!("spThin" %in% installed.packages())){
  install.packages("spThin")
}
library(spThin)

if(!("purrr" %in% installed.packages())){
  install.packages("purrr")
}
library(purrr)

if(!("broom" %in% installed.packages())){
  install.packages("broom")
}
library(broom)

if(!("tibble" %in% installed.packages())){
  install.packages("tibble")
}
library(tibble)

if(!("dplyr" %in% installed.packages())){
  install.packages("dplyr")
}
library(dplyr)

if(!("readr" %in% installed.packages())){
  install.packages("readr")
}
library(readr)

if(!("here" %in% installed.packages())){
  install.packages("here")
}
library(here)




# bias correction function
bias_correction = function(db_table, min_distance) {
  
# create folder to hold the thinned datasets for the region
dir.create(here("bias_correction", substr(db_table, 1, 3)))
  
  
source(here("occurrence_download_template.R"))
  
# get raw data
raw_data = get_data(dataset = occ_data[occ_data$dataset == db_table, ]$dataset,
                        drop_url = occ_data[occ_data$dataset == db_table, ]$drop_url)

  
# thin each species independently
  d = min_distance
      
  paste("Spatially Thinning species occurrences in", db_table, "at", min_distance, "minimum distance") # |> print()
  
  # source modified thinning function
  source(here("bias_correction", "thin_modified.R"))
  
  # remove species with very large frequencies ( > _________) due to memory usage (prevent crashing with 128GB RAM available)
  species_freqs = table(raw_data$species) |> sort()
  species_order = species_freqs[order(species_freqs)] |> names()

  
  ## ***large_sets = species_freqs[]
  #raw_data = raw_data |> filter(species %in% large_sets)
      
      # thin data species by species starting from species with least frequency (smallest dataset)
  thinned_data = split.data.frame(raw_data, raw_data$species)[species_order] |>
    purrr::map(~thin_modified(loc.data = .x,
                             lat.col = "decimalLatitude", long.col = "decimalLongitude",
                             spec.col = "species", thin.par = min_distance, reps = 1,
                             locs.thinned.list.return = TRUE,
                             write.files = TRUE,
                             out.dir = here("bias_correction", substr(db_table, 1, 3)),
                             out.base = NULL,
                             max.files = 1)) 
  ## thinned_data2 = list()
      
  ## paste("Joining Spatially Thinned Species occurrence data") # |> print()
  ## for (i in 1:length(names(thinned_data))) {
  ##   thinned_data2[[i]] = dplyr::mutate(thinned_data[[i]][[1]], species = names(thinned_data)[i])
  ## }
  
  ## thinned_data2 = as.data.frame(do.call(rbind, thinned_data2)) |> 
  ## rename(decimalLatitude = Latitude, decimalLongitude = Longitude, species = species) |>
  ## left_join(raw_data) |>
  ## select(occurrenceID, decimalLongitude, decimalLatitude, species, coordinateUncertaintyInMeters, countryCode, year, month, day)
  
  ##  thinned_data2 = thinned_data2[!duplicated(thinned_data2[, "occurrenceID"]), ]
      
  paste("Thinning", db_table, "occurrence data to local file") # |> print()
  


  
  ## spatially thinning by family
  
## if(is.logical(list.files(here("bias_correction", substr(db_table, 1, 3)))) | !grepl("_km_spatial_thinning_by_family.csv", list.files(here("bias_correction", substr(db_table, 1, 3))))) {
##  name_backbone = readr::read_csv(here("13.  bias assessment results", db_table, paste0(db_table, "_backbone_family.csv")))
##  data = raw_data |> dplyr::left_join(name_backbone[!is.na(name_backbone$family), ], by = "species") |> data.frame()
##  
##  # thin data family by family
##  thinned_data3 =  split.data.frame(data, data$family) |>
##    purrr::map(~spThin::thin(loc.data = .x,
##                             lat.col = "decimalLatitude", long.col = "decimalLongitude",
##                             spec.col = "family", thin.par = min_distance, reps = 10,
##                             locs.thinned.list.return = TRUE,
##                             write.files = FALSE,
##                              max.files = 1)) 
##   thinned_data4 = list()
##   
##  paste("Joining Spatially Thinned families occurrence data") # |> print()
##  for (i in 1:length(names(thinned_data3))) {
##    thinned_data4[[i]] = dplyr::mutate(thinned_data3[[i]][[1]], family = names(thinned_data3)[i])
##  }
##  thinned_data4 = as.data.frame(do.call(rbind, thinned_data4)) |> 
##    rename(decimalLatitude = Latitude, decimalLongitude = Longitude, family = family) |>
##    left_join(raw_data)|>
##    select(occurrenceID, decimalLongitude, decimalLatitude, family, coordinateUncertaintyInMeters, countryCode, year, month, day)
##  
##  thinned_data4 = thinned_data4[!duplicated(thinned_data4[, "occurrenceID"]), ]
##  
##  paste("Saving Thinned by family", db_table, "occurrence data to local file") # |> print()
##  write_csv(thinned_data4, file = here("bias_correction", substr(db_table, 1, 3), paste0(db_table, "_", d, "_km_spatial_thinning_by_family.csv")))
## }
  
## file.remove(here("bias_correction", substr(db_table, 1, 3), "temp.csv"))
  
}








