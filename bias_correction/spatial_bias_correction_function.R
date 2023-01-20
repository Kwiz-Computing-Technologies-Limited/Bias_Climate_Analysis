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





# bias correction function
bias_correction = function(db_table, min_distance) {
  
  # get raw data
  source(here("occurrence_download_template.R"))
  raw_data = get_data(dataset = occ_data[occ_data$dataset == db_table, ]$dataset,
                      drop_url = occ_data[occ_data$dataset == db_table, ]$drop_url)
  
  for (i in 1:length(min_distance)) {
    d = min_distance[i]
    paste("Spatially Thinning species occurrences in", db_table, "at", min_distance[i], "minimum distance") |> print()
    # thin data species by species
    thinned_data =  split.data.frame(raw_data, raw_data$species) |>
      purrr::map(~spThin::thin(loc.data = .x,
                               lat.col = "decimalLatitude", long.col = "decimalLongitude",
                               spec.col = "species", thin.par = min_distance[i], reps = 10,
                               locs.thinned.list.return = TRUE,
                               write.files = FALSE,
                               max.files = 1)) 
    thinned_data2 = list()
    
    paste("Joining Spatially Thinned Species occurrence data") |> print()
    for (i in 1:length(names(thinned_data))) {
      thinned_data2[[i]] = dplyr::mutate(thinned_data[[i]][[1]], species = names(thinned_data)[i])
    }
    thinned_data2 = as.data.frame(do.call(rbind, thinned_data2)) |> 
      rename(decimalLatitude = Latitude, decimalLongitude = Longitude, species = species) |>
      left_join(data)
    
    thinned_data2 = thinned_data2[!duplicated(thinned_data2[, c(1, 2, 3)]), ]
    
    paste("Saving Thinned", db_table, "occurrence data to local file") |> print()
    write_csv(thinned_data2, file = here("bias_correction", paste0(db_table, "_", d, "_km_spatial_thinning_by_species.csv")))
    
    
    ## spatially thinning by family
    name_backbone = readr::read_csv(here("13.  bias assessment results", db_table, paste0(db_table, "_backbone_family.csv")))
    data = raw_data |> dplyr::left_join(name_backbone[!is.na(name_backbone$family), ], by = "species") |> data.frame()
    
    # thin data family by family
    thinned_data3 =  split.data.frame(data, data$family) |>
      purrr::map(~spThin::thin(loc.data = .x,
                               lat.col = "decimalLatitude", long.col = "decimalLongitude",
                               spec.col = "family", thin.par = min_distance[i], reps = 10,
                               locs.thinned.list.return = TRUE,
                               write.files = FALSE,
                               max.files = 1)) 
    thinned_data4 = list()
    
    paste("Joining Spatially Thinned Species occurrence data") |> print()
    for (i in 1:length(names(thinned_data3))) {
      thinned_data4[[i]] = dplyr::mutate(thinned_data3[[i]][[1]], family = names(thinned_data3)[i])
    }
    thinned_data4 = as.data.frame(do.call(rbind, thinned_data4)) |> 
      rename(decimalLatitude = Latitude, decimalLongitude = Longitude, family = family) |>
      left_join(data)
    
    thinned_data4 = thinned_data4[!duplicated(thinned_data4[, c(1, 2, 5)]), ]
    
    paste("Saving Thinned by family", db_table, "occurrence data to local file") |> print()
    write_csv(thinned_data4, file = here("bias_correction", paste0(db_table, "_", d, "_km_spatial_thinning_by_family.csv")))
    
  }
  
}








