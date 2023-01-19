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
bias_correction = function(db_table) {
  
  # get raw data
  source(here("occurrence_download_template.R"))
  raw_data = get_data(dataset = occ_data[occ_data$dataset == db_table, ]$dataset,
                      drop_url = occ_data[occ_data$dataset == db_table, ]$drop_url)
  
  paste("Spatially Thinning species occurrences in", db_table) |> print()
  # thin data species by species
  thinned_data =  split.data.frame(dat, dat$species) |>
    purrr::map(~spThin::thin(loc.data = .x,
                             lat.col = "decimalLatitude", long.col = "decimalLongitude",
                             spec.col = "species", thin.par = 0.001, reps = 10,
                             locs.thinned.list.return = TRUE,
                             write.files = FALSE,
                             max.files = 1)) 
  thinned_data2 = list()
  
  paste("Joining Spatially Thinned Species occurrence data") |> print()
  for (i in 1:length(names(thinned_data))) {
    thinned_data2[[i]] = dplyr::mutate(thinned_data[[i]][[1]], species = names(thinned_data)[i])
  }
  thinned_data2 = as.data.frame(do.call(rbind, thinned_data2))
  
  paste("Saving Thinned", db_table, "occurrence data to local file") |> print()
  write_csv(thinned_data2, file = here("bias_correction", paste0(db_table, "_1meter_spatial_thinning.csv")))
}







