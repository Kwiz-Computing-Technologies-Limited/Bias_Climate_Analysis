
library(dplyr)
library(here)

native_ranges = c("gbr", "jpn", "mdg", "irl", "zaf")
native_paths = stringr::str_c(here("bias_correction_native", native_ranges))

invaded_ranges = c("irl", "mdg","zaf", "jpn", "nzl", "usa", "aus" , "gbr")
invaded_paths = stringr::str_c(here("bias_correction", invaded_ranges))

thinned_files = function(path) {
  files = list.files(path)
  return(files)
}


# species after thinning 
# invaded ranges

thinned_species_invaded = data.frame(region = NA, species = NA)
for(i in 1:length(invaded_ranges)) {
  thinned_species_invaded[i, 1] = stringr::str_c(invaded_ranges[i], "_invaded")
  thinned_species_invaded[i, 2] = thinned_files(path = invaded_paths[i]) |> length()  
}
thinned_species_invaded |> na.omit()

# native ranges
thinned_species_native = data.frame(region = NA, species = NA)
for(i in 1:length(native_ranges)) {
  thinned_species_native[i, 1] = stringr::str_c(native_ranges[i], "_native")
  thinned_species_native[i, 2] = thinned_files(path = native_paths[i]) |> length()
}
thinned_species_native |> na.omit()



# records after thinning 
# invaded ranges

thinned_records_invaded = data.frame(region = rep(NA, 8), records = rep(0, 8))
for(i in 1:length(invaded_ranges)) {
  thinned_records_invaded[i, 1] = stringr::str_c(invaded_ranges[i],  "_invaded")
  
  x = thinned_files(path = here("bias_correction", invaded_ranges[i]))
  thinned_records_invaded[i, 2] = thinned_records_invaded[i, 2] + 
    lapply(here(here("bias_correction", invaded_ranges[i], x)), FUN = function(x) {
      readr::read_csv(x) |> nrow()
    }) |> unlist() |> sum()  
}

# native ranges
thinned_records_native = data.frame(region = rep(NA, 5), records = rep(0, 5))
for(i in 1:length(native_ranges)) {
  thinned_records_native[i, 1] = stringr::str_c(native_ranges[i],  "_native")
  
  x = thinned_files(path = here("bias_correction_native", native_ranges[i]))
  thinned_records_native[i, 2] = thinned_records_native[i, 2] + 
    lapply(here(here("bias_correction_native", native_ranges[i], x)), FUN = function(x) {
      readr::read_csv(x) |> nrow()
    }) |> unlist() |> sum()  
}



thinned_records_invaded |> na.omit()
thinned_records_native |> na.omit()

# spatial bias analysis
library(occAssess)

