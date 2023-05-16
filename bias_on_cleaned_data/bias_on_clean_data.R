occ_data_native = tibble(dataset = c(
  "gbr-native-filtered", 
  "jpn-native-filtered", 
  "mdg-native-filtered",
  "irl-nir-native-filtered",
  "zaf-native-filtered"), 
  
  drop_url = c(
    "https://www.dropbox.com/s/jy57axiea62cqr8/GBR-native-filtered.csv?dl=1",
    "https://www.dropbox.com/s/vsshp1x06bstwbx/JPN-native-filtered.csv?dl=1",
    "https://www.dropbox.com/s/avjerougghb56j0/MDG-native-filtered.csv?dl=1",
    "https://www.dropbox.com/s/9a9ww83psn12rqw/IRL-NIR-native-filtered.csv?dl=1",
    "https://www.dropbox.com/s/ojemwko9qctjt1h/ZAF-native-filtered.csv?dl=1")) |>
  na.omit()


occ_data_invaded = tibble(dataset = c(
  "irl_Invaded_clean", 
  "mdg_invaded_clean",
  "zaf_invaded_clean", 
  "jpn_invaded_clean",
  "nzl_glonaf_invaded_clean",
  "usa_invaded_clean",
  
  "aus_glonaf_invaded_clean",
  "gbr_invaded_clean"
), 

drop_url = c(
  "https://www.dropbox.com/s/6093mg9vl4ibhrj/IRL-NIR-invaded-clean.csv?dl=1",
  "https://www.dropbox.com/s/m8yxddeze4hovpv/MDG-invaded-clean.csv?dl=1",
  "https://www.dropbox.com/s/rn5y3vebqt5igad/ZAF-invaded-clean.csv?dl=1",
  "https://www.dropbox.com/s/c6y84zikkrvtqjs/JPN-invaded-clean.csv?dl=1",
  "https://www.dropbox.com/s/rkb7a2rutw7vsel/NZL-Glonaf-invaded-clean.csv?dl=1",
  "https://www.dropbox.com/s/7blp6dvql7kf9xk/USA-invaded-clean.csv?dl=1",
  "https://www.dropbox.com/s/rg1h1wm1ctje9dh/AUS-Glonaf-invaded-clean-v1.csv?dl=1",
  "https://www.dropbox.com/s/rbyvnu3wap6wyh7/GBR-invaded-clean-v1.csv?dl=1"
)) |>
  na.omit()

## cleaned occurrence data

native_ranges = c("gbr", "jpn", "mdg", "irl", "zaf")
native_paths = stringr::str_c(here("bias_correction_native", native_ranges))

invaded_ranges = c("irl", "mdg","zaf", "jpn", "nzl", "usa", "aus" , "gbr")
invaded_paths = stringr::str_c(here("bias_correction", invaded_ranges))

thinned_files = function(path) {
  files = list.files(path)
  return(files)
}




# records after thinning 
# invaded ranges

for(i in 1:length(invaded_ranges)) {
  x = thinned_files(path = here("bias_correction", invaded_ranges[i]))
  
  thinned_records_invaded = lapply(here(here("bias_correction", invaded_ranges[i], x)), FUN = function(x) {
    readr::read_csv(x) |> dplyr::mutate(region = stringr::str_c(invaded_ranges[i],  "_invaded")) 
  }) 
  
  thinned_records_invaded = Reduce(f = full_join, x = thinned_records_invaded) |> 
    dplyr::left_join(readr::read_csv(occ_data_invaded[i, 2]))
  
  # spatial bias
  
  
}




# native ranges
for(i in 1:length(native_ranges)) {
  x = thinned_files(path = here("bias_correction_native", native_ranges[i]))
  
  thinned_records_native = lapply(here(here("bias_correction_native", native_ranges[i], x)), FUN = function(x) {
    readr::read_csv(x) |> dplyr::mutate(region = stringr::str_c(native_ranges[i],  "_native"))
  }) 
  
  
  thinned_records_native = Reduce(f = full_join, x = thinned_records_native) |> 
    dplyr::left_join(readr::read_csv(occ_data_native[i, 2]))
  
  
  # spatial bias
  
}




