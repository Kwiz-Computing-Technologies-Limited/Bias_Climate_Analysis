# original data

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


# periods

occ_periods_native = tibble(dataset = c(
  "gbr-native-filtered",
  "jpn-native-filtered",
  "mdg-native-filtered",
  "irl-nir-native-filtered",
  "zaf-native-filtered"),

  country = c(
    "United Kingdom",
    "Japan",
    "Madagascar",
    "Ireland",
    "South Africa"),

  drop_url = c(
    "",
    "",
    "",
    "",
    ""
  )) |>
  na.omit()

occ_periods_invaded = tibble(dataset = c(
  "irl_Invaded_clean",
  "mdg_invaded_clean",
  "zaf_invaded_clean",
  "jpn_invaded_clean",
  "nzl_glonaf_invaded_clean",
  "usa_invaded_clean",
  "aus_glonaf_invaded_clean",
  "gbr_invaded_clean"
),

country = c(
  "Ireland",
  "Madagascar",
  "South Africa",
  "Japan",
  "New Zealand",
  "US",
  "Australia",
  "United Kingdom"
),

drop_url = c(
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  ""
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

  paste("Fetch name backbone ...") |> print()
  family = sapply((thinned_records_invaded$species |> unique() |> na.omit()),
                  function(x) name_backbone(name = x, kingdom = "plants"),
                  simplify = FALSE) |> bind_rows()

  aaa = family |>
    dplyr::select(species, family)

  rownames(aaa) <- NULL
  thinned_records_invaded = thinned_records_invaded |>
    dplyr::left_join(aaa)

  dat = thinned_records_invaded
  dir.create(here("bias_on_cleaned_data", "invaded", occ_data_invaded[i, 1]))

  # spatial bias
  paste("Fetching environment mask for spatial bias for", substr(occ_periods_invaded$country[i], 1, 3), "...") |> print()
  mask = geodata::worldclim_country(country = occ_periods_invaded$country[i], level = 0, res = 2.5, var = "tavg",
                                    path = here(substr(occ_periods_invaded$country[i], 1, 3)))
  mask2 = raster::brick(mask[[1]])
  options(timeout = getOption("timeout")^5)

  # by family
  spatBias <- assessSpatialBias(dat = dat[!is.na(dat$family), ],
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

  readr::write_csv(spatBias$data, file = here("bias_on_cleaned_data", "invaded_by_family", occ_data_invaded[i, 1],
                                              paste0(occ_data_invaded[i, 1], "cleaned",
                                                     "_assessSpatialBias_output.csv")))

  # remove used R objects from environment
  rm(spatBias)

  # by species
  spatBias <- assessSpatialBias(dat = dat[!is.na(dat$family), ],
                                species = "species",
                                y = "decimalLatitude",
                                x = "decimalLongitude",
                                year = "year",
                                spatialUncertainty = "coordinateUncertaintyInMeters",
                                identifier = "species",
                                periods = periods,
                                mask = mask2,
                                nSamps = 1,
                                degrade = TRUE)

  spatBias$data = spatBias$data |>
    dplyr::mutate(Period = as.integer(Period))

  readr::write_csv(spatBias$data, file = here("bias_on_cleaned_data", "invaded_by_species", occ_data_invaded[i, 1],
                                              paste0(occ_data_invaded[i, 1], "cleaned",
                                                     "_assessSpatialBias_output.csv")))


  # remove used R objects from environment
  rm(spatBias)

  # sampling intensity

  # by family
  paste("Fetching number of records in each year for", occ_periods_invaded$country[i], "...") |> print()
  nRec <- assessRecordNumber(dat = dat[!is.na(dat$family), ],
                             periods = periods,
                             species = "species",
                             y = "decimalLatitude",
                             x = "decimalLongitude",
                             year = "year",
                             spatialUncertainty = "coordinateUncertaintyInMeters",
                             identifier = "family",
                             normalize = FALSE)


  readr::write_csv(nRec$data, file = here("bias_on_cleaned_data", "invaded_by_family", occ_data_invaded[i, 1],
                                          paste0(occ_data_invaded[i, 1], "cleaned",
                                                "assessRecordNumber_output.csv", sep = "_")))
  # remove used R objects from environment
  rm(nRec)

  # by species
  nRec <- assessRecordNumber(dat = dat[!is.na(dat$family), ],
                             periods = periods,
                             species = "species",
                             y = "decimalLatitude",
                             x = "decimalLongitude",
                             year = "year",
                             spatialUncertainty = "coordinateUncertaintyInMeters",
                             identifier = "species",
                             normalize = FALSE)


  readr::write_csv(nRec$data, file = here("bias_on_cleaned_data", "invaded_by_species", occ_data_invaded[i, 1],
                                          paste0(occ_data_invaded[i, 1], "cleaned",
                                                 "assessRecordNumber_output.csv", sep = "_")))

  # remove used R objects from environment
  rm(nRec)


  # rarity bias
  source("assessRarityBias_modified.R")

  # by family
  taxBias <- assessRarityBias_modified(dat = dat[!is.na(dat$family), ],
                                       periods = periods,
                                       res = 0.5,
                                       prevPerPeriod = FALSE,
                                       species = "species",
                                       y = "decimalLatitude",
                                       x = "decimalLongitude",
                                       year = "year",
                                       spatialUncertainty = "coordinateUncertaintyInMeters",
                                       identifier = "family")

  readr::write_csv(taxBias$data, file = here("bias_on_cleaned_data", "invaded_by_species", occ_data_invaded[i, 1],
                                             paste0(occ_data_invaded[i, 1], "cleaned",
                                                   "assessRarityBias_output.csv", sep = "_")))

  # remove used R objects from environment
  rm(taxBias)

  # by species
  taxBias <- assessRarityBias_modified(dat = dat[!is.na(dat$family), ],
                                       periods = periods,
                                       res = 0.5,
                                       prevPerPeriod = FALSE,
                                       species = "species",
                                       y = "decimalLatitude",
                                       x = "decimalLongitude",
                                       year = "year",
                                       spatialUncertainty = "coordinateUncertaintyInMeters",
                                       identifier = "species")

  readr::write_csv(taxBias$data, file = here("bias_on_cleaned_data", "invaded_by_species", occ_data_invaded[i, 1],
                                             paste0(occ_data_invaded[i, 1], "cleaned",
                                                   "assessRarityBias_output.csv", sep = "_")))

  # remove used R objects from environment
  rm(taxBias)


  # Env bias
  paste("Fetching environmental data for", substr(occ_periods_invaded$country[i], 1, 3), "...") |> print()
  # get spatial bias
  gc()

  options(timeout = getOption("timeout")^10)
  paste("Fetching 'bio' data") |> print()
  env_data = geodata::worldclim_country(country = occ_periods_invaded$country[i], res = 5, var = "bio",
                                        path = here(substr(occ_periods_invaded$country[i], 1, 3))) |> raster::stack()

  # select the climate variables of interest
  paste("Selecting 8 'bio' variables") |> print()
  env_data2 = env_data[[c(1, 4, 10, 11, 12, 15, 16, 17)]]
  print(nrow(env_data2))

  dat2 = dat[!is.na(dat$family), ]
  print(nrow(dat2))
  env_data3 = terra::extract(env_data2, dat2[, c("decimalLongitude", "decimalLatitude")])

  paste("Fetch environmental data complete! Fetching environment bias from", occ_periods_invaded$country[i], "...") |> print()
  print(nrow(env_data3))

  # by family
  envBias <- assessEnvBias(dat = dat2,
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

  readr::write_csv(env_bias_data, file = here("bias_on_cleaned_data", "invaded_by_family", occ_data_invaded[i, 1],
                                              paste0(occ_data_invaded[i, 1], "cleaned", "_assessEnvBias_output.csv")))


  # remove used R objects from environment
  rm(env_data, env_data2, env_data3, dat2, envBias, env_bias_data)


  # by species
  envBias <- assessEnvBias(dat = dat2,
                           species = "species",
                           y = "decimalLatitude",
                           x = "decimalLongitude",
                           year = "year",
                           spatialUncertainty = "coordinateUncertaintyInMeters",
                           identifier = "species",
                           periods = periods,
                           envDat = (env_data3 |> as.data.frame()),
                           backgroundEnvDat = raster::sampleRandom(env_data2, size = 100000, xy = F))

  env_bias_data = envBias$data |>
    dplyr::select(Period, identifier, `scores.PC1`, `scores.PC2`, xVar, yVar)

  readr::write_csv(env_bias_data, file = here("bias_on_cleaned_data", "invaded_by_species", occ_data_invaded[i, 1],
                                              paste0(occ_data_invaded[i, 1], "cleaned", "_assessEnvBias_output.csv")))


  # remove used R objects from environment
  rm(env_data, env_data2, env_data3, dat2, envBias, env_bias_data)
}




# native ranges
for(i in 1:length(native_ranges)) {
  x = thinned_files(path = here("bias_correction_native", native_ranges[i]))

  thinned_records_native = lapply(here(here("bias_correction_native", native_ranges[i], x)), FUN = function(x) {
    readr::read_csv(x) |> dplyr::mutate(region = stringr::str_c(native_ranges[i],  "_native"))
  })


  thinned_records_native = Reduce(f = full_join, x = thinned_records_native) |>
    dplyr::left_join(readr::read_csv(occ_data_native[i, 2]))


  paste("Fetch name backbone ...") |> print()
  family = sapply((thinned_records_invaded$species |> unique() |> na.omit()),
                  function(x) name_backbone(name = x, kingdom = "plants"),
                  simplify = FALSE) |> bind_rows()

  aaa = family |>
    dplyr::select(species, family)

  rownames(aaa) <- NULL
  thinned_records_native = thinned_records_native |>
    dplyr::left_join(aaa)

  dat = thinned_records_native
  dir.create(here("bias_on_cleaned_data", "native", occ_data_native[i, 1]))


  # spatial bias
  paste("Fetching environment mask for spatial bias for", substr(occ_periods_native$country[i], 1, 3), "...") |> print()
  mask = geodata::worldclim_country(country = occ_periods_native$country[i], level = 0, res = 2.5, var = "tavg",
                                    path = here(substr(occ_periods_native$country[i], 1, 3)))
  mask2 = raster::brick(mask[[1]])
  options(timeout = getOption("timeout")^5)

  # by family
  spatBias <- assessSpatialBias(dat = dat[!is.na(dat$family), ],
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

  readr::write_csv(spatBias$data, file = here("bias_on_cleaned_data", "invaded", occ_data_native[i, 1],
                                              paste0(occ_data_native[i, 1], "cleaned_by_family",
                                                     "_assessSpatialBias_output.csv")))

  # remove used R objects from environment
  rm(spatBias)

  # by species
  spatBias <- assessSpatialBias(dat = dat[!is.na(dat$family), ],
                                species = "species",
                                y = "decimalLatitude",
                                x = "decimalLongitude",
                                year = "year",
                                spatialUncertainty = "coordinateUncertaintyInMeters",
                                identifier = "species",
                                periods = periods,
                                mask = mask2,
                                nSamps = 1,
                                degrade = TRUE)

  spatBias$data = spatBias$data |>
    dplyr::mutate(Period = as.integer(Period))

  readr::write_csv(spatBias$data, file = here("bias_on_cleaned_data", "invaded_by_species", occ_data_invaded[i, 1],
                                              paste0(occ_data_invaded[i, 1], "cleaned",
                                                     "_assessSpatialBias_output.csv")))

  # remove used R objects from environment
  rm(spatBias)

  # sampling intensity

  # by family
  paste("Fetching number of records in each year for", occ_periods_invaded$country[i], "...") |> print()
  nRec <- assessRecordNumber(dat = dat[!is.na(dat$family), ],
                             periods = periods,
                             species = "species",
                             y = "decimalLatitude",
                             x = "decimalLongitude",
                             year = "year",
                             spatialUncertainty = "coordinateUncertaintyInMeters",
                             identifier = "family",
                             normalize = FALSE)


  readr::write_csv(nRec$data, file = here("bias_on_cleaned_data", "native_by_family", occ_data_invaded[i, 1],
                                          paste0(occ_data_invaded[i, 1], "cleaned",
                                                 "assessRecordNumber_output.csv", sep = "_")))
  # remove used R objects from environment
  rm(nRec)

  # by species
  nRec <- assessRecordNumber(dat = dat[!is.na(dat$family), ],
                             periods = periods,
                             species = "species",
                             y = "decimalLatitude",
                             x = "decimalLongitude",
                             year = "year",
                             spatialUncertainty = "coordinateUncertaintyInMeters",
                             identifier = "species",
                             normalize = FALSE)


  readr::write_csv(nRec$data, file = here("bias_on_cleaned_data", "native_by_species", occ_data_invaded[i, 1],
                                          paste0(occ_data_invaded[i, 1], "cleaned",
                                                 "assessRecordNumber_output.csv", sep = "_")))

  # remove used R objects from environment
  rm(nRec)


  # rarity bias
  source("assessRarityBias_modified.R")

  # by family
  taxBias <- assessRarityBias_modified(dat = dat[!is.na(dat$family), ],
                                       periods = periods,
                                       res = 0.5,
                                       prevPerPeriod = FALSE,
                                       species = "species",
                                       y = "decimalLatitude",
                                       x = "decimalLongitude",
                                       year = "year",
                                       spatialUncertainty = "coordinateUncertaintyInMeters",
                                       identifier = "family")

  readr::write_csv(taxBias$data, file = here("bias_on_cleaned_data", "native_by_species", occ_data_native[i, 1],
                                             paste0(occ_data_native[i, 1], "cleaned",
                                                    "assessRarityBias_output.csv", sep = "_")))

  # remove used R objects from environment
  rm(taxBias)

  # by species
  taxBias <- assessRarityBias_modified(dat = dat[!is.na(dat$family), ],
                                       periods = periods,
                                       res = 0.5,
                                       prevPerPeriod = FALSE,
                                       species = "species",
                                       y = "decimalLatitude",
                                       x = "decimalLongitude",
                                       year = "year",
                                       spatialUncertainty = "coordinateUncertaintyInMeters",
                                       identifier = "species")

  readr::write_csv(taxBias$data, file = here("bias_on_cleaned_data", "native_by_species", occ_data_native[i, 1],
                                             paste0(occ_data_native[i, 1], "cleaned",
                                                    "assessRarityBias_output.csv", sep = "_")))

  # remove used R objects from environment
  rm(taxBias)


  # Env bias
  paste("Fetching environmental data for", substr(occ_periods_invaded$country[i], 1, 3), "...") |> print()
  # get spatial bias
  gc()

  options(timeout = getOption("timeout")^10)
  paste("Fetching 'bio' data") |> print()
  env_data = geodata::worldclim_country(country = occ_periods_invaded$country[i], res = 5, var = "bio",
                                        path = here(substr(occ_periods_invaded$country[i], 1, 3))) |> raster::stack()

  # select the climate variables of interest
  paste("Selecting 8 'bio' variables") |> print()
  env_data2 = env_data[[c(1, 4, 10, 11, 12, 15, 16, 17)]]
  print(nrow(env_data2))

  dat2 = dat[!is.na(dat$family), ]
  print(nrow(dat2))
  env_data3 = terra::extract(env_data2, dat2[, c("decimalLongitude", "decimalLatitude")])

  paste("Fetch environmental data complete! Fetching environment bias from", occ_periods_invaded$country[i], "...") |> print()
  print(nrow(env_data3))

  # by family
  envBias <- assessEnvBias(dat = dat2,
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

  readr::write_csv(env_bias_data, file = here("bias_on_cleaned_data", "native_by_family", occ_data_native[i, 1],
                                              paste0(occ_data_native[i, 1], "cleaned", "_assessEnvBias_output.csv")))


  # remove used R objects from environment
  rm(env_data, env_data2, env_data3, dat2, envBias, env_bias_data)


  # by species
  envBias <- assessEnvBias(dat = dat2,
                           species = "species",
                           y = "decimalLatitude",
                           x = "decimalLongitude",
                           year = "year",
                           spatialUncertainty = "coordinateUncertaintyInMeters",
                           identifier = "species",
                           periods = periods,
                           envDat = (env_data3 |> as.data.frame()),
                           backgroundEnvDat = raster::sampleRandom(env_data2, size = 100000, xy = F))

  env_bias_data = envBias$data |>
    dplyr::select(Period, identifier, `scores.PC1`, `scores.PC2`, xVar, yVar)

  readr::write_csv(env_bias_data, file = here("bias_on_cleaned_data", "native_by_species", occ_data_native[i, 1],
                                              paste0(occ_data_native[i, 1], "cleaned", "_assessEnvBias_output.csv")))


  # remove used R objects from environment
  rm(env_data, env_data2, env_data3, dat2, envBias, env_bias_data)
}




