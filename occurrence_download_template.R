
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
if (!"here" %in% installed.packages()) install.packages("here")
library(here)

# create directory for "occurrence files" 
if(!("occurrence_files" %in% list.files(here()))) {
  dir.create(path = here("occurrence_files"))
}

# create directory for "species name backbone files" 
if(!("name_backbone" %in% list.files(here()))) {
  dir.create(path = here("name_backbone"))
}

# function to download the relevant columns from dropbox
get_data = function(dataset, drop_url) {
  set_a = c("aus_glonaf_invaded_clean", "aus_randal_invaded_clean",
            "gbr_native_clean", "gbr_invaded_clean", "irl_native_clean", 
            "irl_invaded_clean", "jpn_invaded_clean", "jpn_native_clean",
            "mdg_native_clean", "mdg_invaded_clean", "nzl_aiko_invaded_clean",
            "nzl_aiko_native_clean", "nzl_glonaf_native_clean", "nzl_glonaf_invaded_clean",
            "usa_native_clean", "usa_invaded_clean", "zaf_native_clean", "zaf_invaded_clean")
  
  paste("fetching", dataset, "occurrences") |> print()
  if(dataset %in% set_a) {
    value = readr::read_csv(file = drop_url) 
  } else {
    tryCatch({
      value = read.csv(file = drop_url)
    })
  }
  
  value = value[complete.cases(value$species), ] 
  #paste("Fetch complete! Saving", dataset, "occurrences to file") |> print()
  #readr::write_csv(x = value, file = here("occurrence_files", paste0(dataset, ".csv")))
  return(value)
}




