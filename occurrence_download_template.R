
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
  paste("fetching", dataset, "occurrences") |> print()
  value = readr::read_csv(file = drop_url)
   
  value = value[complete.cases(value$species), ] 
  #paste("Fetch complete! Saving", dataset, "occurrences to file") |> print()
  #readr::write_csv(x = value, file = here("occurrence_files", paste0(dataset, ".csv")))
  return(value)
}




