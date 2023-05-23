## Climate data


library(dplyr)
library(stringr)
library(raster)
library(foreach)
library(here)

# register clusters for parallel computations
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

# map time ids to years they reference
time_IDs = tibble(time_id = 17:20, 
                  year_from = seq(from = 1600, to = 1900, by = 100))

# bio vars for download
bio_vars = c("bio01", "bio04", "bio10","bio11","bio12","bio15", "bio16", "bio17")

# creating file paths to the "CHELSA-TraCE21k – 1km climate timeseries since the LGM" bio vars at <https://chelsa-climate.org/chelsa-trace21k/>

bio_file_path = function(bio_var, time_id) {
  stringr::str_c("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/chelsa_trace/bio/CHELSA_TraCE21k_", 
                 bio_var, "_-", time_id, "_V1.0.tif")
}

paths = foreach(i = 1:length(bio_vars), .combine = "c") %:% foreach(j = 1:length(time_IDs$time_id), .combine = "c") %dopar% 
  bio_file_path(bio_var = bio_vars[i], time_id = time_IDs$time_id[j])


# fetch and save climate datasets
dir.create(here("climate_data"))

# get the data as a matrix
bio_data_download = function(bio_var, time_id, year_from) {
  path = bio_file_path(bio_var = bio_var, time_id = time_id)
  x = raster::raster(path, varname = bio_var) |> raster::rasterToPoints()
  readr::write_csv(as.data.frame(x), here("climate_data", stringr::str_c(bio_var, "_from_", year_from, ".csv")))
}

climate_fun = function ( i, j) {
  paste("fetching", bio_vars[i], "from", time_IDs$year_from[j]) |> print()
  bio_data_download(bio_var = bio_vars[i], time_id = time_IDs$time_id[j], year_from = time_IDs$year_from[j])
}

for(i in 4:length(bio_vars)) {
  for(j in 1:length(time_IDs$year_from)) {
    if(i == 4 & j == 1) {
        NULL
      } else {
        climate_fun( i = i, j = j)
      }
  }
}


