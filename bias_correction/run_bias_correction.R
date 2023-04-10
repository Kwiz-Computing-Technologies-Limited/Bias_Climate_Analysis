# urls to data to be spatially corrected
library(dplyr)
library(here)

occ_data = tibble(dataset = c(# "irl_Invaded_clean", 
                              # "mdg_invaded_clean",
                              # "zaf_invaded_clean", 
                              # "jpn_invaded_clean",
                              # "nzl_glonaf_invaded_clean",
                              # "usa_invaded_clean",
  
                               "aus_glonaf_invaded_clean" # ,(incomplete)
                              # "gbr_invaded_clean"
  ), 
                  
                  drop_url = c(# "https://www.dropbox.com/s/6093mg9vl4ibhrj/IRL-NIR-invaded-clean.csv?dl=1",
                               # "https://www.dropbox.com/s/m8yxddeze4hovpv/MDG-invaded-clean.csv?dl=1",
                               # "https://www.dropbox.com/s/rn5y3vebqt5igad/ZAF-invaded-clean.csv?dl=1",
                               # "https://www.dropbox.com/s/c6y84zikkrvtqjs/JPN-invaded-clean.csv?dl=1",
                               #"https://www.dropbox.com/s/rkb7a2rutw7vsel/NZL-Glonaf-invaded-clean.csv?dl=1",
                               # "https://www.dropbox.com/s/7blp6dvql7kf9xk/USA-invaded-clean.csv?dl=1",
                                "https://www.dropbox.com/s/rg1h1wm1ctje9dh/AUS-Glonaf-invaded-clean-v1.csv?dl=1"  # ,(incomplete)
                               # "https://www.dropbox.com/s/rbyvnu3wap6wyh7/GBR-invaded-clean-v1.csv?dl=1"
                               )) |>
  na.omit()




# generate thinned datasets
tables = occ_data$dataset
source(here("bias_correction", "spatial_bias_correction_function.R"))

## thinning distances
min_distances_km = 1 # c(1, 5, 10, 20)
for (db_table in tables) {
  gc()
  bias_correction(db_table = db_table, 
                           min_distance = min_distances_km)
}


# db_table = tables[5]
