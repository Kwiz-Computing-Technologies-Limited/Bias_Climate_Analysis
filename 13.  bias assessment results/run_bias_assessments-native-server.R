
# source bias function
source("C:/Users/Robeck/Downloads/Bias_Climate_Analysis/occAssess_bias_function.R")

# urls to be downloaded from Dropbox
occ_data = tibble(dataset = c(# "gbr-native-filtered",
                              # "irl-nir-native-filtered", 
                              # "jpn-native-filtered", 
                              # "mdg-native-filtered",
                              # "zaf-native-filtered", 
                              # "nzl-Glonaf-native-clean",
                              # "usa-native-filtered",
                              "aus_glonaf_native_filtered"), 
                  
                  drop_url = c(# "https://www.dropbox.com/s/jy57axiea62cqr8/GBR-native-filtered.csv?dl=1",
                               # "https://www.dropbox.com/s/9a9ww83psn12rqw/IRL-NIR-native-filtered.csv?dl=1",
                               # "https://www.dropbox.com/s/vsshp1x06bstwbx/JPN-native-filtered.csv?dl=1",
                               # "https://www.dropbox.com/s/avjerougghb56j0/MDG-native-filtered.csv?dl=1",
                               # "https://www.dropbox.com/s/ojemwko9qctjt1h/ZAF-native-filtered.csv?dl=1",
                               # "https://www.dropbox.com/s/ig0mqx06n31e5rq/NZL-Glonaf-native-clean.csv?dl=1",
                               # "https://www.dropbox.com/s/ufmvdr7f6t73cbs/USA-native-filtered.csv?dl=1",
                               "https://www.dropbox.com/s/388ugmdgem36359/AUS-Glonaf-native-filtered-v1.csv?dl=1")) |>
  na.omit()


# generate bias output files
tables = occ_data$dataset

for (db_table in tables) {
  Bias_assessment_function(db_table = db_table, 
                           periods_length = 10)
}


# Errors to be reviewed
# 1. irl-nir-native-filtered, env_bias (skipped)
# 2. assessSpatialCov with species as the taxonomic group from line 484 - 517 in occAssess_bias_function.R skipped to be run later. This is the most time consuming part of each analysis

