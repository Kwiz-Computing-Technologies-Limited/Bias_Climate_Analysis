
# source bias function

source("occAssess_bias_function.R")

# urls to be downloaded from Dropbox
occ_data = tibble(dataset = c("aus_glonaf_native_filtered",
                              "GBR-native-filtered',
                              "irl_Invaded_clean", 
                              "jpn_invaded_clean", 
                              "mdg_invaded_clean", 
                              "nzl_glonaf_invaded_clean",
                              "usa_invaded_clean",
                              "zaf_invaded_clean"), 
                  
                  drop_url = c("https://www.dropbox.com/s/388ugmdgem36359/AUS-Glonaf-native-filtered-v1.csv?dl=1",
                               "https://www.dropbox.com/s/jy57axiea62cqr8/GBR-native-filtered.csv?dl=0",
                               "https://www.dropbox.com/s/6093mg9vl4ibhrj/IRL-NIR-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/s/c6y84zikkrvtqjs/JPN-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/s/m8yxddeze4hovpv/MDG-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/s/rkb7a2rutw7vsel/NZL-Glonaf-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/s/7blp6dvql7kf9xk/USA-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/s/rn5y3vebqt5igad/ZAF-invaded-clean.csv?dl=1")) |>
  na.omit()


# generate bias output files
tables = occ_data$dataset
for (db_table in tables) {
  Bias_assessment_function(db_table = db_table, 
                           periods_length = 10)
}
