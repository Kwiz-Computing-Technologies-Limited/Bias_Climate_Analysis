
# source bias function
source("~/Desktop/Documents/GitHub/bias assessment/occAssess_bias_function.R")

# urls to be downloaded from Dropbox
occ_data = tibble(dataset = c("aus_glonaf_invaded_clean",
                              "gbr_invaded_clean",
                              "irl_Invaded_clean", 
                              "jpn_invaded_clean", 
                              "mdg_invaded_clean", 
                              "nzl_glonaf_invaded_clean",
                              "usa_invaded_clean",
                              "zaf_invaded_clean"), 
                  
                  drop_url = c("https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAAB4mDO8o1lZyW20EVBaUGEa/AUS-Glonaf/AUS-Glonaf-invaded-clean-v1.csv?dl=1",
                               "https://www.dropbox.com/s/rbyvnu3wap6wyh7/GBR-invaded-clean-v1.csv?dl=1",
                               "https://www.dropbox.com/s/6093mg9vl4ibhrj/IRL-NIR-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/s/c6y84zikkrvtqjs/JPN-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/s/m8yxddeze4hovpv/MDG-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/s/rkb7a2rutw7vsel/NZL-Glonaf-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/s/7blp6dvql7kf9xk/USA-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/s/rn5y3vebqt5igad/ZAF-invaded-clean.csv?dl=1")) |>
  na.omit()


# generate bias output files
tables = occ_data$dataset[2]
for (db_table in tables) {
  Bias_assessment_function(db_table = db_table, 
                           periods_length = 10)
}
