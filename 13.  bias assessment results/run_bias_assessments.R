
# source bias function
source("~/Desktop/Documents/GitHub/bias assessment/occAssess_bias_function.R")

# urls to be downloaded from Dropbox
occ_data = tibble(dataset = c("aus_glonaf_native_filtered", "aus_glonaf_invaded_clean",
                              "aus_glonaf_native_clean", "aus_randal_native_filtered",
                              "aus_randal_invaded_clean", "aus_randal_native_clean",
                              "gbr_native_filtered", "gbr_invaded_clean",
                              "gbr_native_clean", "irl_native_filtered",
                              "irl_Invaded_clean", "irl_native_clean",
                              "jpn_native_clean", "jpn_native_filtered",
                              "jpn_invaded_clean", "mdg_native_filtered",
                              "mdg_invaded_clean", "mdg_native_clean",
                              "nzl_aikio_native_filtered", "nzl_aikio_invaded_clean",
                              "nzl_aikio_native_clean", "nzl_glonaf_invaded_clean",
                              "nzl_glonaf_native_clean", "nzl_glonaf_native_filtered",
                              "usa_native_filtered", "usa_invaded_clean",
                              "usa_native_clean", "zaf_native_filtered",
                              "zaf_invaded_clean", "zaf_native_clean"), 
                  
                  drop_url = c("https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAAF1mnPJIdN1ogrQRQqr0fZa/AUS-Glonaf/AUS-Glonaf-native-filtered-v1.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAAB4mDO8o1lZyW20EVBaUGEa/AUS-Glonaf/AUS-Glonaf-invaded-clean-v1.csv?dl=1",
                               NA,
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AADCW2eJp1ZWFWl5jO6ouG8-a/AUS-Randall/AUS-Randall.native-filtered.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AACG5bydJd8HCrcF1U2XgTLja/AUS-Randall/AUS-Randall-invaded-clean.csv?dl=1",
                               NA,
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAD3li8wLWSHBH9Vv7mYcO2ba/GBR/GBR-native-filtered.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAASTF8v1Eq0N_FP1QUDYuxGa/GBR/GBR-invaded-clean-v1.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AACds29mdx_awJM1S7_tKpXfa/GBR/GBR-native-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAC6HWdsbJARiq8MEx7iE9ona/IRL/IRL-native-filtered.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AADeejaZjhcCw5wRTDATynqAa/IRL/IRL-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AACrVVjolnVfBHbfS1MXo7bTa/IRL/IRL-native-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAA8yHeMlL-xV0EseYNrpi3Ma/JPN/JPN-native-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AADB9UE8pXEfqZnNNmv3k-yha/JPN/JPN-native-filtered.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AADuSJWpCJ5LcMpbkNqLsWola/JPN/JPN-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AABMGtats8ytJVuye4w2MwKsa/MDG/MDG-native-filtered.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AADNNQGMWaNwIJufSYG_jJABa/MDG/MDG-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AABqyaNLd2vTj-nwBJpBBmA2a/MDG/MDG-native-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAB8JaYFJg9mW1CSEc7_36Zza/NZL-Aikio/NZL-Aikio-native-filtered.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AABImLpXuN4ANJ5SIIoNvvJla/NZL-Aikio/NZL-Aikio-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAAhkJh8YKNvFD7orziL4XZya/NZL-Aikio/NZL-Aikio-native-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AADOzX6t5YJPH7W1Z7x03SL9a/NZL-Glonaf/NZL-Glonaf-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AACg4EsovSUZOHwpc-bWaG0Da/NZL-Glonaf/NZL-Glonaf-native-clean.csv?dl=1",
                               NA,
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AACit7ZDwxFiCRBLd5KzU3RCa/USA/USA-native-filtered.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAAjX_rwjf9Wyl8GoeorcSpda/USA/USA-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAD19fTySKPG1xzpeLvC2ujza/USA/USA-native-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AABkBNRP_wnKQoTf2eMdUn9pa/ZAF/ZAF-native-filtered.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAByRGPsPkve-tNACI2QsNpEa/ZAF/ZAF-invaded-clean.csv?dl=1",
                               "https://www.dropbox.com/sh/g7hbgbfyzocizmx/AAA_Hky88uStwNHsHu8-Vtx4a/ZAF/ZAF-native-clean.csv?dl=1")) |>
  na.omit()


# generate bias output files
tables = occ_data$dataset
for (db_table in tables) {
  Bias_assessment_function(db_table = db_table, 
                           periods_length = 10)
}
