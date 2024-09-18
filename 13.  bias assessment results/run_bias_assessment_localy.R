
# source bias function
source("occAssess_spatial_env_bias_function.R")

# urls to be downloaded from Dropbox
occ_data = tibble(dataset = c("mdg_invaded_clean",
                              "jpn_invaded_clean",
                              "irl_Invaded_clean",
                              "aus_glonaf",
                              "gbr_invaded_clean",
                              "zaf_invaded_clean",
                              "nzl_glonaf_invaded_clean",
                              "usa_invaded_clean"), 
                  
                  drop_url = c("https://www.dropbox.com/scl/fi/3hx8gbgd7858cpubyo4f1/MDG-invaded-clean.rds?rlkey=98cs05vrn2s55igxuuxhj5qdt&dl=1",
                               "https://www.dropbox.com/scl/fi/qezmtd6oj1jf1lsvtkmux/JPN-invaded-clean.rds?rlkey=rs3d5mgwys2s1731if509ef1v&dl=1",
                               "https://www.dropbox.com/scl/fi/gw9dpvdifdgp3sksinngo/IRL-invaded-clean.rds?rlkey=boyzevieh6vc8vi3ucwwwjo0c&dl=1",
                               "https://www.dropbox.com/scl/fi/kizxr5oy7oe5d54kyw76p/AUS-Glonaf-invaded-clean.rds?rlkey=3ukbga0g9q7hjgowshl018n2s&dl=1",
                               "https://www.dropbox.com/scl/fi/n0ridwusbnqk2lnrm078i/GBR-invaded-clean-v1.rds?rlkey=tejxsv5l3tge1552ld76fajrr&dl=1",
                               "https://www.dropbox.com/scl/fi/5a0a1pepumq1t1ujwo7k8/ZAF-invaded-clean.rds?rlkey=ltdovycts12kvvyjt1smc1arg&dl=1",
                               "https://www.dropbox.com/scl/fi/ux9levicn4cx3pz43v9i0/NZL-Glonaf-invaded-clean.csv?rlkey=9223dy2cm3atbg0dgbp8s81y3&dl=1",
                               "https://www.dropbox.com/scl/fi/rxrutz13ogzpcu8h28ycu/USA-invaded-clean.rds?rlkey=ptf649kbc44nflu0nsv3sdvnh&dl=1"
                               )) |>
  na.omit()


# generate bias output files
tables = occ_data$dataset[6] # 5, 4, 3, 2, 1
for (db_table in tables) {
  Bias_assessment_function(db_table = db_table, 
                           periods_length = 10)
}
