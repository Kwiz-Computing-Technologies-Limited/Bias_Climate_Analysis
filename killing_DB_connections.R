killDbConnections <- function () {
  
  all_cons <- DBI::dbListConnections(dbDriver("PostgreSQL"))
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

killDbConnections()
gc()
