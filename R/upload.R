upload <- function(u) {
  require(RMySQL)
  u<-as.data.frame(u)
  type <- substr(unique(u$Lot), 1, 1)
  my_db <- adminKraken::con_mysql()
  dbWriteTable(
    my_db,
    name = wetqc::type(type),
    value = u,
    append = TRUE,
    overwrite = FALSE,
    row.names = FALSE
  )
  dbDisconnect(my_db)
  
}
