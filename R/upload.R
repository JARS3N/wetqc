upload <- function(u) {
  require(RMySQL)

  my_db <- adminKraken::con_mysql()
  dbWriteTable(
    my_db,
    name = wetqc::type(unique(u$type)),
    value = u,
    append = TRUE,
    overwrite = FALSE,
    row.names = FALSE
  )
  dbDisconnect(my_db)

}
