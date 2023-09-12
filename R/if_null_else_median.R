if_null_else_median <- function(vec) {
  all_null <- all(is.null(vec))
  if (all_null) {
    return(1)
  } else{
    median(vec,na.rm=T)
  }
}
