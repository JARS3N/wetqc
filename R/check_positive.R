check_positive <- function(vec) {
  #should return positive even for NA
  if(all(is.na(vec))){return(TRUE)}
  #all_null <- all(is.null(vec))

  med <- median(vec, na.rm = T)
  if (med <= 0) {
    return(FALSE)
  } else{
    return(TRUE)
  }
}

