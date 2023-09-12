remove_deselected <- function(data, deselection) {
  if (is.null(deselection)) {
    return(data)
  }
  else {
    return(data[-1 * deselection])
  }
}
