upload_all <- function(lst) {
  lapply(lst, function(u) {
    try(wetqc::upload(u))

  })
}
