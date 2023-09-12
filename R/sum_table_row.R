sum_table_row <- function(u) {
  barcoded <- c(ksv = u$O2_coefs$Ksv,
                gain = u$pH_coefs$gain)

  df <-
    tibble(
      use = NA,
      Lot = paste0(u$type, u$lot),
      sn = u$sn,
      assay = u$assay,
      pH.status = all(u$CAL$pH.Status == "Good"),
      o2.status = all(u$CAL$O2.Status == "Good"),
      Positive_Gain = check_positive(u$summary$Gain),
      Positive_KSV = check_positive(u$summary$KSV),
      injections = check_injections(u),
      file = basename(u$file)
    )
  checks <- c(df$pH.status,
              df$o2.status,
              df$Positive_Gain,
              df$Positive_KSV,
              df$injections)

  df$valid <- mean(checks[complete.cases(checks)]) == 1

  df
}
