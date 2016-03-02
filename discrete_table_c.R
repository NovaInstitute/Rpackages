
discrete_table_c <- function (dvars, group = 1, d.cap = "", d.lab = "", fn) {
  require(reporttools)
  #capture.output(
    tableNominal(vars = dvars,
               cumsum = FALSE,
               group = group,
               cap = ifelse(exists("d.cap"),d.cap, ""),
               lab = ifelse(exists("d.lab"),d.lab, ""),caption.placement = "top")#, file = fn)
  cat("\\input{",fn,"}", sep="")
}