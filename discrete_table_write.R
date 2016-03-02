
discrete_table_write <- function (dvars, group = 1, d.cap = "", d.lab = "", maxlevels=10, fn) {
  require(reporttools)
  dvars <- only_discrete(dvars)
  dvars = data.frame(lapply(dvars, as.factor))
  dvars = dvars[, sapply(dvars, nlevels) <= maxlevels, drop=FALSE]
  dropzero = which(sapply(dvars, function(x) length(levels(x))) == 0)
  if (length(dropzero) > 0) dvars <- dvars[ ,-dropzero, drop=FALSE]
  stopifnot(ncol(dvars) > 0)
  capture.output(
    tableNominal(vars = dvars,
               cumsum = FALSE,
               group = group,
               cap = ifelse(exists("d.cap"),d.cap, ""),
               lab = ifelse(exists("d.lab"),d.lab, ""),caption.placement = "top"), file = fn)
  cat("\\input{",fn,"}", sep="")
}