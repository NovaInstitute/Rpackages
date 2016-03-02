discrete_table <- function (dvars, maxlevels=20, ..., verbose = FALSE) {
  dots <- list(...)
  # if there are no categorical variables there is no sense in continuing
  if (any_discrete(dvars)){
  if(!is.na(match("groupvar", names(dots)))){
    if (!is.na(match(dots[["groupvar"]], names(dvars)))){groep =  dvars[,dots[["groupvar"]]]} else {groep = 1}
    } else {groep = 1}
    dvars <- only_discrete(dvars)
    if (length(levels(groep)) > 1) dvars = cbind(dvars, groep)
    dvars = data.frame(lapply(dvars, as.factor))
    dvars = dvars[, lapply(dvars, nlevels) <= maxlevels, drop=FALSE]
    dropzero = which(sapply(dvars, function(x) length(levels(x))) == 0)
    if (length(dropzero) > 0) dvars <- dvars[ ,-dropzero, drop=FALSE]
    stopifnot(ncol(dvars) > 0)
    require(reporttools)
    tableNominal(vars = dvars,
                 cumsum = FALSE,
                 group = groep,
                 cap = ifelse(!is.na(match("cap", names(dots))), dots[["cap"]], ""),
                 lab = ifelse(!is.na(match("lab", names(dots))), dots[["lab"]], ""),
                 caption.placement = "top", table.placement = "!H")
    if (verbose == TRUE) message("Discrete table success", " \n")
  }
  if (verbose == TRUE) message("Jy verlaat discrete table\n")
}