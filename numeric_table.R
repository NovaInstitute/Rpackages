# numeric_table

numeric_table <- function (nvars, verbose = FALSE, ...){
  dots <- list(...)

  # maak numeriese items met twee vlakke diskreet (tipies 0,1 vir ja nee)
  to.factor.idx = which(sapply(nvars, function(x) nlevels(as.factor(x))) == 2)
  if (length(to.factor.idx > 0)) for (i in 1:length(to.factor.idx)){
    nvars[,to.factor.idx[i]] <- as.factor(nvars[,to.factor.idx[i]])
  }

  if (any_numeric(nvars)){

    if(!is.na(match("groupvar", names(dots)))){
      if (!is.na(match(dots[["groupvar"]], names(nvars)))){groep =  nvars[,dots[["groupvar"]]]} else {groep = 1}
    } else {groep = 1}

    nvars <- only_numeric(nvars)
    if (ncol(nvars) > 0){
      require(reporttools)
      tableContinuous(vars = nvars,
                      group = groep,
                      cap = ifelse(!is.na(match("cap", names(dots))), dots[["cap"]], ""),
                      lab = ifelse(!is.na(match("lab", names(dots))), dots[["lab"]], ""),
                      caption.placement = "top", table.placement = "!H")
      if (verbose == TRUE) message("Numeric table success", " \n")
    }
  }

}

numeric_table_write <- function(nvarss, fn, ...){
  capture.output(numeric_table(nvarss), file = fn)
}