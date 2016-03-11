#' Numeric Table
#' 
#' This function checks for any numeric values in a set data frame, isolates them and provides
#' a LaTex table of descriptive statistics seperately per group and jointly for all observations, 
#' per variable
#' 
#' @param nvars Data frame containing continuous variables
#' @param verbose Logical to display function messages
#' @param forcegvar Logical that forces a chosen grouping variable
#' @param ... Arguments passed down from the calling function
#' @export

numeric_table <- function (nvars, verbose = FALSE, forcegvar = FALSE, varSizeC = "0.15", levSizeC = "0.15", ...){
  dots <- list(...)

  if (!forcegvar){
  if (length(unique(nvars[, dots[["groupvar"]]])) > 10){
          stop("Numeric table function stopped due to groupvar having more than 10 levels")
  }
  }
  
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
            names(nvars) = gsub("_", " ", names(nvars))
            
      tableContinuous2(vars = nvars,
                      group = groep,
                      cap = ifelse(!is.na(match("cap", names(dots))), dots[["cap"]], ""),
                      lab = ifelse(!is.na(match("lab", names(dots))), dots[["lab"]], ""),
                      caption.placement = "top", table.placement = "!H", comment = FALSE, varSizeC = varSizeC, levSizeC = levSizeC)
      if (verbose == TRUE) message("Numeric table success", " \n")
    }
  }

}

#' Numeric Table Write
#' 
#' Writes the numeric table to a file in the chosen directory
#' 
#' @param nvarss Data frame containing continuous variables
#' @param fn A file name or a connection, if NULL returns the output as a character vector
#' @param ... Arguments passed down from the calling function
#' @export

numeric_table_write <- function(nvarss, fn, ...){
  capture.output(numeric_table(nvarss), file = fn)
}