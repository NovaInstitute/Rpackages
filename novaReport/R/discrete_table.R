#' Discrete Table
#' 
#' This function uses the any_discrete and only_discrete functions to isolate discrete variables, 
#' whereby it uses the custom tableNominal2 function to create a LaTeX table of descriptive statistics
#' 
#' @param dvars Data frame containing the variables to be tested
#' @param maxlevels Numeric containing the maximum amount of levels
#' @param ... Arguments passed down from the calling function
#' @param verbose Logical to display function messages
#' @param forcegvar Logical that forces the usage of a variable. This will result in
#' tables being too large for display purposes
#' @param varSizeN Character vector that contains the percentage column size to be
#' attributed to the column containing variables. This is used to customize tables.
#' @param levSizeN Character vector that contains the percentage column size to be
#' attributed to the column containing levels. This is used to customize tables.
#' @export

discrete_table <- function (dvars, 
                            maxlevels=20, 
                            ..., 
                            verbose = FALSE, 
                            forcegvar = FALSE, 
                            varSizeN = "0.15", 
                            levSizeN = "0.05") {
  dots <- list(...)
  
  if (!forcegvar){
          if (length(unique(dvars[, dots[["groupvar"]]])) > 10){
                  stop("Discrete table function stopped due to groupvar having more than 10 levels")
          }
  }
  
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
    
    names(dvars) = gsub("\\.", " ", names(dvars))
    tableNominal2(vars = dvars,
                 cumsum = FALSE,
                 group = groep,
                 cap = ifelse(!is.na(match("cap", names(dots))), dots[["cap"]], ""),
                 lab = ifelse(!is.na(match("lab", names(dots))), dots[["lab"]], ""),
                 caption.placement = "top", table.placement = "!H", comment = FALSE, varSizeN = varSizeN, levSizeN = levSizeN)
    if (verbose == TRUE) message("Discrete table success", " \n")
  }
  if (verbose == TRUE) message("Jy verlaat discrete table\n")
}