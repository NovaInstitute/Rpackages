#' @title discrete_table_write
#' @description This function is used to write the discrete table to a file in the working directory. 
#' @description This file contains the source code for a LaTeX document.
#' @param dvars Data frame containing the nominal variables
#' @param group Optional grouping vector
#' @param d.cap The caption of the resulting LaTeX table
#' @param d.lab The lable of the resulting LaTeX table
#' @param maxlevels Numerical containing the maximum amount of levels
#' @param fn Character vector containing the desired name of the output file
#' @export

discrete_table_write <- function (dvars, 
                                  group = 1, 
                                  d.cap = "", 
                                  d.lab = "", 
                                  maxlevels=10, 
                                  fn) {
  
        if(!require(reporttools)){
                message("Package reporttools not found, installing package")
                install.packages("reporttools")
                if(!require(reporttools)) message("Reporttools install failed, install manually")
        }
        
  dvars <- only_discrete(dvars)
  dvars = data.frame(lapply(dvars, as.factor))
  dvars = dvars[, sapply(dvars, nlevels) <= maxlevels, drop=FALSE]
  dropzero = which(sapply(dvars, function(x) length(levels(x))) == 0)
  if (length(dropzero) > 0) dvars <- dvars[ ,-dropzero, drop=FALSE]
  stopifnot(ncol(dvars) > 0)
  capture.output(
    tableNominal2(vars = dvars,
               cumsum = FALSE,
               group = group,
               cap = ifelse(exists("d.cap"),d.cap, ""),
               lab = ifelse(exists("d.lab"),d.lab, ""),caption.placement = "top"), file = fn)
  cat("\\input{",fn,"}", sep="")
}