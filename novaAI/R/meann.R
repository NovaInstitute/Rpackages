#' Meann
#'
#' A simple mean function
#'
#' @param x The object to have its mean computed
#' @param ... Further arguments passed to or from other methods
#' @export

meann <- function(x, ...)  {
        c(mean = mean(x, na.rm=TRUE),
          n = length(x),
          NA. = length(x) - length(na.omit(x)))
}

#' Sumn
#'
#' A simple sum function
#'
#' @param x The object to have its sum computed
#' @param ... Further arguments passed to or from other methods
#' @export

sumn <- function(x, ...) {
        c(mean=mean(x, na.rm=TRUE),
          sum=sum(x,na.rm=TRUE),
          n=length(x),
          NA.=length(x)-length(na.omit(x)))
}
