#' Yapply
#'
#' This function was provided by Romain Francois based on something by Thomas Lumley
#'
#' @param X an atomic object, typically a vector
#' @param FUN Function to apply
#' @param ... Optional arguments to FUN: the Note section.
#' @note Optional arguments to FUN supplied by the ... argument
#' are not divided into cells. It is therefore inappropriate for
#' FUN to expect additional arguments with the same length as X.
#' @export

yapply=function(X,FUN, ...) {
  index <- seq(length.out=length(X))
  namesX <- names(X)
  if(is.null(namesX)) namesX <- rep(NA,length(X))

  FUN <- match.fun(FUN)
  fnames <- names(formals(FUN))
  if( ! "INDEX" %in% fnames ){
    formals(FUN) <- append( formals(FUN), alist(INDEX=) )   }
  if( ! "NAMES" %in% fnames ){
    formals(FUN) <- append( formals(FUN), alist(NAMES=) )   }
  mapply(FUN,X,INDEX=index, NAMES=namesX,MoreArgs=list(...)) }
