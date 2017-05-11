#' Any numeric variables
#' 
#' Function checks if there are any numeric variables in a set datafame with op
#' 
#' @param x Data frame
#' @export

any_numeric <- function(x){
  if (!is.data.frame(x) | all(is.na(x))) FALSE else any(grep("integer|numeric", sapply(x, class) ))
}

#' Isolate numeric varables
#' 
#' Function isolates only the numeric variables (use x <- only_numeric(x))
#' 
#' @param x Data frame 
#' @export

only_numeric <- function(x){
  stopifnot(any_numeric(x))
  idx <- grep("integer|numeric", sapply(x, class))
  x[,idx, drop=FALSE]
}