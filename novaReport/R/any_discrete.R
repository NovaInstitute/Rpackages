#' Any discrete variables
#' 
#' Function checks if there are any discrete variables in a set datafame with op
#' 
#' @param x Data frame
#' @export

any_discrete <- function(x){
  if (!is.data.frame(x) | all(is.na(x))) FALSE else any(grep("character|logical|factor", sapply(x, class) ))
}

#' Isolate discrete varables
#' 
#' Function isolates only the discrete variables (use x <- only_discrete(x))
#' 
#' @param x Data frame 
#' @export

only_discrete <- function(x){
  stopifnot(any_discrete(x))
  idx <- grep("character|logical|factor", sapply(x, class))
  x[,idx, drop=FALSE]
}