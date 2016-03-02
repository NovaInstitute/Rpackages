# function to check if there are any numeric variables in a set dataframe with op

any_numeric <- function(x){
  if (!is.data.frame(x) | all(is.na(x))) FALSE else any(grep("integer|numeric", sapply(x, class) ))
}

# function to isolate only the numeric: use x <- only_numeric(x)
only_numeric <- function(x){
  stopifnot(any_numeric(x))
  idx <- grep("integer|numeric", sapply(x, class))
  x[,idx, drop=FALSE]
}