# function to check if there are any discrete variables in a set dataframe with op

any_discrete <- function(x){
  if (!is.data.frame(x) | all(is.na(x))) FALSE else any(grep("character|logical|factor", sapply(x, class) ))
}

# function to isolate only the discrete: use x <- only_discrete(x)
only_discrete <- function(x){
  stopifnot(any_discrete(x))
  idx <- grep("character|logical|factor", sapply(x, class))
  x[,idx, drop=FALSE]
}