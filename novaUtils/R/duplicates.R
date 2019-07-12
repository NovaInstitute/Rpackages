#'@title duplicates
#'@name duplicates
#'
#'@description function that identifies duplicates
#'
#'@param x [data.frame/vector] The data frame/vector containing duplicates.
#'@return duplicate rows/values
#'
#'@export


duplicates <- function(x) {
  if (is.vector(x)) {
    return(unique(x[which(duplicated(x))]))
  }
  if (is.data.frame(x)) {
    x <- x[duplicated(x),]
    return(x[which(!duplicated(x)),])
  }
}
