#' Make variable names lower case
#' 
#' Receives variable names and converts them to lower 
#' case, without any spaces or punctuation marks - whether
#' within the name, leading or trailing. The user may also specify a replacement character if so desired
#' 
#' @param data_names Names to be converted (function converts data into an array)
#' @param replacementChar The character that will be replacing either a punctuation mark, space or underscore. By 
#' default this function replaces spaces and punctuation marks with underscores
#' @return Simplified variable names
#' @export

fixname <- function(data_names, replacementChar = c("_", ".")[1]) {

  if (!(replacementChar %in% c("_", "."))) {
    stop("Invalid replacement character specified. Must be either '_' or '.' .")
  }
  
  data_names <- tolower(data_names)
  
  data_names <- sapply(X = as.array(data_names), FUN = function(nm) {
    nm <- gsub(pattern = "[[:punct:]]", replacement = replacementChar, x = nm)
    nm <- gsub(pattern = "[[:space:]]", replacement = replacementChar, x = nm)
    nm <- gsub(pattern = "\\.", replacement = replacementChar, x = nm, fixed = TRUE)
    nm <- gsub(pattern = "_{2,}", replacement = replacementChar, x = nm)
    nm <- gsub(pattern = "^_", replacement = "", x = nm)
    nm <- gsub(pattern = "_$", replacement = "", x = nm)
    nm <- gsub(pattern = "\\.{2,}", replacement = replacementChar, x = nm)
    nm <- gsub(pattern = "^\\.", replacement = "", x = nm)
    nm <- gsub(pattern = "\\.$", replacement = "", x = nm)    
    return(nm)
  })
  names(data_names) <- NULL
  
  data_names <- gsub(pattern = "ï_submission_id", 
                     replacement = "submission_id", 
                     x = data_names, 
                     fixed = TRUE)
  
  return(data_names)
}