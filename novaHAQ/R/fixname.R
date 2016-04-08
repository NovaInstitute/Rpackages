
## fixname function: Make variable names lower case,
# without any spaces or punctution marks - whether within the name,
# leading or trailing
fixname <- function(data_names) {
  if (!is.array(data_names)) {
    data_names <- as.array(data_names)
  }

  data_names <- tolower(data_names)

  data_names <- sapply(X = data_names, FUN = function(nm) {
    nm <- gsub(pattern = "[[:punct:]]", replacement = "_", x = nm)
    nm <- gsub(pattern = "[[:space:]]", replacement = "_", x = nm)
    nm <- gsub(pattern = "\\.", replacement = "_", x = nm, fixed = TRUE)
    nm <- gsub(pattern = "_{2,}", replacement = "_", x = nm)
    nm <- gsub(pattern = "^_", replacement = "", x = nm)
    nm <- gsub(pattern = "_$", replacement = "", x = nm)
    return(nm)
  })

  data_names <- gsub(pattern = "ï_submission_id",
                     replacement = "submission_id",
                     x = data_names,
                     fixed = TRUE)

  return(data_names)
}
