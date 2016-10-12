vowelSet <- c("a","e","i","o","u")
consonantSet <- c("b","c","d","f","g","h","j","k","l","m",
                  "n","p","q","r","s","t","v","w","x","y","z")
alphabetSet <- c(vowelSet, consonantSet)
NATOvowelSet <- c("alpha", "echo", "india", "oscar", "uniform")
NATOconsonantSet <- c("bravo", "charlie", "delta", "foxtrot", "gholf", "hotel", 
                      "juliet", "kilo", "lima", "mike", "november", "papa", 
                      "qubec", "romeo", "sierra", "tango", "victor", "whiskey", 
                      "x-ray", "yankee", "zulu")
NATOalphabetSet <- c(NATOvowelSet, NATOconsonantSet)
numericSet <- c(0:9)
alphanumericSet <- c(alphabetSet, numericSet)


#' Convert NA into "NADA"
#' 
#' @param x Vector
#' @return Vector with NA values replaced with "NADA
#' @export

nada <- function(x) {
  
  if (!is.vector(x)) {
    warning("Non-vector argument received. Returning.")
    return(x)
  } 
  
  if (any(is.na(x))) {
    x[is.na(x)] <- "NADA"
  }
  
  return(x)
}

#' Search for "NADA" values
#' 
#' @param x Vector
#' @return Logical specifying whether "NADA" values were found
#' @export

is.nada <- function(x) {
  res <- apply(X = as.array(x), MARGIN = 1, FUN = function(e) {
    if (is.na(e)) {
            warning("Actual NA detected.")
            return(FALSE)
    }
    if (toupper(as.character(e)) == "NADA") { return(TRUE) } else {return(FALSE)}
  })
}

#' Undo changes
#' 
#' @param x Vector
#' @return If "NADA" values are found, they are converted back to NA
#' @export

unnada <- function(x) {
  if (is.null(x)) {return(x)}
  if (is.nada(x)) {return(NA)} else {return(x)}
}

#' Get file extension
#'
#' @param fileName Character string containing the name of the file
#' @return File extension
#' @export 
 
getFileExtension <- function(fileName = NULL) {
  if (is.null(fileName)) {
    warning("No argument received by parameter 'fileName'. Returning NULL.")
    return(NULL)
  }
  
  splits <- strsplit(x = fileName, split = ".", fixed = TRUE)[[1]]
  if (length(splits) < 2) {
    warning("Extension marker ('.') not found. Failed to extract extension. Returning NULL.")
    return(NULL)
  }
  
  return(paste(".", splits[[length(splits)]], sep = ""))
}

#' Wrapper for 'unique function'
#' 
#' Added 'na.rm' option to standard unique function
#' 
#' @param x Vector, data frame or array
#' @param na.rm Logical that removes NA values if TRUE
#' @return Vector, data frame or array with duplicate values removed
#' @export

uniqueS <- function(x, na.rm = TRUE) {
  res <- unique(x)
  if (na.rm) {
    res <- res[!is.na(res)]
  }
  return(res)
}

#' Match fields
#' 
#' This function calculates which fields of two given objects are similar
#' 
#' @param record1 Array containing data related to the first record
#' @param record2 Array containing data related to the second record
#' @param fieldsToMatch Either an integer vector containing the fields to match or a character vector 
#' containing the indices of the fields to match
#' @param numRequiredMatches Integer that specifies the number of required matches in order for the
#' fields to be considered as matching
#' @param verbose Logical that displays function information when TRUE
#' @return Depending on the information supplied, the function will return FALSE for no matches or when 
#' number of matches is less than number of required matches, or return true when the records matchs completely, 
#' also returning the fields that match
#' @export

isSimilar <- function(record1 = NULL, 
                      record2 = NULL, 
                      fieldsToMatch = NULL, 
                      numRequiredMatches = NULL, 
                      verbose = FALSE) {
  
  if (is.null(record1) | is.null(record2)) {
    stop("Both record1 and record2 must be specified.")
  }
  
  if (length(record1) < 1 | length(record2) < 1) {
    warning("Empty argument(s) found among record1 and record2. Records considered as not matching. Returning FALSE.") 
    return(FALSE)
  }
  
  if (is.null(fieldsToMatch) | length(fieldsToMatch) < 1) {
    warning("No (non-empty) argument received by parameter fieldsToMatch. Matching will be done across all available fields.")
    f <- c(1:max((length(record1)), (length(record2)), na.rm = TRUE))
    fieldsToMatch <- f
  }
  
  if (is.null(numRequiredMatches)) {
    warning("No argument received by parameter numRequiredMatches. Records will only be regarded as matching if they agree across all tested fields.")
    numRequiredMatches <- length(fieldsToMatch)
  }
  
  similar <- apply(X = as.array(fieldsToMatch), MARGIN = 1, FUN = function(f) {
    if (is.integer(f)) {
      if (f > length(record1) | f > length(record2)) {return(FALSE)}
    }
    
    if (is.character(f)) {
      if (!(f %in% names(record1)) | !(f %in% names(record2))) {return(FALSE)}
    }
    
    return(record1[[f]] == record2[[f]])
  })
  
  if (length(similar[similar]) < numRequiredMatches) {
    if (verbose) {message("Not similar. No of matches: ", length(similar[similar]))}  
    return(FALSE)
  }
    
  # reaching this point means that length(similar[similar]) is greater than or equal to numRequiredMatches
  if (verbose) {message("Similar. No of matches: ", length(similar[similar]), ".")}
  if (verbose) {
    message("Matching fields: ")
    apply(X = as.array(fieldsToMatch[similar]), 
          MARGIN = 1, FUN = function(fnm) {message(" ", fnm)})
  }
  return(TRUE)
}

#' Generate random string
#' 
#' Generates a random string from a set of characters
#' 
#' @param charSet Character vector
#' @param stringLength Length of the string to be created
#' @param replace Logical that determines whether sampling should be with replacement or not
#' @return Returns a random string using character vector concatenation
#' @export

generateRandomString <- function(charSet = NULL, 
                                 stringLength = 7, 
                                 replace = TRUE) {
  if (is.null(charSet)) {
    stop("NULL argument received by parameter 'charSet'.")
  }
  
  chars <- sample(x = charSet, size = stringLength, replace = replace)
  randomString <- NULL
  
  for (c in 1:stringLength) {
    randomString <- paste(randomString, chars[[c]], sep = "")
  }
  
  return(randomString)
}