#' Format integer-like character field
#' 
#' Function that receives an integer-like character field and removes leading zeros,
#' punctuation marks, alphabetic characters and blanks
#' 
#' @param field Vector to be converted to a character vector
#' @return Formatted field returned as a integer vector
#' @export

format_int <- function(field = NULL) {
  if (is.null(field)) {
    warning("NULL object received by parameter 'field'. Returning NULL.")
    return(field)
  }
  
  field <- as.character(field)
  field <- gsub(pattern = "[[:punct:]]", replacement = "", x = field)
  field <- gsub(pattern = "[[:alpha:]]", replacement = "", x = field)
  field <- gsub(pattern = "[[:blank:]]", replacement = "", x = field)
  field <- gsub(pattern = "^0{1,}", replacement = "", x = field)
  field <- gsub(pattern = "^$", replacement = NA, x = field)
  field <- as.integer(field)
  
  return(field)
}

#' Format integer-like character field
#' 
#' Function that receives an integer-like character field and removes leading zeros,
#' punctuation marks, alphabetic characters and blanks using the format.int function, but 
#' returns a character value
#' 
#' @param field Vector to be converted to a character vector
#' @return Formatted field returned as a character vector
#' @export 

format_int_char <- function(field = NULL) {
  if (is.null(field)) {
    warning("NULL object received by parameter 'field'. Returning NULL.")
    return(field)
  }
  
  field <- format.int(field)
  field <- as.character(field)
  
  return(field)
}

#' Format integer-like character field
#' 
#' Function that receives an integer-like character field that converts it to lower case and matches 
#' various patterns whereby it replaces them with chosen values 
#' 
#' @param field Vector to be converted to a character vector
#' @return Formatted field returned with replaced values
#' @export

format_char <- function(field = NULL) {
       
  if (is.null(field)) {
    warning("NULL object received by parameter 'field'. Returning NULL.")
    return(field)
  }
  
  field <- as.character(field)
  field <- tolower(field)
  field <- gsub(pattern = "[[:punct:]]", replacement = "_", x = field)
  field <- gsub(pattern = "[[:space:]]", replacement = "_", x = field)
  field <- gsub(pattern = ".", replacement = "_", x = field, fixed = TRUE)
  field <- gsub(pattern = "_{2,}", replacement = "_", x = field)
  field <- gsub(pattern = "^_", replacement = "", x = field)
  field <- gsub(pattern = "_$", replacement = "", x = field)
  field <- gsub(pattern = "^$", replacement = NA, x = field)
  
  return(field)
}

#' Format integer-like character field
#' 
#' Function that receives an integer-like character field and replaces character strings 
#' containing "1" and "0" with "yes" and "no" whilst also removing duplicate values
#' 
#' @param field Vector to be converted to a character vector
#' @param yes Integer containing the number associated with "yes"
#' @param no Integer containing the number associated with "no"
#' @param asFactor Logical that determines whether the field should be encoded as a factor
#' @return Formatted field returned either as a character vector or a vector encoded as a factor
#' @export

format_yesno <- function(field = NULL, yes = 1, 
                         no = 0, 
                         asFactor = FALSE) {
  if (is.null(field)) {
    warning("NULL object received by parameter 'field'. Returning NULL.")
    return(field)
  }
  
  yes <- as.character(yes)
  no <- as.character(no)
  field <- format.char(field)
  field <- gsub(pattern = yes, replacement = "yes", x = field, fixed = TRUE)
  field <- gsub(pattern = no, replacement = "no", x = field, fixed = TRUE)
  
  # check that there are no other weird values in the field
  uniqs <- unique(field)
  uniqs <- uniqs[!is.na(uniqs)]
  if (any(!(uniqs %in% c("yes", "no")))) {
    warning("Values found in received field that are not yes, no or NA.")
  }
  
  if (asFactor) {
    field <- as.factor(field)
  }
  
  return(field)
}

#' Format integer-like character field
#' 
#' Function that receives an integer-like character field and removes leading zeros,
#' punctuation marks, alphabetic characters and blanks
#' 
#' @param field Vector
#' @return Formatted field returned as a numeric vector
#' @export

format_num <- function(field = NULL) {
  
  if (is.null(field)) {
    warning("No argument received by parameter 'field'. Returning NULL.")
    return(NULL)
  }
  
  # doen nog al die ander toetse hierso
  
  field <- gsub(pattern = "[[:blank:]]", replacement = "", x = field)
  field <- gsub(pattern = "[[:alpha:]]{1,}[[:punct:]]{1,}", replacement = "", x = field)
  field <- gsub(pattern = "[[:punct:]]{1,}[[:alpha:]]{1,}", replacement = "", x = field)
  field <- gsub(pattern = "[[:alpha:]]", replacement = "", x = field)
  field <- gsub(pattern = "^$", replacement = NA, x = field)
  field <- as.numeric(field)
  
  return(field)
}

#' Format integer-like character field
#' 
#' Function that receives an integer-like character field referring to a phone number and 
#' formats the field to remove leading zeros, punctuation marks, alphabetic characters and blanks 
#' as well as the 'plus sign' and extra numbers
#' 
#' @param field Vector to be converted to a character vector
#' @return Formatted field returned as a character vector
#' @export

format_phonenumber <- function(field = NULL) {
  if (is.null(field)) {
    warning("NULL object received by parameter 'field'. Returning NULL.")
    return(field)
  }
  
  hasPlus <- FALSE
  
  field <- as.character(field)
  
  if ("+" %in% substr(x = field, start = 1, stop = 1)) {hasPlus <- TRUE}
  field <- gsub(pattern = "[[:punct:]]", replacement = "", x = field)
  field <- gsub(pattern = "[[:alpha:]]", replacement = "", x = field)
  field <- gsub(pattern = "[[:blank:]]", replacement = "", x = field)
  if (hasPlus) {field <- paste("+", field, sep = "")}
  field <- gsub(pattern = "^0{2,}", replacement = "0", x = field)
  field <- gsub(pattern = "^$", replacement = NA, x = field)
  field[which(nchar(field) < 7)] <- NA
  
  return(field)
}


