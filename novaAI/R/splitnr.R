#' Split Number
#'
#' Isolate stand number without the letter at the end: remove the '^[[:alpha:]]*'
#' in the beginning to isolate the number only (i.e. strip letters before and after)
#'
#' @param x a character vector where matches are sought, or an object
#' which can be coerced by as.character to a character vector. Long vectors are supported.
#' @export

splitnr=function(x){
patt='(^[[:alpha:]]*[[:digit:]]*)([[:alpha:]]*)([[:space:]]*)([[:alpha:]]*)'
gsub(patt, '\\1', x)
	 }
