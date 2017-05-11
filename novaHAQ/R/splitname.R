#' SplitName
#'
#' A useful function for splitting names with undesired formatting
#'
#' @param x A character vector where matches are sought

splitname=function(x){
	patt='(^[[:print:]]+)([[:blank:]]{1})(\\([[:alnum:]]{8}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{4}-[[:alnum:]]{12}\\)$)'
	gsub(patt, '\\1', x)
	 	 }
