#' Refactor
#'
#' Function drops unneeded factor levels
#'
#' @param x Object to have its levels dropped
#' @export

refact=function(x){
factor(as.character(x))
	}

#' Refactor Data Frame
#'
#' Function drops unneeded factor levels of a data frame
#'
#' @param x Object to have its levlels dropped
#' @export

refact.df=function(x){
	for(i in 1:length(x)) x[,i]=refact(x[,i])
	x
	}
