#' Accuracy
#'
#' Function to calculate the accuracy of a sample. The function returns an e value
#'
#' @param x A dataset
#' @param propvar The proportion variable as character vector
#' @param propopt The proportion option as character vector
#' @export

accuracy=function(x,propvar,propopt){
	n=length(x[,1])
	p=length(which(x[,paste(propvar)]==paste(propopt)))/n
	e=1.645 * sqrt(n/p*(1-p))
	e
	}
