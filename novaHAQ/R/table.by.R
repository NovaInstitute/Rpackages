# by.table function. gives table without 0 counts. very usefull to use as function with by() or tapply()

#' By Table Function
#'
#' Gives table without 0 counts. Very useful to use as function with by() or tapply()
#'
#' @param x The table to be manipulated
#' @export

table.by=function(x){
	table(x)[which(table(x)!=0)]
	}
