#' Report Table
#'
#' Function to export a LaTeX table with caption and label
#'
#' @param x The object you wish to export
#' @param caption Character vector of length 1 or 2 containing the table's caption or title
#' @param label Character vector of length 1 containing the the LaTeX label
#' @param directory Character vector containing the directory to which the table must be saved
#' @export

report.table=function(x,caption,label,directory){
	print(xtable(x, caption=as.character(caption), label=as.character(label),digits=2,align=c("l",as.character(rep("r",length(data.frame(x)))))), hline.after=c(-1,0,nrow(x)-1,nrow(x)),file=paste(as.character(directory),as.character(label),".tex", sep=""))
	}
