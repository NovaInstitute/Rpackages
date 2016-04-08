#' Report Table
#'
#' Function to export a Latex table with caption and label
#'
#' @param x An R object of class found among methods(xtable).
#' See below on how to write additional method functions for xtable
#' @param caption Character vector of length 1 or 2 containing the table's
#' caption or title. If length is 2, the second item is the "short caption"
#' used when LaTeX generates a "List of Tables". Set to NULL to suppress
#' the caption. Default value is NULL
#' @param label Character vector of length 1 containing the LaTeX label or HTML anchor.
#' Set to NULL to suppress the label. Default value is NULL
#' @param directory Name of file where the resulting code should be saved.
#' If file="", output is displayed on screen. Note that the function
#' also (invisibly) returns a character vector of the results (which can be helpful for post-processing).
#' Default value is ""
#' @export

report.table=function(x,caption,label,directory){

        print(xtable(x, caption=as.character(caption), label=as.character(label),digits=2,
	             align=c("l",as.character(rep("r",length(data.frame(x)))))),
	             hline.after=c(-1,0,nrow(x)-1,nrow(x)),file=paste(as.character(directory),
	             as.character(label),".tex", sep=""))
}
