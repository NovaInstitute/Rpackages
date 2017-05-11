#' Stand Number Plot QC Function
#'
#' Match a list of stand numbers to the stand numbers in a geografic
#' file (like a SpatialPolygonsDataFrame) and prints a summary of whether
#' a suburb is plotted or not
#'
#' @param x Data frame containing the stand numbers
#' @param standnumbervar The name of the variable that corresponds to the stand number as character vector
#' @param suburbvar The name of the variable that corresponds to the suburb as character vector
#' @param SPx A SpatialPolygonsDataFrame containing geographic data
#' @param TEXT The name of the text variable as character vector
#' @export

nr.plot.report=function(x,standnumbervar="standnumber",suburbvar="suburb",SPx,TEXT="TEXT"){
y=data.frame(x)
standnumbers=y[,standnumbervar]
mapnumbers=SPx@data[,TEXT]
mapmatch=match(standnumbers,mapnumbers)
plotsubs=data.frame(table(x[which(is.na(mapmatch)==FALSE),match(suburbvar,names(y))]))
noplotsubs=data.frame(table(x[which(is.na(mapmatch)==TRUE),match(suburbvar,names(y))]))
subs=cbind(plotsubs,noplotsubs)[-3]
colnames(subs)=c("Suburb","Plotted","Not Plotted")
subs
}
