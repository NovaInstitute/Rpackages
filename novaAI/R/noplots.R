#' No Plots
#'
#' Creates varible called noplots that provides suburbs that are not plotted
#'
#' @param x Data frame containing the stand numbers
#' @param standnumbervar The name of the variable that contains stand numbers as character vector
#' @param suburbvar The name of the variable that contains suburbs as character vector
#' @param SPx A SpatialPolygonsDataFrame containing geographic data
#' @param TEXT The name of the text variable as character vector
#' @export

noplots=function(x,standnumbervar="standnumber",suburbvar="suburb",SPx,TEXT="TEXT"){
y=data.frame(x)
standnumbers=y[,standnumbervar]
mapnumbers=SPx@data[,TEXT]
mapmatch=match(standnumbers,mapnumbers)
plotsubs=data.frame(table(x[which(is.na(mapmatch)==FALSE),match(suburbvar,names(y))]))
noplotsubs=data.frame(table(x[which(is.na(mapmatch)==TRUE),suburbvar]))
subs=cbind(plotsubs,noplotsubs)[-3]
colnames(subs)=c("Suburb","Plotted","Not Plotted")
noplots=x[which(is.na(mapmatch)==TRUE),standnumbervar]
}
