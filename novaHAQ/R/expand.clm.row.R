#' Expand Coal Merchant Row
#'
#' Function expands the coal merchant rows for every format - town combination
#'
#' @param x Data frame containing caol merchant survey information
#' @param levelvar The variable that is to have its levels accessed
#' @param del.var The delivery variable as character vector
#' @param verbose Logical that displays function messages when TRUE
#' @details Use after clm$Delivery.suburbs_0 <- apply(clm,1, find.place.row)
#' @examples clm$town <- expand.clm.row(clm)
#' @export

expand.clm.row <- function(x = clm,
                           levelvar = haq$town,
                           del.var = "Delivery.suburbs_0",
                           verbose = TRUE){
if (is.data.frame(x)==FALSE) message("x is not a data frame: this is not going to work!")
if (is.na(match(del.var, names(x)))) message("No such column as ", del.var, ". This is not going to work")
l.var = levels(as.factor(levelvar))
if(verbose==TRUE) message("l.var: ", l.var)
if(verbose==TRUE) message("del.var: ", del.var)
z = strsplit(x[,del.var], split=" ")
if(verbose==TRUE) message("z: ", z, "\n 1:length(z) ", 1:length(z) )
idx = rep(1:length(z), sapply(z, length))
if(verbose==TRUE) message("idx: ", idx)
towns = unlist(sapply(as.list(x[del.var]), strsplit, split=" "))
if(verbose==TRUE) message("towns: ",towns)
x = x[idx, ]
x[,del.var] = towns
x
}
