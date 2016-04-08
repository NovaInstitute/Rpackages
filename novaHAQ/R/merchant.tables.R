#' Helper Function Yapply
#'
#' A function provided by Romain Francois based on something by Thomas Lumley, used in the creation of
#' merchant tables
#'
#' @param X An atomic object
#' @param FUN function to apply
#' @param ... optional arguments to FUN
#' @export

yapply=function(X,FUN, ...) {
  index <- seq(length.out=length(X))
  namesX <- names(X)
  if(is.null(namesX)) namesX <- rep(NA,length(X))

  FUN <- match.fun(FUN)
  fnames <- names(formals(FUN))
  if( ! "INDEX" %in% fnames ){
    formals(FUN) <- append( formals(FUN), alist(INDEX=) )   }
  if( ! "NAMES" %in% fnames ){
    formals(FUN) <- append( formals(FUN), alist(NAMES=) )   }
  mapply(FUN,X,INDEX=index, NAMES=namesX,MoreArgs=list(...)) }

#' Merchant Tables
#'
#' Function to make coal merchant tables by town
#'
#' @param df Data frame containing coal survey data
#' @param coal.var The variable referring to coal use as character vector
#' @param town.var The variable referring to towns as character vector
#' @param merchant.var The variable referring to coal merchants as character vector
#' @param datadir The path of the data directory as character vector
#' @param limit Numeric containing a limiting number to be used in output
#' @param n Numeric used in creating the top5 dataset
#' @param append If TRUE the data are appended to the connection when creating latex tables
#' @param xlsx If tRUE the fuction writes an xlsx document for the top5 dataset
#' @export

merchant.tables <- function(df = haq,
                            coal.var = "coal.use",
                            town.var = "town",
                            merchant.var = "coal.merchant.r",
                            datadir=datadir,
                            limit =5,
                            n=5,
                            append = FALSE, xlsx = TRUE){
require(xtable)
require(Hmisc)

merch.sub = tapply(df[which(haq$coal.use=="YES" | haq$coal.use=="Yes"), merchant.var],
                 df[which(haq$coal.use=="YES" | haq$coal.use=="Yes"), town.var],
                 function(x) as.data.frame(table.by(data.frame(as.character(x))),row.names=NULL))

message("merch.sub defined")

merch.sub.perc=yapply(merch.sub,function(x)print(xtable
                                                 (data.frame(
                                                   #Merchant = names(x),
                                                   frequency = x,
                                                   percent = paste(perc.=round(x/sum(x)*100,2),"%",sep="")
                                                   ),
                                                  align ="lrr",
                                                  title =   paste("Coal merchants in",NAMES),
                                                  caption = paste("Coal merchants in",NAMES),
                                                  label =   paste("CoalMerchants",NAMES,sep="")
                                                  ),file=paste(datadir,NAMES,".tex",sep="")
                                                 )
                      )

message("Merchant tables for ",names(merch.sub.perc), " written")
assign("merch.sub",merch.sub,envir=.GlobalEnv)


merch.sub.short = lapply(merch.sub, function(x) x[x >= (ifelse(max(x) < limit, max(x), limit))])
assign("merch.sub.short",merch.sub.short ,envir=.GlobalEnv)


merch.sub.short.perc = yapply(merch.sub.short,function(x)print(xtable
                                                 (data.frame(
                                                   #Merchant = names(x),
                                                   frequency = x,
                                                   percent = paste(perc.=round(x/sum(x)*100,2),"%",sep="")
                                                 ),
                                                  align ="lrr",
                                                  title =   paste("Abbreviated coal merchants in",NAMES),
                                                  caption = paste("Abbreviated coal merchants in",NAMES),
                                                  label =   paste("Abbreviated Coal Merchants",NAMES,sep="")
                                                 ),file=paste(datadir,NAMES,"Abbr.tex",sep="")
                                                )
                      )



message("Abbreviated merchant tables for ",names(merch.sub.short), " written")

top5 <- lapply(merch.sub.short, function(x){
  z = data.frame(
    #Merchant = names(x),
    frequency = x,
    percent = paste(perc.=round(x/sum(x)*100,2),"%",sep="")
  )
  z = z[order(z$frequency, decreasing=TRUE)[1:n], ]
  z
})

merch.sub.perc=yapply(top5,function(x)print(xtable
                                                 (x,
                                                  align ="lrr",
                                                  title =   paste("Top five coal merchants in",NAMES),
                                                  caption = paste("Top five coal merchants in",NAMES),
                                                  label =   paste("Top5CoalMerchants",NAMES,sep="")
                                                 ),
                                            append = append,
                                            file = ifelse(append==FALSE, paste(datadir,NAMES,".top5.tex",sep=""), paste(datadir,"saam.top5.tex", sep=""))
)
)


if (xlsx == TRUE){
  require(openxlsx)
  write.xlsx(x=top5[[1]], file = paste(datadir,"top5.xlsx",sep=""), asTable=TRUE)
}



}
