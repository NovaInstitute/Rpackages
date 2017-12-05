### function to make coal merchant tables by town  ##

################################################################################################
##  You need the yapply function for this

##   This function was provided by Romain Francois based on something by Thomas Lumley:

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
  mapply(FUN, X, INDEX=index, NAMES=namesX, MoreArgs=list(...)) }

################################################################################################

##  Now the merchant tables

merchant.tables <- function(df = haq,
                            coal.var = "coal.use",
                            yesoption = "yes",
                            town.var = "town",
                            merchant.var = "coal.merchant.r",
                            datadir=datadir,
                            limit =5,
                            n=5,
                            append = FALSE, xlsx = TRUE, debug = FALSE){
require(xtable)
require(Hmisc)

merch.sub = tapply(df[which(df[ ,coal.var]== yesoption), merchant.var],
                 df[which(df[ , coal.var] ==yesoption), town.var],
                 function(x) table.by(data.frame(as.character(x))))
if (debug) assign("merch.sub", merch.sub, envir = .GlobalEnv)

message("merch.sub defined")

merch.sub.perc = yapply(merch.sub,function(x)print.xtable(
  xtable(data.frame(Merchant = names(x),
                    frequency = as.numeric(x),
                    percent = paste(perc.=round(x/sum(x)*100,2),"%",sep="")
                    ),
         include.rownames = FALSE,
         align ="llrr",
         title =   paste("Coal merchants in",NAMES),
         caption = paste("Coal merchants in",NAMES),
         label =   paste("CoalMerchants",NAMES,sep="")),
  file=paste(datadir,NAMES,".tex",sep="")
  )
  )

message("Merchant tables for ",names(merch.sub.perc), " written")
assign("merch.sub",merch.sub,envir=.GlobalEnv)


merch.sub.short = lapply(merch.sub, function(x) x[x >= (ifelse(max(x) < limit, max(x), limit))])
if (debug) assign("merch.sub.short",merch.sub.short ,envir=.GlobalEnv)


merch.sub.short.perc = yapply(merch.sub.short,function(x)print(xtable
                                                 (data.frame(
                                                   Merchant = names(x),
                                                   frequency = as.numeric(x),
                                                   percent = paste(perc.=round(x/sum(x)*100,2),"%",sep="")
                                                 ),
                                                  align ="llrr",
                                                  title =   paste("Abbreviated coal merchants in",NAMES),
                                                  caption = paste("Abbreviated coal merchants in",NAMES),
                                                  label =   paste("Abbreviated Coal Merchants",NAMES,sep="")
                                                 ),file=paste(datadir,NAMES,"Abbr.tex",sep="")
                                                )
                      )


message("Abbreviated merchant tables for ", names(merch.sub.short), " written")

top5 <- lapply(merch.sub.short, function(x){
  z = data.frame(
    Merchant = names(x),
    frequency = as.numeric(x),
    percent = paste(perc.=round(x/sum(x)*100,2),"%",sep="")
  )
  if (nrow(z) < n) n = nrow(z)
  z = z[order(z$frequency, decreasing=TRUE)[1:n], ]
  z
})

if (debug) assign("top5", top5, envir = .GlobalEnv)
if (debug) assign("merch.sub.short", merch.sub.short, envir = .GlobalEnv)

merch.sub.perc = yapply(top5,function(x)print(xtable
                                                 (x,
                                                  align ="llrr",
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
