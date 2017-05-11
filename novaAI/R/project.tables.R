#' Project Tables
#'
#' Function creates summary tables such as summer project coal use by town and also
#' assigns Project coal consumptions and emissions by town for different per season to
#' the global environment.
#'
#' @param haq Data frame containing HAQ survey data
#' @param Ualt Data frame containing alternative usage information
#' @export

project.tables=function(haq = get("haq", envir=sys.frame(which=0)),
                        Ualt = get("Ualt", envir=sys.frame(which=0))){

  # winter project coal use by town

  wintercurrent=summaryBy(winter.current.year~town,
                          data=haq[which(haq$hh.BM.use=="BM" ),],FUN=sumn)

  summaryBy.table(wintercurrent,varname="CPy.w",
                  fn="winterprojectbytown",
                  label="projectbytownwinter",
                  caption="Winter Project coal consumption by town")

  message("CPy.w = ",CPy.w)
  message("wintercurrent written to:", paste(basedir,tabdir,"winterprojectbytown",sep="") )

  # summer project coal use by town

  summercurrent=summaryBy(summer.current.year~town,
                          data=haq[which(haq$hh.BM.use=="BM" ),],FUN=sumn)


  summaryBy.table(summercurrent,varname="CPy.s",
                  fn="summerprojectbytown",
                  label="projectbytownsummer",
                  caption="Summer Project coal consumption by town")

  message("CPy.s = ", CPy.s)
  message("summercurrent writen to:", paste(basedir,tabdir,"summerprojectbytown",sep="") )

  # Annual project coal use

  yearcurrent=summaryBy(annual.current~town,
                          data=haq[which(haq$hh.BM.use=="BM"),],FUN=sumn)


  summaryBy.table(yearcurrent,varname="CPy",
                  fn="yearprojectbytown",
                  label="projectbytownyear",
                  caption="Annual Project coal consumption by town")

  message(cat("CPy = ",as.numeric(CPy)))
  message("yearcurrent written to:", paste(basedir,tabdir,"yearprojectbytown",sep="") )

# Total project coal use and emissions

  CPc.w = Ualt * CPy.w
  CPc.s = Ualt * CPy.s
  CPc = Ualt * CPy
message("\nEk gebruik die Ualt as ",  Ualt)


  PE.w = CPc.w*COEF
  PE.s = CPc.s*COEF
  PE = CPc*COEF

  assign("CPy",CPy,envir=.GlobalEnv)
  message(cat("CPy = ", CPy))

  assign("CPy.s",CPy.s,envir=.GlobalEnv)
  message(cat("CPy.s = ", CPy.s))

  assign("CPy.w",CPy.w,envir=.GlobalEnv)
  message(cat("CPy.w = ", CPy.w))

  assign("CPc.w",CPc.w,envir=.GlobalEnv)
  message(cat("CPc.w = ", CPc.w))

  assign("CPc.s", CPc.s,envir=.GlobalEnv)
  message(cat("CPc.s = ", CPc.s))

  assign("CPc",CPc,envir=.GlobalEnv)
  message(cat("CPc = ", CPc))

  assign("PE", PE,envir=.GlobalEnv)
  message(cat("PE = ", PE))

  assign("PE.w", PE.w,envir=.GlobalEnv)
  message(cat("PE.w = ", PE.w))

  assign("PE.s", PE.s,envir=.GlobalEnv)
  message(cat("PE.s = ", PE.s))

  assign("wintercurrent", wintercurrent,envir=.GlobalEnv)
  assign("summercurrent", summercurrent,envir=.GlobalEnv)
  assign("yearcurrent", yearcurrent,envir=.GlobalEnv)

  lst=list(wintercurrent,summercurrent,yearcurrent)
  list.l <- latex.list(lst,digits=4)

}

###Shorten coal merchant table#####

#latex(merc.perc.tab.short,#
#      title="Merchant")

#abbrv.perc.tab <- function(x=haq$coal.merchant.r,limit=5,basedir=basedir,tabdir=tabdir,latex=FALSE,outname="abbrv.perc.tab.short"){
#  abbrv.perc.tab=as.data.frame(cbind(Frequency=table(x, dnn=label(x)),
#                                     Percent=round(100*as.table(table(x))/sum(as.table(table(x))),2 ))
#                               )
#
#  abbrv.perc.tab.short  <-  merc.perc.tab[table(x)>limit,]
#
#if(latex==TRUE){
#    latex(abbrv.perc.tab.short,
#          title="Merchant",
#          caption=label(x),
#          label=label(x),
#          file=paste(basedir,tabdir,label(x),"abbrv.tex",sep="")
#          )
#  }
#
#  assign(outname,abbrv.perc.tab.short,envir=.GlobalEnv)
#}
