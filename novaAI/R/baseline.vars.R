#' Baseline Variables
#'
#' Creates summary tables for variables related to BM use
#'
#' @param haq Data frame containing HAQ survey data
#' @param basedata Data frame containing baseline data
#' @param cl.alt Data frame containing 90 percent confidence
#' interval data for the estimation of BM users attributable to the project
#' activity in the Target Area
#' @param basedir The baseline directory as character vector
#' @param tabdir Directory to which tables are saved withing the baseline directory
#' as character vector
#' @param meth The method to be used, "old" or "new"
#' @note By default this function will retrieve all of its information from the global environment
#' @export

baseline.vars=function(haq      = get("haq", envir=sys.frame(which=0)),
                       basedata = get("basedata", envir=sys.frame(which=0)),
                       cl.alt   = get("cl.alt", pos = sys.frame(which=0)),
                       basedir  = get("basedir", envir=sys.frame(which=0)),
                       tabdir   = get("tabdir", sys.frame(which=0)),
                       meth     = ifelse(exists("meth", where=sys.frame(which=-2)),
                                         get("meth", sys.frame(which=-2)),
                                         c("old", "new")[1])){

message("Die diepte hier is ", sys.nframe()," en sy ouer is ", sys.parent(), "\n")
# Winter
if(is.na(match("bm.saved.coal.winter.0",names(haq)))==TRUE){
based=summaryBy(winterconbase.year~town,
                data=haq[which(haq$hh.BM.use=="BM"  &
                  (haq$bm.saved.coal.winter!="recent" |
                  haq$bm.saved.coal.winter!="I have only started recently and cannot tell exactly")
                               ),],
                FUN=sumn) } else {
based=summaryBy(winterconbase.year~town,
                  data=haq[which(haq$hh.BM.use=="BM"  &
                    (haq$bm.saved.coal.winter.0!="recent" |
                    haq$bm.saved.coal.winter.0!="I have only started recently and cannot tell exactly")
                               ),],
                                  FUN=sumn)
                }
names(based) <- gsub("conbase",".conbase",names(based))

assign("based",based,envir=.GlobalEnv)

summaryBy.table(based,varname="CBy.w",fn="basebytown",label="basebytown",caption="Winter baseline by town")

assign("CBy.w",CBy.w,envir=.GlobalEnv)
message("CBy.w= ",CBy.w)

# Summer

if(is.na(match("bm.saved.coal.winter.0",names(haq)))==TRUE){
summerbased=summaryBy(summerconbase.year~town,
                      data=haq[which(haq$hh.BM.use=="BM"
                                     & (haq$bm.saved.coal.winter!="recent" |
                                        haq$bm.saved.coal.winter!="I have only started recently and cannot tell exactly")
                                     ),],
                      FUN=sumn) } else {
summerbased=summaryBy(summerconbase.year~town,
                       data=haq[which(haq$hh.BM.use=="BM"
                                      & (haq$bm.saved.coal.winter.0!="recent" |
                                         haq$bm.saved.coal.winter.0!="I have only started recently and cannot tell exactly")
                                                             ),],
                                              FUN=sumn)
                                        }

names(summerbased) <- gsub("conbase",".conbase",names(summerbased))

assign("summerbased",summerbased,envir=.GlobalEnv)

summaryBy.table(summerbased,varname="CBy.s",
                label="basebytownsummer",
                fn="summerbasebytown",
                caption="Summer baseline by town")

assign("CBy.s",CBy.s,envir=.GlobalEnv)

message("CBy.s= ",CBy.s)

# Annual
if(is.na(match("bm.saved.coal.winter.0",names(haq)))==TRUE){
yearbased=summaryBy(annual.base~town,
                    data=haq[which(haq$hh.BM.use=="BM"
                                   & (haq$bm.saved.coal.winter!="recent" |
                                     haq$bm.saved.coal.winter!="I have only started recently and cannot tell exactly")
                                   )
                                   ,],FUN=sumn)
                                          } else {
  yearbased=summaryBy(annual.base~town,
                      data=haq[which(haq$hh.BM.use=="BM"
                                     & (haq$bm.saved.coal.winter.0!="recent" |
                                       haq$bm.saved.coal.winter.0!="I have only started recently and cannot tell exactly")
                                    )
                               ,],FUN=sumn)
                                                  }

assign("yearbased",yearbased,envir=.GlobalEnv)

summaryBy.table(yearbased,varname="CBy",
                fn="yearbasebytown",
                label="basebytownyear",
                caption="Annual baseline by town")
assign("CBy",CBy,envir=.GlobalEnv)

message("CBy= ",CBy)

# Make Baseline table for report with formulae and results

Winter=c(round(sum(based[[3]]),2),
         cl.alt[1,"point prop."],
         sum(based$winter.conbase.year.n-based$winter.conbase.year.NA.),
         sum(based$winter.conbase.year.NA.),
         length(haq[,1]),
         CBy.w)
Summer=c(round(sum(summerbased[,"summer.conbase.year.sum"]),2),
         cl.alt[1,"point prop."],
         sum(summerbased$summer.conbase.year.n-summerbased$summer.conbase.year.NA.),
         sum(summerbased$summer.conbase.year.NA.),
         length(haq[,1]),
         CBy.s)
Annual=c(round(sum(yearbased[,"annual.base.sum"]),2),
         cl.alt[1,"point prop."],
         sum(based$winter.conbase.year.n-based$winter.conbase.year.NA.),
         sum(based$winter.conbase.year.NA.),
         length(haq[,1]),
         round(CBy,2))

base.formula.tab=rbind(Winter, Summer, Annual)
Season=base.formula.tab
latex(Season,
      colheads = c("$\\sum_{i}\\sum_{j}C_{B,i,j,y}\\times\\mathit{FC}_{i}$",
                   "$XU_{alt,y}$",
                   "$n_{alt}$",
                   "$n.NA$",
                    "$n$",
                   "$\\bar{C}_{B,y}$"),
      rowname = c("Winter","Summer","Annual"),
      n.rgroup = c(2,1),
      digits=3,
      label  = "Average-baseline-for",
      caption = "Average seasonal and annual baseline coal consumption",
      file = paste(basedir,tabdir,"BaseFormulaTab.tex",sep=""))

# Total baseline coal use
if (meth == "new"){
  Ualt = (cl.alt[1,"point estimate"])
  CBc.w = Ualt * CBy.w
  CBc.s = Ualt * CBy.s
  CBc   = Ualt * CBy
  message("\nEk gebruik die puntberamer\n")
} else {
  Ualt = (cl.alt[1,"low estimate"])
  CBc.w = Ualt * CBy.w
  CBc.s = Ualt * CBy.s
  CBc   = Ualt * CBy
  message("\nEk gebruik die laer vertrouensinterval\n")
}


assign("Ualt", Ualt, envir=.GlobalEnv)
assign("CBc.w", CBc.w, envir=.GlobalEnv)
assign("CBc.s", CBc.s, envir=.GlobalEnv)
assign("CBc", CBc, envir=.GlobalEnv)


if(is.na(match("bm.saved.coal.winter.0",names(haq)))==TRUE){
lst=list(summaryBy(winterconbase.year~town+coal.buying.format,data=haq[which(haq$hh.BM.use=="BM" & haq$bm.saved.coal.winter!="recent"),],FUN=sumn),
summaryBy(summerconbase.year~town+coal.buying.format,data=haq,FUN=sumn),
summaryBy(annual.base~town+coal.buying.format,data=haq[which(haq$hh.BM.use=="BM" & haq$bm.saved.coal.winter!="recent"),],FUN=sumn))
latex.list(lst,digits=4) }

}
