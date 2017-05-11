#' ER Discount Table
#'
#' Crates a table of calender year adjusted emission reductions in CO2
#'
#' @param projectname name of file to create without the .tex
#' extension. If this option is not set, value/string of x (see above) is
#' printed in the top left corner of the table. Set title=&rdquo; to suppress this output. By
#' default the name of the project will be retrieved from the global environment
#' @param basedir The baseline directory as character vector
#' @param tabdir Directory to which tables are saved within the baseline directory
#' as character vector
#' @export

er.disc.table <- function(projectname = get("projectname", envir=sys.frame(which=0)),
                          basedir  = get("basedir", envir=sys.frame(which=0)),
                          tabdir   = get("tabdir", envir=sys.frame(which=0))){

  ER.disc.tab <- data.frame("ER"=ER/1000,
                            "Winter ER"=ER.w/1000,
                            "Summer ER"=ER.s/1000,
                            "Months to discount"=ER.s/2/1000,
                            "May to Des ER" = ER.cal/1000)


  tmp=latex(ER.disc.tab,
        caption = "Calendar year adjusted Emission Reductions in $tCO_2$",
        label= "ER.disc.tab",
        rowname=NULL ,
        digits=2,
        title = projectname,
        file=paste(basedir,tabdir,"ERdisctab.tex",sep="")
        )
  message("ER Discount Tab Printed")
}

#' ER Jan April
#'
#' Function to discount last 4 summer months
#'
#' @param df Data frame containing HAQ survey data
#' @param bmvar The Basa Magogo household summer use variable as character vector
#' @param yearvar The variable referring to the year of initiation as character vector
#' @param summerconbaseyear The base summer year variable as character vector
#' @param summercurrentyear The current summer year variable as character vector
#' @param startyear The start year as a numeric
#' @param basedirr The baseline directory as character vector
#' @param tabdirr Directory to which tables are saved within the baseline directory
#' as character vector
#' @param year The year to appear in the caption of the emissions reduction output table as a numeric
#' @export

er.jan.april <- function(df=haq,
                         bmvar="hh.summer.BM.use",
                         yearvar="year.of.initiation.0",
                         summerconbaseyear="summerconbase.year",
                         summercurrentyear="summer.current.year",
                         startyear=2010,
                         basedirr=basedir,
                         tabdirr=tabdir,
                         year=2011){
  bm.idx <- match(bmvar,names(df))
  yr.idx <- match(yearvar,names(df))
  sby.idx <- match(summerconbaseyear,names(df))
  scy.idx <- match(summercurrentyear,names(df))

  n.bm.s.usr <- length(which(df[,bm.idx]=="BM"))
  message("n bm ", n.bm.s.usr)
  x.bm.s.usr <- n.bm.s.usr / length(df[,1])
  message("%bm ",100*x.bm.s.usr)

  n.ligit.bm.s.usr <- length(which(df[,bm.idx]=="BM" & df[,yr.idx]>=startyear ))
  message("legit.summer.users ", n.ligit.bm.s.usr)
  x.legit.s.users <- n.ligit.bm.s.usr / length(df[,1])
  message("legit.summer.users % of pop ", 100*x.legit.s.users)

  pop.legit <- x.legit.s.users*cl.alt[2]

  legit.idx <- which(df[,bm.idx]=="BM" & df[,yr.idx]>=startyear )

  jan.april.CBy.s <- mean(df[legit.idx,sby.idx],na.rm=TRUE)
  jan.april.CBc.s <- (jan.april.CBy.s*pop.legit) / 2 /1000
  jan.april.BE.s  <- jan.april.CBc.s*COEF

  assign("jan.april.CBc.s", jan.april.CBc.s, envir=.GlobalEnv)
  assign("jan.april.CBy.s", jan.april.CBy.s, envir=.GlobalEnv)
  assign("jan.april.BE.s",  jan.april.BE.s,  envir=.GlobalEnv)

  jan.april.CPy.s <- mean(df[legit.idx,scy.idx],na.rm=TRUE)
  jan.april.CPc.s <- (jan.april.CPy.s*pop.legit) /2 /1000
  jan.april.PE.s  <- jan.april.CPc.s*COEF

  assign("jan.april.CPc.s", jan.april.CPc.s, envir=.GlobalEnv)
  assign("jan.april.CPy.s", jan.april.CPy.s, envir=.GlobalEnv)
  assign("jan.april.PE.s", jan.april.PE.s, envir=.GlobalEnv)

  jan.april.ER.s <- jan.april.BE.s -jan.april.PE.s
  assign("jan.april.ER.s", jan.april.ER.s, envir=.GlobalEnv)

  jan.april.tab <- data.frame(rbind(
    Baseline=c("Average use or save"=jan.april.CBy.s, Users=pop.legit, Total=jan.april.CBc.s, Emission=jan.april.BE.s),
    Project=c("Average use or save"=jan.april.CPy.s,  Users=pop.legit, Total=jan.april.CPc.s, Emission=jan.april.PE.s),
    Reduction=c("Average use or save"=jan.april.CBy.s-jan.april.CPy.s, Users=pop.legit, Total=jan.april.CBc.s-jan.april.CPc.s,Emission=jan.april.ER.s)
        ))

  jan.april.tab$Test <- round(jan.april.tab$Average.use.or.save*jan.april.tab$Users*COEF/2/1000,2)==
                            round(jan.april.tab$Emission,2)

  assign("jan.april.tab", jan.april.tab, envir=.GlobalEnv)

  print(
    xtable(jan.april.tab,
           caption = paste("Emission reductions for January to April ",year,sep=""),
           label = "jan.april.tab",
           display = c("s","f","d","f","f","s")
     ), file=paste(basedirr,tabdirr,"jan.april.tab.tex",sep="")
    )

}


