#' Emission Reductions Table
#'
#' Function to make ER tables
#'
#' @param projectname name of file to create without the .tex
#' extension. If this option is not set, value/string of x (see above) is
#' printed in the top left corner of the table. Set title=&rdquo; to suppress this output. By
#' default the name of the project will be retrieved from the global environment
#' @param basedir The baseline directory as character vector
#' @param tabdir Directory to which tables are saved within the baseline directory
#' as character vector
#' @export

er.table=function(projectname = get("projectname", envir=sys.frame(which=0)),
                  basedir  = get("basedir", envir=sys.frame(which=0)),
                  tabdir   = get("tabdir", envir=sys.frame(which=0))){

  ER.tab=matrix(rbind(
  c(round(BE.w/1000,2),round(PE.w/1000,2),round(ER.w/1000,2)),
  c(round(BE.s/1000,2),round(PE.s/1000,2),round(ER.s/1000,2)),
  c(round(BE/1000,2),round(PE/1000,2),round(ER/1000,2))
  ),ncol=3,
              dimnames=list(c("Winter","Summer","Annual"),
                            c(" $BE$(tonnes $CO_2$) " ,
                              " $PE$(tonnes $CO_2$) " ,
                              " $ER$(tonnes $CO_2$)")
                            )
              )

latex(ER.tab,
      caption = "Emission Reductions",
      title = projectname,
      n.rgroup = c(2,1),
      file=paste(basedir,tabdir,"ERtab.tex",sep=""))

assign("ER.tab",ER.tab, envir=.GlobalEnv)
}
