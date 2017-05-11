#' Baseline Emissions table
#'
#' Funtion to write a baseline emissions LaTeX table
#'
#' @param CBc to COEF Data frames that contain the necessary data to create a baseline
#' emissions table
#' @param projectname Name of file to create without the .tex
#' extension. If this option is not set, value/string of x (see above)
#' is printed in the top left corner of the table. Set title=&rdquo; to suppress this output
#' @param basedir The base directory to store files in as character vector
#' @param tabdir The directory to which tables are saved in the base directory
#' @export

baseline.emissions.table <- function(CBc    = get("CBc", envir=sys.frame(which=1)),
                                     CBc.w  = get("CBc.w", envir=sys.frame(which=1)),
                                     CBc.s  = get("CBc.s", envir=sys.frame(which=1)),
                                     BE     = get("BE", envir=sys.frame(which=1)),
                                     BE.w   = get("BE.w", envir=sys.frame(which=1)) ,
                                     BE.s   = get("BE.s", envir=sys.frame(which=1)),
                                     COEF   = get("COEF", envir=sys.frame(which=1)),
                                     projectname = get("projectname", envir=sys.frame(which=1)),
                                     basedir  = get("basedir", envir=sys.frame(which=1)),
                                     tabdir   = get("tabdir", envir=sys.frame(which=1))
                                     ){
base.emission.tab=matrix(rbind(
  c(round(CBc.w/1000,2),round(COEF,4),round(BE.w/1000,2)),
  c(round(CBc.s/1000,2),round(COEF,4),round(BE.s/1000,2)),
  c(round(CBc/1000,2),round(COEF,4),round(BE/1000,2))
  ),ncol=3,
                         dimnames=list(c("Winter","Summer","Annual"),
                                       c("$C_{B,y}$(tonnes coal)",
                                         "$COEF$",
                                         "$BE_{y}$(tonnes $CO_{2}$)")
                                       )
                         )

assign("base.emission.tab",base.emission.tab,envir=.GlobalEnv)

latex(base.emission.tab,
      caption = "Baseline Emissions",
      label="Baseline-Emissions",
      title = projectname,
      n.rgroup = c(2,1),
      file=paste(basedir,tabdir,"BaseEmissionTab.tex",sep="")
      )
}
