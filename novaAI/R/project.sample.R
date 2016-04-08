#' Project Sample
#'
#' Creates a sample of relevant variables for inspection as well as a summary of project
#' emission variables
#'
#' @note The function contains no arguments and relies on the haq data frame as well as
#' base and table directories being present in the global environmet in order to work.
#' @export

project.sample=function()
 {
 # sample of relevnt variables for inspection
yearproject=haq[sample(which(haq$hh.BM.use=="BM"),5),c("hh.summer.BM.use","winter.current.year","summer.current.year","annual.current")]
names(yearproject)=gsub("\\.","\\ ",names(yearproject))
names(yearproject)=gsub("(winter)(*)","\\1\\ ",names(yearproject))
names(yearproject)=gsub("(summer)(*)","\\1\\ ",names(yearproject))

latex(yearproject,
      caption = "Calculation of annual project coal consumption (sample)" ,
      display = c("d","s",rep("d",3)),
      col.just = c("l","l","x{2cm}","x{2cm}","x{2cm}"),
      multicol = FALSE,
      numeric.dollar = FALSE,
      label = "yearproject",title="",
      digits = 4,
      rowlabel = "ID",
      file = paste(basedir,tabdir,"yearproject.tex",sep=""))
message("yearproject gedruk")

# summary of project emission variables
label(haq$annual.current) <- "Adjusted current annual coal use in kg"
projectvars=as.list(haq[which(haq$hh.BM.use=="BM"),match(c("winter.current.year","summer.current.year","annual.current"),names(haq))])
projectnams=Hmisc::label(haq[which(haq$hh.BM.use=="BM"),match(c("winter.current.year","summer.current.year","annual.current"),names(haq))])

capture.output(
  tableContinuous(vars = projectvars,
                  nams=projectnams,
                  stats = c("n", "min", "mean", "max", "na"),
                  cap="Project coal use", lab = "project",
                  longtable = FALSE,prec = 2),
  file=paste(basedir,tabdir,"projectcoalsummary.tex",sep=""))
}
