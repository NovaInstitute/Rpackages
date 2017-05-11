#' Project variable Table
#'
#' Function to print summary of project variables and save it to the table directory
#' as specified in the global environment
#'
#' @note The function contains no arguments and relies on the haq data frame as well as
#' base and table directories being present in the global environmet in order to work.
#' @export

project.var.table <- function(){
projectvars=as.list(haq[which(haq$hh.BM.use=="BM"),
                        match(c("winter.current.year","summer.current.year","annual.current"),
                              names(haq))])

message("projectvars \n", str(projectvars))

label(haq$annual.current) <- "Adjusted annual seasonal summer coal use in kg"
projectnams=Hmisc::label(haq[which(haq$hh.BM.use=="BM"),
                      match(c("winter.current.year","summer.current.year","annual.current"),
                            names(haq))])

message("projectnams ", projectnams)

capture.output(tableContinuous(vars = projectvars,nams=projectnams,
                               stats = c("n", "min", "mean", "max", "na"),
                               cap="Project coal use", lab = "project",
                               longtable = FALSE,prec = 2),
               file=paste(basedir,tabdir,"projectcoalsummary.tex",sep="")
               )

message("Table written to ", paste(basedir,tabdir,"projectcoalsummary.tex",sep=""))
}
