#' Survey Date Table
#'
#' Creates a table that statistically describe the survey dates per town
#'
#' @param haq Data frame containing HAQ survey data (retreives from global environment
#' by default)
#' @param basedir The base directory where files created are to be saved
#' @param tabdir The directory to which latex tables will be saved
#' @export

survey.date.table=function(haq=get("haq", envir=environment(survey.date.table)),
                           basedir = get("basedir", envir=environment(survey.date.table)),
                           tabdir =get("tabdir", envir=environment(survey.date.table) )){

dates=data.frame(as.Date(haq$received, format= "%d-%m-%Y %H:%M:%S"))
names(dates)<-"Survey date"

capture.output(tableDate(dates,print.pval=FALSE,
                         group = haq$town,
                         stats = c("n", "min", "mean", "max"),
                         cap="Survey dates per town", lab="surveydates"),
               file=paste(basedir,tabdir,"Surveydate.tex",sep=""))


}
