#' Season Dates
#'
#' Function is supplied with a year of implementation, whereby it assigns values to the global environment
#' that indicate the start, middle and end as well as the number of days for winter and summer.
#'
#' @param impyear The year of implementation as a numeric
#' @export

season.dates <- function(impyear=impyear){
  winterstart <-as.Date(paste(impyear,"-05-01",sep="")) # First day of winter for the implementation year
  midwinter <-as.Date(paste(impyear,"-07-01",sep="")) # Middle of winter
  winterend <- as.Date(paste(impyear,"-08-31",sep="")) # Last day of winter for the implementation year
  fullwinter<-as.numeric(winterend-winterstart) # number of days counted as winter
  summerstart <-as.Date(paste(impyear,"-09-01",sep="")) # Start of summer
  summerend <- as.Date(paste(impyear+1,"-04-30",sep="")) # Last day of summer for the  implementation year
  fullsummer<-as.numeric(summerend-summerstart) # number of days counted as summer

  assign("winterstart", winterstart, envir=.GlobalEnv)
  assign("midwinter", midwinter, envir=.GlobalEnv)
  assign("winterend", winterend, envir=.GlobalEnv)
  assign("fullwinter", fullwinter, envir=.GlobalEnv)
  assign("summerstart", summerstart, envir=.GlobalEnv)
  assign("summerend", summerend, envir=.GlobalEnv)
  assign("fullsummer", fullsummer, envir=.GlobalEnv)
}
