# BM years
bm.years <- function(haq, fix.text = FALSE, verbose=FALSE,fixdate=TRUE){
  require(chron)
  # Capture the date of the interview
  if(fixdate == TRUE) haq$date.r=as.Date(haq$received)
  #haq$date.r = as.Date(haq$received, format= "%Y-%m-%d %H:%M:%S")
  if(verbose==TRUE) message("haq$date.r: ", paste(print(summary(haq$date.r)), " "))
  if(class(haq$bm.usage.start.month)== "character" | class(haq$bm.usage.start.month)== "factor") haq$bm.usage.start.month <- match(haq$bm.usage.start.month, month.name)
  if(verbose==TRUE) message("haq$bm.usage.start.month: ", paste(print(summary(haq$bm.usage.start.month)), " "))

haq$surveyyear <- years(haq$date.r)
  if(verbose==TRUE) message("haq$surveyyear: years= ", levels(haq$surveyyear)," n=", paste(print(summary(haq$surveyyear))," "))

if (fix.text == TRUE) {
  haq$year.of.initiation <- as.character(haq$year.of.initiation)
  haq[which(haq$year.of.initiation=="In 2011 (last year)"),"year.of.initiation"] <- 2011
  haq[which(haq$year.of.initiation=="Before 2011"),"year.of.initiation"] <- 2010
   if(verbose==TRUE) summary("haq$year.of.initiation: ", haq$year.of.initiation)
}

haq$bmstartyearmonth=paste(as.character(haq$year.of.initiation),
                           "-",
                           as.character(haq$bm.usage.start.month),"-15",sep="") # Combine year and month and standardise day to 15th
haq$bmstartyearmonth[which(haq$bmstartyearmonth=="NA-NA-15")] = NA
haq$bmstartyearmonth=as.Date(haq$bmstartyearmonth, format="%Y-%m-%d", origin="1970-01-01")# make it a date to use in calculation

  # A non BM user cannot have a start date
haq$bmstartyearmonth[which(haq$hh.BM.use=="Bottom Up")]=NA

haq$winterdays.used <- winterend - haq$bmstartyearmonth # List number of winter days in which BM was used
haq$winterdays.used <- ifelse(haq$winterdays.used > fullwinter, fullwinter, haq$winterdays.used)
haq$summerdays.used <- summerend - haq$bmstartyearmonth # List number of summer days in which BM was used
haq$summerdays.used <- ifelse(haq$summerdays.used > fullsummer, fullsummer, haq$summerdays.used)
haq[which(haq$coal.use.in.summer=="No"),"summerdays.used"] <- NA
haq$bmstartyearmonth <- as.Date(as.integer(haq$bmstartyearmonth),origin="1970-01-01")

# A BM user who for some reason do not have a startyear and month is set to the middle of winter 2010: 2010-07-01
haq$bmstartyearmonth[which(haq$hh.BM.use=="BM" & is.na(haq$bmstartyearmonth)==TRUE)]=midwinter

assign("haq", haq, envir=.GlobalEnv)
}
