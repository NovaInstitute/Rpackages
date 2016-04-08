#' Unfactor
#'
#' Converts a factor into numeric
#'
#' @param x The factor to be converted
#' @export

unfactor <- function(x){if (is.factor(x)) as.numeric(unclass(levels(x)))[as.numeric(x)]  else x }

#' Assemble Basedata
#'
#' Assemble the data needed to make the baseline calculation
#'
#' @param df Data frame containing HAQ survey data
#' @param select.idx The selection indices for people who use BM as a numeric
#' @param beforevar The variable referring to coal use before BM as character vector
#' @param aftervar The variable referring to coal use after BM as character vector
#' @param currentvar The variable referring to current coal use as character vector
#' @param vars The variables under consideration for creating the new data frame for only
#' the selection indices as a character vector
#' @param shortnames Shorter names to replace the longer variable names
#' @param verbose Displays function messages when TRUE
#' @export

assemble.basedata=function(df=haq,
                           select.idx = which(haq$hh.BM.use=="BM"),
                           beforevar  = "coal.units.winter.before.bm.0" ,
                           aftervar   = "coal.units.winter.after.bm.0",
                           currentvar = "current.coal.units.winter.0",
                           vars=c("response.id","town","surveyyear","bm.usage.start.year.0","bmstartyearmonth",
                                  "coal.buying.format.r","coal.units.winter.before.bm.0","coal.units.winter.after.bm.0",
                                  "current.coal.units.winter.0"),
                           shortnames=c("response.id","town","surveyyear","BM start year","BM start date","Coal format",
                                        "winter before BM","winter after BM","Current winter","eef"),
                           verbose=FALSE){

x=df[select.idx,vars]
  x[,beforevar] = unfactor(x[,beforevar])
  x[,aftervar]  = unfactor(x[,aftervar])
  x[,currentvar]= unfactor(x[,currentvar])
if(verbose==TRUE) message("dim(basedata) = " , dim(x))

x[which(is.na(x[,beforevar])==TRUE), beforevar] = x[which(is.na(x[,beforevar])==TRUE), currentvar]
x[which(is.na(x[,aftervar])==TRUE), aftervar] = x[which(is.na(x[,aftervar])==TRUE), currentvar]
x["eef"] = x[,beforevar] / x[, aftervar]
names(x)=shortnames
x
}

#' Assemble Basedata Nozero
#'
#' Assemble the data needed to make the baseline calculation with zero values excluded
#'
#' @param df Data frame containing HAQ survey data
#' @param select.idx The selection indices for people who use BM as a numeric
#' @param vars The variables under consideration for creating a new data frame that
#' will only contain their values for each selection index
#' @param shortnames Shorter names to replace the longer variable names
#' @export

assemble.basedata.nozero=function(df=haq,select.idx=which(haq$hh.BM.use=="BM"),
                                  vars=c("response.id","town","surveyyear","bm.usage.start.year",
                                         "bmstartyearmonth","coal.buying.format.r","coal.units.winter.before.bm",
                                         "coal.units.winter.after.bm","current.coal.units.winter"),
                                  shortnames=c("response.id","town","surveyyear","BM start year","BM start date",
                                               "Coal format","winter before BM","winter after BM","Current winter","eef")){
x=df[select.idx,vars]
x$"coal.units.winter.before.bm"[which(is.na(x$"coal.units.winter.before.bm")==TRUE)]=x$"current.coal.units.winter"
x$"coal.units.winter.after.bm"[which(is.na(x$"coal.units.winter.after.bm")==TRUE)]=x$"current.coal.units.winter"
x$"eef"=x$"coal.units.winter.before.bm" / x$"coal.units.winter.after.bm"
names(x)=shortnames
x
}

#' Assemble Basedata Simple
#'
#' Create a simple assembly of the data needed to make the baseline calculation
#'
#' @param df Data frame containing HAQ survey data
#' @param select.idx The selection indices for people who use BM as a numeric
#' @param vars The variables under consideration for creating a new data frame that
#' will only contain their values for each selection index
#' @param shortnames Shorter names to replace the longer variable names
#' @export

assemble.basedata.simple=function(df=haq,select.idx=which(haq$hh.BM.use=="BM"),
                                  vars=c("response.id","town","surveyyear","bm.usage.start.year.0",
                                         "bmstartyearmonth","coal.buying.format.r","coal.units.winter.before.bm.0",
                                         "coal.units.winter.after.bm.0","current.coal.units.winter.0"),
                                  shortnames=c("response.id","town","surveyyear","BM start year",
                                               "BM start date","Coal format","winter before BM","winter after BM",
                                               "Current winter","eef")){
x=df[select.idx,vars]
x$"eef"=x$"coal.units.winter.before.bm.0" / x$"coal.units.winter.after.bm.0"
names(x)=shortnames
x
}

#' Assemble QC Data
#'
#' Assemble the data needed to make the baseline calculation
#'
#' @param df Data frame containing HAQ survey data
#' @param select.idx The selection indices for people who use BM as a numeric
#' @param vars The variables under consideration for creating a new data frame that
#' will only contain their values for each selection index
#' @param shortnames Shorter names to replace the longer variable names
#' @export

assemble.qcdata=function(df=haq,select.idx=which(df[,"hh.BM.use"]=="BM"),
                         vars=c("response.id","contact.number.0","respondent.name.0" ,"hh.surname.0",
                                "town","fieldworker","surveyyear","hh.coal.use.r","hh.BM.use",
                                "bm.saved.coal.winter.r","bm.usage.start.year.0","bmstartyearmonth",
                                "coal.buying.format.r","coal.units.winter.before.bm.0","coal.units.winter.after.bm.0",
                                "current.coal.units.winter.0"),
                         shortnames=c("response.id","Contact number","Name","Surname","town","Fieldworker",
                                      "surveyyear","Coal use","BM use","BM saved coal winter","BM start year",
                                      "BM start date","Coal format","winter before BM","winter after BM",
                                      "Current winter","eef")){
x=df[select.idx,vars]
x$"coal.units.winter.before.bm.0"[which(is.na(x$"coal.units.winter.before.bm.0")==TRUE)]=x$"current.coal.units.winter.0"
x$"coal.units.winter.after.bm.0"[which(is.na(x$"coal.units.winter.after.bm.0")==TRUE)]=x$"current.coal.units.winter.0"
x$"eef"=x$"coal.units.winter.before.bm.0" / x$"coal.units.winter.after.bm.0"
names(x)=shortnames
x
}

#' Assemble QC Data 2
#'
#' Assemble the data needed to make the baseline calculation
#'
#' @param df Data frame containing HAQ survey data
#' @param select.idx The selection indices for people who use BM as a numeric
#' @param vars The variables under consideration for creating a new data frame that
#' will only contain their values for each selection index
#' @param shortnames Shorter names to replace the longer variable names
#' @export

assemble.qcdata.2=function(df=haq,select.idx=which(df[,"hh.BM.use"]=="BM"),
                           vars=c("response.id","contact.number.0","respondent.name.0" ,"hh.surname.0",
                                  "town","fieldworker","surveyyear","hh.coal.use.r","hh.BM.use",
                                  "bm.saved.coal.winter.r","bmstartyearmonth","bmstartyearmonth",
                                  "coal.buying.format.r","coal.units.winter.before.bm.0","coal.units.winter.after.bm.0",
                                  "current.coal.units.winter.0"),
                           shortnames=c("response.id","Contact number","Name","Surname","town",
                                        "Fieldworker","surveyyear","Coal use","BM use","BM saved coal winter",
                                        "BM start year","BM start date","Coal format","winter before BM",
                                        "winter after BM","Current winter","eef")){
x=df[select.idx,vars]
x$"coal.units.winter.before.bm.0"[which(is.na(x$"coal.units.winter.before.bm.0")==TRUE)]=x$"current.coal.units.winter.0"
x$"coal.units.winter.after.bm.0"[which(is.na(x$"coal.units.winter.after.bm.0")==TRUE)]=x$"current.coal.units.winter.0"
x$"eef"=x$"coal.units.winter.before.bm.0" / x$"coal.units.winter.after.bm.0"
names(x)=shortnames
x
}

#' Assemble Basedata CMM
#'
#' Assemble the data needed to make the baseline calculation
#'
#' @param df Data frame containing HAQ survey data
#' @param select.idx The selection indices for people who use BM as a numeric
#' @param vars The variables under consideration for creating a new data frame that
#' will only contain their values for each selection index
#' @param shortnames Shorter names to replace the longer variable names
#' @export

assemble.basedata.cmm=function(df=haq,select.idx=which(haq$hh.BM.use=="BM"),
                               vars=c("response.id","town","surveyyear","year.of.initiation.0",
                                      "bmstartyearmonth","coal.buying.format.r","coal.units.winter.before.bm.0",
                                      "coal.units.winter.after.bm.0","current.coal.units.winter.0"),
                               shortnames=c("response.id","town","surveyyear","BM start year","BM start date",
                                            "Coal format","winter before BM","winter after BM","Current winter","eef")){
x=df[select.idx,vars]
x$"coal.units.winter.before.bm.0"[which(is.na(x$"coal.units.winter.before.bm.0")==TRUE)]<-x$"current.coal.units.winter.0"
x$"coal.units.winter.after.bm.0"[which(is.na(x$"coal.units.winter.after.bm.0")==TRUE)]<-x$"current.coal.units.winter.0"
x$"eef"<-x$"coal.units.winter.before.bm.0" / x$"coal.units.winter.after.bm.0"
names(x)=shortnames
x
}

#' Assemble Basedata CMM Nozero
#'
#' Assemble the data needed to make the baseline calculation with zero values excluded
#'
#' @param df Data frame containing HAQ survey data
#' @param select.idx The selection indices for people who use BM as a numeric
#' @param vars The variables under consideration for creating a new data frame that
#' will only contain their values for each selection index
#' @param shortnames Shorter names to replace the longer variable names
#' @export

assemble.basedata.cmm.nozero=function(df=haq,select.idx=which(haq$hh.BM.use=="BM"),
                                      vars=c("response.id","town","surveyyear","year.of.initiation","bmstartyearmonth",
                                             "coal.buying.format","coal.units.winter.before.bm",
                                             "coal.units.winter.after.bm","current.coal.units.winter"),
                                      shortnames=c("response.id","town","surveyyear","BM start year",
                                                   "BM start date","Coal format","winter before BM","winter after BM",
                                                   "Current winter","eef")){
x=df[select.idx,vars]
x$"coal.units.winter.before.bm"[which(is.na(x$"coal.units.winter.before.bm")==TRUE)]<-x$"current.coal.units.winter"
x$"coal.units.winter.after.bm"[which(is.na(x$"coal.units.winter.after.bm")==TRUE)]<-x$"current.coal.units.winter"
x$"eef"<-x$"coal.units.winter.before.bm" / x$"coal.units.winter.after.bm"
names(x)=shortnames
x
}

#' Assemble Basedata CMM Simple
#'
#' Create a simple assembly of the data needed to make the baseline calculation
#'
#' @param df Data frame containing HAQ survey data
#' @param select.idx The selection indices for people who use BM as a numeric
#' @param vars The variables under consideration for creating a new data frame that
#' will only contain their values for each selection index
#' @param shortnames Shorter names to replace the longer variable names
#' @export

assemble.basedata.cmm.simple=function(df=haq,select.idx=which(haq$hh.BM.use=="BM"),
                                      vars=c("response.id","town","surveyyear","year.of.initiation.0",
                                             "bmstartyearmonth","coal.buying.format.r","coal.units.winter.before.bm.0",
                                             "coal.units.winter.after.bm.0","current.coal.units.winter.0"),
                                      shortnames=c("response.id","town","surveyyear","BM start year","BM start date",
                                                   "Coal format","winter before BM","winter after BM","Current winter",
                                                   "eef")){
x=df[select.idx,vars]
#x$"coal.units.winter.before.bm.0"[which(is.na(x$"coal.units.winter.before.bm.0")==TRUE)]<-x$"current.coal.units.winter.0"
#x$"coal.units.winter.after.bm.0"[which(is.na(x$"coal.units.winter.after.bm.0")==TRUE)]<-x$"current.coal.units.winter.0"
x$"eef"<-x$"coal.units.winter.before.bm.0" / x$"coal.units.winter.after.bm.0"
names(x)=shortnames
x
}
