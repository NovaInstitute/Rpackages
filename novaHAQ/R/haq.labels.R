#' Initial HAQ Labeling
#'
#' This fuction creates labels for different variables contained in a data frame. It is used
#' early in the process for formats that are needed immediately.
#'
#' @param haq Data frame that contains household data
#' @export


initial.labels <- function(haq=haq){
  if(is.na(match("hh.firemaker.id", names(haq))) == FALSE) label(haq$hh.firemaker.id)<-"Who makes the fire in the house?"
  if(is.na(match("hh.coal.ignition", names(haq))) == FALSE) label(haq$hh.coal.ignition)<-"Method used to ignite fire"
  if(is.na(match("multi.firemaker..coal.ignition", names(haq))) == FALSE) label(haq$multi.firemaker..coal.ignition)<-"Method used to ignite fire (multiple firemakers)"
  if(is.na(match("bm.saved.coal.winter", names(haq))) == FALSE) label(haq$bm.saved.coal.winter)<-"Has the use of BM helped you to save coal in winter"
  if(is.na(match("bm.saved.coal.summer", names(haq))) == FALSE) label(haq$bm.saved.coal.summer)<-"Has the use of BM helped you to save coal in summer"
  if(is.null(haq$coal.use)==FALSE) label(haq$coal.use)<-"Do you use coal in your home?"
  if(is.null(haq$contact.coal.use)==FALSE) label(haq$contact.coal.use)<-"Do you use coal in your home?"
  if(length(haq$historic.coal.user)>0) label(haq$historic.coal.user)<-"Did you use coal during the past three years?"

  assign("haq",haq,envir=.GlobalEnv)

}

#' HAQ Labels
#'
#' This function is used to create labels for all remaining variables contained in the HAQ survey data
#' frame considered to be in need of a label
#'
#' @param haq Data frame that contains survey data
#' @export

haq.labels <- function(haq=haq){
  if(is.na(match("date.r", names(haq))) == FALSE) label(haq$date.r) <- "Date of questionnaire"
  if(is.na(match("hh.surname", names(haq))) == FALSE) label(haq$hh.surname) <- "Household surname"
  if(is.na(match("respondent.name", names(haq))) == FALSE) label(haq$respondent.name) <- "Name of person with whom interview was conducted"

  if(is.na(match("hh.coal.device", names(haq))) == FALSE) label(haq$hh.coal.device) <- "Coal burning device"
  if(is.na(match("coal.merchant", names(haq))) == FALSE) label(haq$coal.merchant) <- "Coal merchant"
  if(is.na(match("coal.merchant.r", names(haq))) == FALSE) label(haq$coal.merchant.r) <- "Coal merchant"
  if(is.na(match("coal.buying.format", names(haq))) == FALSE) label(haq$coal.buying.format) <- "In which format do you buy coal?"
  if(is.na(match("purchase.frequency", names(haq))) == FALSE) label(haq$purchase.frequency) <- "How often do you buy coal?"
  if(is.na(match("coal.units.winter.before.bm", names(haq))) == FALSE) label(haq$coal.units.winter.before.bm) <- "Number of units of coal per WINTER month BEFORE BM"
  if(is.na(match("coal.units.winter.after.bm", names(haq))) == FALSE) label(haq$coal.units.winter.after.bm) <- "Number of units of coal per WINTER month directly AFTER BM"
  if(is.na(match("coal.saved.initial.winter", names(haq))) == FALSE) label(haq$coal.saved.initial.winter) <- "How many units did the respondent save per WINTER month due to BM?"
  if(is.na(match("current.coal.units.winter", names(haq))) == FALSE) label(haq$current.coal.units.winter) <- "How many units does the household use per WINTER month at the present time?"

  if(is.na(match("coal.use.in.summer", names(haq))) == FALSE) label(haq$coal.use.in.summer) <- "Do you use coal in summer?"
  if(is.na(match("coal.use.in.summer", names(haq))) == FALSE) label(haq$coal.use.in.summer) <- "Do you use coal in summer?"
  if(is.na(match("bm.saved.coal.summer", names(haq))) == FALSE) label(haq$bm.saved.coal.summer) <- "Has the use of BM helped you to save coal in summer"
  if(is.na(match("coal.units.summer.before.bm", names(haq))) == FALSE) label(haq$coal.units.summer.before.bm) <- "Number of units of coal per SUMMER month BEFORE BM"
  if(is.na(match("coal.units.summer.after.bm", names(haq))) == FALSE) label(haq$coal.units.summer.after.bm) <- "Number of units of coal per SUMMER month directly AFTER BM"
  if(is.na(match("coal.saved.initial.summer", names(haq))) == FALSE) label(haq$coal.saved.initial.summer) <- "How many units did the respondent save per SUMMER month due to BM?"
  if(is.na(match("current.coal.units.summer", names(haq))) == FALSE) label(haq$current.coal.units.summer) <- "How many units does the household use per SUMMER month at the present time?"
  if(is.na(match("year.of.initiation", names(haq))) == FALSE) label(haq$year.of.initiation) <- "In which year did you start using BM"
  if(is.na(match("bm.usage.start.month", names(haq))) == FALSE) label(haq$bm.usage.start.month) <- "In which month this year did you start using BM?"
  if(is.na(match("introduction.to.bm", names(haq))) == FALSE) label(haq$introduction.to.bm) <- "How did you learn about BM?"
  if(is.na(match("bm.demo.invitation.", names(haq))) == FALSE) label(haq$bm.demo.invitation.) <- "Was anyone in the household ever invited to a BM demonstration"#let op dubbel punt
  if(is.na(match("bm.demo.attendance", names(haq))) == FALSE) label(haq$bm.demo.attendance) <- "Has someone in the household ever attended a BM demonstration?"
  if(is.na(match("wood.format", names(haq))) == FALSE) label(haq$wood.format) <- "What format of wood do you mostly use?"
  if(is.na(match("wood.units", names(haq))) == FALSE) label(haq$wood.units) <- "What is the number of units of wood per month?"

  #LABELS FOR CREATED VARIABLES
  if(is.na(match("town", names(haq))) == FALSE) label(haq$town) <- "Town where survey was conducted"
  if(is.na(match("surveyyear", names(haq))) == FALSE) label(haq$surveyyear) <- "Year in which survey was conducted"
  if(is.na(match("bmstartyearmonth", names(haq))) == FALSE) label(haq$bmstartyearmonth) <- "Date that BM was started, day standardised to the 15th of the month"
  if(is.na(match("winterend", ls())) == FALSE)   label(winterend) <- "Date for the end of the winter, 31 Aug"
  if(is.na(match("winterdays.used", names(haq))) == FALSE)label(haq$winterdays.used) <- "Number of days that BM was used in the WINTER"
  if(is.na(match("fullwinter", ls())) == FALSE) label(fullwinter) <- "Number of days in the whole winter, 1 May until 31 Aug"
  if(is.na(match("summerend", ls())) ==FALSE) label(summerend) <- "Date for the end of the summer, 30 April"
  if(is.na(match("summerdays.used", names(haq))) == FALSE)label(haq$summerdays.used) <- "Number of days that BM was used in the SUMMER"
  if(is.na(match("fullsummer", ls())) == FALSE) label(fullsummer) <- "Number of days in the whole summer, 1 Sept until 30 April"
  if(is.na(match("kg", names(haq))) == FALSE) label(haq$kg) <- "associated weights in kg for formats"
  if(is.na(match("hh.BM.use", names(haq))) == FALSE) label(haq$hh.BM.use) <- "Number of households that use BM"
  if(is.na(match("winter.before.bm.kg", names(haq))) == FALSE) label(haq$winter.before.bm.kg) <- "Reported monthly winter use in kg before BM"
  if(is.na(match("totalwinter.before.bm.kg", names(haq))) == FALSE) label(haq$totalwinter.before.bm.kg) <- "Initial winter use per season before BM in kg"
  if(is.na(match("summer.before.bm.kg", names(haq))) == FALSE) label(haq$summer.before.bm.kg) <- "Reported monthly summer use in kg before BM"
  if(is.na(match("totalsummer.before.bm.kg", names(haq))) == FALSE) label(haq$totalsummer.before.bm.kg) <- "Initial summer use per season before BM in kg"
  if(is.na(match("winter.save.initial.kg", names(haq))) == FALSE) label(haq$winter.save.initial.kg) <- "REPORTED monthly winter savings in kg directly after starting BM"
  if(is.na(match("totalwinter.save.initial.kg", names(haq))) == FALSE) label(haq$totalwinter.save.initial.kg) <- "REPORTED seasonal winter savings in kg directly after starting BM"
  if(is.na(match("summer.save.initial.kg", names(haq))) == FALSE) label(haq$summer.save.initial.kg) <- "REPORTED monthly summer savings in kg directly after starting BM"
  if(is.na(match("totalsummer.save.initial.kg", names(haq))) == FALSE) label(haq$totalsummer.save.initial.kg) <- "REPORTED seasonal summer savings in kg directly after starting BM"
  if(is.na(match("winterdays.prop", names(haq))) == FALSE) label(haq$winterdays.prop) <- "Proportion of winter days in which BM was used (kg)"
  if(is.na(match("winter.current.kg", names(haq))) == FALSE) label(haq$winter.current.kg) <- "Reported current monthly winter use in kg"
  if(is.na(match("totalwinter.current.kg", names(haq))) == FALSE) label(haq$totalwinter.current.kg) <- "Reported current winter use per season"
  if(is.na(match("summerdays.prop", names(haq))) == FALSE) label(haq$summerdays.prop) <- "Proportion of summer days in which BM was used (kg)"
  if(is.na(match("summer.current.kg", names(haq))) == FALSE) label(haq$summer.current.kg) <- "Reported current monthly summer use in kg"
  if(is.na(match("totalsummer.current.kg", names(haq))) == FALSE) label(haq$totalsummer.current.kg) <- "Reported current summer use per season"
  if(is.na(match("winter.after.bm.kg", names(haq))) == FALSE) label(haq$winter.after.bm.kg) <- "Reported monthly winter use in units directly after starting BM"
  if(is.na(match("totalwinter.after.bm.kg", names(haq))) == FALSE) label(haq$totalwinter.after.bm.kg) <- "Reported winter use per season directly after starting BM in kg"
  if(is.na(match("summer.after.bm.kg", names(haq))) == FALSE) label(haq$summer.after.bm.kg) <- "Reported monthly summer use in kg directly after starting BM"
  if(is.na(match("totalsummer.after.bm.kg", names(haq))) == FALSE) label(haq$totalsummer.after.bm.kg) <- "Reported summer use per season directly after starting BM in kg"

  if(is.na(match("winterbase", names(haq))) == FALSE) label(haq$winterbase) <- "Calculated monthly winter baseline in kg"
  if(is.na(match("total.winterbase", names(haq))) == FALSE) label(haq$total.winterbase) <- "Calculated seasonal winter baseline use in kg"
  if(is.na(match("winter.save", names(haq))) == FALSE) label(haq$winter.save) <- "Savings per winter month in kg"
  if(is.na(match("calculated.winter.save", names(haq))) == FALSE) label(haq$calculated.winter.save) <- "Calculated savings per winter month in kg"
  if(is.na(match("total.calculated.winter.save", names(haq))) == FALSE) label(haq$total.calculated.winter.save) <- "Calculated seasonal winter savings in kg"
  if(is.na(match("diff1", names(haq))) == FALSE) label(haq$diff1) <- "Difference between reported initial monthly winter savings and calculated initial monthly winter savings"
  if(is.na(match("diff2", names(haq))) == FALSE) label(haq$diff2) <- "Difference between current monthly winter coal use in kg and reported monthly winter coal use directly after starting BM, (rising baseline)"
  if(is.na(match("total.winter.save", names(haq))) == FALSE) label(haq$total.winter.save) <- "Annual FULL winter savings in kg"
  if(is.na(match("winterconbase", names(haq))) == FALSE) label(haq$winterconbase) <- "Conservatively calculated monthly winter coal baseline in kg"

  if(is.na(match("winterconbase.year", names(haq))) == FALSE) label(haq$winterconbase.year) <- "Conservatively calculated seasonal winter coal baseline in kg"
  if(is.na(match("winter.current.year", names(haq))) == FALSE) label(haq$winter.current.year) <- "Adjusted current seasonal winter coal use in kg"
  if(is.na(match("winter.save.year", names(haq))) == FALSE) label(haq$winter.save.year) <- "Adjusted annual winter savings in kg"

  if(is.na(match("summerbase", names(haq))) == FALSE) label(haq$summerbase) <- "Calculated monthly summer baseline in kg"
  if(is.na(match("total.summerbase", names(haq))) == FALSE) label(haq$total.summerbase) <- "Calculated seasonal summer baseline use in kg"
  if(is.na(match("summer.save", names(haq))) == FALSE) label(haq$summer.save) <- "Savings per summer month in kg"
  if(is.na(match("calculated.summer.save", names(haq))) == FALSE) label(haq$calculated.summer.save) <- "Calculated savings per summer month in kg"
  if(is.na(match("total.calculated.summer.save", names(haq))) == FALSE) label(haq$total.calculated.summer.save) <- "Calculated seasonal summer savings in kg"
  if(is.na(match("diff3", names(haq))) == FALSE) label(haq$diff3) <- "Difference between reported initial monthly summer savings and calculated initial monthly summer savings"
  if(is.na(match("diff4", names(haq))) == FALSE) label(haq$diff4) <- "Difference between current monthly summer coal use in kg and reported monthly summer coal use directly after starting BM, (rising baseline)"
  if(is.na(match("total.summer.save", names(haq))) == FALSE) label(haq$total.summer.save) <- "Annual FULL summer savings in kg"
  if(is.na(match("summerconbase", names(haq))) == FALSE) label(haq$summerconbase) <- "Conservatively calculated monthly summer coal baseline in kg"

  if(is.na(match("summerconbase.year", names(haq))) == FALSE) label(haq$summerconbase.year) <- "Conservatively calculated seasonal summer coal baseline in kg"
  if(is.na(match("summer.current.year", names(haq))) == FALSE) label(haq$summer.current.year) <- "Adjusted current seasonal summer coal use in kg"
  if(is.na(match("summer.save.year", names(haq))) == FALSE) label(haq$summer.save.year) <- "Adjusted annual summer savings in kg"

  if(is.na(match("proportion.save.winter", names(haq))) == FALSE) label(haq$proportion.save.winter) <- "Monthly winter savings as proportion of monthly coal usage before BM"
  if(is.na(match("proportion.save.summer", names(haq))) == FALSE) label(haq$proportion.save.summer) <- "Monthly summer savings as proportion of monthly coal usage before BM"

  assign("haq",haq,envir=.GlobalEnv)
}
