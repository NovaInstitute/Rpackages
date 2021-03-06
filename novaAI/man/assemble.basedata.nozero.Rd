% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basedata.R
\name{assemble.basedata.nozero}
\alias{assemble.basedata.nozero}
\title{Assemble Basedata Nozero}
\usage{
assemble.basedata.nozero(df = haq, select.idx = which(haq$hh.BM.use ==
  "BM"), vars = c("response.id", "town", "surveyyear", "bm.usage.start.year",
  "bmstartyearmonth", "coal.buying.format.r", "coal.units.winter.before.bm",
  "coal.units.winter.after.bm", "current.coal.units.winter"),
  shortnames = c("response.id", "town", "surveyyear", "BM start year",
  "BM start date", "Coal format", "winter before BM", "winter after BM",
  "Current winter", "eef"))
}
\arguments{
\item{df}{Data frame containing HAQ survey data}

\item{select.idx}{The selection indices for people who use BM as a numeric}

\item{vars}{The variables under consideration for creating a new data frame that
will only contain their values for each selection index}

\item{shortnames}{Shorter names to replace the longer variable names}
}
\description{
Assemble the data needed to make the baseline calculation with zero values excluded
}

