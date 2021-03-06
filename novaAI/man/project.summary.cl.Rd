% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project.summary.R
\name{project.summary.cl}
\alias{project.summary.cl}
\title{Project Summary CI}
\usage{
project.summary.cl(df, BM = "hh.BM.use", var = "total.winter.save",
  coef = 2.2099, pop = pop, group = haq$town)
}
\arguments{
\item{df}{Data frame containing BM data}

\item{BM}{The BM use variable as character vector}

\item{var}{The variable corresponding to seasonal savings}

\item{coef}{The coefficient used to calculate variable CO2 emissions}

\item{pop}{Data frame containing population data}

\item{group}{The grouping variable}
}
\description{
Creates project summary of BM use, coal use and Emission results
with Confidence Interval included
}

