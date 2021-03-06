% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numeric_table.R
\name{numeric_table}
\alias{numeric_table}
\title{Numeric Table}
\usage{
numeric_table(
  nvars,
  verbose = FALSE,
  forcegvar = FALSE,
  varSizeC = "0.15",
  levSizeC = "0.15",
  ...
)
}
\arguments{
\item{nvars}{Data frame containing continuous variables}

\item{verbose}{Logical to display function messages}

\item{forcegvar}{Logical that forces a chosen grouping variable}

\item{varSizeC}{Character vector that contains the percentage column size to be
attributed to the column containing variables. This is used to customize tables.}

\item{levSizeC}{Character vector that contains the percentage column size to be
attributed to the column containing levels. This is used to customize tables.}

\item{...}{Arguments passed down from the calling function}
}
\description{
This function checks for any numeric values in a set data frame, isolates them and provides
a LaTex table of descriptive statistics seperately per group and jointly for all observations, 
per variable
}
