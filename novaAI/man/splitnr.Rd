% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitnr.R
\name{splitnr}
\alias{splitnr}
\title{Split Number}
\usage{
splitnr(x)
}
\arguments{
\item{x}{a character vector where matches are sought, or an object
which can be coerced by as.character to a character vector. Long vectors are supported.}
}
\description{
Isolate stand number without the letter at the end: remove the '^[[:alpha:]]*'
in the beginning to isolate the number only (i.e. strip letters before and after)
}

