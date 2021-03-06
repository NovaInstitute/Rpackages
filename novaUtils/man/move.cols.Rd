% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/move.col.R
\name{move.cols}
\alias{move.cols}
\title{Move columns}
\usage{
move.cols(df = NULL, colNames = NULL, colIdxx = NULL, verbose = FALSE)
}
\arguments{
\item{df}{Data frame containing data to be manipulated}

\item{colNames}{Vector containing the names of the columns to be moved}

\item{colIdxx}{Vector that refers to the index that the column must be moved to}

\item{verbose}{Logical that displays function messages when TRUE}
}
\value{
Returns the data frame with the columns moved to the specified indices
}
\description{
This function moves any number of columns in a given data frame to any desired indices at once
}
