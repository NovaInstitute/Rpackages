% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readCodebook.R
\name{readCodebook}
\alias{readCodebook}
\title{Read CodeBook (Recoder)}
\usage{
readCodebook(cb, data, verbose = FALSE)
}
\arguments{
\item{cb}{Data frame containing codes and their corresponding meanings}

\item{data}{Data frame to which the codes should be applied to}

\item{verbose}{Displays function messages when TRUE}
}
\description{
Function is used to read a given codebook and apply the codes to variables present in the
supplied survey data. This function is sensitive to variable names
}

