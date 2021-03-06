% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capwords.R
\name{capwords}
\alias{capwords}
\title{capwords}
\usage{
capwords(s, strict = FALSE)
}
\arguments{
\item{strict}{If TRUE, all letters which are not the first letter of a word will
be forced to lower case.}

\item{x}{An object of type character containing word(s) of which the first letter
must be capitalised. Characters separated by one or more blanks are taken as
separate words.}
}
\description{
Capitalises the first letter of each word
}
\examples{
capwords("andy and sandy") returns "Andy And Sandy"
capwords(c("andy", "sandy")) returns c("Andy", "Sandy")
capwords(c("andy and tommy", "sandy")) returns c("Andy And Tommy", "Sandy")
}
