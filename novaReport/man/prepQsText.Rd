% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qs.text.R
\name{prepQsText}
\alias{prepQsText}
\title{prepQsText}
\usage{
prepQsText(df, nm, vars = c("question_name", "question_text", "section_name"))
}
\arguments{
\item{df}{tibble from tbl_questions}

\item{nm}{Character section_name to choose}

\item{vars}{Character. Columns in df to keep. Must include section_name}
}
\description{
vat tbl_questions en berei hom voor vir gebruik deur qs.text
}
