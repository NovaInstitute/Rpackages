% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baseline.emissions.table.R
\name{baseline.emissions.table}
\alias{baseline.emissions.table}
\title{Baseline Emissions table}
\usage{
baseline.emissions.table(CBc = get("CBc", envir = sys.frame(which = 1)),
  CBc.w = get("CBc.w", envir = sys.frame(which = 1)), CBc.s = get("CBc.s",
  envir = sys.frame(which = 1)), BE = get("BE", envir = sys.frame(which = 1)),
  BE.w = get("BE.w", envir = sys.frame(which = 1)), BE.s = get("BE.s", envir
  = sys.frame(which = 1)), COEF = get("COEF", envir = sys.frame(which = 1)),
  projectname = get("projectname", envir = sys.frame(which = 1)),
  basedir = get("basedir", envir = sys.frame(which = 1)),
  tabdir = get("tabdir", envir = sys.frame(which = 1)))
}
\arguments{
\item{CBc}{to COEF Data frames that contain the necessary data to create a baseline
emissions table}

\item{projectname}{Name of file to create without the .tex
extension. If this option is not set, value/string of x (see above)
is printed in the top left corner of the table. Set title=&rdquo; to suppress this output}

\item{basedir}{The base directory to store files in as character vector}

\item{tabdir}{The directory to which tables are saved in the base directory}
}
\description{
Funtion to write a baseline emissions LaTeX table
}

