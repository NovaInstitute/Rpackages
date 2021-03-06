% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slotspol.R
\name{slotspol}
\alias{slotspol}
\title{Slots in Polygons}
\usage{
slotspol(pol, sl = "ID")
}
\arguments{
\item{pol}{An SPDF}

\item{sl}{The name of the slot. The operator takes a fixed name,
which can be unquoted if it is syntactically a name in the language. A slot
name can be any non-empty string, but if the name is not made up of letters,
numbers, and ., it needs to be quoted (by backticks or single or double quotes).
In the case of the slot function, name can be any expression that evaluates
to a valid slot in the class definition. Generally, the only reason to use
the functional form rather than the simpler operator is because the slot name has to be computed.}
}
\description{
Slots in polygons inside SPDF
}

