% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maak.vuur.R
\name{pl.pca}
\alias{pl.pca}
\title{Experimental Plotting Function}
\usage{
pl.pca(x = dpca$ind$coord[, 1], y = ddpca$ind$coord[, 2], tp = "p",
  sdif = "sdif", sdifval = 3, pcadf = cluscont, cl, clII, mn, nm, nmII,
  envr = zz, qplot = TRUE, verbose = TRUE, naam = "naam")
}
\arguments{
\item{x}{Coordinates of points in the plot. Alternatively, a single
plotting structure, function or any R object with a plot method can be provided}

\item{y}{The y coordinates of points in the plot, optional if x is an appropriate structure.}

\item{tp}{Character vector. The type of plot. Defaults to a point plot. Types are
"p" for points,
"l" for lines,
"b" for both,
"c" for the lines part alone of "b",
"o" for both overplotted,
"h" for histogram like (or high-density) vertical lines,
"s" for stair steps,
"S" for other steps,
"n" for no plotting.}

\item{sdif}{Character vector. Name of the column containing difference between wall and chimney temp}

\item{sdifval}{Numeric. Used in logical as minimum sdif}

\item{pcadf}{Data frame. The PCA data frame}

\item{cl}{Numeric. Contribution. To be used in the contribution slider}

\item{clII}{Numeric. Cos2 contribution. To be used in the cos2 > slider}

\item{mn}{Numeric. To be used for the month slider}

\item{nm}{Character vector. Contribution dimension to be used in picker.}

\item{nmII}{Character vector. Cos2 dimension to be used in picker.}

\item{envr}{Environment under consideration}

\item{qplot}{Logical. Create a qplot or not?}

\item{verbose}{Logical. Display function messages or not?}

\item{naam}{Character vector. Header for graphs}
}
\description{
Function is used to plot data output from the PCA
}

