#' Confounder Chi-net
#' 
#' Function to graphically display chi2 test results to 
#' ID confounders. Requires a data frame with outcome and exposures 
#' such as the one provided by the chi.2.eksp function. Receives a data frame 
#' and returns a list of network objects
#' 
#' @param df Data frame
#' @param outcome Character vector
#' @param exposure Character vector
#' @param outdir Character vector referring to output directory
#' @param stoor Logical that stores the file as pdf
#' @export

########################################################################
# Confounder Chi-net
# graphically disply chi2 test results to ID confounders
# requires a df withn outcome ad exposures such as the one provided
# by the chi.2.eksp function. Neem 'n df en gee 'n lys van netwerk objekte
# (c) Nova Institute 2014
#########################################################################

chi.confounder.net <- function(df, outcome = "outcome", exposure = "exposure", outdir = "graphs/", stoor = TRUE){
        
        if (!require("igraph")) {
                message("Loading igraph")
                install.packages("igraph", dependencies = TRUE)
                if (!require("igraph")) stop("Load igraph manually")
        }
        
  # Maak 'n lys van dataframes doen vir elke uitkoms waar die uitkoms óf in die outcome of die exposure is
  outcomes = unique(df[,outcome])
  lst = lapply(outcomes, FUN=function(x) kry.uitkoms.df(x, df=df))
  names(lst) = outcomes
  p.lst = lapply(lst, FUN=function(x) graph.data.frame(x[,c(outcome, exposure)]))
  names(p.lst) = outcomes
  lapply(1:length(p.lst), FUN=function(x)
    {
    if (stoor == TRUE){ #dev.new(); 
      pdf(file=paste(outdir , outcomes[[x]], ".conf.pdf", sep=""), width=12)}
    plot.igraph(p.lst[[x]], vertex.size = 6, vertex.size2 = 1, mark.border=0.25,edge.arrow.size=0.1, main=outcomes[[x]])
    if (stoor == TRUE) dev.off()
    }
  )
}

#' HELPER FUNCTION
#' 
#' Helper function for chi.confounder.net
#' 
#' @param varname Character vector
#' @param outcome Character vector
#' @param expname Character vector
#' @param df Data frame as used in chi.confounder.net
#' @param verbose **verbose not used
#' @export

######## Helper functions
kry.uitkoms.df <- function(varname = "town", outname = "outcome", expname = "exposure", df, verbose=TRUE){
  index = apply(df, 1, FUN=function(x) length(grep(varname, x))!= 0)
  exposures = unique(unlist(df[index, expname]))
  dx = apply(p.df[,c("outcome","exposure")], 1, FUN=function(x) length(which(is.na(match(exposures, x)) == FALSE)) > 1)
  df[dx,]
} 
