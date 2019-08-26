#' @title chi.confounder.net
#' @description Function to graphically display chi2 test results to 
#' ID confounders. Requires a data frame with outcome and exposures 
#' such as the one provided by the chi.2.eksp function. Returns a list of network objects in pdf
#' @param df Data frame containing outcomes and exposures
#' @param outcome Character vector referring to the name of the outcome column
#' @param exposure Character vector referring to the name of the exposure column
#' @param outdir Character vector containing to output directory
#' @param stoor Logical that stores the file as pdf when TRUE
#' @param remPiq Logical that removes "Piqola_" from outcomes and exposures when TRUE
#' @export

chi.confounder.net <- function(df, 
                               outcome = "outcome", 
                               exposure = "exposure", 
                               outdir = "graphs/", 
                               stoor = TRUE, 
                               remPiq = FALSE){
        
        if (!require("igraph")) {
                message("Loading igraph")
                install.packages("igraph", dependencies = TRUE)
                if (!require("igraph")) stop("Load igraph manually")
        }
        
        if (remPiq){
        p.dfG = df
        p.dfG[, "outcome"] = gsub("piqola_", "", p.dfG[, "outcome"])
        p.dfG[, "exposure"] = gsub("piqola_", "", p.dfG[, "exposure"])
  outcomes = unique(p.dfG[,outcome])
  lst = lapply(outcomes, FUN=function(x) kry.uitkoms.df(x, df=p.dfG, remPiq = TRUE))
        }else{
                outcomes = unique(df[,outcome])
                lst = lapply(outcomes, FUN=function(x) kry.uitkoms.df(x, df=df, remPiq = FALSE))     
        }
  names(lst) = outcomes
  p.lst = lapply(lst, FUN=function(x) graph.data.frame(x[,c(outcome, exposure)]))
  names(p.lst) = outcomes
  lapply(1:length(p.lst), FUN=function(x)
    {
    if (stoor == TRUE){ #dev.new(); 
      pdf(file=paste(outdir , outcomes[[x]], ".conf.pdf", sep=""), width=12)}
    plot.igraph(p.lst[[x]], vertex.size = 6, vertex.size2 = 1, mark.border=0.25,edge.arrow.size=0.1, main=outcomes[[x]], vertex.label.dist = 0.47)
    if (stoor == TRUE) dev.off()
    }
  )
}

#' @title kry.uitkoms.df
#' @description Helper function for chi.confounder.net
#' @param varname Character vector of the variable name
#' @param outname Character vector referring to the name of the outcome column
#' @param expname Character vector referring to the name of the exposure column
#' @param df Data frame as used in chi.confounder.net
#' @param remPiq Logical that removes "Piqola_" from outcomes and exposures when TRUE
#' @export

kry.uitkoms.df <- function(varname = "town", outname = "outcome", expname = "exposure", df, remPiq = FALSE){
  index = apply(df, 1, FUN=function(x) length(grep(varname, x))!= 0)
  exposures = unique(unlist(df[index, expname]))
  if (remPiq){
  p.dfG = p.df
  p.dfG[, outname] = gsub("piqola_", "", p.dfG[, outname])
  p.dfG[, expname] = gsub("piqola_", "", p.dfG[, expname])
  dx = apply(p.dfG[,c(outname, expname)], 1, FUN=function(x) length(which(is.na(match(exposures, x)) == FALSE)) > 1)
  }else dx = apply(p.df[,c(outname, expname)], 1, FUN=function(x) length(which(is.na(match(exposures, x)) == FALSE)) > 1)
  df[dx,]
} 
