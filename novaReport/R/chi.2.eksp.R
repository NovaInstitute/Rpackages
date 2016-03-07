#' Chi.2.exploration
#' 
#' Chi.2.exploration with custom chi2 function
#' 
#' @param data  an R object (data set)
#' @param out.idx Integer referring to the index of the name that matches "piqola"
#' @param min.length Numeric referring to the minimum length
#' @param max.levels Numeric referring to the maximum amount of levels
#' @param ver.idx Integer used in forecasting
#' @param out.raw Logical to display raw list
#' @param out.raw.name Character vector containing the name of the raw list
#' @param out.df Logical to display data frame
#' @param out.df.name Character vector containing the name of the data frame
#' @param out.list Logical to display list
#' @param out.list.name Character vector containing the name of the list
#' @param p.cutoff Numeric that refers to the probability coëfficient
#' @param dig Numeric that refers to the number of digits
#' @param suiwer Logical to leave those that give warnings
#' @param verbose Logical to display messages
#' @param debug Logical for debugging process
#' @param xlsx Logical to write xlsx file
#' @param outdir Character vector that contains the output directory
#' @param xlsx.name Character vector containing the name of the written xlsx file
#' @param plot Logical whether to plot the data
#' @param net.name Character vector containing the name of the gml network diagram
#' @export

## chi.2.eksplorasie met aangepaste chi2 funksie onderaan ###

chi.2.eksp <- function(data = des,
                       out.idx = grep("piqola", names(data)),
                       min.length = 4*16, 
                       max.levels = 12,
                       ver.idx = NULL,
                       out.raw = FALSE,
                       out.raw.name = "out.raw",
                       out.df = TRUE, 
                       out.df.name = "p.df", 
                       out.list = TRUE, 
                       out.list.name = "spdf",
                       p.cutoff = 0.05, # standard p.value
                       dig = 3, # number of digits
                       suiwer = TRUE, # los die wat 'n warning gee
                       verbose = TRUE,
                       debug = TRUE,
                       xlsx = TRUE,
                       outdir = datadir, 
                       xlsx.name = "PIQOLA_chi2_uitkomste.xlsx",
                       plot = TRUE,
                       net.name = paste(naam, "chinet")){ 
  
  message("Jy benodig die withWarnings funksie vir hierdie ding om te werk\nJy benodig ook die openxlsx pakket as jy xlsx wil uitskryf en igraphs as jy die netwerk-grafiek wil hê")
  
  if (verbose == TRUE) {message("out.idx is ", length(out.idx),  " lank" )}
  dx <- which(apply(X = as.array(out.idx), MARGIN = 1, FUN = function(x) {sum(table(data[,x]))}) > min.length)
  #dx = which(sapply(data[,out.idx], function(x) {sum(table(x))}) > min.length)
  if (verbose == TRUE) message("out.idx is ", length(out.idx),  " lank en dx is ", paste(dx, " ") )
  out.idx = out.idx[dx]
  xx = sapply(data[out.idx], nlevels) >= 2
  if (verbose == TRUE) message("xx is ", paste(xx,  " ", sep=""))

    #which(sapply(data[out.idx], function(x) length(levels(as.factor(as.character(x)[which(table(x) != 0 & names(table(x)) != "")]))) >1))
  out.idx = out.idx[xx]
  if (verbose == TRUE) message("out.idx is ", paste(out.idx,  " ", sep=""))
 # if (verbose == TRUE) message("out.idx is ", length(out.idx),  " lank na lank genoeg filter en lank genoeg is ", paste(lank.genoeg," ") ,"\n")
  
  zz <- ((apply(X = as.array(out.idx), MARGIN = 1, FUN = function(idx) {nlevels(data[, idx])})) < max.levels)
  #zz <- sapply(data[,out.idx], nlevels) < max.levels
  if (verbose == TRUE) message("zz is ", paste(zz,  " ", sep=""), "\n")
  te.lank = na.omit(as.integer(which(zz == FALSE)))
  if (verbose == TRUE) message("te lank is ", length(te.lank), " lank en dis ", paste(te.lank, " ") )
  if (length(te.lank) > 0) {out.idx <- out.idx[- te.lank[!is.na(te.lank)]]}
  if (verbose == TRUE) message("out.idx is ", length(out.idx),  " lank na te lank filter")
  if (verbose == TRUE) message("out.idx is nou ", paste(out.idx,  " ", sep=""))
  
  if (length(out.idx) < 1) {
    warning("Hierdie veranderlike(s) is nie geskik vir 'n chi2 toets ens. nie. Ek stuur NULL terug.")
    return(NULL)
  }
  
  if (verbose == TRUE ) message("Uitkomste:\n", paste(names(data)[out.idx], "\n", sep=""))
  
  if (is.null(ver.idx) == TRUE) {ver.idx <- out.idx}
  if (verbose == TRUE) {message("ver.idx is ", length(ver.idx),  " lank")}
  if (verbose == TRUE ) {message("Voorspellers:\n", paste(names(data)[ver.idx], "\n", sep="")) }

  if (debug ==  TRUE){
    assign("out.idx", out.idx, envir=.GlobalEnv)
    assign("ver.idx", ver.idx, envir=.GlobalEnv)
  }
  
  z = apply(X = as.array(out.idx), MARGIN = 1, FUN = function(oidx) {
        x <- data[, oidx]
        apply(X = as.array(ver.idx), MARGIN = 1, FUN = function(vidx) {
          w <- data[, vidx]
          if (debug == TRUE) message("x ", levels(as.factor(x)))
          #if (debug == TRUE) message("w ", levels(as.factor(w)))
          if (sum(margin.table(table(x, w), 2)[1]) != 0 &
              sum(margin.table(table(x, w), 2)[2]) != 0 &
              sum(margin.table(table(x, w), 1)[1]) != 0 &
              sum(margin.table(table(x, w), 1)[2]) != 0 ) { 
                #returnobj <- withWarnings(my.chisq.test(x=x, y=w, correct=TRUE, simulate.p.value=FALSE))
                returnobj <- my.chisq.test(x=x, y=w, correct=TRUE, simulate.p.value=FALSE)
                return(returnobj)
          }
          return(NULL)
        })
      })
  if (length(z) == 1) z <- z[[1]]
  
  if (is.null(z)) {
    warning("x en w sou nie saamgewerk het vir die my.chisq.test nie, so ons kan niks verder doen nie. Stuur NULL terug.")
    return(NULL)
  }
  if (all(is.null(z))) {    
    warning("x en w sou nie saamgewerk het vir die my.chisq.test nie, so ons kan niks verder doen nie. Stuur NULL terug.")
    return(NULL)
  }
  
  if (debug == TRUE) assign("z", z, envir = .GlobalEnv)
  if (debug == TRUE) assign("ver.idx", ver.idx, envir = .GlobalEnv)
  if (debug == TRUE) assign("data", data, envir = .GlobalEnv)
  names(z) = names(data)[ver.idx]

  names(z) = paste(names(z),"__",sep="")
  
  if (out.raw == TRUE){
    if (verbose == TRUE ) message("Ek skryf die rou lys uit want jy het gevra. Sy naam is: ", out.raw.name , " en hy is ", length(z) , " items lank")
    assign(out.raw.name, z, envir=.GlobalEnv)
  }
  
  ### Maak die out.df
  #p.values  = unlist(z)[grep("p.value$", names(unlist(z)))]
  p.values  = unlist(z)[grep("p.value", names(unlist(z)), fixed = TRUE)]
  df        = unlist(z)[grep("df", names(unlist(z)))]
  statistic = unlist(z)[grep("statistic", names(unlist(z)))]
  n         = unlist(z)[grep("\\.n$", names(unlist(z)))]
  craemer   = unlist(z)[grep("craemer", names(unlist(z)))]
  out.names <- rep(names(data)[out.idx], each = length(ver.idx))
  ver.names <- rep(names(data)[ver.idx], times = length(out.idx))

  #warnings = unlist(lapply(z, lapply, "[", "Warnings"))
  #names(warnings) = gsub("__.","__",names(warnings))
  #waarsku  = rep("Geen", length(p.values))
  waarsku  = rep("None", length(p.values))
  #war.idx = match(gsub("Warnings", "", names(warnings)), gsub("Value.p.value", "", p.names))
#   war.idx = match(gsub("warnings", "", names(warnings)), gsub("Value.p.value", "", p.names))
#   if (any(!is.na(war.idx))) { waarsku[war.idx[!is.na(war.idx)]] = warnings[war.idx[!is.na(war.idx)]]}
    

  
  p.df <- data.frame(outcome = out.names,
                     exposure = ver.names, 
                     p.value = round(as.numeric(p.values), dig),
                     df = df, 
                     n = n, 
                     statistic = round(as.numeric(statistic), dig),
                     craemer = round(as.numeric(craemer), dig), 
                     warnings = waarsku
  )
  
  p.df = p.df[order(p.df$p.value),]
  rownames(p.df) = 1:nrow(p.df)
  if (verbose == TRUE) message("Dataframe van ", dim(p.df)[1], " by ", dim(p.df)[2], " gemaak\nName is: ", paste(names(p.df), " ", sep=" "))
  # hierdie is suboptimaal: gebruik transform
  # maak hom korter: 
  #if (suiwer == TRUE) p.df = p.df[which(p.df$warnings == "Geen"), ]
  if (suiwer == TRUE) p.df = p.df[which(p.df$warnings == "None"), ]
  p.cutoff.idx = which(p.df$p.value < p.cutoff)
  if (length(p.cutoff.idx) > 0){
          p.df = p.df[p.cutoff.idx, ]
          if (verbose) message("p.cutoff is ", p.cutoff)
  }else{
          stop("Nie een p waarde is groter as p.cutoff nie")
  }
  
        ref.idx = which(p.df$outcome != p.df$exposure)
        if (length(ref.idx) > 0){
        p.df = p.df[ref.idx, ] # remove self-reference
}
  
  if (out.df == TRUE){
    message("Ek skryf 'n dataframe uit want jy het gevra. Sy naam is ", out.df.name, "\nsy dimensies is", paste(dim(p.df), collapse = " by "))
    assign(out.df.name, p.df, envir=.GlobalEnv) 
  }
  
  ## Maak out.list
  spdf =  split(p.df, f=p.df$outcome)
  if (verbose == TRUE) message("\nspdf gemaak deur p.df te split volgens outcome. Sy lengte is ", length(spdf))
  if (verbose == TRUE & debug == TRUE) message("Resultaat-lys gemaak: lengte van ", length(spdf))
  nspdf = gsub("(^[[:print:]]+)_+[[:print:]]+\\.p\\.value", "\\1", names(spdf))
  if (verbose == TRUE) message("nspdf is :", paste(nspdf, " "))
  if (verbose == TRUE & debug == TRUE) message("Resultaat-lys gemaak: lengte van ", length(spdf))
  names(spdf) = nspdf
  if (verbose == TRUE) message("\nspdf se naam gegee")
  if (length(grep("\\.$", names(data))) > 0) {
    names(data) = gsub("\\.$", "" , names(data))
  }
  if (verbose == TRUE & debug == TRUE ) {
    message("\nnames spdf \n", paste(names(spdf), " "))
  }
  if (verbose == TRUE & debug == TRUE) {
    ##message("\ndata        \n", paste(names(data), " "))
  }
  if (verbose == TRUE & debug == TRUE) {
    message("\ngrep('\\.1$', names(spdf))", grep("\\.1$", names(spdf), value=TRUE))
  }
  if (length(grep("\\.1$", names(spdf))) > 0) {spdf = spdf[-grep("\\.1$", names(spdf))]} 
  if (verbose == TRUE & debug == TRUE) {message("\nspdf dim ", length(spdf))}
#   s.idx = lapply(data[,nspdf], FUN=function(x) {sum(table(x))})
#   if (verbose == TRUE & debug == TRUE) {message("\ns.idx ", s.idx)}
#   spdf = spdf[which(s.idx > 16*4)] # throw away the short variables
  
  if (verbose == TRUE) message("\nResultaat-lys: lengte van ", length(spdf))
    
  if (out.list == TRUE){
    if (verbose == TRUE) message("Ek skryf 'n lys uit want jy het gevra. Sy naam is ", out.list.name)
    assign(out.list.name, spdf, envir=.GlobalEnv) 
  }
  
  if (xlsx == TRUE){
    old.wd = getwd()
    setwd(outdir)
    library(openxlsx)
    write.xlsx(p.df,
               file=paste(xlsx.name, ".xlsx", sep=""),
               sheetName="p.values")
    if (verbose == TRUE) message("Ek skryf 'n xlsx uit\nSy naam is ", xlsx.name, " en hy sit in \n", outdir)
    setwd(old.wd)
  }
  
  if (plot == TRUE){
    require(igraph)
    if (nrow(p.df) > 1){
      g = graph.data.frame(p.df[,c("outcome",   "exposure")])
      if (debug == TRUE) assign("p.df", p.df, envir = .GlobalEnv)
      plot(g, vertex.size = 6, vertex.size2 = 1 , mark.border=0.25, edge.arrow.size=0.25)
      if(!exists("naam")) naam = "g"
      assign(net.name, g, envir=.GlobalEnv)
      write.graph(g, file = paste(datadir, net.name, ".graphml", sep=""), format = "graphml")
      if (verbose == TRUE) message("Ek skryf 'n gml netwerk diagram uit\nSy naam is ", net.name, " en hy sit in \n", outdir)
    }
  }
  
  if (!is.null(p.df)) {
    for (c in 1:ncol(p.df)) {
      if (is.numeric(p.df[, c])) {
        p.df[, c] <- round(x = p.df[, c], digits = 5)
      }  
    }
  }
  return(p.df)
}

#' HELPER FUNCTION
#' 
#' Helper function called my.chisq.test that also supplies n and craemer's v. Test this vs 
#' assocstats in vcd package
#' 
#' @param x Either a data frame or a matrix
#' @param y R object that x will be compared to
#' @param correct Logical that applies Yates continuity correction
#' @param p Integer that applies the replicate function over the length of x
#' @param rescale.p Logical to rescale the probability
#' @param simulate.p.value Logical that simulates a probability value
#' @param Numeric that describes the number of replicates the simulated p value is based on
#' @export

###################################### HELPER FUNCTION: my.chisq.test


#  my chi2 toets wat n en craemer se V ook gee. Teot hom teen assocstats in vcd pakket

#  my chi2 toets wat n en craemer se V ook gee. Teot hom teen assocstats in vcd pakket

my.chisq.test <- function (x, y = NULL, correct = TRUE, p = rep(1/length(x), length(x)), 
                           rescale.p = FALSE, simulate.p.value = FALSE, B = 2000) 
{
  DNAME <- deparse(substitute(x))
  if (is.data.frame(x)) 
    x <- as.matrix(x)
  if (is.matrix(x)) {
    if (min(dim(x)) == 1L) 
      x <- as.vector(x)
  }
  if (!is.matrix(x) && !is.null(y)) {
    if (length(x) != length(y)) 
      stop("'x' and 'y' must have the same length")
    DNAME2 <- deparse(substitute(y))
    xname <- if (length(DNAME) > 1L || nchar(DNAME, "w") > 
                   30) 
      ""
    else DNAME
    yname <- if (length(DNAME2) > 1L || nchar(DNAME2, "w") > 
                   30) 
      ""
    else DNAME2
    OK <- complete.cases(x, y)
    x <- factor(x[OK])
    y <- factor(y[OK])
    if ((nlevels(x) < 2L) || (nlevels(y) < 2L)) 
      stop("'x' and 'y' must have at least 2 levels")
    x <- table(x, y)
    names(dimnames(x)) <- c(xname, yname)
    DNAME <- paste(paste(DNAME, collapse = "\n"), "and", 
                   paste(DNAME2, collapse = "\n"))
  }
  if (any(x < 0) || any(is.na(x))) 
    stop("all entries of 'x' must be nonnegative and finite")
  if ((n <- sum(x)) == 0) 
    stop("at least one entry of 'x' must be positive")
  if (simulate.p.value) {
    setMETH <- function() METHOD <<- paste(METHOD, "with simulated p-value\n\t (based on", 
                                           B, "replicates)")
    almost.1 <- 1 - 64 * .Machine$double.eps
  }
  if (is.matrix(x)) {
    METHOD <- "Pearson's Chi-squared test"
    nr <- as.integer(nrow(x))
    nc <- as.integer(ncol(x))
    if (is.na(nr) || is.na(nc) || is.na(nr * nc)) 
      stop("invalid nrow(x) or ncol(x)", domain = NA)
    sr <- rowSums(x)
    sc <- colSums(x)
    E <- outer(sr, sc, "*")/n
    v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
    V <- outer(sr, sc, v, n)
    dimnames(E) <- dimnames(x)
    if (simulate.p.value && all(sr > 0) && all(sc > 0)) {
      setMETH()
      tmp <- .Call(C_chisq_sim, sr, sc, B, E)
      STATISTIC <- sum(sort((x - E)^2/E, decreasing = TRUE))
      PARAMETER <- NA
      PVAL <- (1 + sum(tmp >= almost.1 * STATISTIC))/(B + 
                                                        1)
    }
    else {
      if (simulate.p.value) 
        warning("cannot compute simulated p-value with zero marginals")
      if (correct && nrow(x) == 2L && ncol(x) == 2L) {
        YATES <- min(0.5, abs(x - E))
        if (YATES > 0) 
          METHOD <- paste(METHOD, "with Yates' continuity correction")
      }
      else YATES <- 0
      STATISTIC <- sum((abs(x - E) - YATES)^2/E)
      PARAMETER <- (nr - 1L) * (nc - 1L)
      PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    }
  }
  else {
    if (length(x) == 1L) 
      stop("'x' must at least have 2 elements")
    if (length(x) != length(p)) 
      stop("'x' and 'p' must have the same number of elements")
    if (any(p < 0)) 
      stop("probabilities must be non-negative.")
    if (abs(sum(p) - 1) > sqrt(.Machine$double.eps)) {
      if (rescale.p) 
        p <- p/sum(p)
      else stop("probabilities must sum to 1.")
    }
    METHOD <- "Chi-squared test for given probabilities"
    E <- n * p
    V <- n * p * (1 - p)
    STATISTIC <- sum((x - E)^2/E)
    names(E) <- names(x)
    if (simulate.p.value) {
      setMETH()
      nx <- length(x)
      sm <- matrix(sample.int(nx, B * n, TRUE, prob = p), 
                   nrow = n)
      ss <- apply(sm, 2L, function(x, E, k) {
        sum((table(factor(x, levels = 1L:k)) - E)^2/E)
      }, E = E, k = nx)
      PARAMETER <- NA
      PVAL <- (1 + sum(ss >= almost.1 * STATISTIC))/(B + 1)
    }
    else {
      PARAMETER <- length(x) - 1
      PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    }
  }
  N = sum(x)
  K = min(dim(x))
  CRAEMER = sqrt(STATISTIC / (sum(x) * (K-1)))
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  names(N) <- "n"
  names(K) <- "k"
  names(CRAEMER) <- "creamer"
  
  if (any(E < 5) && is.finite(PARAMETER))
    warning("Chi-squared approximation may be incorrect")
  WARN <- any(E < 5) && is.finite(PARAMETER)
  structure(list(statistic = STATISTIC, 
                 parameter = PARAMETER, 
                 p.value = PVAL, 
                 method = METHOD,
                 data.name = DNAME,
                 observed = x, 
                 expected = E, 
                 residuals = (x - E)/sqrt(E),
                 stdres = (x - E)/sqrt(V),
                 n = N,
                 warnings = WARN,
                 k = K,
                 craemer = CRAEMER
  ), 
  class = "htest")
}