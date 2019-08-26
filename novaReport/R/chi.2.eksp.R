#' @title chi.2.eksp
#' @descriptin Chi.2.eks with custom chi2 function that generates a chi2 statistic as well as a craemer's v
#' for a given input. The output is displayed in terms of outputs and exposures in the form of the data frame p.df
#' and list spdf
#' @param data an R object (data set)
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



# ## FOR DEBUGGING PURPOSES ONLY!!! COMMENT OUT BEFORE USE >>>
# # args for chi.2.eksp
#
# min.length = 4*16
# max.levels = 12
# out.raw = FALSE
# out.raw.name = "out.raw"
# out.df = TRUE
# out.df.name = "p.df"
# out.list = TRUE
# out.list.name = "spdf"
# dig = 3 # number of digits
# suiwer = TRUE # los die wat 'n warning gee
# verbose = TRUE
# xlsx = TRUE
# write.igraph = FALSE
#
# ## <<< ------------------------------------------------------

## chi.2.eksplorasie met aangepaste chi2 funksie onderaan ###
chi.2.eksp <- function(data = qol2,
                       out.fields = c(),
                       ver.fields = c(),
                       out.idx = "body_",
                       ver.idx = "sol_",
                       min.length = 4*16,
                       max.levels = 12,
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
                       debug = F,
                       xlsx = TRUE,
                       outdir = datadir,
                       xlsx.name = "PIQOLA_chi2_uitkomste.xlsx",
                       plot = TRUE,
                       write.igraph = FALSE,
                       net.name = paste(naam, "chinet")) {


  message("Jy benodig die withWarnings funksie vir hierdie ding om te werk\nJy benodig ook die openxlsx pakket as jy xlsx wil uitskryf en igraphs as jy die netwerk-grafiek wil he")

  require(novaReport)
  if (xlsx) {require(openxlsx)}
  if (write.igraph) {require(igraph)}

  # toets die argumente
  if ((length(out.fields) == 0) & (length(out.idx) == 0)) {
    stop("Spesifiseer asseblief out.fields of out.idx.")
  }
  if ((length(ver.fields) == 0) & (length(ver.idx) == 0)) {
    ver.fields <- out.fields
    ver.idx <- out.idx
  }

  # stel out.idx op
  ## (preference is given to out.field's argument (as opposed to out.idx's argument))
  if (length(out.fields) > 0) {
    out.idx <- which(names(data) %in% out.fields)
    if (length(out.idx) < 1) {
      stop("Ek kon geen van die velde in out.fields in die datastel kry nie.")
    }
  } else {
    out.idx.pattern <- out.idx
    out.idx <- grep(paste("^", out.idx, sep = "", collapse = "|" ), names(data))
    if (length(out.idx) < 1) {
      stop(sprintf("Ek kon geen velde in die datastel kry wat met '%s' begin nie.", out.idx.pattern))
    }
  }

  if (debug) {
    for (i in 1:length(out.idx)) message(names(data)[out.idx[i]])
  }

  if (verbose == TRUE) {message("out.idx is ", length(out.idx),  " lank" )}



  # stel ver.idx op
  ## (preference is given to ver.fields's argument (as opposed to ver.idx's argument))
  if (length(ver.fields) > 0) {
    ver.idx <- which(names(data) %in% ver.fields)
    if (length(ver.idx) < 1) {
      stop("Ek kon geen van die velde in ver.fields in die datastel kry nie.")
    }
  } else {
    ver.idx.pattern <- ver.idx
    ver.idx <- grep(paste("^", ver.idx, sep = "", collapse = "|" ), names(data))
    if (length(ver.idx) < 1) {
      stop(sprintf("Ek kon geen velde in die datastel kry wat met '%s' begin nie.", ver.idx.pattern))
    }
  }

  if (debug) {
    for (i in 1:length(ver.idx)) message(names(data)[ver.idx[i]])
  }

  if (verbose == TRUE) {message("ver.idx is ", length(ver.idx),  " lank" )}
  if (verbose == TRUE ) {message("Voorspellers:\n", paste(names(data)[ver.idx], "\n", sep="")) }


  # toets vir genoeg waarnemings in elk van die uitkomste velde en skrap die wat te min het
  dx <- which(sapply(X = out.idx, FUN = function(idx) {sum(table(data[,idx]))}) > min.length)
  #dx = which(sapply(data[,out.idx], function(x) {sum(table(x))}) > min.length)
  if (verbose == TRUE) message("out.idx is ", length(out.idx),  " lank en dx is ", paste(dx, " ") )
  out.idx = out.idx[dx]

  # toets vir genoeg faktorvlakke in elk van die uitkomste velde en skrap die wat te min het
  xx = sapply(data[out.idx], nlevels) >= 2
  if (verbose == TRUE) message("xx is ", paste(xx,  " ", sep=""))
  #which(sapply(data[out.idx], function(x) length(levels(as.factor(as.character(x)[which(table(x) != 0 & names(table(x)) != "")]))) >1))
  out.idx = out.idx[xx]
  if (verbose == TRUE) message("out.idx is ", paste(out.idx,  " ", sep=""))
  # if (verbose == TRUE) message("out.idx is ", length(out.idx),  " lank na lank genoeg filter en lank genoeg is ", paste(lank.genoeg," ") ,"\n")

  # toets vir te veel faktorvlakke in elk van die uitkomste velde en skrap die wat te veel het
  zz <- sapply(data[out.idx], nlevels) < max.levels
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

  if (verbose == TRUE ) {message("Uitkomste:\n", paste(names(data)[out.idx], "\n", sep=""))}

  if (debug ==  TRUE){
    assign("out.idx", out.idx, envir=.GlobalEnv)
    assign("ver.idx", ver.idx, envir=.GlobalEnv)
  }

  # ctch <- sapply(out.idx, FUN = function(oidx) {
  #   if (is.factor(qol2[[oidx]])){
  #     levels <- levels(qol2[[oidx]])
  #
  #     qol2[[oidx]] <<- as.character(qol2[[oidx]])
  #     idxxNAs <- which(is.na(qol2[[oidx]]))
  #
  #     qol2[idxxNAs, oidx] <<- "NA"
  #
  #     idxxNAs <- which(is.na(qol2[[oidx]]))
  #
  #     print(length(which(is.na(levels))))
  #     levels[which(is.na(levels))] <- "NA"
  #
  #     qol2[[oidx]] <<- factor(x = qol2[[oidx]], levels = levels)
  #
  #     print(levels)
  #   }
  # }); rm(ctch)
  #



  # if (debug) {
  #   oidx <- out.idx[1]
  #   vidx <- ver.idx[1]
  # }
  #
  #


  z <- lapply(X = out.idx, FUN = function(oidx) {

    if (debug) {
      print(sprintf("oidx = %d", oidx))
      print(names(data)[oidx])
    }

    lsVers <- lapply(X = ver.idx, FUN = function(vidx) {

      if (debug) {
        print(sprintf("vidx = %d", vidx))
        print(names(data)[vidx])
      }

      idxx <- which(complete.cases(data[, c(oidx,vidx)]))
      if (length(idxx) < 1) {
        return(NULL)
      }
      if (debug) {message(sprintf("Number of complete cases: %d", length(idxx)))}
      x <- data[idxx, oidx]
      w <- data[idxx, vidx]

      tbleXW <- table(x,w)
      if ((ncol(tbleXW) < 2) | (nrow(tbleXW) < 2)) {
        return(NULL)
      }

      if (debug == TRUE) message(vidx, " of length ", length(ver.idx))
      if (debug == TRUE) message("x ", levels(as.factor(x)))
      if (debug == TRUE) message("w ", levels(as.factor(w)))

      # if (sum(margin.table(table(x, w), 2)[1]) != 0 &
      #     sum(margin.table(table(x, w), 2)[2]) != 0 &
      #     sum(margin.table(table(x, w), 1)[1]) != 0 &
      #     sum(margin.table(table(x, w), 1)[2]) != 0 ) {

      returnobj <- withWarnings(my.chisq.test(x = x,
                                              y = w,
                                              correct = TRUE,
                                              simulate.p.value = FALSE))

      #returnobj <- my.chisq.test(x=x, y=w, correct=TRUE, simulate.p.value=FALSE)

      return(returnobj)

      # } else {
      #    return(NULL)
      #  }
    })

    names(lsVers) <- names(data)[ver.idx]
    lsVers <- lsVers[which(!sapply(X = lsVers, FUN = is.null))]

    return(lsVers)
  })

  if (is.null(z)) {
    warning("x en w sou nie saamgewerk het vir die my.chisq.test nie, so ons kan niks verder doen nie. Stuur NULL terug.")
    return(NULL)
  }

  names(z) <- names(data)[out.idx]

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
  p.names = gsub("__.","__",names(unlist(z))[grep("p.value$", names(unlist(z)))])

  #warnings = unlist(lapply(z, lapply, "[", "Warnings"))
  #names(warnings) = gsub("__.","__",names(warnings))
  #waarsku  = rep("Geen", length(p.values))

  #waarsku   = unlist(z)[grep("Warnings", names(unlist(z)))]
  waarsku  = rep(NA_character_, length(p.values))


  #war.idx = match(gsub("Warnings", "", names(warnings)), gsub("Value.p.value", "", p.names))
  #   war.idx = match(gsub("warnings", "", names(warnings)), gsub("Value.p.value", "", p.names))
  #   if (any(!is.na(war.idx))) { waarsku[war.idx[!is.na(war.idx)]] = warnings[war.idx[!is.na(war.idx)]]}

  # maak sy name reg
  p.names = gsub("__p\\.value", "", p.names)
  p.names = gsub("\\.Value\\.", "\\.", p.names)
  p.names = gsub("\\._", "_", p.names)
  p.names = gsub("___\\.", "__", p.names)
  p.values = as.numeric(p.values)
  # maar doen dieselfde vir data
  names(data) = gsub("\\._", "_", names(data))
  names(data) = gsub("___\\.", "__", names(data))

  p.df = data.frame(var.names = p.names,
                    p.value = round(as.numeric(p.values), dig),
                    df = df,
                    n = n,
                    statistic = round(as.numeric(statistic), dig),
                    craemer = round(as.numeric(craemer), dig),
                    warnings = waarsku)

  if (nrow(p.df) < 1) {return(NULL)}

  p.df$var.names = as.character(p.df$var.names)
  p.df = p.df[order(p.df$p.value),]
  rownames(p.df) = 1:nrow(p.df)
  if (verbose == TRUE) {
    message("Dataframe van ",
            dim(p.df)[1],
            " by ",
            dim(p.df)[2],
            " gemaak\nName is: ",
            paste(names(p.df),
                  " ",
                  sep=" "))}

  # # hierdie is suboptimaal: gebruik transform
  # if (length(unique(names(data)[out.idx])) > 1){
  #   outcome = do.call("rbind", sapply(p.df$var.names, FUN=function(x) strsplit(x, split="__")))[,1]
  # } else {
  #   outcome = rep(names(data)[out.idx], each = nrow(p.df))
  # }
  # names(outcome) = NULL
  #if (length(out.idx) >= 1) p.df$outcome = names(data)[out.idx]

  # split the var.names field into 'exposure' and 'outcome' fields
  corrSplits <- strsplit(x = p.df$var.names, split = "_{2,}")
  outcomes <- sapply(X = corrSplits, FUN = function(splits) {return(splits[2])})
  outcomes <- gsub(pattern = ".p.value", replacement = "", x = outcomes)
  exposure <- sapply(X = corrSplits, FUN = function(splits) {return(splits[1])})
  p.df$outcome <- outcomes
  p.df$var.names <- exposure
  names(p.df) <- gsub(pattern = "var.names", replacement = "exposure", x = names(p.df))
  p.df <- move.col(df = p.df, colName = "outcome", colIdx = 2)

  # remove the rows where the var and the outcome are the same
  idxx <- which(p.df$exposure == p.df$outcome)
  if (length(idxx) > 0) {
    p.df <- p.df[-idxx,]
  }

  p.df = p.df[,c("outcome", "exposure", "n", "df", "statistic", "p.value", "craemer", "warnings")]


  # maak hom korter:
  #if (suiwer == TRUE) {p.df = p.df[which(p.df$warnings == "None"| p.df$warnings == "Geen"), ]}
  p.df = p.df[which(p.df$p.value < p.cutoff), ]

  if (nrow(p.df) < 1) {
    message("\n\nDaar niks om te doen nie!\n\n")
    return(NULL)
  }

  if (out.df == TRUE){
    message("Ek skryf 'n dataframe uit want jy het gevra. Sy naam is ", out.df.name)
    assign(out.df.name, p.df, envir=.GlobalEnv)
  }

  ## Maak out.list
  spdf =  split(p.df, f=p.df$outcome)
  if (verbose == TRUE) message("\nspdf gemaak deur p.df te split volgens outcome. Sy lengte is ", length(spdf))
  if (verbose == TRUE & debug == TRUE) message("Resultaat-lys gemaak: lengte van ", length(spdf))
  nspdf = gsub("(^[[:print:]]+)_+[[:print:]]+\\.p\\.value", "\\1", names(spdf))
  if (verbose == TRUE) {
    message("nspdf is :")
    ctch <- sapply(X = nspdf, FUN = message); rm(ctch)
  }
  names(spdf) = nspdf
  if (verbose == TRUE) message("\nspdf se naam gegee")
  if (length(grep("\\.$", names(data))) > 0) {
    names(data) = gsub("\\.$", "" , names(data))
  }
  if (verbose == TRUE & debug == TRUE ) {
    message("\n names spdf \n")
    ctch <- sapply(X = nspdf, FUN = message); rm(ctch)
  }
  if (verbose == TRUE & debug == TRUE) {
    message("\n data \n")
    ctch <- sapply(X = names(data), FUN = message); rm(ctch)
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
      if (write.igraph) write.graph(g, file = paste(datadir, net.name, ".graphml", sep=""), format = "graphml")
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

###################################### HELPER FUNCTION: my.chisq.test


#  my chi2 toets wat n en craemer se V ook gee. Toets hom teen assocstats in vcd pakket

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
    xname <- if (length(DNAME) > 1L || nchar(DNAME, "w") > 30) {""} else DNAME
    yname <- if (length(DNAME2) > 1L || nchar(DNAME2, "w") > 30) {""} else DNAME2
    OK <- complete.cases(x, y)
    x <- x[OK]
    if (!is.factor(x)) {x <- factor(x)}
    y <- y[OK]
    if (!is.factor(y)) {y <- factor(y)}

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

