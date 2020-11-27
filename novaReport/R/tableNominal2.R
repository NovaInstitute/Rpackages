#' @title tableNominal2
#' @description This function takes a data frame of nominal variables and possible grouping, weighting and subset
#' variables and provides a LaTeX table of descriptive statistics separately per group
#' and jointly for all observations, per variable
#' @param vars A list of nominal variables
#' @param weights Optional vector of weights of each observation
#' @param subset Optional logical vector, indicates subset of observations to be used
#' @param group Optional grouping vector
#' @param miss.cat Vector specifying the factors in vars that
#' should have their NAs transformed to a separate category
#' @param print.pval Add p-values of Fisher's exact or χ^2 test for a difference of distributions
#' between groups to the table, if there is more than one group
#' @param pval.bound p-values below pval.bound are formatted as < pval.bound
#' @param fisher.B Number of simulations to compute p-value for Fisher's exact test. Note that in the function fisher.test the option simulate.p.value
#' is set to TRUE, unless fisher.B == Inf which asks for the exact computation
#' @param vertical If TRUE, add vertical lines to the table, separating labels and groups, if applicable
#' @param cap The caption of the resulting LaTeX table
#' @param lab The label of the resulting LaTeX table
#' @param col.tit.font Choose the font for the column titles here (default: boldface)
#' @param font.size Font size for the generated table in LaTeX
#' @param longtable If TRUE, function makes use of package longtable in LaTex to generate tables that
#' span more than one page. If FALSE, generates a table in tabular environment
#' @param nams A vector of strings, containing the names corresponding to the variables in vars, if vars is
#' not a data frame but a list of variables. These are then the names that
#' appear in the LaTeX table. This option is only kept for backward compatibility.
#' @param cumsum f TRUE, the cumulative sums of the percentages are included for every level of the grouping variable
#' @param varSizeN Size of the column containing the variables
#' @param levSizeN Size of the column containing the levels
#' @param ... Arguments pass through to print.xtable
#' @export

tableNominal2 <- function(vars, 
                          weights = NA, 
                          subset = NA, 
                          group = NA, 
                          miss.cat = NA,
                          print.pval = c("none", "fisher", "chi2"), 
                          pval.bound = 10^-4,
                          fisher.B = 2000, 
                          vertical = TRUE, 
                          cap = "", 
                          lab = "", 
                          col.tit.font = c("bf", "", "sf", "it", "rm"), 
                          font.size = "footnotesize", 
                          longtable = TRUE,
                          nams = NA, 
                          cumsum = FALSE, 
                          debug=FALSE, 
                          n.max.rows = 50, 
                          n.max.char = 90, 
                          ...) {
        
  print.pval <- match.arg(print.pval)
  if (is.data.frame(vars) == TRUE) {
    tmp <- vars
    vars <- list()
    for (i in 1:ncol(tmp)) {
      vars[[i]] <- tmp[, i]
    }
    nams <- colnames(tmp)
  }
  n.var <- length(nams)
  if (identical(subset, NA) == FALSE) {
    if (identical(group, NA) == FALSE) {
      group <- group[subset]
    }
    if (identical(weights, NA) == FALSE) {
      weights <- weights[subset]
    }
    for (i in 1:n.var) {
      vars[[i]] <- vars[[i]][subset]
    }
  }
  vert.lin <- "|"
  if (vertical == FALSE) {
    vert.lin <- ""
  }
  for (i in 1:length(nams)) {
    nams[i] <- gsub("_", "\\\\_", as.character(nams[i]))
  }
  if (max(is.na(miss.cat)) == 0) {
    for (i in miss.cat) {
      vars[[i]] <- NAtoCategory(vars[[i]], label = "missing")
    }
  }
  if (identical(group, NA) == TRUE) {
    group <- rep(1, length(vars[[1]]))
  }
  if (identical(weights, NA) == TRUE) {
    weights2 <- 1
  }
  if (identical(weights, NA) == FALSE) {
    weights2 <- weights
  }
  for (i in 1:n.var) {
    vars[[i]][vars[[i]] == "NA"] <- NA
    vars[[i]] <- rep(vars[[i]], times = weights2)
  }
  group <- rep(group, times = weights2)
  vars <- lapply(vars, as.factor)
  group <- as.factor(group)
  ns.level <- unlist(lapply(lapply(vars, levels), length))
  n.group <- length(levels(group))
  cumsum <- as.logical(cumsum)
  stopifnot(identical(length(cumsum), 1L))
  nColPerGroup <- 2L + as.integer(cumsum)
  out <- matrix(NA, ncol = 2 + nColPerGroup * (n.group + 1),
                nrow = (sum(ns.level) + n.var))
  out <- data.frame(out)
  for (i in 1:n.var) {
    ind <- max(cumsum(ns.level[1:i])) - ns.level[i] + 1:(ns.level[i] +
                                                           1) + (i - 1)
    splits <- split(vars[[i]], group)
    for (g in 1:n.group) {
      tmp <- splits[[g]]
      tmp <- tmp[is.na(tmp) == FALSE]
      if (sum(is.na(tmp)) > 0) {
        excl <- NULL
      }
      else {
        excl <- NA
      }
      tab <- table(tmp, exclude = excl)
      tab.s <- round(100 * tab/sum(tab), 2)
      out[ind, 2 + nColPerGroup * (g - 1) + 1] <- c(tab,
                                                    sum(tab))
      out[ind, 2 + nColPerGroup * (g - 1) + 2] <- c(tab.s,
                                                    sum(tab.s))
      if (cumsum) {
        out[ind, 2 + nColPerGroup * (g - 1) + 3] <- c(cumsum(tab.s),
                                                      NA)
      }
    }
    out[ind[1], 1] <- nams[[i]]
    out[ind, 2] <- c(levels(vars[[i]]), "all")
    tab2 <- table(vars[[i]])
    tab2.s <- round(100 * tab2/sum(tab2), 2)
    out[ind, 2 + nColPerGroup * n.group + 1] <- c(tab2, sum(tab2))
    out[ind, 2 + nColPerGroup * n.group + 2] <- c(tab2.s,
                                                  sum(tab2.s))
    if (cumsum) {
      out[ind, 2 + nColPerGroup * n.group + 3] <- c(cumsum(tab2.s),
                                                    NA)
    }
    v1 <- vars[[i]]
    g1 <- as.character(group)
    indNA <- (is.na(g1) == FALSE) & (g1 != "NA") & (is.na(v1) ==
                                                      FALSE) & (v1 != "NA")
    v2 <- as.character(v1[indNA])
    g2 <- g1[indNA]
    ind1 <- length(unique(g2)) > 1
    ind2 <- print.pval %in% c("fisher", "chi2")
    ind3 <- length(unique(v2)) > 1
    splits2 <- split(v2, g2)
    ind4 <- 1 - max(unlist(lapply(lapply(splits2, is.na),
                                  sum)) == unlist(lapply(lapply(splits2, is.na), length)))
    if (ind1 * ind2 * ind3 * ind4 == 1) {
      if (print.pval == "fisher") {
        pval <- if (fisher.B == Inf)
          fisher.test(v2, g2, simulate.p.value = FALSE)$p.value
        else fisher.test(v2, g2, simulate.p.value = TRUE,
                         B = fisher.B)$p.value
      }
      if (print.pval == "chi2") {
        pval <- chisq.test(v2, g2, correct = TRUE)$p.value
      }
      out[max(ind), 1] <- paste("$p", formatPval(pval,
                                                 includeEquality = TRUE, eps = pval.bound), "$",
                                sep = "")
    }
  }
  
  col.tit <- if (cumsum) {
    c("n", "\\%", "\\sum \\%")
  } else {
    c("n", "\\%")
  }
  
  # shorten the table if it has too many rows
  if (nrow(out) > n.max.rows) {
          out <- out[order(out[[3]], decreasing = TRUE),]
          rownames(out) <- 1:nrow(out)
          out[1,1] <- na.omit(unique(out[[1]]))
          out[2:nrow(out),1] <- NA_character_
          idxx <- (n.max.rows):nrow(out)
          idxxCnum <- which(sapply(X = out, FUN = is.numeric))
          out[n.max.rows,2] <- sprintf("...%d more rows...", length(idxx))
          for (idxc in idxxCnum) {
                  out[n.max.rows,idxc] <- sum(out[[idxc]][idxx], na.rm = TRUE)
          }
          out <- out[1:(n.max.rows),]
  }
  
  # shorten labels that are too long
  nChars <- nchar(out[[2]])
  idxx <- which(nChars > n.max.char)
  if (length(idxx) > 0) {
          out[idxx,2] <- sprintf("%s...", substr(x = out[idxx,2], start = 1, stop = (n.max.char-3)))
  }
  
  col.tit.font <- match.arg(col.tit.font)
  fonts <- getFonts(col.tit.font)
  digits <- if (cumsum) {
    c(0, 1, 1)
  } else {
    c(0, 1)
  }
  groupAlign <- paste(rep("r", nColPerGroup), collapse = "")
  al <- paste("lll", vert.lin, groupAlign, sep = "")
  tmp <- cumsum(ns.level + 1)
  hlines <- sort(c(0, tmp - 1, rep(tmp, each = 2)))
  tab.env <- "longtable"
  float <- FALSE
  if (!longtable) {
    tab.env <- "tabular"
    float <- TRUE
  }
  if (n.group > 1) {
    zz <- rep(c(levels(group), "all"), each = nColPerGroup)
    zz[-match(unique(zz), zz)] <- ""
    zz = toupper(sapply(zz, substr, start=1, stop=3))
    dimnames(out)[[2]] <- c(fonts$text("Variable"), fonts$text("Levels"),
                            fonts$math(paste(col.tit, "_{\\mathrm{", zz, "}}", sep = "")))
    for (i in 1:n.group) {
      al <- paste(al, vert.lin, groupAlign, sep = "")
    }
    al = paste("p{2.5}", substr(al, 2, nchar(al)), sep="")

    out[length(out[,1]),2] <- out[length(out[,1]),1] # skuif die p waarde regs

    #if (debug==TRUE) 
    {
      assign("zz", zz, envir=.GlobalEnv)
      assign("n.group", n.group, envir=.GlobalEnv)
      assign("out", out, envir=.GlobalEnv)
      assign("digits", digits, envir=.GlobalEnv)
      assign("col.tit", col.tit, envir=.GlobalEnv)
      assign("al", al, envir=.GlobalEnv)
    }


    digitz = ifelse(sapply(out, is.numeric),1,0)
    names(digitz) = NULL
    if (debug==TRUE) assign("digitz", digitz, envir=.GlobalEnv)

    rr = names(out)[-1]
    out = out[,-1]
    names(out) = rr

    xtab1 <- xtable::xtable(out,
                            digits = digitz,
                            align = sub("l","", al), #substring(al, 2, nchar(al)),
                            caption = cap,
                            label = lab)
    xtab2 <- print(xtab1, include.rownames = FALSE, floating = float,
                   type = "latex", hline.after = hlines, size = font.size,
                   sanitize.text.function = function(x) {
                     gsub("_", " ", x)
                   }, tabular.environment = tab.env, ...)
  }
  if (n.group == 1) {
    out <- if (cumsum) {
      out[, 1:5]
    } else {
      out[, 1:4]
    }
    dimnames(out)[[2]] <- outdimnames <- c(fonts$text("Variable"), fonts$text("Levels"),
                                           fonts$math(col.tit))
    if (debug == TRUE) assign("outdimnames", outdimnames, envir=.GlobalEnv)
    out[length(out[,1]),2] <- out[length(out[,1]),1]
    
    if (nrow(out) > n.max.rows) { 
            out <- out[order(out[[ncol(out)]], decreasing = TRUE),]
            rownames(out) <- 1:nrow(out)
            out <- out[1:n.max.rows]
            out[n.max.rows]
    }
    
    xtab1 <- xtable::xtable(out[-1],
                            digits = c(rep(0, 3), digits)[-1],
                            align = substring(al, 2, nchar(al)), caption = cap, label = lab)
    xtab2 <- print(xtab1, include.rownames = FALSE, floating = float,
                   type = "latex", hline.after = hlines, size = font.size,
                   sanitize.text.function = function(x) {
                     gsub("_", " ", x)
                   }, tabular.environment = tab.env, ...)
  }
}

# debugging aid
# weights = NA
# subset = NA
# group = NA
# miss.cat = NA
# print.pval = c("none", "fisher", "chi2")[1]
# pval.bound = 10^-4
# fisher.B = 2000
# vertical = TRUE
# cap = ""
# lab = ""
# col.tit.font = c("bf", "", "sf", "it", "rm")[2]
# font.size = "footnotesize"
# longtable = TRUE
# nams = NA
# cumsum = FALSE
# debug=TRUE
# n.max.rows = 50
# n.max.char = 90
