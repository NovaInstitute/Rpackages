#' Generate a LaTeX table of descriptive statistics for continuous variables
#' 
#' This function takes a data frame of continuous variables and possible grouping, weighting, and subset 
#' variables and provides a LaTeX table of descriptive statistics separately per group and jointly 
#' for all observations, per variable.
#'  
#' @param vars A data frame containing continuous variables. 
#' See nams for an alternative way of specifying the variables to be displayed.
#' @param weights Optional vector of weights of each observation.
#' @param subset Optional logical vector, indicates subset of observations to be used.
#' @param group Optional grouping variable.
#' @param stats Specify which descriptive statistics should be displayed in the table, 
#' by either directly providing one or more of the default character strings (in arbitrary order) 
#' or a user-defined function. A user-defined function must bear a name, take a vector as an 
#' argument (NA's are removed by default) and return a single number (the desired statistic).
#' @param prec Specify number of decimals to be displayed.
#' @param col.tit pecify titles of columns. Note that the length of this vector must be equal 
#' to the length of stats plus the number of potential user-defined functions added to stats.
#' @param col.tit.font If col.tit has not been specified, choose the font for the column titles 
#' here (default: no special font face)
#' @param print.pval If print.pval == "anova", p-values for an analysis of variance for a location 
#' difference between groups are added to the table. If print.pval == "kruskal", p-values of a 
#' Kruskal-Wallis test are given. If group has only two levels, the respective p-values of a 
#' t- or Mann-Whitney test are provided. Only applies if group is provided. Note that by default, 
#' any missing values are removed for computation of p-values. If missings should be considered a 
#' separate level, define the input variables accordingly.
#' @param pval.bound p-values below pval.bound are formatted as < pval.bound.
#' @param declare.zero Computed descriptive statistics (not p-values) below that constant 
#' are set to 0. Yields nicer tables, especially when displaying centered or standardized variables.
#' @param cap The caption of the resulting LaTeX table.
#' @param lab The label of the resulting LaTeX table.
#' @param font.size Font size for the generated table in LaTeX.
#' @param longtable If TRUE, function makes use of package longtable in LaTex to generate 
#' tables that span more than one page. If FALSE, generates a table in tabular environment.
#' @param disp.cols Only included for backward compatibility. Needs to be a vector built of (some of) 
#' the default statistics character strings if not equal to NA. From package version 1.0.2 on use of 
#' stats is recommended.
#' @param nams A vector of strings, containing the names corresponding to the variables in vars, 
#' if vars is not a data frame but a list of variables. These are then the names that appear in the 
#' LaTeX table. This option is only kept for backward compatibility.
#' @param varSizeC Character vector that can be used to specify the size of the column (as a %) that contain the variables. 
#' @param ... Arguments pass through to print.xtable.
#' @export

tableContinuous2 <- function (vars, weights = NA, subset = NA, group = NA, stats = c("n", 
                                                                 "min", "q1", "median", "mean", "q3", "max", "s", "iqr", "na"), 
          prec = 1, col.tit = NA, col.tit.font = c("bf", "", "sf", 
                                                   "it", "rm"), print.pval = c("none", "anova", "kruskal"), 
          pval.bound = 10^-4, declare.zero = 10^-10, cap = "", lab = "", 
          font.size = "footnotesize", longtable = TRUE, disp.cols = NA, 
          nams = NA, varSizeC = "0.15", levSizeC = "0.15", ...) 
{
        col.tit0 <- col.tit
        print.pval <- match.arg(print.pval)
        if (identical(disp.cols, NA) == FALSE) {
                stats <- disp.cols
        }
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
        for (i in 1:length(nams)) {
                nams[i] <- gsub("_", "\\\\_", as.character(nams[i]))
        }
        if (identical(col.tit, NA) == TRUE) {
                col.tit.font <- match.arg(col.tit.font)
                fonts <- getFonts(col.tit.font)
                col.tit <- c(fonts$text("Variable"), fonts$text("Levels"), 
                             fonts$math("n"), fonts$text("Min"), fonts$math("q_1"), 
                             fonts$math("\\widetilde{x}"), fonts$math("\\bar{x}"), 
                             fonts$math("q_3"), fonts$text("Max"), fonts$math("s"), 
                             fonts$text("IQR"), fonts$text("\\#NA"))
        }
        if (identical(weights, NA) == TRUE) {
                weights2 <- 1
        }
        if (identical(weights, NA) == FALSE) {
                weights2 <- weights
        }
        n.levels <- 1
        if (identical(group, NA) == FALSE) {
                group <- factor(group, exclude = NULL)
                group <- as.factor(group)
                n.levels <- length(levels(group))
                group <- rep(group, times = weights2)
        }
        for (i in 1:n.var) {
                vars[[i]] <- rep(vars[[i]], times = weights2)
        }
        ncols <- length(stats)
        s1 <- unlist(lapply(stats, is.character))
        s1 <- (1:ncols)[s1]
        s2 <- unlist(lapply(stats, is.function))
        s2 <- (1:ncols)[s2]
        out <- matrix(NA, ncol = 12, nrow = (n.levels + 1) * n.var)
        out <- data.frame(out)
        out.fct <- matrix(NA, ncol = length(s2), nrow = (n.levels + 
                                                                 1) * n.var)
        out.fct <- data.frame(out.fct)
        for (i in 1:n.var) {
                ind <- (i - 1) * (n.levels + 1) + 1:(n.levels + 1)
                splits <- list(vars[[i]])
                if (identical(group, NA) == FALSE) {
                        splits <- split(vars[[i]], group)
                }
                for (j in 1:n.levels) {
                        tmp <- as.vector(splits[[j]])
                        if (sum(is.na(tmp) == FALSE) != 0) {
                                out[ind[j], 3] <- sum(is.na(tmp) == FALSE)
                                out[ind[j], 4] <- min(tmp, na.rm = TRUE)
                                out[ind[j], 5] <- quantile(tmp, 0.25, na.rm = TRUE)
                                out[ind[j], 6] <- median(tmp, na.rm = TRUE)
                                out[ind[j], 7] <- mean(tmp, na.rm = TRUE)
                                out[ind[j], 8] <- quantile(tmp, 0.75, na.rm = TRUE)
                                out[ind[j], 9] <- max(tmp, na.rm = TRUE)
                                out[ind[j], 10] <- sd(tmp, na.rm = TRUE)
                                out[ind[j], 11] <- out[ind[j], 8] - out[ind[j], 
                                                                        5]
                                out[ind[j], 12] <- sum(is.na(tmp) == TRUE)
                                if (length(s2) > 0) {
                                        for (f in 1:length(s2)) {
                                                out.fct[ind[j], f] <- stats[[s2[f]]](tmp[is.na(tmp) == 
                                                                                                 FALSE])
                                        }
                                }
                        }
                }
                vi <- as.vector(vars[[i]])
                out[max(ind), 3] <- sum(is.na(vi) == FALSE)
                out[max(ind), 4] <- min(vi, na.rm = TRUE)
                out[max(ind), 5] <- quantile(vi, 0.25, na.rm = TRUE)
                out[max(ind), 6] <- median(vi, na.rm = TRUE)
                out[max(ind), 7] <- mean(vi, na.rm = TRUE)
                out[max(ind), 8] <- quantile(vi, 0.75, na.rm = TRUE)
                out[max(ind), 9] <- max(vi, na.rm = TRUE)
                out[max(ind), 10] <- sd(vi, na.rm = TRUE)
                out[max(ind), 11] <- out[max(ind), 8] - out[max(ind), 
                                                            5]
                out[max(ind), 12] <- sum(is.na(vi) == TRUE)
                out[, 3:12][abs(out[, 3:12]) <= declare.zero] <- 0
                if (length(s2) > 0) {
                        for (f in 1:length(s2)) {
                                out.fct[max(ind), f] <- stats[[s2[f]]](vi[is.na(vi) == 
                                                                                  FALSE])
                        }
                        out.fct[abs(out.fct) <= declare.zero] <- 0
                }
                v1 <- vars[[i]]
                g1 <- as.character(group)
                indNA <- (is.na(g1) == FALSE) & (g1 != "NA") & (is.na(v1) == 
                                                                        FALSE) & (v1 != "NA")
                v2 <- v1[indNA]
                g2 <- g1[indNA]
                ind1 <- length(unique(g2)) > 1
                ind2 <- print.pval %in% c("anova", "kruskal")
                ind3 <- 1
                if (ind1 >= 1) {
                        splits2 <- split(v2, g2)
                        for (s in 1:length(splits2)) {
                                if (sum(is.na(splits2[[1]]) == TRUE) == length(splits2[[1]])) {
                                        ind3 <- 0
                                }
                        }
                }
                if (ind1 * ind2 * ind3 == 1) {
                        g2 <- as.factor(g2)
                        if (print.pval == "anova") {
                                pval <- anova(lm(v2 ~ g2))$"Pr(>F)"[1]
                        }
                        if (print.pval == "kruskal") {
                                pval <- kruskal.test(v2 ~ g2)$p.value
                        }
                        out[(i - 1) * (n.levels + 1) + n.levels + 1, 1] <- paste("$p", 
                                                                                 formatPval(pval, includeEquality = TRUE, eps = pval.bound), 
                                                                                 "$", sep = "")
                }
        }
        dc <- c("n", "min", "q1", "median", "mean", "q3", "max", 
                "s", "iqr", "na")
        stats.num <- pmatch(stats[s1], dc)
        stats2 <- c(2 + stats.num)
        out2 <- matrix(NA, ncol = 2 + length(s1) + length(s2), nrow = (n.levels + 
                                                                               1) * n.var)
        out2 <- data.frame(out2)
        out2[, c(1, 2, 2 + s1)] <- out[, c(1, 2, stats2)]
        colnames(out2)[c(1:2, 2 + s1)] <- col.tit[c(1:2, stats2)]
        out2[, 2 + s2] <- out.fct
        if (length(s2) > 0) {
                colnames(out2)[2 + s2] <- names(stats)[names(stats) != 
                                                               ""]
        }
        out2[((1:n.var) - 1) * (n.levels + 1) + 1, 1] <- nams
        align.stats <- ""
        for (i in 1:ncols) {
                align.stats <- paste(align.stats, "r", sep = "")
        }
        if (n.levels == 1) {
                prec <- c(rep(0, 1), rep(prec, ncols))
                ali <- "ll"
                out2 <- out2[, -2]
        }
        if (n.levels > 1) {
                prec <- c(rep(0, 2), rep(prec, ncols))
                ali <- paste("lp{", varSizeC,"\\columnwidth}|p{",levSizeC,"\\columnwidth}", sep = "")
        }
        for (c in 2:ncol(out2)) {
                if ((all(out2[, c] == round(out2[, c]), na.rm = TRUE) == 
                     TRUE) & (all(is.na(out2[, c])) == FALSE)) {
                        out2[, c] <- format(out2[, c], nsmall = 0)
                }
                else {
                        out2[, c] <- format(round(out2[, c], prec[c]), nsmall = prec[c])
                }
        }
        tmp <- cumsum(rep(n.levels, n.var) + 1)
        tab.env <- "longtable"
        float <- FALSE
        if (identical(longtable, FALSE)) {
                tab.env <- "tabular"
                float <- TRUE
        }
        if (n.levels == 1) {
                out3 <- out2[(1:n.var - 1) * 2 + 1, ]
                hlines <- 0
                xtab3 <- xtable::xtable(out3, align = paste(ali, align.stats, 
                                                            sep = ""), caption = cap, label = lab)
                xtab4 <- print(xtab3, include.rownames = FALSE, floating = float, 
                               type = "latex", hline.after = hlines, size = font.size, 
                               sanitize.text.function = function(x) {
                                       x
                               }, tabular.environment = tab.env, ...)
        }
        if (n.levels > 1) {
                out2[, 2] <- rep(c(levels(group), "all"), times = n.var)
                hlines <- sort(c(0, tmp - 1, tmp))
                xtab1 <- xtable::xtable(out2, align = paste(ali, align.stats, 
                                                            sep = ""), caption = cap, label = lab)
                xtab2 <- print(xtab1, include.rownames = FALSE, floating = float, 
                               type = "latex", hline.after = hlines, size = font.size, 
                               sanitize.text.function = function(x) {
                                       x
                               }, tabular.environment = tab.env, ...)
        }
}