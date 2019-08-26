#' Dependancy tables
#'
#' Prints out tables for report out of data frame p.df, which is the result of the chi.2.exp function.
#' Bonferroni's method is used here, which is used to correct for multiple comparisons
#'
#' @param x Data frame, which is the outcome of chi.2.exp
#' @param alpha Numeric to be used in Bonferroni
#' @param outcome Character vector referring to the column containing outcomes
#' @param exposure Character vector referring to the column containing exposures
#' @param p.value Character vector referring to the column containing the probability value
#' @param craemer Character vector referring to the column containing creamer values
#' @param df Character vector referring to the column containing degrees of freedom
#' @param n Character vector referring to the column containing the total number of observations
#' @param statistic Character vector referring to the column containing the
#' calculated chi2 statistic
#' @param lateX Logical that prints a LaTeX table if TRUE
#' @param markDown Logical that creates a table for use in MarkDown
#' @param warnings Character referring to the column containing warnings
#' @param sonder.warnings Logical referring to no warnings being present if TRUE
#' @param geen Character vector to indicating that no warnings are present
#' @param tp Character vector for the table placement
#' @param tabdir Directory of the table
#' @param verbose Display function messages
#' @param bonferroni Logical that makes a bonferroni adjustment when TRUE
#' @param latSave Logical that saves the tables in the Tabelle folder in your working directory
#' @export

#################################################
# Druk tabelle vir verslag uit die lys spdf wat
# die resultaat is van die chi.2.eksp funksie
#
#################################################

afh.tab <- function(x,
                    alpha = 0.05,
                    outcome = "outcome",
                    var.names = "var.names",
                    exposure = "exposure",
                    p.value = "p.value",
                    craemer = "craemer",
                    df = "df",
                    n = "n",
                    statistic = "statistic",
                    x.names = c("Related variable", "n" ,"p value", "df", paste("$\\mathbf{\\chi^2}$", "statistic"), "Craemer's V"),
                    verkort = FALSE,
                    warnings = "warnings",
                    sonder.warnings = TRUE,
                    geen = "Geen",
                    tp="H",
                    tabdir = NULL,
                    verbose = FALSE, bonferroni = TRUE){
  require(Hmisc)
  require(xtable)
  # verkort
  if (sonder.warnings == TRUE){
    x = x[which(x[ ,warnings] == geen), ]
  }
  #vervang punte met " "
  for (i in 1:length(spdf)) {
    x <- as.data.frame(spdf[i])
    names(x) <- c("outcome", "exposure",
                  "n", "df",
                  "statistic", "p.value",
                  "craemer", "warnings"
    )



    x[, exposure] = gsub("_", " ", x[, exposure])
    x[, exposure] = gsub("\\.", " ", x[, exposure])
    # Onttrek die afhanklikke veranderlike (dis die deel van kolom 1 ('var.names') voor die '__')
    if (verkort == TRUE){
      lengte = regexec(pattern="__", text=x[ ,var.names])[[1]][[1]]
      naam = substr(x[ ,exposure], start=1, stop=lengte-1)
      naam = gsub("_"," ", naam)
      x[ ,outcome] = naam
      if (verbose == TRUE) message("naam: ", naam)
      if (verbose == TRUE) message(unique(x[, outcome]))
    } else {naam = x[ ,outcome]
    naam = gsub("_"," ", naam)}
    x[, df] = as.integer(unfactor(x[, df]))
    x[, statistic] = unfactor(x[, statistic])
    x[, craemer] = unfactor(x[, craemer])
    x = x[ ,c(exposure, n, p.value, df, statistic, craemer)]

    # Maak die bonferroni aanpassing
    if (bonferroni == TRUE){
      alpha2 = alpha / nrow(x)
      message(unique(naam), " alpha2 = ", alpha2)
      if (verbose == TRUE) message("Ons gaan nou Bonferroni doen. dim() is tans :", dim(x)[[1]], " by ", dim(x)[[2]])
      x = x[x$p.value <= alpha2,]
      if (verbose == TRUE) message("Ons het sopas Bonferroni gedoen. dim() is tans :", dim(x)[[1]], " by ", dim(x)[[2]])
    }
    names(x) = x.names

    # Druk hom
    if (is.null(tabdir) == FALSE){
      naam <- gsub(pattern = ".y$|.x$", replacement = "", x = naam)
      fn = paste(tabdir,  "dep.", gsub(" ",".", unique(naam)), ".tex",sep="")
      alp = paste("$\\alpha = ",alpha ,"$)", sep="")
      bfr = paste(" (",alp, " with Bonferroni correction)" , sep="")
      nbfr = paste(" (",alp,")", sep="")
      cap = paste("Variables dependent on or from ", "\\emph{", unique(naam), "}", ifelse(bonferroni==TRUE, bfr, nbfr) , sep="")
      lab = paste("Variables.dep", unique(naam), sep="")
      if (verbose == TRUE) message("cap: " , cap)
      if (verbose == TRUE) message("lab: " , lab)
      print(xtable(x,
                   digits = 4,
                   align = "lp{0.6\\columnwidth}rrrrr",
                   caption = cap,
                   label = lab),
            caption.placement = "top",
            table.placement = tp,
            size = "footnotesize",
            include.rownames = FALSE,
            file  = fn,
            sanitize.colnames.function = identity)
    } else {x}
  }
}

############################# hulpfunksie #############################
unfactor <- function(x){if (is.factor(x)) as.numeric(unclass(levels(x)))[as.numeric(x)]  else x }

