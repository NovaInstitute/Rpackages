#' Print out tables for report
#' 
#' Prints out tables for report out of list spdf, which is the result of the chi.2.exp function. Bonferroni's method is 
#' used here, which is used to correct for multiple comparisons
#' 
#' @param alpha Numeric to be used in Bonferroni
#' @param outcome Character vector of the outcome to be tested
#' @param var.names Character vector of the variable names
#' @param exposure Character vector used to specify replacement of points with " "
#' @param p.value Character vector of the probability value
#' @param craemer Character vector that is a measure of the association between two nominal variables
#' @param df Character vector
#' @param n Character vector
#' @param statistic Character vector
#' @param x.names Character vecor of the Related variable, n, p value, df, statistic and craemer's v
#' @param verkort Logical that replaces substrings and shortens 
#' @param warnings Character string indicating a warning
#' @param sonder.warnings Logical referring to no warnings being present if TRUE
#' @param geen Character vector that coincides with displaying that no warnings are present
#' @param tp Character vector for the table placement
#' @param tabdir Directory of the table
#' @param verbose Display function messages
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
        
        if (!require("Hmisc")) {
                message("Loading Hmisc")
                install.packages("Hmisc", dependencies = TRUE)
                if (!require("Hmisc")) stop("Load Hmisc manually")
        }
        
        if (!require("xtable")) {
                message("Loading xtable")
                install.packages("xtable", dependencies = TRUE)
                if (!require("Hmisc")) stop("Load xtable manually")
        }
        
  # verkort
  if (sonder.warnings == TRUE){
    x = x[which(x[ ,warnings] == geen), ]
  }
  #vervang punte met " "
  x[, exposure] = gsub("_", " ", x[, exposure])
  x[, exposure] = gsub("\\.", " ", x[, exposure])
  # Onttrek die afhanklikke veranderlike (dis die deel van kolom 1 ('var.names') voor die '__')
  if (verkort == TRUE){
    lengte = regexec(pattern="__", text=as.character(x[ ,var.names]))[[1]][[1]]
    naam = substr(x[ ,var.names], start=1, stop=lengte-1)
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
  	if (verbose == TRUE) message("Ons gaan nou Bonferroni doen. dim() is tans : ", dim(x)[[1]], " by ", dim(x)[[2]])
  	x = x[x$p.value <= alpha2,]
  	if (verbose == TRUE) message("Ons het sopas Bonferroni gedoen. dim() is tans : ", dim(x)[[1]], " by ", dim(x)[[2]])
  	}
   names(x) = x.names
  
  # Druk hom
  if (is.null(tabdir) == FALSE){
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

#' Unfactor
#' 
#' Help function that converts a factor to a numeric
#' 
#' @param x Factor to be converted


############################# hulpfunksie #############################
unfactor <- function(x){if (is.factor(x)) as.numeric(unclass(levels(x)))[as.numeric(x)]  else x }