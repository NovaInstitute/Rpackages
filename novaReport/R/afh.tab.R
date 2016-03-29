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

afh.tab <- function(x, 
                    alpha = 0.05,
                    outcome = "outcome",
                    exposure = "exposure",
                    p.value = "p.value",
                    craemer = "craemer",
                    df = "df", 
                    n = "n",
                    statistic = "statistic",
                    lateX = FALSE,
                    markDown = TRUE,
                    warnings = "warnings",
                    sonder.warnings = TRUE,
                    geen = "None", 
                    tp="H",
                    tabdir = NULL,
                    verbose = FALSE, bonferroni = TRUE, latSave = FALSE){
        
        if (!require("Hmisc")) {
                message("Loading Hmisc")
                install.packages("Hmisc", dependencies = TRUE)
                if (!require("Hmisc")) stop("Load Hmisc manually")
        }
        
        if (!require("xtable")) {
                message("Loading xtable")
                install.packages("xtable", dependencies = TRUE)
                if (!require("xtable")) stop("Load xtable manually")
        }
        
  # verkort
  if (sonder.warnings == TRUE){
    x = x[which(x[ ,warnings] == geen), ]
  }
  
  #vervang punte met " "
  x[, exposure] = gsub("_", " ", x[, exposure])
  x[, exposure] = gsub("\\.", " ", x[, exposure])
  x[, outcome] = gsub("_", " ", x[, outcome])
  naam = x[ ,outcome]
  
  # Onttrek die afhanklikke veranderlike (dis die deel van kolom 1 ('var.names') voor die '__')
  
  x[, df] = as.integer(unfactor(x[, df]))
  x[, statistic] = unfactor(x[, statistic])
  x[, craemer] = unfactor(x[, craemer])
  x = x[ ,c(outcome, exposure, n, p.value, df, statistic, craemer)]
  
 
  # Maak die bonferroni aanpassing 
  if (bonferroni == TRUE){
  	alpha2 = alpha / nrow(x)
  	if (verbose == TRUE) message("alpha2 = ", alpha2)
  	if (verbose == TRUE) message("Ons gaan nou Bonferroni doen. dim() is tans : ", dim(x)[[1]], " by ", dim(x)[[2]])
  	x = x[x$p.value <= alpha2,]
  	if (verbose == TRUE) message("Ons het sopas Bonferroni gedoen. dim() is tans : ", dim(x)[[1]], " by ", dim(x)[[2]])
  	}
   
  
  # Druk hom
  if (is.null(tabdir) == FALSE){
    if (lateX){
            fn = paste(tabdir,  "/tabelle/dep.", gsub(" ",".", unique(naam)), ".tex",sep="")
            alp = paste("$\\alpha = ",alpha ,"$)", sep="")
            bfr = paste(" (",alp, " with Bonferroni correction)" , sep="")
            nbfr = paste(" (",alp,")", sep="")
            cap = paste("Variables dependent on or from ", "\\emph{", unique(naam), "}", ifelse(bonferroni==TRUE, bfr, nbfr) , sep="")
            lab = paste("Variables.dep", unique(naam), sep="")
            colnames(x) <- gsub("statistic", "$\\mathbf{\\chi^2}$ statistic", names(x))
    if (verbose == TRUE) message("lab: " , lab)
    for (i in 1:length(unique(x[, outcome]))) {
            if (verbose == TRUE) message("cap: " , cap[i], "\n")
            print(xtable(x[which(x[, outcome] == unique(x[, outcome])[i]), ],
                         digits = 4,
                         align = "rp{0.3\\columnwidth}p{0.3\\columnwidth}rrrrl", 
                         caption = cap[i], 
                         label = lab[i]),
                  include.rownames = FALSE,
                  caption.placement = "top", 
                  table.placement = tp,
                  size = "footnotesize",
                  file = if (latSave){fn[i]}else{""},
                  sanitize.colnames.function = identity, comment = FALSE)
            message("Table number: ",i, " Saved in: ",fn[i], "\n")
    }
            
   }
    
  } 
  
  if (markDown){
          if (!require("knitr")) {
                  message("Loading knitr")
                  install.packages("knitr", dependencies = TRUE)
                  if (!require("knitr")) stop("Load knitr manually")
          }
          printf <- function(x, cp, lb){
                  
                  kable(x, caption = cp, 
                              digits = 4, 
                              row.names = FALSE)
                         }
         
          out.list = split(x, f = x[, outcome])
          i = length(out.list)
          cap = paste("Variables dependent on or from ", names(out.list), sep="")
          lapply(1:length(out.list), function(i) printf(x = out.list[[i]], cp = cap[i]))
          
  }
}

#' Unfactor
#' 
#' Help function that converts a factor to a numeric
#' 
#' @param x Factor to be converted
#' @export

unfactor <- function(x){if (is.factor(x)) as.numeric(unclass(levels(x)))[as.numeric(x)]  else x }