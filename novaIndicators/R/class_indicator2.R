# Date created: 2017-09-01
# Owner: Nova Institute (Reg# 1994/002614/08).

setClass("indicator",
         slots = list(
           "name" = "character",
           "val" = "ANY",
           "conf_print" = "character",
           "unit" = "character",
           "formula" = "ANY",
           "sourcepath" = "character",
           "date_created" = "character",
           "description" = "ANY",
           "comments" = "ANY",
           "plots" = "ANY"))

#'@description creates an indicator object from a numeric vector
#'@title make.indicator
#'@name make.indicator
#'@param x A numeric vector
#'@param name name of the indicator
#'@param digits number of decimals after the comma when rounding
#'@param NAsToZero should NA's be made Zero
#'@param indicObj indicator object
#'@export
#'
make.indicator <- function(x, 
                           name = "",
                           digits = 2, 
                           NAsToZero = FALSE, 
                           indicObj = NULL) {
  if (is.null(indicObj)) {
    indicObj <- new("indicator")
  }
  
  indicObj@name <- name
  indicObj@val <- summaryX(x = x, digits = digits, NAsToZero = NAsToZero)
  indicObj@date_created <- as.character(Sys.time())
  
  return(indicObj)
}

#'@description function to indicatorise non-numeric (Nn) variables
#'@title make.indicatorNn
#'@name make.indicatorNn
#'@param df data.frame containing the data
#'@param name name of the indicator object
#'@param indicVar name of the variable that must be indicatified
#'@param indicOpt option that must be indicatified
#'@param groupVar group variable, which the data must be grouped by when summarised
#'@param indicObj the indicator object
#'@export

make.indicatorNn <- function(df, 
                             name,
                             indicVar, 
                             indicOpt, 
                             groupVar = NULL, 
                             indicObj = NULL) {
  
  if (is.null(indicObj)) {indicObj <- new("indicator")}
  
  if (is.null(groupVar)) {
    df$grp <- "all"
    groupVar <- "grp"
  }
  
  dfIndic <- indicatorMaker(dfData = df, 
                            indicVar = indicVar, 
                            indicOption = indicOpt, 
                            groupVar = groupVar)
  names(dfIndic)[1] <- "stat"
  dfIndic <- melt(dfIndic, id.vars = "stat")
  dfIndic <- dcast(data = dfIndic, formula = variable ~ stat)
  dfIndic <- rename.vars(data = dfIndic, from = "variable", to = groupVar, 
                         info = FALSE)
  dfIndic <- dfIndic[c(groupVar, "PointEst", "Lower", "Upper", "n")]
  
  indicObj@name <- name
  indicObj@val <- dfIndic
  indicObj@date_created <- as.character(Sys.time())
  
  return(indicObj)
}



#'@description function that prints indicator neatly
#'@title print.indicator
#'@name print.indicator
#'@param x An object of class 'indicator'
#'@param inclCreatedBy logical, if TRUE it includes the persons name who created it
#'@param inclComments logical, if TRUE it includes the comment from the creator
#'@export


print.indicator <- function(x, inclCreatedBy = FALSE, inclComments = TRUE) {
  
  print(sprintf("Name: %s", ifelse(length(x@name) == 0, "-", x@name)), 
        quote = FALSE)
  print(sprintf("Unit of measurement: %s", 
                ifelse(length(x@unit) == 0, "-", x@unit)), 
        quote = FALSE)
  print(sprintf("Description: %s", 
                ifelse(length(x@description) == 0, "-", x@description)), 
                quote = FALSE)
  print("Value:", quote = FALSE)
  if (is.null(x@val)) {
    print("", quote=FALSE)
  } else {
    if (is.data.frame(x@val)) {
      if (nrow(x@val) <= 6) {
        print(x@val, quote = FALSE)
      } else {
        print(head(x@val, n = 6), quote = FALSE)
        cat(sprintf("...%d rows omitted...\n", nrow(x@val) -6))
      }
    } else {
      print(x@val, quote=FALSE)}
  }
  
  print(sprintf("Created on: %s", 
                ifelse(length(x@date_created) == 0, "-", x@date_created)), 
                quote = FALSE)
  if (inclCreatedBy) {
    print(sprintf("Created by: %s", 
                  ifelse(length(x@sourcepath) == 0, "-", x@sourcepath)), 
          quote = FALSE)
  }
  if (inclComments) {
    print(sprintf("Comments: %s", ifelse(is.null(x@comments), "-", x@comments)),
          quote = FALSE)
  }
}




# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##
