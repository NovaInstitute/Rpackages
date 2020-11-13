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


#'@param x A numeric vector 
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

# function to indicatorise non-numeric (Nn) variables
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


#'@param x An object of class 'indicator'
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
