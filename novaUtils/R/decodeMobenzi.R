
#' Decodes Mobenzi questionnaire data using the data's codebook.
#' @author A. Howard
#' @param dfSurvey The questionnaire data as a data frame.
#' @param dfCodebook The questionnaire's code book as a data frame.
#' @param fldnmVariable The name of the 'Variable' field in the code book.
#' @param fldnmValue The name of the 'Value' field in the code book.
#' @param fldnmLabel The name of the 'Label' field in the code book.
#' @param replacementChar The replacement character to be used by function 'fixname'.
decodeMobenzi <- function(dfSurvey = NULL, 
                          dfCodeBook = NULL,
                          fldnmVariable = "Variable", 
                          fldnmValue = "Value", 
                          fldnmLabel = "Label",
                          replacementChar = "_") {
  
  # doen al die toetse hier
  ## is.null
  ## !is.data.frame
  ## nrow < 1
  ## fldnms not found in names(dfCodebook)
  ## variable names not found in dfSurvey

  # format the fields of dfCodeBook
  dfCodeBook[[fldnmVariable]] <- fixname(dfCodeBook[[fldnmVariable]], replacementChar)
  dfCodeBook[[fldnmValue]] <- format_char(dfCodeBook[[fldnmValue]])
  dfCodeBook[[fldnmLabel]] <- format_char(dfCodeBook[[fldnmLabel]])

  # fix the problematic "Y - we use animal dung" or "N - we use animal dung" format in the code book
  idxx <- grep(pattern = "^y_", x = dfCodeBook[[fldnmLabel]])
  if (length(idxx) > 0) {
    dfCodeBook[idxx, fldnmLabel] <- "yes"
  }
  idxx <- grep(pattern = "^n_", x = dfCodeBook[[fldnmLabel]])
  if (length(idxx) > 0) {
    dfCodeBook[idxx, fldnmLabel] <- "no"
  }

  oldNames <- names(dfSurvey)
  names(dfSurvey) <- fixname(names(dfSurvey), replacementChar)

  varsplits <- split(x = dfCodeBook, f = dfCodeBook[[fldnmVariable]])
  if (length(varsplits) < 1) {
    warning("Unsuccessful splitting on fldnmVariable. Returning dfSurvey as is.")
    return(dfSurvey)
  }

  ctch <- apply(X = as.array(names(varsplits)), MARGIN = 1, FUN = function(varnm) {
    if (!(varnm %in% names(dfSurvey))) {
      warning(varnm, " not found in dfSurvey.")
      return(1)
    }

    vardf <- varsplits[[varnm]]
    dfSurvey[[varnm]] <<- format_char(dfSurvey[[varnm]])

    for (r in 1:nrow(vardf)) {
      if (is.na(vardf[r, fldnmValue])) {
        warning("Blanks to be replaced according to dfCodeBook.")
      }
      pattrn <- paste("^", vardf[r, fldnmValue], "$", sep = "")
      idxx <- grep(pattern = pattrn, x = dfSurvey[[varnm]])
      if (length(idxx) > 0) {
        dfSurvey[idxx, varnm] <<- vardf[r, fldnmLabel]
      }
    } ; rm(r, idxx)
    return(0)
  }) ; rm(ctch)

  names(dfSurvey) <- oldNames ; rm(oldNames)
  return(dfSurvey)
}