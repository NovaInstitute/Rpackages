
#' Decodes data from mobenzi.
#' 
#' @param dfSurvey data.frame containing the data to be decoded.
#' @param dfCodeBook data.frame containing the codebook.
#' @param formatOpsies if true, the labels containing the options will be fixnamed.
#' @return a decoded dataframe
#' @export
decodeMobenzi <- function(dfSurvey = NULL, 
                          dfCodeBook = NULL,
                          fldnmVariable = "Variable", 
                          fldnmValue = "Value", 
                          fldnmLabel = "Label",
                          replacementChar = "_",
                          formatOpsies = FALSE,
                          fixnames = TRUE) {
  
  # doen al die toetse hier
  ## is.null
  ## !is.data.frame
  ## nrow < 1
  ## fldnms not found in names(dfCodebook)
  ## variable names not found in dfSurvey

  oldNames <- names(dfSurvey)
  names(dfSurvey) <- fixname(names(dfSurvey))
  
  dfCodeBook[[fldnmVariable]] <- fixname(data_names = dfCodeBook[[fldnmVariable]], 
                                         replacementChar = replacementChar)
  dfCodeBook[[fldnmValue]] <- format_char(dfCodeBook[[fldnmValue]]) # this does not format the options, so don't hash this out! This only makes matching easier and will not cause the question options to be formatted if the user does not wish to have it formatted.
  
  
  
  lsdfCodeBook <- by.data.frame(dfCodeBook, INDICES = dfCodeBook$Question, FUN = function(df){
          
          values <- unique(df[[fldnmLabel]])
          
          if(length(values) < 3){

          # fix the problematic "Y - we use animal dung" or "N - we use animal dung" format in the code book
          idxx <- grep(pattern = "^[Yy]{1}[ |_|-]{1}|Yes, [[:print:]]+", x = df[[fldnmLabel]])
          if (length(idxx) > 0) {
                  df[idxx, fldnmLabel] <- "Yes"
          }
          idxx <- grep(pattern = "^[Nn]{1}[ |_|-]{1}|No, [[:print:]]+", x = df[[fldnmLabel]])
          if (length(idxx) > 0) {
                  df[idxx, fldnmLabel] <- "No"
          }
          
          }
         return(df) 
  })
  
  
  
  dfCodeBook <- do.call("rbind", lsdfCodeBook)
  
  rownames(dfCodeBook) <- NULL


  # format the fields of dfCodeBook, if the user so requests
  if (formatOpsies){
    dfCodeBook[[fldnmLabel]] <- format_char(dfCodeBook[[fldnmLabel]])
  }

  varsplits <- split(x = dfCodeBook, f = dfCodeBook[[fldnmVariable]])
  if (length(varsplits) < 1) {
    warning("Unsuccessful splitting on fldnmVariable. Returning dfSurvey as is.")
    return(dfSurvey)
  }

  ctch <- apply(X = as.array(names(varsplits)), MARGIN = 1, FUN = function(varnm) {
    if (!(varnm %in% names(dfSurvey))) {
      #warning(varnm, " not found in dfSurvey.")
      return(1)
    }

    isFactor <- is.factor(dfSurvey[[varnm]])
    
    vardf <- varsplits[[varnm]]
    dfSurvey[[varnm]] <<- format_char(dfSurvey[[varnm]]) # this does not format the options, so don't hash this out! This only makes matching easier and will not cause the question options to be formatted if the user does not wish to have it formatted.

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
    
    if (isFactor) {
      dfSurvey[[varnm]] <<- factor(x = dfSurvey[[varnm]], levels = factorLevels)
    }
    return(0)
  }) ; rm(ctch)
  
  if (!fixnames) {
    names(dfSurvey) <- oldNames ; rm(oldNames)
  }

  return(dfSurvey)
  
}
