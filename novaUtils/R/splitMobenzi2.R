
#' Decodes Mobenzi data with extraction of repeating sections
#'
#' @param filePaths A character argument containing the paths to .csv files (one per 
#' section) as downloaded from Mobenzi.
#' @param formatOtions If true, formats the question options of code book variables
#' with 'format_char'.
#' @param tidy Should sections with the same number of rows be combined into one 
#' section?
#' @param twoLists If TRUE, returns two lists - the first containing the data 
#' frame(s) with the actual data and the second containing the metadata, 
#' question book and code book. If FALSE, returns only one list with the different
#' data frames (those from the data as well as the dfs for the metadata, code book
#' and question book) simply as separate items in the list.
#' @return One or two lists of data frames. See params 'tidy' and 'twoLists' for
#' more information.
#' @export
splitMobenzi2 <- function(filePaths, 
                          tidy = FALSE, 
                          twoLists = FALSE, 
                          formatOptions = FALSE) {
  
  require(novaUtils)
  
  # check for empty files
  nLines <- sapply(X = filePaths, FUN = R.utils::countLines)
  idxx <- which(nLines == 0)
  if (length(idxx) > 0) {
    warning(sprintf("Ignoring %d empty files...", length(idxx)))
    filePaths <- filePaths[which(nLines > 0)]
  }
  
  if (length(filePaths) == 0) {
    warning("No non-empty files found. Returning NULL.")
    return(NULL)
  }
  
  # read all the files
  ls_dfDataBySection <- lapply(X = filePaths, FUN = function(fp) {
    df <- read.csv(file = fp, header = TRUE, stringsAsFactors = FALSE)
    
    return(df)
  })

  # give the list some names
  if (!is.null(names(filePaths))) {
    names(ls_dfDataBySection) <- names(filePaths)
  } else {
    sectionNames <- fixname(basename(filePaths))
    sectionNames <- gsub(pattern = "(^[[:digit:]]{1,}_)|(.csv$)", 
                         replacement = "", 
                         x = sectionNames)
    names(ls_dfDataBySection) <- sectionNames
  }
  
  # extract the code book, questions and metadata from the list
  dfCodeBook <- ls_dfDataBySection$code_book
  names(dfCodeBook) <- fixname(names(dfCodeBook))
  dfQuestions <- ls_dfDataBySection$questions
  names(dfQuestions) <- fixname(names(dfQuestions))
  dfMetadata <- ls_dfDataBySection$submissions
  
  ls_dfDataBySection <- ls_dfDataBySection[which(!(names(ls_dfDataBySection) %in% c("code_book", 
                                                                                    "questions", 
                                                                                    "submissions")))]
  
  # remove the weird ï_ that Mobenzi adds to the first variable of all the dfs
  names(dfCodeBook) <- gsub(pattern = "^[[:print:]]{0,}question$",
                            replacement = "question",
                            x = fixname(names(dfCodeBook)))
  names(dfQuestions) <- gsub(pattern = "^[[:print:]]{0,}question_name$",
                             replacement = "question_name",
                             x = fixname(names(dfQuestions)))
  names(dfMetadata) <- gsub(pattern = "^[[:print:]]{0,}submission_id$",
                            replacement = "submission_id",
                            x = fixname(names(dfMetadata)))
  
  # first round of formatting to dfQuestions
  dfQuestions$question_name <- fixname(dfQuestions$question_name)  
  
  # decodeMobenzi, fixname and remove unnecessary/empty fields
  colsToIgnore <-  c("fieldworker_name", 
                     "fieldworker_id", 
                     "repeats_on_question", 
                     "repeat_question_value", 
                     "received")
  
  ls_dfDataBySection <- lapply(X = ls_dfDataBySection, FUN = function(df) {
    
    names(df) <- fixname(names(df))
    
    # try to work around the problem of '_other' as option, followed by an explanatory
    # text field also named '_other' that causes duplicate fields in the end
    idxx <- which(duplicated(names(df)) & 
                    (names(df) %in% dfQuestions[["question_name"]]))
    #idxx2 <- grep(pattern = "[[:print:]]{1,}_other$", x = names(df))
    #idxx <- intersect(idxx, idxx2)
    idxxQB <- which(dfQuestions[["question_name"]] %in% names(df)[idxx])
    names(df)[idxx] <- paste(names(df)[idxx], "_txt", sep = "")
    dfQuestions[["question_name"]][idxxQB] <<- paste(dfQuestions[["question_name"]][idxxQB], 
                                                    "_txt", sep = "")
    
    df <- decodeMobenzi(dfSurvey = df, 
                        dfCodeBook = dfCodeBook, 
                        fldnmVariable = "variable",
                        fldnmValue = "value", 
                        fldnmLabel = "label", 
                        formatOpsies = formatOptions)
    
    names(df) <- gsub(pattern = "^[[:print:]]{0,}submission_id$",
                      replacement = "submission_id",
                      x = names(df))
    
    df <- df[, which(!(names(df) %in% colsToIgnore)), drop = FALSE]
    
    idxx <- which(sapply(X = df, FUN = function(v) {return(all(v == "N/A"))}))
    if (length(idxx) > 0) {
      df <- df[, -idxx, drop = FALSE]
    }
    
    return(df)
  })
  
  # remove the '.1' etc that is sometimes added at the end of variable names
  ls_dfDataBySection <- lapply(X = ls_dfDataBySection, FUN = function(df) {
    names(df) <- gsub(pattern = "\\.[[:digit:]]{1,}$", 
                      replacement = "", 
                      x = names(df))
    
    return(df)
  })
  
  # format fields and names of dfCodeBook
  names(dfCodeBook) <- fixname(names(dfCodeBook))
  dfCodeBook$question <- fixname(dfCodeBook$question)
  dfCodeBook$variable <- fixname(dfCodeBook$variable)
  dfCodeBook$label <- fixname(dfCodeBook$label)
  
  # format fields and names of dfQuestions
  dfQuestions$section <- format_char(dfQuestions$section)
  dfQuestions$question_type <- format_char(dfQuestions$question_type)
  
  # format names of dfMetadata
  names(dfMetadata) <- fixname(names(dfMetadata))
  
  # put the data sections back into their original order
  ls_dfDataBySection <- ls_dfDataBySection[intersect(unique(dfQuestions$section), 
                                                   names(ls_dfDataBySection))]
  
  # if 'tidy', combine sections of equal nrows
  if (tidy) {
    
    ls_dfDataBySection$metadata <- dfMetadata
    fldnmsBySect <- sapply(X = ls_dfDataBySection, FUN = names)
    
    ## reorder - metadata df should be first in the list
    ls_dfDataBySection <- ls_dfDataBySection[c("metadata",
                                               setdiff(names(ls_dfDataBySection), 
                                                       "metadata"))]
    
    ## combine
    nrows <- unlist(sapply(X = ls_dfDataBySection, FUN = nrow))
    isRpt <- unlist(sapply(X = ls_dfDataBySection, FUN = function(df) {
      "repeating_index" %in% names(df)
    }))
    lsdfData <- list()
    
    for (nrw in unique(nrows)) {
      
      for (isrpt in unique(isRpt)) {
        
        idxx <- which(nrows == nrw & isRpt == isrpt)
        if (length(idxx) == 0) {next}
        if (length(idxx) == 1) {
          lsdfData[[length(lsdfData)+1]] <- ls_dfDataBySection[[idxx]]
          next
        }

        if (isrpt) {
          nmsMergeFlds <- c("submission_id", "repeating_index")
        } else {
          nmsMergeFlds <- c("submission_id")
        }        
                
        df <- ls_dfDataBySection[[idxx[1]]]
        for (idx in idxx[2:length(idxx)]) {
          df <- merge.data.frame(x = df, y = ls_dfDataBySection[[idx]], 
                                 by = nmsMergeFlds, all = TRUE)
        }
        
        lsdfData[[length(lsdfData)+1]] <- df; rm(df)
      }
    }
    
    ## return
    
    if (length(lsdfData) == 1) {
      names(lsdfData) <- "data"
    } else {
      names(lsdfData) <- paste("data", 1:length(lsdfData), sep = "")
    }
    
    if (twoLists) {
      return(list(lsData = lsdfData, 
                  lsExtra = list(code_book = dfCodeBook,
                                 questions = dfQuestions,
                                 fldnms_by_sect = fldnmsBySect)))
    } else {
      lsdfData$questions <- dfQuestions
      lsdfData$code_book <- dfCodeBook
      lsdfData$fldnms_by_sect <- fldnmsBySect
      
      return(lsdfData)
    }
  }
  
  # reaching this point means 'tidy' is FALSE, so return the data in lsdf format
  if (!twoLists) {
    ls_dfDataBySection[[length(ls_dfDataBySection) +1]] <- dfCodeBook
    ls_dfDataBySection[[length(ls_dfDataBySection) +1]] <- dfQuestions
    ls_dfDataBySection[[length(ls_dfDataBySection) +1]] <- dfMetadata
    
    names(ls_dfDataBySection)[(length(ls_dfDataBySection) - 2):(length(ls_dfDataBySection))] <- fixname(c("Code Book", "Questions", "Metadata"))
    
    return(ls_dfDataBySection)
    
  } else {
    lsExtra <- list(dfCodeBook, dfQuestions, dfMetadata)
    names(lsExtra) <- fixname(c("Code Book", "Questions", "Metadata"))
    
    return(list(lsData = ls_dfDataBySection, lsExtra = lsExtra))
  }
  
}
