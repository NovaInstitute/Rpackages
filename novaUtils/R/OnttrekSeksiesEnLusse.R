


splitMobenzi <- function(dfData, 
                         dfCodeBook, 
                         dfQuestions,
                         metafields = c("submission_id", 
                                        "fieldworker_name", 
                                        "fieldworker_id", 
                                        "device", 
                                        "received", 
                                        "start", 
                                        "end", 
                                        "duration_seconds", 
                                        "latitude", 
                                        "longitude", 
                                        "language", 
                                        "survey_version", 
                                        "modified_by", 
                                        "modified_on", 
                                        "complete")) {
  
  require(novaUtils)
  
  names(dfCodeBook) <- fixname(names(dfCodeBook))
  names(dfQuestions) <- fixname(names(dfQuestions))
  names(dfData) <- fixname(names(dfData))

  # remove the weird ï_ that Mobenzi adds to the first variable of all the dfs
  names(dfCodeBook) <- gsub(pattern = "^[[:print:]]{0,}question$",
                            replacement = "question",
                            x = names(dfCodeBook))
  names(dfQuestions) <- gsub(pattern = "^[[:print:]]{0,}question_name$",
                             replacement = "question_name",
                             x = names(dfQuestions))
  names(dfData) <- gsub(pattern = "^[[:print:]]{0,}submission_id$",
                        replacement = "submission_id",
                        x = names(dfData))
  
  dfCodeBook$question <- fixname(dfCodeBook$question)
  dfCodeBook$variable <- fixname(dfCodeBook$variable)
  dfCodeBook$label <- fixname(dfCodeBook$label)
  
  dfQuestions$question_name <- fixname(dfQuestions$question_name)
  dfQuestions$section <- fixname(dfQuestions$section)
  
  # determine which questions repeat
  dfQuestions$in_rep_sect <- FALSE
  for (q in 1:nrow(dfQuestions)) {
    repFields <- grep(pattern = sprintf("^(%s)_[[:digit:]]{1,}", 
                                   dfQuestions[q, "question_name"]), 
                 x = names(dfData), 
                 value = TRUE)
    if (length(repFields) > 0) {
      dfQuestions[q, "in_rep_sect"] <- TRUE
    }
  }

  # extract the question_name (as it appears in dfQuestion) from the 'question' 
  # field of dfCodeBook
  dfCodeBook$question_name <- NA_character_
  
  for (q in 1:nrow(dfQuestions)) {
    idxx <- grep(pattern = sprintf("^(%s)(_{0,1})([[:digit:]]{0,2})$", 
                                   dfQuestions[q, "question_name"]), 
                 x = dfCodeBook$question)
    if (length(idxx) > 0) { # not all questions appear in the CodeBook (e.g. free-text questions)
      dfCodeBook[idxx, "question_name"] <- dfQuestions[q, "question_name"]
    }
  }
  dfCodeBook <- move.col(df = dfCodeBook,colName = "question_name", colIdx = 1)

  # split dfQuestions by section
  ls_dfQuestionsBySection <- split(x = dfQuestions, f = dfQuestions$section)
  
  # split dfData by section
  ls_dfDataBySection <- lapply(X = ls_dfQuestionsBySection, FUN = function(dfSectionInfo) {
    sectionFldnms <- c()
    
    for (q in 1:nrow(dfSectionInfo)) {
      
      
      questFldnms <- grep(pattern = sprintf("^(%s)", dfSectionInfo[q, "question_name"]), 
                          x = names(dfData), 
                          value = TRUE)
      sectionFldnms <- unique(c(sectionFldnms, questFldnms))
    }
    
    dfSectData <- dfData[, c("submission_id", sectionFldnms)]
    
    # if it is not a repeating section, return it now
    if (unique(dfSectionInfo$in_rep_sect) == FALSE) {
      return(dfSectData)
    }
    
    # if it is a repeating section, do the melt-cast thing
    dfSectData_m <- melt(data = dfSectData, id.vars = "submission_id")

    # extract the subids from the variable field
    dfSectData_m$sub_id <- dfSectData_m$variable
    
    for (r in 1:nrow(dfSectionInfo)) {
      dfSectData_m$sub_id <- gsub(pattern = sprintf("^(%s)_([[:digit:]]{1,2})([[:print:]]{0,})$", 
                                                    dfSectionInfo[r, "question_name"]), 
                                  replacement = "\\2", 
                                  x = dfSectData_m$sub_id)
    }
    
    for (r in 1:nrow(dfSectionInfo)) {
      dfSectData_m$variable <- gsub(pattern = sprintf("^(%s_)([[:digit:]]{1,2})([[:print:]]{0,})$",
                                                    dfSectionInfo[r, "question_name"]),
                                  replacement = "\\1LVAR\\3",
                                  x = dfSectData_m$variable)
      
    }
    
    dfSectData_m$sub_id <- as.integer(dfSectData_m$sub_id)
    
    # now recast
    dfSectData_c <- dcast(data = dfSectData_m, 
                          formula = submission_id + sub_id ~ variable)
    
    # and return
    return(dfSectData_c)
  })

  # factorise the data according to dfCodeBook
  ## remove the y_ and n_ from the label field of dfCodeBook
  dfCodeBook$label <- gsub(pattern = "^y_[[:print:]]{1,}$",
                           replacement = "yes", 
                           x = dfCodeBook$label)
  dfCodeBook$label <- gsub(pattern = "^n_[[:print:]]{1,}$",
                           replacement = "no", 
                           x = dfCodeBook$label)
  
  ## factorise
  for (r in 1:nrow(dfQuestions)) {
    dfCodeBook$variable <- gsub(pattern = sprintf("^(%s_)([[:digit:]]{1,2})([[:print:]]{0,})$",
                                                  dfQuestions[r, "question_name"]),
                                replacement = "\\1LVAR\\3",
                                x = dfCodeBook$variable)
  }
  
  ls_dfDataBySection <- lapply(X = ls_dfDataBySection, FUN = function(df) {
    for (f in 1:ncol(df)) {
      if (!(names(df)[f] %in% dfCodeBook$variable)) {next()}
      options <- unique(dfCodeBook[which(dfCodeBook$variable == names(df)[f]), "label"])
      df[[f]] <- factor(x = df[[f]], levels = options)
    }
    
    return(df)
  })
  
  # finally collect all those metafields into one section
  metafields <- metafields[which(metafields %in% names(dfData))]
  if (length(metafields) > 0) {
    df_metadata <- dfData[, metafields]
    ls_dfDataBySection[[length(ls_dfDataBySection) +1]] <- df_metadata
    names(ls_dfDataBySection)[length(ls_dfDataBySection)] <- "metadata"
  }
  
  return(ls_dfDataBySection)
}








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
splitMobenzi2 <- function(filePaths, tidy = FALSE, twoLists = FALSE, 
                          formatOptions = FALSE) {
  
  require(novaUtils)
  
  # check for empty files
  try(expr = {
    
    nLines <- sapply(X = filePaths, FUN = R.utils::countLines)
    idxx <- which(nLines == 0)
    if (length(idxx) > 0) {
      warning(sprintf("Ignoring %d empty files...", length(idxx)))
      filePaths <- filePaths[which(nLines > 0)]
    }}, silent = TRUE)
  
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
  ls_dfDataBySection <- lapply(X = ls_dfDataBySection, FUN = function(df) {
    names(df) <- fixname(names(df))
    names(df) <- gsub(pattern = "^[[:print:]]{0,}submission_id$",
                      replacement = "submission_id",
                      x = fixname(names(df)))
    return(df)
  })
  
  # first round of formatting to dfQuestions
  dfQuestions$question_name <- fixname(dfQuestions$question_name)  
  
  # decodeMobenzi, fixname and remove unnecessary/empty fields
  colsToIgnore <-  c("fieldworker_name", 
                     "fieldworker_id", 
                     "repeats_on_question", 
                     "repeat_question_value", 
                     "received")
  
  ls_dfDataBySection <- lapply(X = ls_dfDataBySection, FUN = function(df) {
    
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
    
    names(dfCodeBook)[names(dfCodeBook) == "question"] <- "Question"
    df <- decodeMobenzi(dfSurvey = df, 
                        dfCodeBook = dfCodeBook, 
                        fldnmVariable = "variable",
                        fldnmValue = "value", 
                        fldnmLabel = "label", 
                        formatOpsies = formatOptions)
    names(dfCodeBook)[names(dfCodeBook) == "Question"] <- "question"
    
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



  
  
# REMEMBER, NOT ALL QUESTIONS (AND THUS VARIABLES) ARE LISTED IN THE CODEBOOK! ONLY THOSE THAT HAVE OPTIONS...
# Also remember: a section cannot contain a mix of repeating and non-repeating questions. Either all questions in the section repeat, or none of them repeat. Repeating is set on section level, not on question level.