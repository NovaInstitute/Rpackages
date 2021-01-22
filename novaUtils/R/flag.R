#' 22 November 2017

flags <- function() {
  c("duplicate" = "D",
    "incomplete" = "I",
    "warning" = "W",
    "followup" = "F",
    "test" = "T",
    "pilot" = "P",
    "unsuccessful" = "U",
    "modified" = "M",
    "unusable" = "E" # i.e. too erroneous to use
    )
}

flag <- function(df, idxxToFlag, 
                 flags = names(flags())[4], 
                 comments = "", nmFlagVar = "flags", nmCommVar = "flagcomms", 
                 sep = ";", 
                 verbose = TRUE) {
  
  # flag map
  flagVals <- flags()
  
  # create flag and comment fields if they do not exist yet
  if (!(nmFlagVar %in% names(df))) { df[[nmFlagVar]] <- NA_character_ }
  if (!(nmCommVar %in% names(df))) { df[[nmCommVar]] <- NA_character_ }
  
  if (length(idxxToFlag) == 0) { return(df) }
  
  # check that all flags specified are already defined
  if (any(!(flags %in% names(flagVals)))) {
    stop("Attempt to use undefined flag.")
  }
  
  # because the absence of a comment will cause 'unflag' to function incorrectly
  if (nchar(comments) == 0) { comments <- paste(flags, collapse = ";") }
  
  # encode the flags
  flags <- paste(flagVals[match(x = flags, table = names(flagVals))],
                 sep = sep, collapse = sep)
  
  # add the flags
  if (nchar(flags) > 0) {
    idxx <- intersect(idxxToFlag, which(nchar(df[[nmFlagVar]]) > 0))
    df[[nmFlagVar]][idxx] <- sprintf("%s%s%s",
                                     df[[nmFlagVar]][idxx], 
                                     sep, flags)
    
    idxx <- intersect(idxxToFlag, 
                      which(is.na(df[[nmFlagVar]]) | 
                              nchar(df[[nmFlagVar]]) == 0))
    df[[nmFlagVar]][idxx] <- flags
  }
  
  # add the comments
  if (nchar(comments) > 0) { 
    
    idxx <- intersect(idxxToFlag, which(nchar(df[[nmCommVar]]) > 0))
    df[[nmCommVar]][idxx] <- sprintf("%s%s%s",
                                     df[[nmCommVar]][idxx], 
                                     sep, comments)
    idxx <- intersect(idxxToFlag, 
                      which(is.na(df[[nmCommVar]]) | 
                              nchar(df[[nmCommVar]]) == 0))
    df[[nmCommVar]][idxx] <- comments
  }
  
  if (verbose) {
    message(sprintf("Flagged %d %s, '%s'.%s", 
                    length(idxxToFlag), 
                    ifelse(length(idxxToFlag) == 1, "record", "records"),
                    flags,
                    ifelse(nchar(comments) > 0, 
                           sprintf(" Reason: '%s'.", comments), 
                           "")))
  }
  
  return(df)
}


is.flagged <- function(df, nmFlagVar = "flags", 
                       flagsToCheck = names(flags()),
                       sep = ";", collapse = FALSE) {
  
  if (is.null(df)) { return(NULL) }
  if (nrow(df) == 0) { return(FALSE) }
  
  # flag map
  flagVals <- flags() 
  
  flagsToCheck <- unique(flagsToCheck)
  flagsOk <- flagsToCheck %in% names(flagVals)
  if (any(!flagsOk)) {
    stop(sprintf("Undefined flags specified: %s", paste(flagsToCheck[!flagsOk], collapse = ",")))
  }
  flagsToCheck <- flagVals[flagsToCheck]
  
  if (!(nmFlagVar %in% names(df))) { return(rep(FALSE, nrow(df))) }
  
  flagSplits <- strsplit(x = df[[nmFlagVar]], split = sep)
  
  isFlagged <- lapply(X = flagsToCheck, FUN = function(f) {
    status <- sapply(X = flagSplits, FUN = function(flags) f %in% flags)
  })
  
  if (length(isFlagged) == 1) {return(isFlagged[[1]])}
  
  if (collapse) {
    idxxFlagged <- c()
    for (fl in isFlagged) {
      idxxFlagged <- c(idxxFlagged, which(fl))
    }
    idxxFlagged <- unique(idxxFlagged)
    res <- rep(FALSE, nrow(df))
    res[idxxFlagged] <- TRUE
    return(res)
  }
  
  return(isFlagged)
  
}

#'@param dfs list of data frames that may contain "flags" and "flagcomms" vars
combineFlags <- function(dfs, nmIDvar = "submission_id",
                         nmFlagVar = "flags", nmCommVar = "flagcomms") { 
  
  require(plyr)
  
  idxxNotNullDfs <- which(!unlist(sapply(X = dfs, is.null)))
  if (length(idxxNotNullDfs) == 0) {return(dfs)}
  
  dfFlags <- lapply(X = dfs[idxxNotNullDfs], FUN = function(df) {
    if (!(nmIDvar %in% names(df))) { stop(sprintf("Could not find variable named '%s' in all data frames.", nmIDvar)) }
    if (length(intersect(names(df), c(nmFlagVar, nmCommVar))) == 1) { # the length must be either 0 or 2
      stop("All data frames must either have both '%s' and '%s' vars or neither.", nmFlagVar, nmCommVar) 
    }
    return(df[,intersect(c(nmIDvar, nmFlagVar, nmCommVar), names(df)), drop = FALSE])
  })
  dfFlags <- do.call("rbind.fill", dfFlags)
  if (ncol(dfFlags) == 1) { return(dfs) } 
  # < if dfFlags contains only the ID var, then it means that none of the data frames have flags, so we don't have to do anything
  
  # combine
  dfFlags <- data.frame(stringsAsFactors = FALSE,
                        dplyr::summarise(dplyr::group_by(dfFlags, submission_id),
                                         flags = paste(flags[!is.na(flags)], collapse = ";"),
                                         flagcomms = paste(flagcomms[!is.na(flagcomms)], collapse = ";")))
  
  # check against duplicate flag-flagcomm pairs
  lsFlags <- strsplit(x = dfFlags[[nmFlagVar]], split = ";", fixed = TRUE)
  lsFlagComms <- strsplit(x = dfFlags[[nmCommVar]], split = ";", fixed = TRUE)
  
  for (i in 1:length(lsFlagComms)) {
    if (length(lsFlagComms[[i]]) != length(lsFlags[[i]])) { 
      stop("Orphaned flag / flag comment detected.") 
    }
    
    idxx <- which(duplicated(lsFlagComms[[i]]) | nchar(lsFlagComms[[i]]) == 0)
    if (length(idxx) > 0) {
      lsFlagComms[[i]] <- lsFlagComms[[i]][-idxx]
      lsFlags[[i]] <- lsFlags[[i]][-idxx]
    }
  }
  
  dfFlags[[nmCommVar]] <- unlist(sapply(X = lsFlagComms, FUN = function(s) {
    if (length(s) == 0) { return(NA_character_) }
    return(paste(s, collapse = ";"))
  }))
  dfFlags[[nmFlagVar]] <- unlist(sapply(X = lsFlags, FUN = function(s) {
    if (length(s) == 0) { return(NA_character_) }
    return(paste(s, collapse = ";"))
  }))
  
  dfs[idxxNotNullDfs] <- lapply(X = dfs[idxxNotNullDfs], FUN = function(df) {
    df[,c(nmFlagVar, nmCommVar)] <- NA_character_
    idxx <- match(df[[nmIDvar]], dfFlags[[nmIDvar]])
    df[which(!is.na(idxx)), c(nmFlagVar, nmCommVar)] <- dfFlags[idxx[!is.na(idxx)], c(nmFlagVar, nmCommVar)]
    return(df)
  })
  
  return(dfs)
}

unflag <- function(df, idxxToUnflag, 
                   flagsToRemove = names(flags()), 
                   pattern = NA_character_, fixed = TRUE, sep = ";", 
                   nmFlagVar = "flags", nmCommVar = "flagcomms") {
  
  if (length(idxxToUnflag) == 0) { return(df) }
  if (!(nmFlagVar %in% names(df)) & !(nmCommVar %in% names(df))) { return(df) }
  if (!(nmFlagVar %in% names(df)) | !(nmCommVar %in% names(df))) { 
    stop("df must either have both '%s' and '%s' vars or neither.", nmFlagVar, nmCommVar) 
  }
  
  # check that all flags specified are actually defined
  if (is.na(pattern) & any(!(flagsToRemove %in% names(flags())))) {
    stop("Attempt to use undefined flag.")
  }  
  
  flagsToRemove <- flags()[flagsToRemove]
  
  flagsX <- strsplit(x = df[[nmFlagVar]][idxxToUnflag], split = sep, fixed = TRUE)
  flagcommsX <- strsplit(x = df[[nmCommVar]][idxxToUnflag], split = sep, fixed = TRUE)
  
  nFlags <- unlist(sapply(X = flagsX, FUN = function(s) length(s)))
  nFlagComms <- unlist(sapply(X = flagcommsX, FUN = function(s) length(s)))
  idxx <- which(nFlags != nFlagComms)
  if (length(idxx) > 0) {
    for (idx in idxx) {
      fx <- flagsX[[idx]]
      fcx <- flagcommsX[[idx]]
      
      if (length(fcx) > length(fx)) {
        flagcommsX[[idx]] <- flagcommsX[[idx]][which(nchar(gsub(pattern = "[[:blank:]]", replacement = "", x = flagcommsX[[idx]])) > 0)]
        if (length(flagcommsX[[idx]]) < length(fcx)) {
          message(sprintf("Removed %d orphaned, empty flag comment(s).", length(fcx) - length(flagcommsX[[idx]])))
        }
      }
      
      if (length(fx) > length(fcx)) {
        flagsX[[idx]] <- flagsX[[idx]][which(nchar(gsub(pattern = "[[:blank:]]", replacement = "", x = flagsX[[idx]])) > 0)]
        if (length(flagsX[[idx]]) < length(fx)) {
          message(sprintf("Removed %d orphaned, empty flag(s).", length(fx) - length(flagsX[[idx]])))
        }
      }
      
      idxxDups <- which(duplicated(format_char(fcx)))
      idxxOrphans <- setdiff(1:length(fcx), 1:length(fx))
      idxxRm <- intersect(idxxDups, idxxOrphans)
      if (length(idxxRm) == length(idxxOrphans)) {
        message(sprintf("Removing %d orphaned, duplicated flag comments...", length(idxxRm)))
        flagcommsX[[idx]] <- flagcommsX[[idx]][-c(idxxRm)]
      }
    }
  }
  
  nFlags <- unlist(sapply(X = flagsX, FUN = function(s) length(s)))
  nFlagComms <- unlist(sapply(X = flagcommsX, FUN = function(s) length(s)))
  if (any(nFlags != nFlagComms)) { 
    idxxP <- which(nFlags != nFlagComms)
    message("Error: Unresolvable orphaned flag / flag comment(s) (%d) detected at the following submissions:")
    if ("submission_id" %in% names(df)) {
      message(paste(df$submission_id[idxxToUnflag][idxxP][1:5], collapse = "\n"))
      if (length(idxxP) > 5) {
        message(sprintf("...(%d more)", length(idxxP) - 5))
      }
    } else { message("<?>") }
    stop("(see above)")
  }
  
  if (is.na(pattern)) {
    for (i in 1:length(flagsX)) {
      idxx <- which(flagsX[[i]] %in% flagsToRemove)
      if (length(idxx) > 0) {
        flagsX[[i]] <- flagsX[[i]][-idxx]
        flagcommsX[[i]] <- flagcommsX[[i]][-idxx]
      }
    }
  } else {
    for (i in 1:length(flagcommsX)) {
      idxx <- grep(pattern = pattern, x = flagcommsX[[i]], fixed = fixed)
      idxx <- intersect(idxx, which(flagsX[[i]] %in% flagsToRemove))
      #message(length(idxx))
      if (length(idxx) > 0) {
        flagsX[[i]] <- flagsX[[i]][-idxx]
        flagcommsX[[i]] <- flagcommsX[[i]][-idxx]
      }
    }    
  }
  
  for (i in 1:length(flagsX)) {
    if (length(flagsX[[i]]) == 0 | length(flagcommsX[[i]]) == 0) {
      flagsX[[i]] <- NA_character_
      flagcommsX[[i]] <- NA_character_
      next
    }
    if (all(is.na(flagsX[[i]]))) { flagcommsX[[i]] <- NA_character_; next}
    if (all(is.na(flagcommsX[[i]]))) { flagsX[[i]] <- NA_character_; next}

    if (all(nchar(flagsX[[i]]) == 0) | all(nchar(flagcommsX[[i]]) == 0)) {
      flagsX[[i]] <- NA_character_
      flagcommsX[[i]] <- NA_character_
    }
  }
  
  df[[nmFlagVar]][idxxToUnflag] <- unlist(sapply(flagsX, FUN = function(s) {
    if (length(s) == 0) { return(NA_character_) }
    if (all(is.na(s))) { return(NA_character_) }
    return(paste(s, collapse = sep))
  }))
  df[[nmCommVar]][idxxToUnflag] <- unlist(sapply(flagcommsX, FUN = function(s) {
    if (length(s) == 0) { return(NA_character_) }
    if (all(is.na(s))) { return(NA_character_) }
    return(paste(s, collapse = sep))
  }))
  
  return(df)
}

# checks that each flag has a flagcomment and vice versa
checkFlagInteg <- function(df, sep = ";", nmFlagVar = "flags", nmCommVar = "flagcomms", nmResVar = "flaginteg", verbose = TRUE) {
  
  df[[nmResVar]] <- NA
  
  lsFlagSplits <- strsplit(x = df[[nmFlagVar]], split = sep, fixed = TRUE)
  lsCommSplits <- strsplit(x = df[[nmCommVar]], split = sep, fixed = TRUE)
  
  nFlags <- sapply(X = lsFlagSplits, FUN = length)
  nComms <- sapply(X = lsCommSplits, FUN = length)
  
  df[[nmResVar]] <- nFlags == nComms
  
  if (verbose) {
    nGood <- length(which(nFlags == nComms))
    nTot <- nrow(df)
    message(sprintf("Flag integrity score: %.2f (%d/%d)", (nGood/nTot)*100, nGood, nTot))
  }
  
  return(df)
}
