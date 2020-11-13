#' Date created: 2017-11-25
#' Owner: nova Institute (Reg# 1994/002614/08).

# ---------------------------------------- #
#' Functions to prepare type 1, type 2 and type 3 indicators from indicator definition tables.
# ---------------------------------------- #

# ---------------------------------------- #
#' Adds the 'popnm' var to dfIndicDefs and generates the lsPops object - needed by functions such as indicsFrmTbl_tp1

prepareIndicPops <- function(dfIndicDefs, dfData, nHhs) {
  
  # add 'popnm' var to dfIndicDefs to make it easier for us to generate the 'N_HHS...' indicators
  dfIndicDefs$popnm <- NA_character_
  
  idxx <- grep(pattern = "^N_[[:alpha:]]{0,7}HHS", x = dfIndicDefs$name)
  for (idx in idxx) {
    popNm <- paste(
      as.character(
        na.omit(c(ifelse(is.na(dfIndicDefs$groupVar[idx]), 
                         NA_character_, 
                         sprintf("GRP_%s", dfIndicDefs$groupVar[idx])),
                  ifelse(is.na(dfIndicDefs$filters[idx]), 
                         NA_character_, 
                         sprintf("FLTRNM_%s", dfIndicDefs$filters[idx]))))), 
      sep = "_", collapse = "_")
    popNm <- gsub(pattern = "==", replacement = "_FLTRVAL_", x = popNm, fixed = TRUE)
    popNm <- gsub(pattern = "[[:blank:]]|[[:punct:]]", replacement = "_", x = popNm)
    popNm <- gsub(pattern = "_{2,}", replacement = "_", x = popNm)
    popNm <- gsub(pattern = "^_|_$", replacement = "", x = popNm)
    if (nchar(popNm) == 0) {popNm <- "all"}
    dfIndicDefs$popnm[idx] <- popNm
  }
  unique(dfIndicDefs$popnm)
  
  # calculate the different population sizes for the 'N_HHS...' indicators
  dfTemp  <- dfIndicDefs[!duplicated(dfIndicDefs$popnm) & !is.na(dfIndicDefs$popnm),]
  
  lsPops <- list()
  lsPops$all <- nHhs
  
  for (ridx in 1:nrow(dfTemp)) {
    
    bigPopSize <- nHhs
    dfSelection <- dfData  
    
    if (!is.na(dfTemp$filters[[ridx]])) {
      filterSplts <- strsplit(x = dfTemp$filters[[ridx]], split = "==", fixed = TRUE)[[1]]
      nmFilterVar <- gsub(pattern = "^[[:blank:]]{0,}|[[:blank:]]{0,}$", replacement = "", x = filterSplts[1])
      filterVal <- gsub(pattern = "^[[:blank:]]{0,}|[[:blank:]]{0,}$|'", replacement = "", x = filterSplts[2])
      dfSelection <- dfSelection[which(dfSelection[[nmFilterVar]] == filterVal),]
      bigPopSize <- (nrow(dfSelection) / nrow(dfData)) * nHhs
    }
    
    nmGrpVar <- dfTemp$groupVar[[ridx]]
    
    if ((is.na(nmGrpVar) | is.na(dfTemp$indicVar[ridx]))) { 
      # if either 'groupVar' or'indicVar', all records in dfSelection were 
      # included in the calculation, so popSize will be the total (filtered) population
      popSize <- bigPopSize
    } else {
      popSize <- prop.table(table(dfSelection[[nmGrpVar]])) * bigPopSize
    }
    
    lsPops[[dfTemp$popnm[ridx]]] <- popSize
  }
  rm(dfTemp)  
  
  return(list("dfIndicDefs" = dfIndicDefs,
              "lsPops" = lsPops))
}

# ---------------------------------------- #
#' Calculate type 1 indicators ('yes'/'no' vars)

indicsFrmTbl_tp1 <- function(dfIndicDefs, dfData, srcpth, nHhs) {
  
  # generate the population object
  popStuff <- prepareIndicPops(dfIndicDefs, dfData, nHhs)
  dfIndicDefs <- popStuff[["dfIndicDefs"]]
  lsPops <- popStuff[["lsPops"]]; rm(popStuff)
  
  # ------------------- #
  # make the indicators
  failures <- c()
  lsIndics <- lapply(X = 1:nrow(dfIndicDefs), FUN = function(ridx) {
    
    #print(ridx)
    #print(dfIndicDefs[ridx,])
    
    dfSelection <- NULL
    bEmptySelection <- FALSE
    
    tryCatch(expr = {
      
      if (!is.na(dfIndicDefs$filters[ridx])) {
        splts <- strsplit(x = dfIndicDefs$filters[ridx], split = "==", fixed = TRUE)[[1]]
        varNm <- gsub(pattern = "^[[:blank:]]{0,}|[[:blank:]]{0,}$", replacement = "", x = splts[1])
        value <- gsub(pattern = "^[[:blank:]]{0,}|[[:blank:]]{0,}$|'", replacement = "", x = splts[2])
        dfSelection <- dfData[which(dfData[[varNm]] == value),]
        bEmptySelection <- nrow(dfSelection) == 0
        if (bEmptySelection) {dfSelection <- dfData}
      }
      # P.S. it is more memory efficient to generate dfSelection only when necessary (as we are doing above) 
      # and not all the time (i.e. not when dfSelection would simply have been a copy of dfData), so that
      # is why we are doing it like this.
      
      # calculate the percentages
      res <- indicatorMaker2(dfData = {if (!is.null(dfSelection)) {dfSelection} else {dfData}},
                             indicVar = dfIndicDefs$indicVar[ridx], 
                             indicOption = dfIndicDefs$indicOpt[ridx], 
                             groupVar = dfIndicDefs$groupVar[[ridx]], 
                             popSize = {if (is.na(dfIndicDefs$popnm[ridx])) {
                               NA_integer_} else {lsPops[[dfIndicDefs$popnm[ridx]]]}},
                             allowNegCIL = FALSE)
      
      if (bEmptySelection) {
        res[,c("PointEst", "Lower", "Upper")] <- NA_real_
      }
      
      # build the indicator obj
      indic <- new("indicator")
      indic@name <- dfIndicDefs$name[ridx]
      indic@val <- res
      indic@unit <- dfIndicDefs$unit[ridx]
      indic@description <- dfIndicDefs$descr[ridx]
      indic@sourcepath <- srcpth
      indic@date_created <- as.character(Sys.time())
      indic@comments <- dfIndicDefs$comment[ridx]
      
      return(indic)    
    }, 
    error = function(e) {
      failures <<- c(failures, sprintf("%s [%d]: %s", 
                                       dfIndicDefs$name[ridx], 
                                       ridx, e))
      return(NULL)})
    
  })
  if (length(failures) > 0) {
    message("Failed to generate the following indicators:")
    ctch <- sapply(X = failures, FUN = message); rm(ctch)
  }
  
  lsIndics <- lsIndics[!sapply(lsIndics,is.null)]
  if (length(lsIndics) > 0) {
    names(lsIndics) <- sapply(X = lsIndics, FUN = function(indicObj) indicObj@name)
  }
  
  return(lsIndics)
}

# ---------------------------------------- #
#' Calculate type 2 indicators (quantitative)

indicsFrmTbl_tp2 <- function(dfIndicDefs, dfData, srcpth) {
  
  failures <- c()
  lsIndics <- lapply(X = 1:nrow(dfIndicDefs), FUN = function(ridx) {
    
    #print(ridx)
    #print(dfIndicDefs[ridx,])
    
    dfSelection <- NULL
    
    tryCatch(expr = {
      
      if (!is.na(dfIndicDefs$filters[ridx])) {
        splts <- strsplit(x = dfIndicDefs$filters[ridx], split = "==", fixed = TRUE)[[1]]
        varNm <- gsub(pattern = "^[[:blank:]]{0,}|[[:blank:]]{0,}$", replacement = "", x = splts[1])
        value <- gsub(pattern = "^[[:blank:]]{0,}|[[:blank:]]{0,}$|'", replacement = "", x = splts[2])
        dfSelection <- dfData[which(dfData[[varNm]] == value),]
      }
      # P.S. it is more memory efficient to generate dfSelection only when necessary (as we are doing above) 
      # and not all the time (i.e. not when dfSelection would simply have been a copy of dfData), so that
      # is why we are doing it like this.
      
      opt <- ifelse(is.null(dfSelection), 2, ifelse(nrow(dfSelection) == 0, 1, 2))
      
      if (opt == 1) {
        res <- summaryXby(dfData = dfData, 
                          sumVar = dfIndicDefs$indicVar[ridx], 
                          groupVar = dfIndicDefs$groupVar[ridx], 
                          includeAll = TRUE, 
                          allowNegCIL = FALSE)    
        res[,setdiff(names(res), as.character(na.omit(c(dfIndicDefs$groupVar[ridx], "n"))))] <- NA_real_
        res[["n"]] <- 0    
      } 
      
      if (opt == 2) {
        res <- summaryXby(dfData = {if (is.null(dfSelection)) {dfData} else {dfSelection}}, 
                          sumVar = dfIndicDefs$indicVar[ridx], 
                          groupVar = dfIndicDefs$groupVar[ridx], 
                          includeAll = TRUE, 
                          allowNegCIL = FALSE)    
      }
      
      # build the indicator obj
      indic <- new("indicator")
      indic@name <- dfIndicDefs$name[ridx]
      indic@val <- res
      indic@unit <- dfIndicDefs$unit[ridx]
      indic@description <- dfIndicDefs$descr[ridx]
      indic@sourcepath <- srcpth
      indic@date_created <- as.character(Sys.time())
      indic@comments <- dfIndicDefs$comment[ridx]
      
      return(indic)    
    }, 
    error = function(e) {
      failures <<- c(failures, sprintf("%s [%d]: %s", 
                                       dfIndicDefs$name[ridx], ridx, e))
      return(NULL)
    })
    
  })
  if (length(failures) > 0) {
    message("Failed to generate the following indicators:")
    ctch <- sapply(X = failures, FUN = message); rm(ctch)
  }
  
  lsIndics <- lsIndics[!sapply(lsIndics,is.null)]
  if (length(lsIndics) > 0) {
    names(lsIndics) <- sapply(X = lsIndics, FUN = function(indicObj) indicObj@name)
  }
  
  return(lsIndics)
  
}

# ---------------------------------------- #
#' Calculate type 3 indicators (indicator X indicator)

#'@param indicators An environment or list of indicator objects to be used in the 
indicsFrmTbl_tp3 <- function(dfIndicDefs, indicators) {
  
  failures <- c()
  lsIndics <- lapply(X = 1:nrow(dfIndicDefs), FUN = function(ridx) {
    
    #print(ridx)
    
    tryCatch(expr = {
      
      indic1 <- indicators[[dfIndicDefs$indic1[ridx]]]
      indic2 <- indicators[[dfIndicDefs$indic2[ridx]]]
      
      theCode <- dfIndicDefs$operation[ridx]
      theCode <- gsub(pattern = "VAR1", 
                      replacement = indic1@val[[intersect(c("Mean", "PointEst"), 
                                                          names(indic1@val))]], 
                      x = theCode, fixed = TRUE)
      theCode <- gsub(pattern = "VAR2", 
                      replacement = indic2@val[[intersect(c("Mean", "PointEst"), 
                                                          names(indic2@val))]], 
                      x = theCode, fixed = TRUE)
      
      res <- eval(expr = parse(text = theCode))
      names(res) <- "PointEst"
      
      # build the indicator obj
      indic <- new("indicator")
      indic@name <- dfIndicDefs$name[ridx]
      indic@val <- res
      indic@unit <- dfIndicDefs$unit[ridx]
      indic@description <- dfIndicDefs$descr[ridx]
      indic@sourcepath <- srcpth
      indic@date_created <- as.character(Sys.time())
      indic@comments <- dfIndicDefs$comment[ridx]
      
      # for type 3 indicators we have to assign each indicator to the indicator env. 
      # the moment it is generated, because there might be other indicators that will need it
      indicators[[indic@name]] <<- indic
      
      return(indic)   
      
    }, error = function(e) {
      failures <<- c(failures, sprintf("%s [%d]: %s", dfIndicDefs$name[ridx], ridx, e))
      return(NULL)
    })
    
  })
  if (length(failures) > 0) {
    message("Failed to generate the following indicators:")
    ctch <- sapply(X = failures, FUN = message); rm(ctch)
  }
  
  lsIndics <- lsIndics[!sapply(lsIndics,is.null)]
  if (length(lsIndics) > 0) {
    names(lsIndics) <- sapply(X = lsIndics, FUN = function(indicObj) indicObj@name)
  }
  
  return(lsIndics)
  
}




# ---------------------------------------- #
# DOODLES
# ---------------------------------------- #

# res <- purrrlyr::invoke_rows(.d = dfIndicDefs[, c("indicVar","indicOption","groupVar")], 
#                              .f = indicatorMaker2, 
#                              dfData = dfHousehold, 
#                              .to = "indic", 
#                              .collate = "list")