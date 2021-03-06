#' Date created: 2017-11-25
#' Owner: nova Institute (Reg# 1994/002614/08).

# ---------------------------------------- #
#' Functions to prepare type 1, type 2 and type 3 indicators from indicator definition tables.
# ---------------------------------------- #

# ---------------------------------------- #
#' Adds the 'popnm' var to dfIndicDefs and generates the lsPops object - needed by functions such as indicsFrmTbl_tp1


#'@description function that turns table objects to indicators objects
#'@title indicatorsFromTbl
#'@name indicatorsFromTbl
#'@param dfIndicDefs dataframe containing the indicator definitions
#'@param dfData dataframe containing the data that will be indicatified
#'@param nHhs number of households in the dataset
#'@export

prepareIndicPops <- function(dfIndicDefs, dfData, nHhs) {
  
  # add 'popnm' var to dfIndicDefs to make it easier for us to generate the 'N_HHS...' indicators
  dfIndicDefs$popnm <- NA_character_
  
  idxx <- grep(pattern = "^N_[[:alpha:]]{0,7}HHS_", x = dfIndicDefs$name)
  for (idx in idxx) {
    popNm <- paste(
      as.character(
        na.omit(c(ifelse(is.na(dfIndicDefs$group_var[idx]), 
                         NA_character_, 
                         sprintf("GRP_%s", dfIndicDefs$group_var[idx])),
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
      nmFilterVar <- format_char(filterSplts[1])
      filterVal <- format_char(filterSplts[2])
      dfSelection <- dfSelection[which(dfSelection[[nmFilterVar]] == filterVal),]
      bigPopSize <- (nrow(dfSelection) / nrow(dfData)) * nHhs
    }
    
    nmGrpVar <- dfTemp$group_var[[ridx]]
    
    if ((is.na(nmGrpVar) | is.na(dfTemp$indic_var[ridx]))) { 
      # if either 'group_var' or'indic_var', all records in dfSelection were 
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
#'  
#'  
#'  
#'@description function that creates indicators for type 1 (discreet) data values e.g 'yes'/'no'
#'@title indicsFrmTbl_tp1
#'@name indicsFrmTbl_tp1
#'@param dfIndicDefs dataframe containing the indicator definitions
#'@param dfData dataframe containing the data that will be indicatified
#'@param srcpth path to the script that will generate this indicator
#'@param nHhs number of households in the dataset
#'@export

indicsFrmTbl_tp1 <- function(dfIndicDefs, dfData, srcpth, nHhs) {
  
 require(dplyr)
 
 # generate the population object
 dfIndicDefs$popnm <- NA_character_
 warning("Temporary fix. Uncomment this.")
 # popStuff <- prepareIndicPops(dfIndicDefs, dfData, nHhs)
 # dfIndicDefs <- popStuff[["dfIndicDefs"]]
 # lsPops <- popStuff[["lsPops"]]; rm(popStuff)
 
 
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
       
        eval(parse(text = paste("dfSelection <- dfData %>% filter(", 
                    dfIndicDefs$filters[ridx], 
                    ") %>% collect() %>% as.data.frame()", 
                    sep = "")))

        bEmptySelection <- nrow(dfSelection) == 0
        if (bEmptySelection) { dfSelection <- dfData }
      }
      # P.S. it is more memory efficient to generate dfSelection only when necessary (as we are doing above) 
      # and not all the time (i.e. not when dfSelection would simply have been a copy of dfData), so that
      # is why we are doing it like this.
      
     if (is.null(dfSelection)) { dfSelection <- dfData }
     
      # calculate the percentages
      res <- indicatorMaker2(dfData = dfSelection,
                             indicVar = dfIndicDefs$indic_var[ridx], 
                             indicOption = dfIndicDefs$indic_opt[ridx], 
                             groupVar = dfIndicDefs$group_var[[ridx]], 
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

#'@description function that creates indicators for type 2 (quantitative) data values
#'@title indicsFrmTbl_tp2
#'@name indicsFrmTbl_tp2
#'@param dfIndicDefs dataframe containing the indicator definitions
#'@param dfData dataframe containing the data that will be indicatified
#'@param srcpth path to the script that will generate this indicator
#'@param nHhs number of households in the dataset
#'@export

indicsFrmTbl_tp2 <- function(dfIndicDefs, dfData, srcpth, bPlot = TRUE) {
  
 require(dplyr)
 
  failures <- c()
  lsIndics <- lapply(X = 1:nrow(dfIndicDefs), FUN = function(ridx) {
    
    #print(ridx)
    #print(dfIndicDefs[ridx,])
    
    dfSelection <- NULL
    
    tryCatch(expr = {
     
     if (!is.na(dfIndicDefs$filters[ridx])) {
       eval(parse(text = paste("dfSelection <- dfData %>% filter(", 
                               dfIndicDefs$filters[ridx], 
                               ") %>% collect() %>% as.data.frame()", 
                               sep = "")))
       
       bEmptySelection <- nrow(dfSelection) == 0
       if (bEmptySelection) { dfSelection <- dfData }
     }
     
      # P.S. it is more memory efficient to generate dfSelection only when necessary (as we are doing above) 
      # and not all the time (i.e. not when dfSelection would simply have been a copy of dfData), so that
      # is why we are doing it like this.
      
      opt <- ifelse(is.null(dfSelection), 2, ifelse(nrow(dfSelection) == 0, 1, 2))
      
      if (is.null(dfSelection)) { dfSelection <- dfData }
      
      dfSelection <- dfSelection[,c("submission_id",
                                    as.character(na.omit(c(dfIndicDefs$indic_var[ridx],
                                                           dfIndicDefs$group_var[ridx]))))]
      
      if (opt == 1) {
        res <- summaryXby(dfData = dfSelection, 
                          sumVar = dfIndicDefs$indic_var[ridx], 
                          groupVar = dfIndicDefs$group_var[ridx], 
                          includeAll = TRUE, 
                          allowNegCIL = FALSE)    
        res[,setdiff(names(res), as.character(na.omit(c(dfIndicDefs$group_var[ridx], "n"))))] <- NA_real_
        res[["n"]] <- 0    
      } 
      
      if (opt == 2) {
        res <- summaryXby(dfData = dfSelection, 
                          sumVar = dfIndicDefs$indic_var[ridx], 
                          groupVar = dfIndicDefs$group_var[ridx], 
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
      
      if (bPlot) {
        tryCatch({
          
          pltStr <- sprintf("p <- ggplot(data = dfSelection, mapping = aes(x = %s%s)) + geom_density() + ggtitle(\"%s\") + xlim(0, %s) + %s",
                            dfIndicDefs$indic_var[ridx],
                            ifelse(is.na(dfIndicDefs$group_var[ridx]), 
                                   "",
                                   ", group = dfIndicDefs$group_var[ridx], fill = dfIndicDefs$group_var[ridx]"),
                            sprintf("Density distribution of variable '%s'", 
                                    dfIndicDefs$indic_var[ridx]),
                            ceiling(max(dfSelection[[dfIndicDefs$indic_var[ridx]]], na.rm = TRUE)),
                            "scale_x_continuous(n.breaks = 10)")
          eval(parse(text = pltStr))
          indic@plots <- list(p)
          
        }, error = function(e) {
          warning(sprintf("Failed to generate plot for variable '%s'. Error: ", dfIndicDefs$indic_var[ridx], e))
        })
      }
      
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

#'@description function that creates indicators for type 2 (indicator X indicator) data values
#'@title indicsFrmTbl_tp3
#'@name indicsFrmTbl_tp3
#'@param dfIndicDefs dataframe containing the indicator definitions
#'@param indicators An environment or list of indicator objects to be used in the 
#'@export

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

# res <- purrrlyr::invoke_rows(.d = dfIndicDefs[, c("indic_var","indicOption","group_var")], 
#                              .f = indicatorMaker2, 
#                              dfData = dfHousehold, 
#                              .to = "indic", 
#                              .collate = "list")