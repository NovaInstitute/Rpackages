# Date created: 2017-11-22
# Owner: Nova Institute (Reg# 1994/002614/08).

setClass("batch",
         slots = list(
           batchName = "character",
           fieldworkerName = "character",
           fieldworkerID = "character",
           deviceID = "character",
           date = "ANY",
           dataMobenziStands = "ANY",
           dataMobenziHhs = "ANY",
           dataMobenziStructs = "ANY",
           dataEndomondo = "ANY",
           targetErven = "ANY",
           bbox = "ANY",
           dataMobenziQC = "ANY",
           correctionsLog = "ANY",
           unresolved = "ANY")) 

print.batch <- function(x) {
  stop("TBC")
}

plot.batch <- function(x, type = c("std", "ggplot2", "plotly")[2], zoomToPoints = TRUE) {
  
  if (type == "std") {
    message("Functionality not yet ready. Please try another plot type.")
    return(NULL)
  }
  
  if (type %in% c("ggplot2", "plotly")) {
    
    plotString <- "ggplot()"
    xMin <- NA_real_
    xMax <- NA_real_
    yMin <- NA_real_
    yMax <- NA_real_
    
    if (!is.null(x@targetErven)) {
      
      erven <- lapply(X = x@targetErven[[1]]@polygons, FUN = function(p) p@Polygons[[1]]@coords)
      names(erven) <- x@targetErven[[1]]@data$Label
      erven <- lapply(X = names(erven), FUN = function(erfno) {
        erf <- erven[[erfno]]
        return(data.frame(stringsAsFactors = FALSE,
                          lon = erf[,1],
                          lat = erf[,2],
                          erf_no = erfno))
      })
      erven <- do.call("rbind", erven)
      ervenNums <- dplyr::summarise(dplyr::group_by(erven, erf_no),
                                    lon = mean(lon),
                                    lat = mean(lat))
      
      plotString <- sprintf("%s + %s + %s",
                            plotString, 
                            "geom_path(data = erven, mapping = aes(x = lon, y = lat, group = erf_no))", 
                            "geom_text(data = ervenNums, mapping = aes(x = lon, y = lat, label = erf_no), size =2)")
  
      if (zoomToPoints) {
        xMin <- max(xMin, min(ervenNums$lon, na.rm = TRUE), na.rm = TRUE)
        xMax <- min(xMax, max(ervenNums$lon, na.rm = TRUE), na.rm = TRUE)
        yMin <- max(yMin, min(ervenNums$lat, na.rm = TRUE), na.rm = TRUE)
        yMax <- min(yMax, max(ervenNums$lat, na.rm = TRUE), na.rm = TRUE)
      } else {
        xMin <- min(xMin, min(ervenNums$lon, na.rm = TRUE), na.rm = TRUE)
        xMax <- max(xMax, max(ervenNums$lon, na.rm = TRUE), na.rm = TRUE)
        yMin <- min(yMin, min(ervenNums$lat, na.rm = TRUE), na.rm = TRUE)
        yMax <- max(yMax, max(ervenNums$lat, na.rm = TRUE), na.rm = TRUE)        
      }
      
    }
    
    if (!is.null(x@dataMobenziStands)) {
      
      dfStands <- x@dataMobenziStands
      dfStands <- dfStands[order(dfStands$date, decreasing = FALSE),]
      rownames(dfStands) <- 1:nrow(dfStands)
      
      plotString <- sprintf("%s + %s + %s + %s",
                            plotString,
                            "geom_point(data = dfStands, mapping = aes(x = longitude, y = latitude),  colour = 'blue')",
                            "geom_path(data = dfStands, mapping = aes(x = longitude, y = latitude), colour = 'blue')",
                            "geom_text(data = dfStands, mapping = aes(x = longitude, y = latitude, label = stand_number_1), colour = 'blue', size = 2)")
      
      if (zoomToPoints) {
        xMin <- max(xMin, min(dfStands$longitude, na.rm = TRUE), na.rm = TRUE)
        xMax <- min(xMax, max(dfStands$longitude, na.rm = TRUE), na.rm = TRUE)
        yMin <- max(yMin, min(dfStands$latitude, na.rm = TRUE), na.rm = TRUE)
        yMax <- min(yMax, max(dfStands$latitude, na.rm = TRUE), na.rm = TRUE)      
      } else {
        xMin <- min(xMin, min(dfStands$longitude, na.rm = TRUE), na.rm = TRUE)
        xMax <- max(xMax, max(dfStands$longitude, na.rm = TRUE), na.rm = TRUE)
        yMin <- min(yMin, min(dfStands$latitude, na.rm = TRUE), na.rm = TRUE)
        yMax <- max(yMax, max(dfStands$latitude, na.rm = TRUE), na.rm = TRUE)          
      }
    }
    
    if (!is.null(x@dataMobenziHhs)) {
      
      dfHhs <- x@dataMobenziHhs
      dfHhs <- dfHhs[order(dfHhs$date, decreasing = FALSE),]
      rownames(dfHhs) <- 1:nrow(dfHhs)
      
      plotString <- sprintf("%s + %s + %s + %s",
                            plotString,
                            "geom_point(data = dfHhs, mapping = aes(x = longitude, y = latitude), colour = 'orange')",
                            "geom_path(data = dfHhs, mapping = aes(x = longitude, y = latitude), colour = 'orange')",
                            "geom_text(data = dfHhs, mapping = aes(x = longitude, y = latitude, label = stand_number_1), colour = 'orange', size = 2)")
      
      if (zoomToPoints) {
        xMin <- max(xMin, min(dfHhs$longitude, na.rm = TRUE), na.rm = TRUE)
        xMax <- min(xMax, max(dfHhs$longitude, na.rm = TRUE), na.rm = TRUE)
        yMin <- max(yMin, min(dfHhs$latitude, na.rm = TRUE), na.rm = TRUE)
        yMax <- min(yMax, max(dfHhs$latitude, na.rm = TRUE), na.rm = TRUE)  
      } else {
        xMin <- min(xMin, min(dfHhs$longitude, na.rm = TRUE), na.rm = TRUE)
        xMax <- max(xMax, max(dfHhs$longitude, na.rm = TRUE), na.rm = TRUE)
        yMin <- min(yMin, min(dfHhs$latitude, na.rm = TRUE), na.rm = TRUE)
        yMax <- max(yMax, max(dfHhs$latitude, na.rm = TRUE), na.rm = TRUE)          
      }
      
    }
    
    if (!is.null(x@dataEndomondo)) {
      
      dfEndo <- x@dataEndomondo
      dfEndo <- dfEndo[order(dfEndo$time, decreasing = FALSE),]
      rownames(dfEndo) <- 1:nrow(dfEndo)
      # dfEndo <- dfEndo[which(dfEndo$time >= min(dfStands$date) &
      #                          dfEndo$time <= max(dfStands$date)),]    
      
      plotString <- sprintf("%s + %s + %s",
                            plotString,
                            "geom_point(data = dfEndo, mapping = aes(x = lon, y = lat), colour = 'brown')",
                            "geom_path(data = dfEndo, mapping = aes(x = lon, y = lat), colour = 'brown')")
      
      if (zoomToPoints) {
        xMin <- max(xMin, min(dfEndo$lon, na.rm = TRUE), na.rm = TRUE)
        xMax <- min(xMax, max(dfEndo$lon, na.rm = TRUE), na.rm = TRUE)
        yMin <- max(yMin, min(dfEndo$lat, na.rm = TRUE), na.rm = TRUE)
        yMax <- min(yMax, max(dfEndo$lat, na.rm = TRUE), na.rm = TRUE)      
      } else {
        xMin <- min(xMin, min(dfEndo$lon, na.rm = TRUE), na.rm = TRUE)
        xMax <- max(xMax, max(dfEndo$lon, na.rm = TRUE), na.rm = TRUE)
        yMin <- min(yMin, min(dfEndo$lat, na.rm = TRUE), na.rm = TRUE)
        yMax <- max(yMax, max(dfEndo$lat, na.rm = TRUE), na.rm = TRUE)              
      }
      
    }
    
    
    # gmap <- get_map(location = c(mean(ervenNums$lon), mean(ervenNums$lat)),
    #                 source = "google", maptype = "roadmap", zoom = 15)
    #p <- ggmap(ggmap = gmap) +
    
    plotString <- sprintf("%s + %s + %s + %s + %s + %s",
                          plotString,
                          "xlim(xMin - ((xMax-xMin)/2), xMax + ((xMax-xMin)/2))",
                          "ylim(yMin - ((yMax-yMin)/2), yMax + ((yMax-yMin)/2))",
                          "xlab('Longitude')",
                          "ylab('Latitude')",
                          "ggtitle(x@batchName, names(x@targetErven)[1])")
    
    p <- eval(parse(text = plotString))
    
    if (type == "plotly") { plotly::ggplotly(p) } else {plot(p)}
    
  }
}

#' Function to combine all the data (Mobenzi x2, Endomondo, maps from LSG) into
#' batches (batch = 1 fw x 1 day) 
makeBatches <- function(dfStands = NULL, 
                        dfHhs = NULL, 
                        dfStructs = NULL, 
                        dfEndo = NULL, 
                        lsErvenSpdfs = NULL,
                        rdadir = NULL,
                        nmStandRda = "SQDT",
                        nmHhRda = "HQDT",
                        nmEndoRda = "EMDT",
                        nmErvenRda = "ERVDT") {

  require(plyr)
    
  if (any(is.null(c(dfStands, dfHhs, dfStructs, dfEndo, lsErvenSpdfs))) &
      is.null(rdadir)) {
    stop("Either 'rdadir' or all in c(dfStands, dfHhs, dfStructs, dfEndo, lsErvenSpdfs) must be specified.")
  }
  
  #' Check/get stand questionnaire data
  if (is.null(dfStands)) {
    load(paste(rdadir, nmStandRda, ".Rda", sep = ""))
    dfStands <- dfData_SQDT; rm(dfData_SQDT, lsExtra_SQDT)
  }
  
  #' Get/check household questionnaire data
  if (is.null(dfHhs) | is.null(dfStructs)) {
    load(paste(rdadir, nmHhRda, ".Rda", sep = ""))
    dfHhs <- dfHhs_HQDT; rm(dfHhs_HQDT)
    dfStructs <- dfStructs_HQDT; rm(dfStructs_HQDT)
    rm(lsExtra_HQDT)
  }
  
  #' Get/check Endomondo data
  if (is.null(dfEndo)) {
    load(paste(rdadir, nmEndoRda, ".Rda", sep = ""))
    dfEndo <- dfData_EMDT; rm(dfData_EMDT)
  }
  
  #' Get/check erven data
  if (is.null(lsErvenSpdfs)) {
    load(paste(rdadir, nmErvenRda, ".Rda", sep = ""))
  }
  
  # ------------------- #

  #' Prepare 'Stand questionnaire' data
  dfStands$batch_nm <- paste(dfStands$fieldworker_name, 
                            gsub(pattern = "[[:punct:]]",
                                 replacement = "", 
                                 x = dfStands$date2),
                            sep = "_")
  lsStands <- split(x = dfStands, f = dfStands$batch_nm)
  
  #' Prepare 'Household' questionnaire data
  dfHhs$batch_nm <- paste(dfHhs$fieldworker_name, 
                         gsub(pattern = "[[:punct:]]",
                              replacement = "", 
                              x = dfHhs$date2),
                         sep = "_")
  lsHhs <- split(x = dfHhs, f = dfHhs$batch_nm)
  
  idxx <- match(x = dfStructs$submission_id, table = dfHhs$submission_id)
  dfStructs$batch_nm <- dfHhs$batch_nm[idxx]
  lsStructs <- split(x = dfStructs, f = dfStructs$batch_nm)
  
  #' Prepare 'Endomondo' data
  lsEndo <- split(x = dfEndo, f = dfEndo$src)
  
  # ------------------- #
  
  #' Make batches
  batchNames <- unique(c(dfStands$batch_nm, dfHhs$batch_nm, dfStructs$batch_nm))
  
  lsBatches <- lapply(X = batchNames, FUN = function(bnm) {
    btch <- new("batch")
    btch@batchName <- bnm
    
    subpl <- c()
    
    # add stand data
    if (bnm %in% names(lsStands)) {
      btch@dataMobenziStands <- lsStands[[bnm]]
      btch@fieldworkerName <- unique(btch@dataMobenziStands$fieldworker_name)
      btch@fieldworkerID <- unique(btch@dataMobenziStands$fieldworker_id)
      btch@deviceID <- unique(btch@dataMobenziStands$device)
      btch@date <- unique(btch@dataMobenziStands$date2)
      
      subpl <- unique(c(subpl, btch@dataMobenziStands$stand_subplace_embalenhle))
    }
    
    # add household data
    if (bnm %in% names(lsHhs)) {
      btch@dataMobenziHhs <- lsHhs[[bnm]]
      
      if (is.null(btch@dataMobenziStands)) {
        btch@fieldworkerName <- unique(btch@dataMobenziHhs$fieldworker_name)
        btch@fieldworkerID <- unique(btch@dataMobenziHhs$fieldworker_id)
        btch@deviceID <- unique(btch@dataMobenziHhs$device)
        btch@date <- unique(btch@dataMobenziHhs$date2)      
      }
      
      subpl <- unique(c(subpl, btch@dataMobenziHhs$stand_subplace_embalenhle))
    }
    
    # add struct data
    if (bnm %in% names(lsStructs)) {
      btch@dataMobenziStructs <- lsStructs[[bnm]]
    }
    
    # add Endomondo data
    idx <- grep(pattern = sprintf("%s_%s", 
                                  gsub(pattern = "nova_",
                                       replacement = "", 
                                       x = btch@deviceID, 
                                       fixed = TRUE),
                                  gsub(pattern = "-",
                                       replacement = "", 
                                       x = btch@date, 
                                       fixed = TRUE)),
                x = names(lsEndo), fixed = TRUE)
    if (length(idx) > 0) {
      btch@dataEndomondo <- do.call("rbind.fill", lsEndo[idx])
      # there shouldn't actually be more than one match, but let's prepare for
      # the rare case where there is more than one match.
    }
    
    # add erven data
    if (length(subpl) > 0) {
      idxx <- which(names(lsSubplaces_ERVDT) %in% subpl)
      if (length(idxx) >0) {
        btch@targetErven <- lsSubplaces_ERVDT[idxx]
      }
    }
    
    return(btch)
  })
  
  names(lsBatches) <- batchNames
  
  # ------------------- #
  
  return(lsBatches)
  
}


#' Returns smallest inclusive bounding box for the batch
getBatchMinBbox <- function(x, inclEndo = TRUE) {
  
  xMin <- NA_real_
  xMax <- NA_real_
  yMin <- NA_real_
  yMax <- NA_real_
  
  if (!is.null(x@dataMobenziStands)) {
    xMin <- min(x@dataMobenziStands$longitude, na.rm = TRUE)
    xMax <- max(x@dataMobenziStands$longitude, na.rm = TRUE)
    yMin <- min(x@dataMobenziStands$latitude, na.rm = TRUE)
    yMax <- max(x@dataMobenziStands$latitude, na.rm = TRUE)
  }
  
  if (!is.null(x@dataMobenziHhs)) {
    xMin <- min(xMin, min(x@dataMobenziHhs$longitude, na.rm = TRUE), na.rm = TRUE)
    xMax <- max(xMax, max(x@dataMobenziHhs$longitude, na.rm = TRUE), na.rm = TRUE)
    yMin <- min(yMin, min(x@dataMobenziHhs$latitude, na.rm = TRUE), na.rm = TRUE)
    yMax <- max(yMax, max(x@dataMobenziHhs$latitude, na.rm = TRUE), na.rm = TRUE)
  }  
  
  if (inclEndo & !is.null(x@dataEndomondo)) {
    xMin <- min(xMin, min(x@dataEndomondo$lon, na.rm = TRUE), na.rm = TRUE)
    xMax <- max(xMax, max(x@dataEndomondo$lon, na.rm = TRUE), na.rm = TRUE)
    yMin <- min(yMin, min(x@dataEndomondo$lat, na.rm = TRUE), na.rm = TRUE)
    yMax <- max(yMax, max(x@dataEndomondo$lat, na.rm = TRUE), na.rm = TRUE)
  }  
  
  x@bbox <- matrix(nrow = 2, ncol = 2,
                   dimnames = list(c("lon", "lat"), c("min","max")), 
                   data = c(xMin, xMax, yMin, yMax), 
                   byrow = TRUE)
  
  return(x@bbox)
}


# idxx <- which(sapply(X = lsBatches, FUN = function(x) {
#   return(!is.null(x@dataMobenziStands) &
#            !is.null(x@dataMobenziHhs) & 
#            !is.null(x@dataMobenziStructs) &
#            !is.null(x@dataEndomondo) & 
#            !is.null(x@targetErven))
# }))


#plot.batch(x = lsBatches[[17]])
