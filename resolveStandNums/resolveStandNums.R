#source("./global.R")
load("extdata/lsBatches_20171127004953.Rda")

batch <- lsBatches[[2]]
dfEndo <- batch@dataEndomondo
dfEndo$time <- dfEndo$time + 2*60*60
batch@dataEndomondo <- dfEndo
dd <- resolveStandNums(batch, plot = T, scale = 17)
hh <- dd@dataMobenziHhs
st <- dd@dataMobenziStands
i <- i + 1


#' Date created: 2017-11-24
#'@name resolveStandNums
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description Finds the most likely standnumber for each entry and gives its measure of certainty
#'@param dfMob [data frame]
#' A data frame containing data from Mobenzi.
#' Must have at least three variables - latitude [num], longitude [num], date [POSIXct].
#'@param dfEndo [SpatialLinesDataFrame]
#' A spatial lines data frame containing the data from Endomondo.
#' Must have at least three variables - latitude [num], longitude [num], date [POSIXct].
#'@param erwe [list/shapefile] that contains the polygons of the stands where the
#'mobenzi/endomondo data was collected
#'@param batch [list] a batch containing slots - dataMobenziStands [data frame], dataMobenziHhs,
#'[data frame],  dataEndomondo [data frame] and targetErven [list]
#'@param plot [boolean] Indicates whether a visual representation of the data should be generated.
#'@param scale [num] allows one to adjust the zoom of the map generated if the data is widely spread.
#'@return [batch/list]
#'  dataMobenziStands [data frame] and dataMobenziHhs [data frame] which has two columns added:
#'  stand_number_final - the most likely stand number
#'  and stand_number_status - description of the certainty of the final standnumber
#'  OR, if a batch was not given as argument, a the Mobenzi data frame given as parameter is
#'  returned with the stand_number_final and stand_number_status added to it.#'
#'@export
resolveStandNums <- function(batch, dfMob, dfEndo, erwe, plot = TRUE, scale = 17) {

  if (!missing(batch)) {
    erwe <- batch@targetErven
    mob_hh <- batch@dataMobenziStands
    mob_st <- batch@dataMobenziHhs

    dfMob <- rbind(mob_st[,c("submission_id",
                             "fieldworker_name",
                             "date", "start", "end",
                             "device",
                             "latitude", "longitude",
                             "gps_location_location_latitude",
                             "gps_location_location_longitude",
                             "stand_number_1", "stand_number_2",
                             "stand_subplace_embalenhle")],
                   mob_hh[,c("submission_id",
                             "fieldworker_name",
                             "date", "start", "end",
                             "device",
                             "latitude", "longitude",
                             "gps_location_location_latitude",
                             "gps_location_location_longitude",
                             "stand_number_1", "stand_number_2",
                             "stand_subplace_embalenhle")])
    dfEndo <- batch@dataEndomondo
  }
  Mobenzi <- dfMob

  dfMob$comments <- NA
  dfMob$Dist <- NA_real_
  dfMob$Time <-  difftime(NA, NA)
  dfMob$Speed <- NA_real_
  dfMob$SpeedStatus <- NA
  dfMob$stand_number_status <- NA
  dfMob$stand_number_final <- NA
  dfMob$len <- NA
  dfMob$status <- "Fine"
  dfMob <- dfMob[order(dfMob$start),]
  dfMob$sort <- 1:nrow(dfMob)
  idxx <- which(dfMob$stand_number_1 != dfMob$stand_number_2)
  dfMob$comments[idxx] <- "Stand numbers mismatch"
  dfMob <- dfMob[order(dfMob$date),]
  rownames(dfMob) <- 1:nrow(dfMob)

  # identifies entries from previous days that only got sent now
  day <- as.POSIXct(x = strptime(x = dfMob$start, format = "%Y-%m-%d"))
  med <- median(day)
  idxx <- which(day != med)
  dfMob$comments[idxx] <- "Wrong day"

  idxx <- which(dfMob$comments == "Wrong day")
  if (length(idxx) > 0) {
    mob <- subset(dfMob[-idxx,])
  } else {
    mob <- dfMob
  }
  mob <- fixMobenzi(dfMob = mob, erwe = erwe)
  dfMob <- rbind(mob, dfMob[idxx,])
  nas <- which(is.na(dfMob$longitude))
  withna <- dfMob
  if (length(nas) > 0) {
    dfMob <- subset(dfMob[-nas,])
  }

  dfMob <- dfMob[order(dfMob$date),]
  rownames(dfMob) <- 1:nrow(dfMob)
  ervenPolys <- erwe[[1]]@polygons
  ervenPolys <- lapply(X = ervenPolys, FUN = function(ep) ep@Polygons[[1]])
  names(ervenPolys) <- erwe[[1]]@data$Label

  dfErwe <- getErwe(erwe = erwe, dfMob = dfMob, ervenPolys = ervenPolys)

  mobstands <- getStandNums(sdf = dfMob, erwe = dfErwe, cutoff = 25, ervenPolys = ervenPolys)
  mobstands$id <- paste(LETTERS[(((1:nrow(mobstands))-1)%%26)+1], ((1:nrow(mobstands))-1)%/%26, sep = "")
  endstands <- NULL

  if (!missing(dfEndo) & !is.null(dfEndo)) {
    endo <- data.frame(longitude = dfEndo$lon, latitude = dfEndo$lat, date = dfEndo$time)
    endo <- endo[order(endo$date),]
   # rownames(endo) <- 1:nrow(rownames(endo))
    endstands <- getStandNums(sdf = endo, erwe = dfErwe, cutoff = 25, ervenPolys = ervenPolys)
    endomenzi <- endoMenzi(dfMob = mobstands, dfEndo = endstands)
    endstands <- mixEndo(mindex = endstands, endomenzi = endomenzi)
  }
  stands <- makeStands(mobstands = mobstands, endstands = endstands)

  idxx <- which(dfMob$comments == "Wrong day")
  if (length(idxx) > 0) {
    mob <- subset(dfMob[-idxx,])
  } else {
    mob <- dfMob
  }

  mob <- getSpeed(dfMob = mob)
  dfMob <- rbind(mob, dfMob[idxx,])
  dfMob <- addStands(stands = stands, mob = dfMob)


  idxx <- which(dfMob$stand_number_status == "VERIFIED" & dfMob$SpeedStatus == "FINE")
  if (length(idxx)) {
    dfMob$status[idxx] <- "Sure"
  }
  idxx <- which(dfMob$stand_number_status == "CHECK STAND_NUMBER_1&2")
  if (length(idxx)) {
    dfMob$status[idxx] <- "Could not verify standnums"
  }
  idxx <- which( dfMob$SpeedStatus == "CHECK")
  if (length(idxx)) {
    dfMob$status[idxx] <- "Speed anomaly"
  }
  idxx <- which( dfMob$SpeedStatus == "CHECK" & dfMob$stand_number_status != "VERIFIED")
  if (length(idxx)) {
    dfMob$status[idxx] <- "Speed anomaly & standnum not verified"
  }
  idxx <- which(dfMob$comments == "Wrong day")
  if (length(idxx) > 0) {
    dfMob$status[idxx] <- paste(dfMob$status[idxx], " Wrong day", sep = ",")
  }
  idxx <- which(dfMob$comments == "Coords not verified")
  if (length(idxx) > 0) {
    dfMob$status[idxx] <- "Coords not verified"
  }

  dfMob <- dfMob[order(dfMob$start),]
  if (plot) {
    if (!is.null(dfEndo)) {
      map <- drawMap(dfErwe = dfErwe, dfMob = dfMob, dfEndo = endo, scale = scale)
    } else {
      map <- drawMap(dfErwe = dfErwe, dfMob = dfMob, scale = scale)
    }
    plot(map)
  }
  dfMob <- rbind(dfMob, withna[nas,])

  if(missing(batch)) {
    dfMob <- dfMob[order(dfMob$date),]
    Mobenzi <- Mobenzi[order(Mobenzi$date),]
    Mobenzi$stand_number_final <- dfMob$stand_number_final
    Mobenzi$stand_number_status <- dfMob$status
    Mobenzi$label <- dfMob$sort
    return(Mobenzi)
  } else {
    if (is.null(mob_hh)) {
      print("No household questionnaires")
    } else {
      mob_hh <- makeBatch(dfMob = dfMob, mob = mob_hh)
      batch@dataMobenziHhs <- mob_hh
    }
    mob_st <- makeBatch(dfMob = dfMob, mob = mob_st)
    batch@dataMobenziStands <- mob_st
    return(batch)
  }
}


#' Date created: 2017-11
#'@name endoMenzi
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description reconciles Mobenzi and Endomondo data according to the times they were recorded
#'@param dfMob [data frame]
#' A data frame containing data from Mobenzi.
#' Must have at least three variables - lat [num], long [num], date [POSIXct].
#'@param spdfEndo [SpatialLinesDataFrame or data frame]
#' A spatial lines data frame containing the data from Endomondo.
#' Must have at least three variables - lat [num], long [num], date [POSIXct].
#'@param mobLat [char,length=1]
#' The name of the 'latitude' variable ("column") in dfMob.
#' Defaults to "latitude".
#'@param mobLong [char,length=1]
#' The name of the 'longitude' variable ("column") in dfMob.
#' Defaults to "longitude".
#'@param mobDate [char,length=1]
#' The name of the 'date' variable ("column") in dfMob.
#' Defaults to "date".
#'@return [data frame]
#' A single data frame containing the reconciled Mobezi and Endomondo data
#'
endoMenzi <- function(dfMob, dfEndo,
                      mobLat = "latitude", mobLong = "longitude",
                      mobDate = "date",
                      endoLat = "latitude", endoLong = "longitude",
                      endoDate = "date") {

  dfEndo$id <- paste(LETTERS[(((1:nrow(dfEndo))-1)%%26)+1], ((1:nrow(dfEndo))-1)%/%26, sep = "")
  dfEndo$sort <- 1:nrow(dfEndo)

  lsdfMob <- lapply(1:nrow(dfMob), function(x) {
    row <-  dfMob[x,]
    submission_id <- row$submission_id
    upperID <- dfEndo$id[which(!(dfEndo$date < row$date))[1]]
    lowerID <- dfEndo$id[which(!row$date < dfEndo$date )[length(which(!row$date < dfEndo$date))]]

    if(is.na(upperID)){
      upperID <- "---"
      erow <- dfEndo[dfEndo$id == lowerID, ]
    } else{
      erow <- dfEndo[dfEndo$id == upperID, ]
    }
    rsort = erow$sort
    df <- data.frame( ID = row$id, submission_id = submission_id, lonEndo = erow$longitude, latEndo = erow$latitude,
                      endoDate = erow$date, mobDate = row$date)
    return(df)
  })
  joined <- do.call("rbind", lsdfMob)
  return(joined)
}

#' Date created: 2017-11
#'@name getErwe
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description identifies the erven relevant to the data
#'@param erwe [list] - list of erwe
#'@param dfMob [data frame] - data frame containing the Mobenzi data
#'@param ervenPolys [list] - list of polygons of the erwe
#'@return [data frame] - a data frame containing erven that are relevat, specifying whether
#'each one is a gps match, was indicated on a questionnaire, or is within the cutoff distance
#'from an indicated stand.
getErwe <- function(erwe, dfMob, ervenPolys){

  table <- getStands(erwe)
  table$gpsmatch <- 0
  table$quest <- 0
  table$surround <- 0

  lonmin <- min(dfMob$longitude)  - 0.0005
  lonmax <- max(dfMob$longitude) + 0.0005
  latmin <- min(dfMob$latitude)  - 0.0005
  latmax <- max(dfMob$latitude) +  0.0005

  lons <- (table$stand_long < lonmax) & (table$stand_long > lonmin)
  lats <- (table$stand_lat < latmax) & (table$stand_lat > latmin)
  coords <- (lons & lats)
  newtable <- table[which(coords),]
  table <- newtable
  table$index <- 1:nrow(table)


  #Submission ids match questionnaires
  h <- hash::hash(table$labels, table$index)
  idxx <- hash::has.key(hash = h, key = dfMob$stand_number_1)
  use <- dfMob$stand_number_1[which(idxx)]
  indices <- data.frame(vals = hash::values(h, keys=use ))
  idxx <- hash::has.key(hash = h, key = dfMob$stand_number_2)
  use <- dfMob$stand_number_2[which(idxx)]
  idxx <- data.frame(vals = hash::values(h, keys=use ))
  idxx <- rbind(indices, idxx)
  tableidxx <- idxx[!duplicated(idxx$vals), ]

  for (i in tableidxx) {
    row <- table[i,]
    idx <- which(names(ervenPolys) == row$labels)
    pol.x <- ervenPolys[[idx]]@coords[,1]
    pol.y <- ervenPolys[[idx]]@coords[,2]
    for (j in 1:nrow(dfMob)) {
      if (row$labels == dfMob$stand_number_1[j] | row$labels == dfMob$stand_number_2[j]) {
        table$quest[i] <- table$quest[i] + 1
        #table$hhdate[i] <- dfm$date[j]
      }
    }
  }

  require(sp)
  points <- cbind(dfMob$longitude, dfMob$latitude)
  for (i in 1:nrow(table)) {
    row <- table[i,]
    point <- cbind(row$stand_long, row$stand_lat)
    dists <- spDistsN1(pts = points, pt = point, longlat = TRUE)*1000
    idxx <- which(dists < 30)
    if (length(idxx) > 0) {
      table$surround[i] <- 1
    }
  }
  table$status <- '-'
  for (i in 1:nrow(table)) {
    row <- table[i,]
    idx <- which(names(ervenPolys) == row$labels)
    pol.x <- ervenPolys[[idx]]@coords[,1]
    pol.y <- ervenPolys[[idx]]@coords[,2]
    for (j in 1:nrow(dfMob)) {

      if (point.in.polygon(dfMob$longitude[j], dfMob$latitude[j], pol.x, pol.y)) {
        table$gpsmatch[i] <- table$gpsmatch[i] + 1
      }
    }
    if (table$quest[i] == table$gpsmatch[i])  {
      table$status[i] <- "GOOD"
    } else if (table$quest[i] != 0 & table$gpsmatch[i] != 0)  {
      table$status[i] <- "FINE"
    }
  }
  newdata <- subset(table, gpsmatch != 0 | quest != 0 | surround == 1)
  return(newdata)
}

#' Date created: 2017-11
#'@name getSpeed
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description finds the velocity of the fieldworker between the current stand and the previous one
#'@param dfMob [data frame] - data frame containing the Mobenzi data
#'@return the data frame from the parameter, with fields added for Distance, Time, Speed and SpeedStatus.
getSpeed <- function(dfMob) {
  require(geosphere)
  dfMob <- dfMob[order(dfMob$date),]
  point <- cbind(dfMob$longitude[1], dfMob$latitude[1])
  time <- dfMob$date[1]
  for (i in 1:(nrow(dfMob)-1)) {
    npoint <- cbind(dfMob$longitude[i+1], dfMob$latitude[i+1])
    ntime <- dfMob$date[i+1]
    Dist <- distm(point, npoint)
    Time <- as.numeric(ntime - time)
    Speed <- Dist/Time
    dfMob$Dist[i] <- Dist
    dfMob$Time[i] <- Time
    dfMob$Speed[i] <- Speed
    if (Speed < 100 & Speed >= 0) {
      dfMob$SpeedStatus[i] <- "FINE"
    } else {
      dfMob$SpeedStatus[i] <-"CHECK"
    }
    point <- npoint
    time <- ntime
  }
  return(dfMob)
}

#' Date created: 2017-11
#'@name makeBatch
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description update the household and stand questionnaires in the batch according to the information in mob
#'@param dfMob [data frame] - data frame containing the Mobenzi data gathered through various functions
#'@param mob [data frame] - the original, unchanged data frame, either household/stand questionnairre, or
#'the combined Mobenzi data frame
#'@return the original data frame with only final_stand_number and stand_number_status added from the Mobenzi data frame
makeBatch <- function(dfMob, mob) {
  mob$stand_number_final <- NA
  mob$stand_number_status <- NA
  mob$label <- NA
  for (i in 1:nrow(mob)) {
    for (j in 1:nrow(dfMob)) {
      if (mob$submission_id[i] == dfMob$submission_id[j]) {
        mob$stand_number_final[i] <- dfMob$stand_number_final[j]
        mob$stand_number_status[i] <- dfMob$status[j]
        mob$label[i] <- dfMob$sort[j]
      }
    }
  }
  return(mob)
}

#' Date created: 2017-11
#'@name fixMobenzi
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description  Replaces NA values in longitude and latitude columns
#'@param dfMob [data frame] - data frame containing the Mobenzi data
#'@param erwe [list] - list of erwe
#'@return the Monenzi data frame, with NA coordinates replaced, if possible
fixMobenzi <- function(dfMob, erwe) {
  idxxlat <- which(is.na(dfMob$latitude) & !(is.na(dfMob$gps_location_location_latitude)))
  idxxlong <- which(is.na(dfMob$longitude) & !(is.na(dfMob$gps_location_location_longitude)))

  dfMob$longitude[idxxlong] <- dfMob$gps_location_location_longitude[idxxlong]
  dfMob$longitude[idxxlat] <- dfMob$gps_location_location_latitude[idxxlat]

  idxxlat <- which(is.na(dfMob$latitude))
  idxxlong <- which(is.na(dfMob$longitude))

  frame <- getStands(erwe)
  for (i in idxxlong) {
    err <- F
    for (j in 1:nrow(frame)) {
      long <- frame$stand_long[j]
      lat <-  frame$stand_lat[j]
      possibleError <- NULL
      possibleError <- try(expr = {
        if ((dfMob$stand_number_1[i] == frame$labels[j] | dfMob$stand_number_2[i] == frame$labels[j]) &
            (long < dfMob$longitude[i+1] & long >  dfMob$longitude[i-1]) |
            (long > dfMob$longitude[i+1] & long <  dfMob$longitude[i-1]) |
            (lat < dfMob$latitude[i+1] & lat > dfMob$latitude[i-1]) |
            (lat > dfMob$latitude[i+1] & lat < dfMob$latitude[i-1])) {
          dfMob$longitude[i] <- frame$stand_long[j]
          dfMob$latitude[i] <- frame$stand_lat[j]
          dfMob$comments[i] <- "Coords verified before added"
        }
      }, silent = T)
      if (!is.null(possibleError)) {
        err <- err | T
        if (dfMob$stand_number_1[i] == frame$labels[j] | dfMob$stand_number_2[i] == frame$labels[j]) {
          dfMob$longitude[i] <- frame$stand_long[j]
          dfMob$latitude[i] <- frame$stand_lat[j]
        }
      }
    }
    if (err) {
      print(sprintf("Coordinates for enrty[%s] is missing and replacements could not be verified.", i))
      dfMob$comments[i] <- "Coords not verified"
    }
  }
  return(dfMob)
}

#' Date created: 2017-11
#'@name addStands
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description  adds the results from the stands data frame to the Mobenzi data frame
#'@param stands [data frame] - data frame returned from makeStands, containing the stand statuses of each entry
#'@param mob [data frame] - data frame containing the Mobenzi data
#'@return the Mobenzi data frame with its stand status added
#
addStands <- function(stands, mob) {

  for (i in 1:nrow(mob)) {
    for (j in 1:nrow(stands)) {
      if (mob$submission_id[i] == stands$submission_id[j]) {
        mob$len[i] <- stands$len[j]
        mob$stand_number_final[i] <- as.character(stands$mob1[j])
        if (stands$inputstatus[j] == "NO") {
          if (stands$mobstatus[j] == "YES2" | stands$endostatus[j] == "YES2"){
            mob$stand_number_final[i] <- as.character(stands$mob2[j])
            mob$stand_number_status[i] <- "VERIFIED"
          } else if (stands$mobstatus[j] == "YES1" | stands$endostatus[j] == "YES1"){
            mob$stand_number_status[i] <- "VERIFIED"
          } else {
            mob$stand_number_status[i] <- "CHECK STAND_NUMBER_1&2"
          }
        } else if (stands$mobstatus[j] == "NO" & stands$endostatus[j] == "NO") {
          mob$stand_number_status[i] <- "NEUTRAL"
        } else {
          mob$stand_number_status[i] <- "VERIFIED"
        }
      }
    }
  }
  return(mob)
}

#' Date created: 2017-11
#'@name makeStands
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description  Matches the submitted standnumbers with the coordinates of those closest to it
#'@param mobstands [data frame] - data frame returned from getStandNums, containing stand information for mobenzi data
#'@param endstands [data frame] - data frame returned from getStandNums, containing stand information for endomondo data
#'@return a data frame containing the combined stand information from Mobenazi and endomondo
makeStands <- function(mobstands, endstands) {
  stands <- data.frame(submission_id = mobstands$submission_id,
                       mob1 = mobstands$stand_number_1,
                       mob2 = mobstands$stand_number_2)

  stands <- data.frame(submission_id = mobstands$submission_id, mob1 = mobstands$stand_number_1, mob2 = mobstands$stand_number_2)
  stands$endo_stands <- '-'
  stands$mob_stands <- '-'
  stands$len <- '-'
  stands$endlen <- '-'
  for (i in 1:nrow(stands)) {
    irow <- stands[i,]
    for (j in 1:nrow(mobstands)) {
      jrow <- mobstands[j,]
      if (irow$submission_id == jrow$submission_id) {
        if (irow$mob1 == jrow$stand1 | irow$mob2 == jrow$stand1){
          stands$mob_stands[i] <- jrow$stand1
          stands$len[i] <- jrow$len1
        } else if (irow$mob1 == jrow$stand2 | irow$mob2 == jrow$stand2){
          stands$mob_stands[i] <- jrow$stand2
          stands$len[i] <- jrow$len2
        } else if (irow$mob1 == jrow$stand3 | irow$mob2 == jrow$stand3){
          stands$mob_stands[i] <- jrow$stand3
          stands$len[i] <- jrow$len3
        } else if (irow$mob1 == jrow$stand4 | irow$mob2 == jrow$stand4){
          stands$mob_stands[i] <- jrow$stand4
          stands$len[i] <- jrow$len4
        } else if (irow$mob1 == jrow$stand5 | irow$mob2 == jrow$stand5) {
          stands$mob_stands[i] <- jrow$stand5
          stands$len[i] <- jrow$len5
        } else if (irow$mob1 == jrow$stand6 | irow$mob2 == jrow$stand6) {
          stands$mob_stands[i] <- jrow$stand6
          stands$len[i] <- jrow$len6
        } else {
          stands$mob_stands[i] <- jrow$stand1
          stands$len[i] <- jrow$len1
        }
      }
    }
    if(!is.null(endstands)) {
      for (j in 1:nrow(endstands)) {
        jrow <- endstands[j,]
        if (irow$submission_id == jrow$submission_id) {
          if (irow$mob1 == jrow$stand1 | irow$mob2 == jrow$stand1){
            stands$endo_stands[i] <- jrow$stand1
            stands$endlen[i] <- jrow$len1
          } else if (irow$mob1 == jrow$stand2 | irow$mob2 == jrow$stand2){
            stands$endo_stands[i] <- jrow$stand2
            stands$endlen[i] <- jrow$len2
          } else if (irow$mob1 == jrow$stand3 | irow$mob2 == jrow$stand3){
            stands$endo_stands[i] <- jrow$stand3
            stands$endlen[i] <- jrow$len3
          } else if (irow$mob1 == jrow$stand4 | irow$mob2 == jrow$stand4){
            stands$endo_stands[i] <- jrow$stand4
            stands$endlen[i] <- jrow$len4
          } else if (irow$mob1 == jrow$stand5 | irow$mob2 == jrow$stand5) {
            stands$endo_stands[i] <- jrow$stand5
            stands$endlen[i] <- jrow$len5
          } else if (irow$mob1 == jrow$stand6 | irow$mob2 == jrow$stand6) {
            stands$endo_stands[i] <- jrow$stand6
            stands$endlen[i] <- jrow$len6
          } else {
            stands$endo_stands[i] <- jrow$stand1
            stands$endlen[i] <- jrow$len1
          }
        }
      }
    }
  }
  stands$inputstatus <- "NO"
  stands$status <- "NO"
  stands$mobstatus <- "NO"
  stands$endostatus <- "NO"
  for (i in 1:nrow(stands)) {
    row <- stands[i,]
    if (as.character(row$mob1) == as.character(row$mob2)){
      stands$inputstatus[i] <- "YES"
      if ((as.character(row$mob_stands) == row$endo_stands | as.character(row$mob1) == as.character(row$endo_stands)
           | as.character(row$mob1) == as.character(row$mob_stands))) {
        stands$status[i] <- "YES"
      }
      if (as.character(row$mob_stands) == as.character(row$mob1)  ) {
        stands$mobstatus[i] <- "YES"
      }
      if (as.character(row$endo_stands) == as.character(row$mob1)  )  {
        stands$endostatus[i] <- "YES"
      }
    }
    else {
      if ((as.character(row$mob_stands) == row$endo_stands | as.character(row$mob1) == as.character(row$endo_stands)
           | as.character(row$mob1) == as.character(row$mob_stands))) {
        stands$status[i] <- "YES1"
      }
      if (as.character(row$mob_stands) == as.character(row$mob1)  ) {
        stands$mobstatus[i] <- "YES1"
      }
      if (as.character(row$endo_stands) == as.character(row$mob1)  )  {
        stands$endostatus[i] <- "YES1"
      }
      if ((as.character(row$mob_stands) == row$endo_stands | as.character(row$mob2) == as.character(row$endo_stands)
           | as.character(row$mob2) == as.character(row$mob_stands))) {
        if (stands$status[i] == "NO") {
          stands$status[i] <- "YES2"
        } else {
          stands$status[i] <- "BOTH"
        }
      }
      if (as.character(row$mob_stands) == as.character(row$mob2)  ) {
        if (stands$mobstatus[i] == "NO") {
          stands$mobstatus[i] <- "YES2"
        } else {
          stands$mobstatus[i] <- "BOTH"
        }
      }
      if (as.character(row$endo_stands) == as.character(row$mob2)  )  {
        if (stands$endostatus[i]== "NO") {
          stands$endostatus[i] <- "YES2"
        } else {
          stands$endostatus[i]<- "BOTH"
        }
      }
    }
  }
  return(stands)
}



#'#' Date created: 2017-11
#'@name gettands
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description creates a data frame from the erwe data, containing its stand numbers and coordinates
#'@param [list] of stands
#'@return a data frame with standnumbers and its coordinates
getStands <- function(erwe) {
  Frame <- data.frame(index = 1:length(erwe[[1]]))
  Frame$labels <- erwe[[1]]$Label
  Frame <- data.frame(lapply(Frame, as.character), stringsAsFactors=FALSE)
  require(stringi)
  Frame$lens <- stri_length(Frame$labels)
  med = median(Frame$lens)
  for (i in 1:length(erwe[[1]])) {
    Frame$stand_long[i] <- erwe[[1]]@polygons[[i]]@labpt[1]
    Frame$stand_lat[i] <- erwe[[1]]@polygons[[i]]@labpt[2]
    if (Frame$lens[i] - 2 == med) {
      Frame$labels[i] <- stri_sub(Frame$labels[i], 3, len + 2)
      Frame$lens[i] <- stri_length(Frame$labels[i])
    }
  }
  Frame$lens = NULL

  return(Frame)
}

#'#' Date created: 2017-11
#'@name getStandNums
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description returns the six closest stands to the position submitted
#'@param sdf [dataframe] sdf - frame containing two columns, one latitude and the other longitude, either mobenzi or endomondo data
#'@param erwe [list] list of erven
#'@param cutoff [num] max distance from center of stand to point of datafrmame
#'@param ervenPolys [list] list of polygons
#'@return the sdf data frame, but with the six closest stands added
getStandNums <- function(sdf, erwe, cutoff, ervenPolys) {

  index <- c()
  sdf$stand1 <- '-'
  sdf$stand2 <- '-'
  sdf$stand3 <- '-'
  sdf$stand4 <- '-'
  sdf$stand5 <- '-'
  sdf$stand6 <- '-'
  sdf$len1 <- '-'
  sdf$len2 <- '-'
  sdf$len3 <- '-'
  sdf$len4 <- '-'
  sdf$len5 <- '-'
  sdf$len6 <- '-'

  points <- cbind(erwe$stand_long, erwe$stand_lat)
  for (r in 1:nrow(sdf)) {
    row <- sdf[r,]
    if (is.na(sdf$longitude[r])){
      sdf$distance[r] <- NA
      sdf$polygon_id[r] <- NA
      next()
    }
    point <- c(sdf$longitude[r], sdf$latitude[r])
    p <- 1
    range <- NULL
    range <- c()
    dists <- spDistsN1(pts = points, pt = point, longlat = TRUE)*1000
    small <- dists < cutoff
    inds <- which(small)
    small <- dists[inds]
    names(small) <- inds
    small <- sort.int(small)
    index1 <- as.numeric(names(small[1]))
    index2 <- as.numeric(names(small[2]))
    index3 <- as.numeric(names(small[3]))
    index4 <- as.numeric(names(small[4]))
    index5 <- as.numeric(names(small[5]))
    index6 <- as.numeric(names(small[6]))
    len1 <- as.numeric(small[1])
    len2 <- as.numeric(small[2])
    len3 <- as.numeric(small[3])
    len4 <- as.numeric(small[4])
    len5 <- as.numeric(small[5])
    len6 <- as.numeric(small[6])

    if (!is.na(index1)) {
      sdf$stand1[r] <- as.character(erwe$labels[index1])
      sdf$len1[r] <- len1
    }
    if (!is.na(index2)) {
      sdf$stand2[r] <- as.character(erwe$labels[index2])
      sdf$len2[r] <- len2
    }
    if (!is.na(index3)) {
      sdf$stand3[r] <- as.character(erwe$labels[index3])
      sdf$len3[r] <- len3
    }
    if (!is.na(index4)) {
      sdf$stand4[r] <- as.character(erwe$labels[index4])
      sdf$len4[r] <- len4
    }
    if (!is.na(index5)) {
      sdf$stand5[r] <- as.character(erwe$labels[index5])
      sdf$len5[r] <- len5
    }
    if (!is.na(index6)) {
      sdf$stand6[r] <- as.character(erwe$labels[index6])
      sdf$len6[r]   <- len6
    }

    min <- min(dists)
    if (min > cutoff) {
      sdf$stand1[r] <- "cut-off"
    }
  }
  sdf <- sdf[order(sdf$date),]

  return(sdf)
}

#'#' Date created: 2017-11
#'@name mixEndo
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description Adds the data obtained from the endomenzi function to the Endo data frame
#'@param mindex [data frame] containing endomondo data
#'@param endomezi [data frame] returned from the endoMenzi function
#'@return the mindex data frame, with endomenzi data added
mixEndo <- function(mindex, endomenzi) {
  mindex$submission_id <- '-'
  mindex$id <- '-'
  for (i in 1:nrow(mindex)) {
    for (j in 1:nrow(endomenzi)) {
      if (mindex$longitude[i] == endomenzi$lonEndo[j] & mindex$latitude[i] == endomenzi$latEndo[j] ) {
        row <- endomenzi[j,]
        mindex$submission_id[i] <- as.character(row$submission_id)
        mindex$id[i] <- as.character(row$ID)
      }
    }
  }
  return(mindex)
}

#'#' Date created: 2017-11
#'@name drawMap
#'@author Nova Institute (Reg# 1994/002614/08)
#'@description Outputs a map that represents the data visually
#'@param dfErwe [data frame] - data frame containing the relevant erven
#'@param dfMob [data frame] - data frame containing Mobenzi data
#'@param dfEndo [data frame] - data frame containing Endomondo data
#'@param scale [num] - the argument for how much the map should be zoomed in
drawMap <- function(dfErwe, dfMob, dfEndo = NULL, scale = 17) {

  if (!is.null(dfEndo)) {
    dfEndo <- dfEndo[order(dfEndo$date),]
    dfEndo$sort <- 1:nrow(dfEndo)
  }

  good <- subset(dfMob, stand_number_status == "VERIFIED" & SpeedStatus == "FINE")
  bad <- subset(dfMob, stand_number_status == "CHECK STAND_NUMBER_1&2" | (SpeedStatus == "CHECK" & stand_number_status != "VERIFIED") | comments == "Coords not verified")
  erwe <- subset(dfErwe, gpsmatch != 0 | quest != 0 )
  goodstand <- subset(erwe, status == '-')
  badstand <- subset(erwe, quest == 0)

  require(ggmap)
  idxx <- which(dfMob$comments == "Wrong day")
  if (length(idxx) > 0) {
    mob <- subset(dfMob[-idxx,])
  } else {
    mob <- dfMob
  }
  lon <- mean(mob$longitude)
  lat <- mean(mob$latitude)
  map <- get_map(location = c(lon, lat),
                 color = "color",
                 source = "google",
                 maptype = "satellite",
                 zoom = scale)

  p <- ggmap(map)
  p <- p + geom_point(data = erwe, aes(x = stand_long, y = stand_lat), col = "yellow") +
    geom_point(data = goodstand, aes(x = stand_long, y = stand_lat), col = "blue") +
    geom_point(data = badstand, aes(x = stand_long, y = stand_lat), col = "red") +
    geom_text(aes(label= labels, x = stand_long, y = stand_lat), data= erwe, size = 3) +
    geom_point(data = dfMob, aes(x = longitude, y = latitude), col = "orange", size = 5) +
    geom_point(data = good, aes(x = longitude, y = latitude), col = "green", size = 5) +
    geom_point(data = bad, aes(x = longitude, y = latitude), col = "red", size = 5) +
    geom_text(aes(label= sort, x = longitude, y = latitude), data=dfMob, size = 3)

  if (!is.null(dfEndo)) {
    p <- p + geom_path(data = dfEndo, aes(x = longitude, y = latitude))
  }
  return(p)
}




