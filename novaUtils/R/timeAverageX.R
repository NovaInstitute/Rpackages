
# Date created: 2017-06-24
# Nova Institute (Reg# 1994/002614/08).

#'@title timeAverageX
#'@name timeAverageX
#'@param df A dataframe containing a date field (named 'date') in POSIXct format
#'and one or more numeric fields to be averaged. Any non-numeric fields will be
#'dropped.
#'@param interval Interval to average to, in minutes
#'@param startDate Date from which to start averaging. All observations prior to
#'that date will be dropped. Argument must be in POSIXct format. If no argument
#'is supplied, function will default to the oldest date the 'date' field of df
#'
#'
timeAverageX <- function(df, interval, startDate = NA) {
        
        if (nrow(df) == 0) {
                return(df)
        }
        
        if (is.POSIXlt(df$date)) {
                df$date <- as.POSIXct(df$date)
        } else {
                if (!is.POSIXct(df$date)) {
                        stop("'date' field must be in POSIXct format.")
                }  
        }
        
        if (is.na(startDate)) {
                startDate <- min(df[["date"]], na.rm = TRUE)
        } else {
                if (is.POSIXlt(startDate)) {
                        startDate <- as.POSIXct(startDate)
                } else {
                        if (!is.POSIXct(startDate)) {
                                stop("'startDate' must be in POSIXct format.")
                        }
                }
        }
        
        nmsNumFlds <- names(df)[which(unlist(sapply(X = df, FUN = is.numeric)))]
        df <- df[c("date", nmsNumFlds)]
        
        df <- df[order(df$date),]
        rownames(df) <- 1:nrow(df)
        
        minDate <- min(df$date)
        maxDate <- max(df$date)
        
        finalDates <- seq.int(from = startDate, to = maxDate, by = interval*60)
        if (nrow(df) == 1) {
                df$date <- finalDates[length(finalDates)]
                return(df)
        }
        
        finalDates <- finalDates[which(finalDates >= minDate)]
        
        
        
        df$dest_date <- as.POSIXct(NA)
        for (di in (length(finalDates)):1) {
                dte <- as.POSIXct(finalDates[di])
                idxx <- which(df$date <= dte)
                df$dest_date[idxx] <- as.POSIXct(dte)
        }
        
        df <- df[which(!is.na(df$dest_date)),]
        
        lsDateSplits <- split(x = df, f = df$dest_date)
        lsDateSplits <- lapply(X = lsDateSplits, FUN = function(dtedf) {
                for (fldnm in nmsNumFlds) {
                        dtedf[[fldnm]] <- mean(dtedf[[fldnm]], na.rm = TRUE)
                }
                return(dtedf[1,])
        })
        df <- do.call("rbind", lsDateSplits)
        df$date <- df$dest_date
        df <- df[-grep(pattern = "dest_date", x = names(df))]
        rownames(df) <- 1:nrow(df)
        
        return(df)
}