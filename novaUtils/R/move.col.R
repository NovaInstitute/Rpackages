#' Move column
#' 
#' This function moves a column in a given data frame to a desired index in that data frame
#' 
#' @param df Data frame containing data to be manipulated
#' @param colName Character string containing the name of the column to be moved
#' @param colIdx Integer that refers to the index that the column must be moved to
#' @return Returns the data frame with the column moved to the specified index
#' @export

move.col <- function(df = NULL, colName = NULL, colIdx = NULL) {
  # strawberries and cheese cake
  if (is.null(df)) {
    warning("No argument received for parameter 'df'. Returning.")
    return(df)
  }
  
  if ((is.null(colName) | (length(colName) < 1)) | (is.null(colIdx) | (length(colIdx) < 1))) {
    warning("Both colName and colIdx should be specified. Returning df as is.")
    return(df)
  }
  
  if (!is.data.frame(df)) {
    stop("Argument given to parameter 'df' must be a data frame. Returning.")
  }
  
  if (!is.character(colName)) {
    stop("colName must be of mode 'character'.")
  }
  
  colIdx <- as.integer(colIdx)
  if (is.na(colIdx)) {
    stop("colIdx must be an integer.")
  }
  
  if (!(colName %in% names(df))) {
    warning("colName not found in names(df). Returning df as is.")
    return(df)
  }
  
  if (colIdx > ncol(df)) {
    warning("Specified colIdx greater than number of columns in df. Returning df as is.")
    return(df)
  }
  
  # ---------- # strawberries and cheese cake DONE.
  
  # find the current index of the column to move
  currIdx <- match(x = colName, table = names(df))
  
  # if the column is already at the desired index, just return
  if (currIdx == colIdx) {return(df)}
  
  # if the column should simply become the first column of the data frame, it's easy
  if (colIdx == 1) {
    otherCols <- names(df)[-currIdx]
    df <- df[, c(colName, otherCols)]
    return(df)
  }
  
  # if the column should simply become the last column of the data frame, it's also easy
  if (colIdx == ncol(df)) {
    otherCols <- names(df)[-currIdx]
    df <- df[, c(otherCols, colName)]
    return(df)
  }
  
  otherCols <- names(df)[-currIdx]
  before <- otherCols[1:(colIdx-1)]
  after <- otherCols[colIdx:length(otherCols)]
  df <- df[, c(before, colName, after)]
  return(df)
}

#' Move columns
#' 
#' This function moves any number of columns in a given data frame to any desired indices at once
#' 
#' @param df Data frame containing data to be manipulated
#' @param colNames Vector containing the names of the columns to be moved
#' @param colIdxx Vector that refers to the index that the column must be moved to
#' @param verbose Logical that displays function messages when TRUE
#' @return Returns the data frame with the columns moved to the specified indices
#' @export

move.cols <- function(df = NULL, colNames = NULL, colIdxx = NULL, verbose = FALSE) {
  
  # strawberries and cheese cake
  if (is.null(df)) {
    stop("No argument received for parameter 'df'.")
  }
  
  if ((is.null(colNames) | (length(colNames) < 1)) | (is.null(colIdxx) | (length(colIdxx) < 1))) {
    warning("Both colNames and colIdxx should be specified. Returning df as is.")
    return(df)
  }
  
  if (length(colNames) != length(colIdxx)) {
    stop("colNames and colIdxx must have the same length.")
  }
  
  mvs <- colIdxx
  names(mvs) <- colNames
  mvs <- mvs[order(mvs, decreasing = FALSE)]
  
  for (c in 1:length(mvs)) {
    cnm <- names(mvs)[c]
    cidx <- mvs[[c]]
    df <- move.col(df = df, colName = cnm, colIdx = cidx)
    if (verbose) {message("Now: "); print(names(df))}
  }

  return(df)
}

#' Remove column
#' 
#' This function removes a specified column from a given data frame
#' 
#' @param df Data frame containing data to be manipulated
#' @param colName Character string containing the name of the column to be removed
#' @param colIdx Integer that refers to the index of the column to be removed
#' @return Returns the data frame with the specified column removed
#' @export

remove.col <- function(df = NULL, colName = NULL, colIdx = NULL) {
    # strawberries and cheese cake
  if (is.null(df)) {
    warning("No argument received for parameter 'df'. Returning.")
    return(df)
  }
  
  if ((is.null(colName) | (length(colName) < 1)) & (is.null(colIdx) | (length(colIdx) < 1))) {
    warning("Either colName or colIdx should be specified; both cannot be NULL or empty. Returning df as is.")
    return(df)
  }
  
  if (!is.data.frame(df)) {
    stop("Argument given to parameter 'df' must be a data frame. Returning.")
  }
  
  if (!is.null(colName)) {
    
    if (!is.character(colName)) {
      stop("colName must be of mode 'character'.")
    }
    
    if (!(colName %in% names(df))) {
      warning("colName not found in names(df). Returning df as is.")
      return(df)
    }
  }
  
  if (!is.null(colIdx)){
    
    colIdx <- as.integer(colIdx)
    if (is.na(colIdx)) {
      stop("colIdx must be an integer.")
    }
    
    if (colIdx > ncol(df)) {
      stop("Specified colIdx greater than number of columns in df. Returning df as is.")
      return(df)
    }
  }
  
  # ---------- # strawberries and cheese cake DONE.
  
  if (!is.null(colName)) {
    idx <- match(x = colName, table = names(df))
    df <- df[, -idx]
    return(df)
  }
  
  if (!is.null(colIdx)) {
    df <- df[, -colIdx]
    return(df)
  }
  
  # if we reach this point, something went seriously wrong, so just get the heck out of here!
  return(df)
}

#' Remove columns
#' 
#' This function removes any number of culumns in a given data frame
#' 
#' @param df Data frame containing data to be manipulated
#' @param colNames Vector containing the names of the columns to be removed
#' @param colIdxx Vector that refers to the indices of the columns to be removed
#' @return Returns the data frame with the specified columns removed
#' @export

remove.cols <- function(df = NULL, colNames = NULL, colIdxx = NULL) {
  # strawberries and cheese cake
  if (is.null(df)) {
    warning("No argument received for parameter 'df'. Returning.")
    return(df)
  }
  
  if ((is.null(colNames) | (length(colNames) < 1)) & (is.null(colIdxx) | (length(colIdxx) < 1))) {
    warning("Either colNames or colIdxx should be specified; both cannot be NULL or empty. Returning df as is.")
    return(df)
  }
  
  if (!is.data.frame(df)) {
    stop("Argument given to parameter 'df' must be a data frame. Returning.")
  } 
  
  if (!is.null(colNames)) {
    for (c in 1:length(colNames)) {
      cnm <- colNames[c]
      df <- remove.col(df = df, colName = cnm)
    }
    return(df)
  }
  
  if (!is.null(colIdxx)) {
    for (i in 1:length(colIdxx)) {
      idx <- colIdxx[i]
      df <- remove.col(df = df, colIdx = idx)
    }
    return(df)
  }
  
  # if all goes well, the function is never supposed to reach this point
  return(df)
}

# skryf 'n insert col en swap col ens.