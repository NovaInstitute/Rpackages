tblToDf <- function(tbl, rwNmsAsCol = FALSE) {
  
  if (is.null(dim(tbl))) {
    nCols <- length(tbl)
    nRows <- 1
    df <- data.frame(matrix(nrow = nRows, ncol = nCols))
    df[1,] <- tbl
    names(df) <- names(tbl)
    return(df)
  } 

  if (length(dim(tbl)) == 1) {
    df <- data.frame(stringsAsFactors = FALSE,
                     resp = names(tbl),
                     val = as.vector(tbl))
    return(df)
  }
  
  nCols <- ifelse(rwNmsAsCol, ncol(tbl) +1, ncol(tbl))
  nRows <- nrow(tbl)
  
  df <- data.frame(matrix(nrow = nRows, ncol = nCols))
  offset <- ifelse(rwNmsAsCol, 1, 0)
  
  for (c in 1:ncol(tbl)) {
    df[[c + offset]] <- tbl[,c]
  }
  
  if (rwNmsAsCol) {
    df[[1]] <- rownames(tbl)
  } else {
    rownames(df) <- rownames(tbl)
  }
  
  colnames(df) <- c("resp", colnames(tbl))[(2-offset):(ncol(tbl) +1)]
  
  return(df)
}
