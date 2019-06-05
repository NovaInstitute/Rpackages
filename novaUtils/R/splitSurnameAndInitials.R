# Date created: 2019-02-20
# Copyright: GNU General Public License

#' Separates initials and surnames combined in strings.
#'@description Separates initials and surnames combined in strings.
#'@param x [character] A vector of strings that each contain any combination of surname and initials.
#'@return A data frame with three columns: 'original', 'surname' and 'initials'.
#'@name splitSurnameAndInitials
#' 
splitSurnameAndInitials <- function(x) {
  
  surnames <- rep(NA_character_, length(x))
  initials <- rep(NA_character_, length(x))
  
  x <- format_char(x)
  splts <- strsplit(x = x, split = "_", fixed = TRUE)
  
  idxx <- which(sapply(X = splts, FUN = length) == 1)
  surnames[idxx] <- x[idxx]
  
  ncharInitials <- 0
  idxxRemaining <- which(is.na(surnames) & is.na(initials) & !is.na(x))
  while (length(idxxRemaining) > 0 & ncharInitials <= 3) {
    
    for (k in idxxRemaining) {
      s <- splts[[k]]
      idxx <- which(nchar(s) > ncharInitials)
      
      if (length(idxx) == 1) {
        surnames[k] <- s[idxx]
        initials[k] <- paste(s[-idxx], collapse = "")
        next
      }
    }
    
    ncharInitials <- ncharInitials +1
  }
  idxx <- which(is.na(surnames) & is.na(initials) & !is.na(x))
  surnames[idxx] <- x[idxx]
  
  return(data.frame(stringsAsFactors = FALSE,
                    original = x,
                    surname = surnames,
                    initials = initials))
}