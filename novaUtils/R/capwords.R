
#'Capitalises the first letter of each word
#'
#'@param x An object of type character containing word(s) of which the first letter
#'must be capitalised. Characters separated by one or more blanks are taken as
#'separate words.
#'@param strict If TRUE, all letters which are not the first letter of a word will
#'be forced to lower case.
#'@param removePunct if TRUE, will remove all special characters
#'@examples 
#'capwords("andy and sandy") returns "Andy And Sandy"
#'capwords(c("andy", "sandy")) returns c("Andy", "Sandy")
#'capwords(c("andy and tommy", "sandy")) returns c("Andy And Tommy", "Sandy")
#'capwords("respondent_name", removePunct = TRUE)  returns "Respondent Name"
#'
capwords <- function(x, strict = TRUE, removePunct = FALSE) {
        if(removePunct){
         x <-  gsub("[^[:alnum:]]", " ", x)    
        }

  resfin <- sapply(X = x, FUN = function(xx) {
         
    words <- strsplit(x = xx, split = "[[:blank:]]{1,}")[[1]]
    words <- sapply(X = words, FUN = function(wrd) {
      if (nchar(wrd) < 2) {return(toupper(wrd))}
      
      wrd <- sprintf("%s%s", 
                     toupper(substr(wrd, 1, 1)),
                     ifelse(strict, 
                            tolower(substr(wrd, 2, nchar(wrd))),
                            substr(wrd, 2, nchar(wrd))))
      return(wrd)
    })
    res <- paste(words, sep = " ", collapse = " ")
    return(res)    
  })
  
  names(resfin) <- names(x)
  return(resfin)
}
