##########################################################
# removes dots and makes title case                      #
##########################################################
trashdot.titl <- function(x) {
  x = paste(toupper(x)[1], tolower(x)[2:length(x)], sep = "")
  patt = "(^[[:alnum:]]*)([[:punct:]]*)([[:space:]]*)([[:alnum:]]*)([[:punct:]]*)([[:space:]]*)"
  gsub(patt, paste("\\1", " ", "\\4", sep = ""), x)
}