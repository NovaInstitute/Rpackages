##########################################################

test = function(x) {
  length(which(is.na(x) == TRUE)) == length(x)
}