##########################################################
# raw latex  table to be used for multi option questions      #
##########################################################
cap.tNN = function(x, cap = m.cap,
                   lab = m.lab,
                   dir = get("datadir", envir = sys.frame(which = 1)),
                   cumsum = FALSE,
                   group = NA,
                   verbose = get("verbose", envir = sys.frame(which = 1))) {
  message("\nYou are inside cap.tN")

  if (is.null(cap) == FALSE) {
    cap = ifelse(test = is.null(cap), yes = trashdot.titl(names(x)[1]), no = cap)
    cap = gsub("_", " ", cap)
  }

  if (is.null(lab) == FALSE) {
    lab = ifelse(test = is.null(lab), yes = names(x)[1], no = lab)
  }
  if(verbose > 1) message("Dim  ",dim(x)[1], " ", dim(x)[2], "\n", "Str  ", str(x))

  if(dim(x)[2] != 0){
    cat(tableNominal(x, cap = cap, lab = lab, cumsum = cumsum, nams = names(x), group = group))
  }

  message("capTNN success:", lab)
}