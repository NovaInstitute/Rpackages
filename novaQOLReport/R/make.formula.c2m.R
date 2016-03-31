# formula maker for the Chi2 machine

#' Make Formula c2m
#'
#' Formula maker for the Chi2 machine
#'
#' @param x An spdf
#' @param add Anything to be added to the formula as character vector
#' @param verbose Logical that displays function messages when TRUE
#' @export

make.formula.c2m = function(x, add = NULL, verbose=FALSE){
	require(formula.tools)
  if (dim(x)[1] > 1) {fml = as.formula(paste(paste(x$outcome[1]," ~ "),
                         paste(as.character(x$exposure[grep(x$outcome[1], x$exposure, invert=TRUE)]), collapse=" + ")
                         )
                   ) } else { fml = as.formula(paste(x$outcome," ~ ", x$exposure))}

  fml = as.formula(paste(as.character.formula(fml), add, sep=""))
  if (verbose == TRUE) message("fml is ", paste(fml))
  fml
}

make.formula.c2m.sat = function(x){
  fml = as.formula(paste(" ~ ", paste(x$exposure[-grep(x$outcome[1], x$exposure)], collapse=" * "))
  )
}

make.c2m.sat.tab = function(x, data){
  nms = as.character(x)[2]
  nms = gsub("\\*", ",", nms)
  nms = unlist(strsplit(nms, " , ") )
  tab = table(data[, nms])
  #mlt = melt(data[, nms])
  #tab = table(mlt)
  tab
}

goodify <- function(x){
  for (i in 1:length(x)){
    z = as.character(x[,i])
    z[z=="positive"] = "bad"
    z[z=="negative"] = "good"
    x[,i] = as.factor(z)
  }
  x
}

#################### Ekstreer resulterede formule uit CHAID objek #####################
get.short.form.chaid <- function(z, type = c("short","sat","right")[1]){
nn = nodeapply(z)
n.names= names(unlist(nn[[1]]))
ext = unlist(sapply(n.names, function(x) grep("split.varid.", x, value=T)))
ext = gsub("kids.split.varid.", "", ext)
ext = gsub("split.varid.", "", ext)
ext = gsub("kids.kids.", "", ext)
ext = gsub("kids.", "", ext)
dep.var <- as.character(terms(z)[1][[2]])
plus = paste(ext, collapse=" + ")
mul = paste(ext, collapse=" * ")
if (type == "right") res = paste(" ~ ", paste(dep.var, plus, sep= " + "), sep ="", collapse="")
if (type == "short") shortform = paste (dep.var, plus, sep = " ~ ")
if (type == "sat") satform = paste (dep.var, mul, sep = " ~ ")
res = gsub(" \\+ $", "", res)
res = as.formula(res)
res
}



