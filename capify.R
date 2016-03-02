# capify
# make captions and lables for latex tables

fixname <- function(data){
  data=as.character(data)
  data=tolower(data)
  data=gsub("[[:space:]]"," ",data)
  data=gsub("/"," ",data)
  data=gsub("-"," ",data)
  data=gsub("_0","",data)
  data=gsub("\\.0","",data)
  data=gsub("\\?","",data)
  data
}

capify <- function(x, cp = NULL, lb = NULL, cumsum=NULL, group=NULL){
  cps <- new.env(parent = .GlobalEnv)
  if (!is.null(cp)) assign("cp", cp, envir = cps) else assign("cp", fixname(names(x)[1]), envir = cps)
  if (!is.null(lb)) assign("lb", lb, envir = cps) else assign("lb", fixname(names(x)[1]), envir = cps)
  if (!is.null(cumsum)) assign("cumsum", cumsum, envir = cps) else assign("cumsum", FALSE, envir = cps)
  if (!is.null(group)) assign("group", group, envir = cps) else assign("group", NA, envir = cps)
  assign("cps",cps, envir = .GlobalEnv)
}