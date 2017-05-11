#' Slots in Polygons
#'
#' Slots in polygons inside SPDF
#'
#' @param pol An SPDF
#' @param sl The name of the slot. The operator takes a fixed name,
#' which can be unquoted if it is syntactically a name in the language. A slot
#' name can be any non-empty string, but if the name is not made up of letters,
#' numbers, and ., it needs to be quoted (by backticks or single or double quotes).
#' In the case of the slot function, name can be any expression that evaluates
#' to a valid slot in the class definition. Generally, the only reason to use
#' the functional form rather than the simpler operator is because the slot name has to be computed.
#' @export

slotspol <- function(pol, sl="ID"){
sapply(slot(pol, "polygons"), function(x) slot(x, sl))}

deep.slotspol<-function(pol,sl="Polygons",deep.sl=c("labpt","area","hole","ringDir","coords")[1])
{sapply(slotspol(pol,"Polygons"),function(x) slot(x, deep.sl))}

deep.coords<-function(pol,sl="Polygons",deep.sl=c("labpt","area","hole","ringDir","coords")[5])
{matrix(unlist(deep.slotspol(pol,deep.sl="coords")),ncol=2)}

deep.slot.length<-function(pol,sl="Polygons",deep.sl=c("labpt","area","hole","ringDir","coords")[5])
{sapply(slotspol(pol,"Polygons"),function(x) length(slot(x, deep.sl)[,1]))}

deep.coord.df<-function(pol,df,varname="TAG_VALUE"){
label=df[,as.character(varname)]
lengths=as.numeric(deep.slot.length(pol,sl="Polygons",deep.sl="coords"))
data.frame(
label=rep(label, lengths)
,x=deep.coords(pol)[,1]
,y=deep.coords(pol)[,2])
	}


