#' Find Suburb
#'
#' Function to find the correct suburb of a known address
#'
#' @param SPDF A spacialPolygonsDataFrame that has the boundaries (the polygons) and their names (data)
#' @param SuburbBoundaries The boundaries related to the suburb under consideration
#' @param var The variable under consideration as a character vector
#' @examples SPDF$Suburnames <- SuburbFind(SPDF,SuburbBoundaries)
#' @export

SuburbFind=function(SPDF,SuburbBoundaries,var="MP_NAME"){
  if (class(SPDF) == "SpatialPointsDataFrame") {idx <- overlay(SPDF,SuburbBoundaries)
   } else {
	idx <- overlay(SpatialPoints(t(slotspol(SPDF,"labpt"))),SuburbBoundaries)}
	as.character(SuburbBoundaries@data[idx, var])
	}
#x=tapply(SPbluegum$TOWNSHIP,idx,function(x) x)

#20   80204                    Ermelo       2556       <NA>
#21   80213                 Wesselton       2557       <NA>
#22   80210                   Phumula       2558       <NA>

#' Erf Find Save
#'
#' Function to find the correct suburb of a known address and save it to the chosen directory
#'
#' @param SPDF A spacialPolygonsDataFrame that has the boundaries (the polygons) and their names (data)
#' @param SuburbBoundaries The boundaries related to the suburb under consideration
#' @param var The variable under consideration as a character vector
#' @param outdir The desired output directory as character vector
#' @export

ErfFindSave=function(SPDF,SuburbBoundaries,var="MP_NAME",outdir="~/Dropbox/Verifikasie2012/Phumula/2.SampleFrame/")
{
	SPDF@data$suburb=SuburbFind(SPDF,SuburbBoundaries,var)
	SPDF[which(SPDF$suburb==var),]
save(SPDF[which(SPDF$suburb==var),],file=paste(outdir,var,".erf.Rda",sep=""))
	}
