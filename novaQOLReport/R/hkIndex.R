#' HK Index
#'
#' Makes an inex out of PCA
#'
#' @details use: hkIndeks(swb.pca, cuts = c(75, 70, 65) ,  dim = 3)
#' @param hk Object of type PCA
#' @param dimensies Number of dimensions as Numeric
#' @param cuts Numeric vector that describe acny cuts to be made (uts should be the length of the number of
#' dimensions for most reliable output)
#' @param verbose Logical to display fuction messages
#' @export

hkIndeks <- function(hk, dimensies = 3, cuts = c(0.75, 0.70, 0.65), verbose = FALSE){
	if (length(cuts) != dimensies) {
		message("cuts is nie so lank soos dimensies nie, die ding kan dalk gemors uitgee gee")
		if (length(cuts) > dimensies){
			cuts = cuts[1:dimensies]
			message("Ek verkort cuts en hoop vir die beste\n", paste(cuts, " "))
		}
		if (length(cuts) < dimensies){
			cuts = rep(cuts,dimensies)
			message("Ek verleng cuts en hoop vir die beste\n", paste(cuts, " "))
		}
	}
	apply(mapply(hkIndeksPerd, hk=list(hk), cuts=cuts, dim=1:length(dimensies)), 1 , sum)
}

#' HK Index Perd
#'
#' Help function for HK Index
#'
#' @param hk Object of type PCA
#' @param cuts Numeric vector of the cuts to be made
#' @param dim Numeric vector containing the number of dimensions
#' @export

hkIndeksPerd <- function(hk, cuts = c(75), dim = 1){
	if (class(swb.pca)[1] != "PCA") stop("Jammer my ou maar hk moet 'n PCA object wees")
		dim1bydrae = cumsum(sort(hk$var$contrib[,dim], decreasing = TRUE))
	topdim1 = dim1bydrae[cumsum(dim1bydrae) < cuts[dim]]
	vloerdim1 = dim1bydrae[cumsum(dim1bydrae) > cuts[dim]]
	vloerdim1[1:length(vloerdim1)] = 0
    dim1gewigte = c(topdim1, vloerdim1)
    dim1telling = apply(hk$call$X, 1, function(x) sum(x * dim1gewigte[match(names(hk$call$X), names(dim1gewigte))], na.rm=TRUE))
    dim1telling
}

