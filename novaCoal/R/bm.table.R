bm.table=function(x){
	ifelse(is.list(x),x,stop("not a list, mugu"))
	z=lapply(x,summary)
	labels=lapply(x,label)
	y=as.data.frame(cbind(labels,z))
	y
	}
	
	