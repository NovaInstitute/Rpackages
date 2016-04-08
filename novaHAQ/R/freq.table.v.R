#' Frequency Table V
#'
#' Creates a table from given information describing a value's frequency and percentage of the
#' total number of occurences for a given variable and saves it to the provided directory
#'
#' @param x Variable to create the frequency table for
#' @param tables.dir Directory to which all tables should be saved as character vector
#' @details The way this function is written, the name of the output .tex file will
#' contain the name of the label. If the variable has no label the output file will
#' not have a name.
#' @examples freq.table.v(haq$hh.BM.use, paste(thisplace,tabdir,sep=""))
#' @export

freq.table.v<-function(x,tables.dir=tabdir){
	y=as.data.frame(rbind(table(x, dnn=Hmisc::label(x)),100*as.table(table(x))/sum(as.table(table(x))) ), row.names=c("Frequency","Percent"))
	y[1,]=sprintf("%.0f",y[1,])
	if(length(y)>1)y[2,]=sprintf("%.2f",y[2,])
	f=as.data.frame(cbind(paste("n=",length(x)-sum(is.na(x)),sep=""),paste("Missing=",sum(is.na(x)),sep="")))
	Sample.info=t(f)
	colnames(Sample.info)="Sample information:"
	y=cbind(y,Sample.info)
	z=t(as.matrix(y))
	a=xtable(z, caption=paste(Hmisc::label(x),sep=""),label=Hmisc::label(x),align=c("l","r","r"), display=c("s","f","d"),here=TRUE)
	print(a, file=paste(tables.dir,Hmisc::label(x),".tex", sep=""),caption.placement="top", caption.lot=paste(Hmisc::label(x)),hline.after=c(-1,0,nrow(a)-1,nrow(a)))
	invisible(a)	 	}

#' Frequency Table By
#'
#' Creates a table from given information describing a value's frequency and percentage of the
#' total number of occurences for given variables and saves it to the provided directory
#'
#' @param x The first variable to create the frequency table for
#' @param by The second variable used to group variable x by
#' @param tables.dir Directory to which any tables should be saved as character vector
#' @details For the best table generation, assign the variable with more different values to
#' the "by" argument.
#' @export

freq.table.by=function(x,by,tables.dir=tabdir)
{z=data.frame(cbind(addmargins(table(by,x),1),prop.table(addmargins(table(by,x),1),1)*100))

colnames(z)=c(names(table(x)),paste(names(table(x)),"%",sep=""))

a=xtable(z,
	caption=paste(Hmisc::label(x)," by ", Hmisc::label(by),sep=""),
	label=Hmisc::label(x),
	align=c("p{2.5cm}",rep("r",length(levels(as.factor(x)))*2)),
	display=c("s",rep("d",length(levels(as.factor(x)))), rep("f",rep(length(levels(as.factor(x)))))
		),table.placement = "!h")

print(a,caption.placement="top", caption.lot=paste(Hmisc::label(x)), hline.after=c(-1,0,nrow(a)-1,nrow(a)), file=paste(tables.dir,Hmisc::label(x),".tex", sep=""))

invisible(a)
}

#' Plain Table V
#'
#' Creates a plain table for a given variable and name and also returns an invisible
#' copy of the table
#'
#' @param x Variable to create the plain table for
#' @param names the names to be included in the table
#' @param tables.dir Directory to which all tables should be saved as character vector
#' @export


plain.table.v<-function(x,names,tables.dir=tabdir){
	y=as.data.frame(cbind(as.character(names),x,round(100*x/sum(x),2)))
	colnames(y)<-c("Category","Count","Percent")
	a=xtable(y, caption=paste("Table of\ ",Hmisc::label(x),sep=""),label=Hmisc::label(x),align=c("l","l","r","r"), display=c("s","s","f","d"),here=TRUE)
	print(a, file=paste(tables.dir,Hmisc::label(x),".tex", sep=""),caption.placement="top", caption.lot=paste(Hmisc::label(x)))
	invisible(a)	 	}
