	#### Fuction to summarise a Clyral datset into a report organised per section ####
	
	## You will need:
	#1. the clyral data as a dataframe: df
	#2. A dataframe with the clyral questions: qs (we can build a auto-extract fro the xlsx format later). 
	
	# fixname both first
	
	#### Usage ####

# report.sections(df=amd.short,
#  qs=qs,
#  question.type="question.type",
#  question.name="question.name",
#  section="section",
#  datadir=datadir,
#  groupvar="main.place")
  
  ################# define helper functions #################
	# FInd empty rows opr columns (giveslogical vector) 
	test=function(x){length(which(is.na(x)==TRUE))==length(x)}

		drop.na.col=function(x)
		{test=apply(x,2,test)
		test
		}
	
		drop.na.row=function(x)
		{test=apply(x,1,test)
		test
		}	
	
	#removes dots and makes title case
	trashdot.titl <-function(x){
		x=paste(toupper(x)[1],tolower(x)[2:length(x)],sep="")
		patt='(^[[:alnum:]]*)([[:punct:]]*)([[:space:]]*)([[:alnum:]]*)([[:punct:]]*)([[:space:]]*)'
		gsub(patt,paste('\\1'," ",'\\4',sep=""),x )
		}
	
	# function to print table to be used for multi option questions
	cap.tN=function(x,
                  dir=dirr,
                  cumsum=FALSE,
                  group=NA)
		{
		message("You are inside cap.tN")
    message(ls(name=1))
    if(length(x)>0){
    message(trashdot.titl(names(x)[1]))
    #message(is.null(m.cap))
		cap=ifelse(test=is.na(match("m.cap",ls()))==TRUE, 
               yes=trashdot.titl(names(x)[1]), 
               no=ifelse(is.nul(m.cap)==FALSE, m.cap,NULL)
               )
		lab=ifelse(test=is.na(match("m.lab",ls()))==TRUE, 
               yes=names(x)[1], 
               no=ifelse(is.nul(m.lab)==FALSE, m.lab,NULL))
		capture.output(tableNominal(x,cap=cap,lab=lab,cumsum=cumsum,nams=names(x),group=group),file=paste(dir,lab,".tex",sep=""))
    message("capTN success:", lab)
    }
		}
	
	# function to make tables for discreat, numeric and multi-option variables	
		
	section.report=function(x,question.type="question.type",question.name="question.name",section="section",dirr=datadir){
	message("You are now inside Section Report")
	# section
	message(section,"\n")
	message(x[1,section][[1]])
	sec=x[1,section][[1]]
	message("sec=",sec,"\n")
	
	# maak al die goed if(length( diskreet  >0))
	
	# discreat vars
	
	d.idx<-which(x[,section]==sec & x$question.type!="Numeric - Integer" & x[,question.type]!="Option List - Multiple Selection")
	message(d.idx)
	if(length(d.idx)>0){
	d.varnames=x[,question.name][d.idx]
	message("discreate vars ",d.varnames," \n")
	d.cap=paste("Summary of discrete variables:",sec)
	d.lab=paste("d",sec)
	
	message("Matches ", match(d.varnames,names(df)))
  
	dvars=data.frame(df[,na.omit(match(d.varnames,names(df)))])
	message("length dvars: ", length(dvars))
  
  
	message("hold on, here we go for tableNominal")
	if(length(dvars)>0){ 
    capture.output(tableNominal(vars=dvars,cumsum=FALSE,cap=d.cap,lab=d.lab),file=paste(dirr,sec,"D",".tex",sep=""))}
	message("Discrete table success"," \n")
			    }
	
	# numeric vars
	n.idx<-which(x[,section]==sec & x$question.type=="Numeric - Integer" & x[,question.type]!="Option List - Multiple Selection")
	if(length(n.idx)>0){
	n.varnames=na.omit(x[,question.name][n.idx])
  
	message("Numeric vars: ",n.varnames,"\n")
	n.cap=paste("Summary of numeric variables:",sec)
	n.lab=paste("n",sec)
	message("Matches ", match(n.varnames,names(df)))
  
	nvars=data.frame(df[,na.omit(match(n.varnames,names(df)))])
  if(length(nvars)>0){
  names(nvars) <- n.varnames
	message("names nvars: ",names(nvars))
  message("groupvar: ",groupvar)
	group=df[,groupvar]
  message("head group: ",head(group))
	capture.output(
			tableContinuous(vars=nvars,cap=n.cap,lab=n.lab,group=group)
		,file=paste(dirr,sec,"N",".tex",sep=""))	
	message("Numeric table success","\n")}
		            }
	
	# Multi option
	m.idx<-which(x[,section]==sec & x$question.type=="Option List - Multiple Selection")
  message("m.idx: ", m.idx)
	if(length(m.idx)>0){
  message("Multi-option")  
	m.varnames=x[,question.name][m.idx]
	message("m.varnames: ", m.varnames,"\n")
	m.cap=paste("Summary of multiple option variables:",sec)
	m.lab=paste("m",sec)
  message("m.lab: ",m.lab)
	# stack multiple option
	m.vars.idx<-lapply(1:length(m.varnames),function(x) grep(m.varnames[x],names(df)))
  message("m.vars.idx: ", m.vars.idx)
	if(sum(sapply(m.vars.idx,sum))>0) {names(m.vars.idx)<-fixname(m.varnames)
	# manualy edit education
	m.vars.idx$employment<-m.vars.idx$employment[1:22]
	## drop unwanted elements (differing lengths)
	#m.vars.idx<-m.vars.idx[-match("explain.employment",names(m.vars.idx))]
	#m.vars.idx<-m.vars.idx[-match("mine.work",names(m.vars.idx))]
	m.vars=lapply(m.vars.idx,function(x) names(df[c(x)]))
	df.list=lapply(m.vars,function(x) cbind(df[x]))
	message("df.list", length(df.list), names(df.list))
  assign("df.list",df.list,envir=.GlobalEnv)                                   
	lapply(df.list,cap.tN)}
					}
	
	message("You are exiting section report")
	}
		
		
	################################## Main function ##################################
	# requires trashdot and drop.na.col
	
	
	report.sections=function(df=amd.short,qs=qs,question.type="question.type",question.name="question.name",section="section",datadir=datadir,groupvar="main.place")
		{
	# shorten qs
	message("qs with instructions=",length(qs[,1]),"\n")
	qs=qs[which(qs[,question.type]!="Instruction"),]
	message("qs without instructions=",length(qs[,1]),"\n")
	
	# Fix section names containing a /
	qs[,section]=gsub("/","",qs[,section])	
	
	# drop empty column in df and remove their names in qs
	message("before dropping empty columns: ",length(df))
	if(length(which(drop.na.col(df)==TRUE))>0) df=df[,-c(as.integer(which(drop.na.col(df)==TRUE)))]
	message("after dropping empty columns: ",length(df))
  
  assign("df",df,envir=.GlobalEnv)
	assign("groupvar",groupvar,envir=.GlobalEnv)
  
	# repeat per section
	by(qs,qs[,section],function(x)section.report(x))
		}
	######################################################################################
	
