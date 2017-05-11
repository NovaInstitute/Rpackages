#' Extract Base Variables
#'
#' Helper function to use for repeating variables e.g. person_1 , person_2 etc
#'
#' @param x Data frame containing survey data
#' @param reflist A reference list
#' @param df.name Name of the data frame that is assigned to the global environment as character vector
#' @param lst.name Name of the list that is assigned to the global environment as character vector
#' @param verbose Displays function messages when TRUE
#' @export

# @example data=readCodebook(cb,data)

extract.basevars=function(x,reflist,df.name="var.name.df",lst.name="lst", verbose = FALSE){
	var.pat='(^[[:print:]]+)(_[[:digit:]]{0,2}$)'  # Match a underscore and
	var.pat.2='(^[[:print:]]+)(_){1}([[:print:]]*$)'
	var.pat.3='(^[[:print:]]+)(_*$)' # remove trailing underscore
	var.name.df=data.frame(name=names(x),base.var.1=gsub(var.pat, '\\1', names(x)),nr= sub(var.pat, '\\2', names(x)))
	var.name.df$test=as.character(var.name.df$name)==as.character(var.name.df$nr)
	var.name.df$base.var.2=gsub(var.pat.2, '\\1',var.name.df$base.var.1 )
	var.name.df$base.var.3=gsub(var.pat.3, '\\1',var.name.df$base.var.2 )
	if (verbose == TRUE) message("data.frame created: ",var.name.df[20:40,],"\n")
	if (verbose == TRUE) assign(df.name,var.name.df,envir=.GlobalEnv)

	# Add _none.of.the.above and cbind

	#
	var.name.df=var.name.df[which(var.name.df$test==FALSE),]
	var.name.df$ref=match(var.name.df$base.var.1,names(reflist)) #
	var.name.df<-var.name.df[which(is.na(var.name.df$ref)==FALSE),]
	var.name.df

	if (verbose == TRUE) message("data.frame shortned: ",var.name.df[1:5,],"\n")
	lst=vector("list", length(var.name.df[,1]))
	lst[1:length(lst)]<-reflist[var.name.df$ref]
	names(lst)=var.name.df$name
	if (verbose == TRUE) assign(lst.name,lst,envir=.GlobalEnv)
	lst
		}

#' Read CodeBook (Recoder)
#'
#' Function is used to read a given codebook and apply the codes to variables present in the
#' supplied survey data. This function is sensitive to variable names
#'
#' @param cb Data frame containing codes and their corresponding meanings
#' @param data Data frame to which the codes should be applied to
#' @param verbose Displays function messages when TRUE
#' @export

readCodebook <- function(cb, data, verbose=FALSE){
# load libraries
library(Hmisc)

# save original names for later use
orig.names=names(data)
if (verbose == TRUE) message("nr. of original names ",length(orig.names))

#fix names to match codebook
colnames(data)=tolower(colnames(data))
names(data)=gsub(" ",".",names(data))
names(data)=gsub("/",".",names(data))
names(data)=gsub("-",".",names(data))
names(data)=gsub("_0","",names(data))
names(data)=gsub("\\.0","",names(data))# get codebook data from csv.

if (verbose == TRUE) message("data: ",head(str(data)),"\n")
# In the absence of column namens columns are names X, X.1, X.2 etc

#name list for later use
nn=as.character(cb[which(cb[,1]!="" & cb[,1]!="''" ),1])
if (verbose == TRUE) message("nn= ", str(nn))
nn=tolower(nn)
if (verbose == TRUE) message("nn.1= ", str(nn))
nn=gsub(" ","\\.",nn)
if (verbose == TRUE) message("nn.2= ", str(nn))
nn=gsub("-","\\.",nn)
if (verbose == TRUE) message("nn.3= ", nn)
nn=gsub("/","\\.", nn)
if (verbose == TRUE) message("nn.4= ", nn)
nn=gsub("\\?","\\.",nn)
if (verbose == TRUE) message("nn.5= ", nn)
nn=gsub("'","",nn)
if (verbose == TRUE) message("nn.6= ", nn)
nn = nn[!nn %in% "."]
if (verbose == TRUE) message("length nn: ", length(nn) )

if (verbose == TRUE) message("names in codebook: \n",paste(head(nn),"\n")," See variable codenames")
if (verbose == TRUE) message("nr. names in codebook: ",length(nn),"\n")
if (verbose == TRUE) assign("codenames",nn,envir=.GlobalEnv)

# First extract the question names which is all the non-empty entries in column 1 (first entry is "")
nx=unique(cb[,1])[-1]
if (verbose == TRUE) message("nx = ", nx)
#name list for later use
nx=tolower(nx)
nx=gsub(" ","\\.",nx)
nx=gsub("-","\\.",nx)
nx=gsub("/","\\.",nx)
nx=gsub("\\?","\\.",nx)
if (verbose == TRUE) message("nx = ", nx)
if (verbose == TRUE) message("nn= ", nn)

# Drop empty cells
cb=cb[which(cb$X.1!=""),]
if (verbose == TRUE) message("length cb = ", length(which(cb$X.1!="")) )

# mark cut-off points
value=which(cb$X.1=="Value"| cb$X.1=="'Value'")
if (verbose == TRUE) message("names(cb)= ", names(cb))
if (verbose == TRUE) message("cb$X.1= ", cb$X.1)
if (verbose == TRUE) assign("cb1",cb,envir=.GlobalEnv)
if (verbose == TRUE) message("which(cb$X.1==\"Value\")", which(cb$X.1=="Value"))
if (verbose == TRUE) message("value= ", value)
# make a index of parts that belong together
#drop first and last. not part of content

idx=lapply(1:(length(value)-1),function(x){as.integer(value[x]+1):as.integer(value[x+1]-1)})
idx.end=list(as.integer(value[length(value)]+1):as.integer(length(cb[,2])))
idx[length(value)]=idx.end

### length of each section of index
ll=lapply(1:length(idx),function(i){length(cb$X.1[idx[[i]]])})
if (verbose == TRUE) message(paste(ll[1:5],"\n"))
if (verbose == TRUE) message("nn= ", str(nn))
names(ll) <- nn
if (verbose == TRUE) message("ll before:", length(ll),"\n")
ll.orig=ll


# index of codes that are not really vars but part of a multi-select var
if (verbose == TRUE) message("nn and names data", nn, names(data))
x=match(nn,names(data))
multi.idx=which(is.na(x))
if (verbose == TRUE) assign("multi.idx",multi.idx,envir=.GlobalEnv)

if (verbose == TRUE) message("nn= ", str(nn))
if (verbose == TRUE) message("names(data)=" ,names(data))
if (verbose == TRUE) message("\n","x =" ,x)
if (verbose == TRUE) message("length of match(nn,names(data)) =", length(x) )
if (verbose == TRUE) message("length(which(is.na(x))==TRUE) ",length(which(is.na(x))==TRUE))

if(length(which(is.na(x)))>0) ll=ll[-which(is.na(x))] #
if(length(which(is.na(x)))>0) nn=nn[-which(is.na(x))] #

if (verbose == TRUE) message("ll after dropping the multis:", length(ll),"\n")
if (verbose == TRUE) message("names after dropping the multis:", length(nn),"\n")

# Extend nn
nn.add=names(extract.basevars(data,ll))
if (verbose == TRUE) message("additional names: ",paste(head(nn.add),"\n"))

if(length(nn.add)>0){
nn=c(nn,nn.add)
if (verbose == TRUE) message("nn after:", length(nn),"\n")
if (verbose == TRUE) assign("nn",nn,envir=.GlobalEnv)
                      }
ll.add=extract.basevars(data,ll)
if (verbose == TRUE) message(head(ll.add),"\n")

if(length(ll.add)>0) {
names(ll.add)=nn.add
if (verbose == TRUE) message("ll additional:", length(ll.add),"\n")
ll=c(ll,ll.add)
if (verbose == TRUE) message("ll after:", length(ll),"\n")
if (verbose == TRUE) assign("ll",ll,envir=.GlobalEnv)
                    }
### make a list of levels
vv=lapply(1:length(idx),function(i){cb$X.1[idx[[i]]][1:ll.orig[[i]]]}) # Values
vv=lapply(vv,function(x){as.factor(as.character(x))})

if (verbose == TRUE) message("vv before:", length(vv),"\n")
if(length(which(is.na(x)))>0){
vv=vv[-which(is.na(x))] # shorten values by taking out the ones that actually belong to the multi options
if (verbose == TRUE) message("vv after dropping the multis:", length(vv),"\n")
names(vv)=nx[-multi.idx] # the names on these values should be the same as the shortend name list
                              }
vv.add=extract.basevars(data,vv)
names(vv.add)#=nn.add
if (verbose == TRUE) message("vv additional:", length(vv.add),"\n")

if(length(vv.add)>0){
vv=c(vv,vv.add) #
if (verbose == TRUE) message("vv after:", length(vv),"\n")
if (verbose == TRUE) assign("vv",vv,envir=.GlobalEnv)
                    }
### Codes
cc=lapply(1:length(idx),function(i){cb$X[idx[[i]]][1:ll.orig[[i]]]})
cc=lapply(cc,function(x){as.factor(as.character(x))})

if (verbose == TRUE) message("cc before:", length(cc),"\n")

if(length(which(is.na(x)))>0){
cc=cc[-which(is.na(x))] #
names(cc)=nx[-multi.idx]
if (verbose == TRUE) message("cc after dropping the multis:", length(cc),"\n")
}

cc.add=extract.basevars(data,cc)
names(cc.add)=nn.add
if (verbose == TRUE) message("cc additional:", length(cc.add),"\n")

if(length(cc.add)>0) {
cc=c(cc,cc.add)
if (verbose == TRUE) message("cc after:", length(cc),"\n")
if (verbose == TRUE) assign("cc",cc,envir=.GlobalEnv)
                      }
nrs=lapply(ll,function(x)dput(1:x)) # Number the levels
if (verbose == TRUE) message("names nrs",names(nrs))
if (verbose == TRUE) assign("nn",nn,envir=.GlobalEnv)
if (verbose == TRUE) assign("nrs",nrs,envir=.GlobalEnv)

### redifine definition of nn
nnx<-names(nrs)
if (verbose == TRUE) message("nr. names: ",length(nnx))
if (verbose == TRUE) message("nr. unique names: ",unique(length(nnx)))
if (verbose == TRUE) assign("nnx",nnx,envir=.GlobalEnv)
###

# recode
cols=na.omit(match(nnx,names(data))) # index of column name matches.
names(cols) <- names(data)[cols]
if (verbose == TRUE) assign("cols",cols,envir=.GlobalEnv)

if (verbose == TRUE) message("Non-matches: ",length(names(data)[which(is.na(match(nnx,names(data)))==TRUE)]),"\n")
if (verbose == TRUE) message(paste(nnx[which(is.na(match(nnx,names(data)))==TRUE)],"\n"))

if (verbose == TRUE) message("number of columns to recode: ",length(cols))

nnllvv<-cbind(nnx,ll,vv,cc,nrs)
if (verbose == TRUE) assign("nnllvv",nnllvv,envir=.GlobalEnv)
if (verbose == TRUE) message("nnllvv asigned")

if (verbose == TRUE) message("length(cols)= ",length(cols))

for(i in 1:length(cols))
{
nr.order=na.delete(data.frame(match(levels(as.factor(data[,cols[[i]]])),as.character(cc[[i]]))))[,1] # get the code numbers
data[,cols[[i]]] <- as.character(data[,cols[[i]]])
spiel <- match(data[,cols[[i]]],cc[[i]])
spiel<- factor(spiel, levels=nr.order[order(nr.order)],labels=vv[[i]][nr.order[order(nr.order)]])
data[,cols[[i]]] <- spiel
rm(spiel)
rm(nr.order)
}
if (verbose == TRUE) message("nr. names final: ",length(names(data)) )
#names(data)=orig.names
data


}


#' coding tests and confirmation
#' @export

coding.test<-function(){
  if (verbose == TRUE) message("codes:", length(cc) ,"\n")
  if (verbose == TRUE) message("values:", length(vv) ,"\n")
  if (verbose == TRUE) message("lengths:", length(ll) ,"\n")
  if (verbose == TRUE) message("names:", length(nn) ,"\n")
  if (verbose == TRUE) message("numbers:", length(nrs) ,"\n")
}

## full factor levels with each var
##
