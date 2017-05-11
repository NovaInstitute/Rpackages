#' SummaryBy Table
#'
#' Function to calculate groupwise summary statistics
#'
#' @param df Data frame to generate summary statistics for
#' @param varname The variable name under consideration
#' @param fn The desired file name for the output LaTeX table
#' @param label Character vector of length 1 containing the
#' LaTeX label or HTML anchor. Set to NULL to suppress the label. Default value is NULL.
#' @param caption Character vector of length 1 or 2 containing the
#' table's caption or title. If length is 2, the second item is the "short caption"
#' used when LaTeX generates a "List of Tables". Set to NULL to suppress the caption.
#' Default value is NULL.
#' @export

summaryBy.table=function(df,varname="CPy",fn="yearprojectbytown",
                         label="projectbytownyear",
                         caption="Annual Project coal consumption by town"){

names(df)=gsub("\\.","\\ ",names(df))
n.rows=nrow(df)
message(cat("n rows =",n.rows,sep=""))
n.cols=ncol(df)
message(cat("n cols =",n.cols,sep=""))

# Make a seperate row of totals
totals <- colSums(df[3:n.cols])
message(cat("Totals =",totals))

means=weighted.mean(df[1:n.rows,2],(df[1:n.rows,4]-df[1:n.rows,5]),na.rm=TRUE)
message(cat("Means=",means))

last.row=c("TOTAL, MEAN",means,totals)
message(cat("last row =" ,last.row))

# Now add them together
# first re-set the factor levels of column 1:
df[,1] <- as.factor(df[,1])
levels(df[,1])=c(levels(df[,1]),"TOTAL, MEAN")
df=rbind(df,last.row)
message(cat("Rownames = ",rownames(df)))
message(cat("Colnames = ",colnames(df)))
#message(cat("Town = ",df[,1]))
#message(cat("Mean = ",df[,2]))

rownames(df)<-df[,1]
message(cat("Rownames = ",rownames(df)))
df=df[-1]
message(cat("rownames=",rownames(df)))

VAR=df["TOTAL, MEAN",1]
assign(varname,as.numeric(VAR),envir=.GlobalEnv)
assign(label,df,envir=.GlobalEnv)

message(str(df))
df[,1]=as.numeric(df[,1])
df[,2]=as.numeric(df[,2])
message(str(df))

#Write out table
#align.string=cat("rl",rep("{x1.8cm}",(length(df)-2)),sep="")
print(xtable(df,
             caption=caption,
             digits=2,
             display=c("s","f","d","d","d"),
             align="lx{1.8cm}x{1.8cm}x{1.8cm}x{1.8cm}",
             label=label,
             title=varname
             ),
      file=paste(basedir,tabdir,fn,".tex",sep=""))
}
