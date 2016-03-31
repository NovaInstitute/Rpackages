#' SWB Barplot
#'
#' Creates a SWB barplot Pos - Neg per town
#'
#' @param data Data frame containing survey data
#' @param var Character vector that contains swb variables
#' @param group Character vector referring to a grouping variable
#' @param plot Logical whether to create a plot (not present in this function)
#' @param teks Logical whether to create a LaTeX output (not present in this functon)
#' @param middel Numeric. Values smaller than this value will have proportion tests done on them for
#' each town.
#' @param graphdir Character vector containing the graph output directory
#' @param verbose Logical to display function messages
#' @export

swb.bar <- function(data, var, group, plot = TRUE, teks = TRUE, middel = 5, graphdir = "~/", verbose=TRUE){
if (verbose == TRUE) message(var)
data$prop = ifelse(data[, var] <= middel, "z.Neg", "a.Pos")
pt = prop.table(table(data$prop, data[, group]),2)
dev.new()
barplot(pt,
  names.arg=levels(data[,group]), ylab = "Proportion +",
  col=c("darkgreen","red"),
  main=paste("Simplified responses to ", gsub("_"," ",var)))

tab = addmargins(table(data$prop, data[, group]),1)
res.list = apply(tab, 2, function(x) prop.test(x[1], x[3]))
center = sapply(res.list, function(x) x["estimate"])
lower = sapply(res.list, function(x) x["conf.int"][[1]][[1]])
upper = sapply(res.list, function(x) x["conf.int"][[1]][[2]])
if (verbose == TRUE) message("pt ", dim(pt), " ", "upper ",length(upper), " lower ", length(lower))
errbar(c(0.7, 1.9, 3.1, 4.3), y=pt[1,], yplus=upper, yminus=lower, add = TRUE)
dev.copy2pdf(file = paste(graphdir, "bar.pos.neg.",var,group,".pdf",sep=""))
dev.off()

message("\nLower = ",lower, "Upper = ", upper, "central = ",center )
}

#' SWB Line
#'
#' Creates an SWB line plot per town. Takes a melted data frame as input
#'
#' @param m A melted data frame
#' @param group Grouping variable as character vector
#' @param facet Logical
#' @param uit Logical that saves the graph in the graphdir when TRUE
#' @param graphdir Directory to which graphs must be saved
#' @param uitnaam Name to be attributed to the output graph as character vector
#' @param outform The type of output desired as character vector
#' @param yfix Logical (Not present in this function)
#' @export

swb.line <- function(m, group = "place", facet=FALSE, uit = TRUE ,
                     graphdir = "~/tmp/", uitnaam = "SWBline", outform = ".pdf", yfix = FALSE){
	require(ggplot2)
	require(doBy)
	if (facet == FALSE) {fml = as.formula(paste("value~variable+",group))} else { fml = as.formula(paste("value~variable+",group, "+",facet))}
	mm = summaryBy(fml, data = m)
	names(mm)[2] = "group"
	mm = mm[is.na(mm$group) == FALSE,]
	message(dim(mm))

	if (facet != FALSE)  names(mm)[3] = "facet"
	lb = paste("Subjective well-being results per",group)
	p = qplot(data=mm, y=value.mean, x = variable,  color=group, geom="path", group=group, ylab="Mean score", main=lb)
	p + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=1))
	if (facet != FALSE) p + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=1)) + facet_wrap(group~facet)
	if (uit == TRUE) ggsave(file=paste(graphdir, uitnaam, outform,sep=""), width = 12, height =7)
}
