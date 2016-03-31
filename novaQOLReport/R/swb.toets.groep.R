#' Test Group
#'
#' Test differences in swb tables per group
#'
#' @param data Data frame containing survey data
#' @param x The swb variable under consideration as character vector
#' @param group Character vector containing the grouping variable
#' @param group2 Character vector containing an optional second grouping variable
#' @param tabdir Directory for tables to be saved in
#' @param graphdir Directory for graphs to be saved in
#' @param middel Numeric. Values smaller than this value will have proportion tests done on them for
#' each town. Bonferroni's method is used to calculate p-values
#' @export

toets.groep <- function(data = qol, x = "swb_satisfaction_general", group = "place",
                        group2  = NULL, tabdir = "~/tmp/", graphdir="~/tmp/", middel = 5)
{
	if (!require(ggplot2)){install.packages("ggplot2")
	        library(ggplot2)}
	if (!require(reshape2)){install.packages("reshape2")
	        library(reshape2)}
	if (!require(Hmisc)){install.packages("Hmisc")
	        library(Hmisc)}

  message("x is ", x , "en groep is ",group)
# Grafiese voorstelling van die gemiddeldes oor die 4 dorpe is nog my voorkeur!
  df = data[ ,c(x, group, group2)]
  names(df)[2] = "group"
  if (is.null(group2)==FALSE) names(df)[3] = "group2"
  mx = melt(df, id.vars = c("group", "group2"))
  pp = qplot(data = mx, x=group, y=value, geom = "boxplot", xlab = group, main = gsub("_", " ", x))
  if (is.null(group2)==FALSE) pp = pp + facet_wrap(group ~ group2, scales = "free_x")
  pp + theme(text = element_text(size=10), axis.text.x = element_text(angle=0, vjust=1))
  if (is.null(group2)==FALSE) {fn = paste(tabdir, "boxplot.",x, ".", group, ".", group2,".pdf", sep="")
  	} else {
  		fn = paste(tabdir, "boxplot.",x, ".", group, ".pdf", sep="")}
  ggsave(pp, file=fn, width = 14)

# Doen 'n toets vir proporsies van waardes kleiner as  <= 5  vir elke dorp. Gebruik Bonferroni se metode om die p-waarde te bereken

  # kategorisee in twee groepe: positief en nagatief
  z = ifelse(data[,x] <= middel, "z.Neg", "a.Pos")
  prop.pos = prop.test(table(z))
  dev.new()
  barplot(prop.table(table(z)), width=1, names.arg=c("Positive","Negative"),
          main=paste("Simplified responses to ", gsub("_","",x)), col=c("darkgreen","red"))

  errbar(0.7, y=prop.table(table(z))[1], yplus=prop.pos$conf.int[2], yminus=prop.pos$conf.int[1], add=TRUE)
  dev.copy2pdf(file=paste(tabdir, "bar.",x, ".", group, ".pdf", sep=""))
  dev.off()
  # Toets of dit betekenisvol van 50% verskil
  res = paste("A total of ",round(prop.pos$estimate,4)*100, "% (95% CI: ", paste(round(prop.pos$conf.int,4) * 100, "%", collapse=" to ", sep=""), "; p-value: ",round(prop.pos$p.value,6) ,") of respondents expressed a positive evaluation", collapse = "", sep="")
   res
  # Toets of hulle onderling verskil

# Toets of die groepe normaal of groot
  # kom ons aanvaar vir die oomblik as daar meet as 100 rekords is dat die gemiddeld normaal verdeel sal wees
  if (length(data[, x]) > 99){

# Is dit multi-modaal of uni-modaal
  # druk digtheid en inspekteer
  dp = qplot(data = mx, x=value, geom = "density", xlab = group, main = gsub("_", " ", x), color = group)
  ggsave(dp, file=paste(tabdir, "density.",x, ".", group, ".pdf", sep=""))
  message("Digtheid gestoor")

  # Bied meermodaal as gebeurlikheidstabel aan saam met staafdiagram
  dbar = qplot(x=value, ..density.. , data = mx, geom = "bar", xlab=(0:10), facets = ~ group, main = gsub("_", " ", x))
  ggsave(dbar, file=paste(tabdir, "bar2.",x, ".", group, ".pdf", sep=""))
  res2 = table(data[,x], data[,group])
  res
}
res
}
