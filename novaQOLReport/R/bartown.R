#' Bar Town
#'
#' Generic bar diagram for 2 x c table with proportion-test and binomial confidence interval
#'
#' @param df Data frame containing personal data
#' @param vr Character vector containing the variable (column) under consideration
#' @param place Character vector of the place under consideration
#' @param maintxt Character vector containing the title of the plot
#' @param coll Character vector containing lable colours
#' @param lt Legend text used in barplot
#' @param fn Character vector containing file name of output plot
#' @param graphdir Directory for graphs to be stored in
#' @param ab Logical to add one or more straight lines through the current plot
#' @param ablevel The y-value(s) for horizontal line(s)
#' @param out Logical that creates a copy of the graphics in pdf format to be stored in the desired location
#' @param ypos Numeric specifying the y axis position of the legend
#' @export

bartown <- function(df,
  vr = "demographics_member_sex",
  place = "place",
  maintxt = paste(gsub("_", " ", vr)," per ", place, sep=""),
  coll = c("red", "darkgreen"),
  lt = levels(df[ ,vr]),
  fn = paste(vr, place, "bar.pdf", sep=""),
  graphdir = grafdir,
  ab = TRUE,
  ablevel = 0.5,
  out = TRUE,
  ypos = 1.15){

barplot(prop.table(table(df[ ,vr], df[ ,place]),2),
        legend.text=lt, col = coll, ylab = paste("Proportion of ",
        capitalize(gsub("_", " ", vr))), xlab = gsub("_", " ", capitalize(place)),
        main = maintxt, sub="errorbar = 95%CI", args.legend = list(y = ypos))

tb = table(df[ ,vr], df[ ,place])
pt = prop.table(tb,2)[1,]
tab = addmargins(tb,1)
res.list = apply(tab, 2, function(x) prop.test(x[1], x[3]))
sig.dff = lapply(res.list, function(x) 0.50 < min(x$conf.int) | 0.5 > max(all(x$conf.int < 50)))
center = sapply(res.list, function(x) x["estimate"])
lower = sapply(res.list, function(x) x["conf.int"][[1]][[1]])
upper = sapply(res.list, function(x) x["conf.int"][[1]][[2]])
errbar(c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7)[1:length(levels(df[,place]))], y=pt, yplus=upper, yminus=lower, add= TRUE)
if (ab == TRUE) abline(h=ablevel, col="red")
if (out == TRUE) dev.copy2pdf( file=paste(graphdir, fn, sep=""))

}
