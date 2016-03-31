#' Population Prevalence Table
#'
#' Creates PPT that can also serve as input to a population pyramid
#'
#' @param data Data frame that contains personal data
#' @param age Character vector referring to member age column
#' @param sex Character vector referring to member sex column
#' @param disease Character vector of the disease under consideration
#' @param binwidth Numeric that is used for calculating the number of intervals into which x is to be cut
#' @param verbose Logical that displays function messages when TRUE
#' @param drop.na Logical that drops NA values when TRUE
#' @param outname Logical that will assign the disease name to the global environment when TRUE
#' @param show.margins Logical vector of variable names
#' (can include "grand\_col" and "grand\_row") to compute margins for,
#' or TRUE to compute all margins . Any variables that can not be margined over will be silently dropped
#' @examples
#' res = pop.prev.table(disease=NULL)
#' dorp.p <- by(data=persoon,INDICES = persoon$place, FUN = function(x) pop.prev.table(data=x, disease=NULL))
#' @details Can then use pyramid.plot(res$Male_Yes,res$Female_Yes,labels=res$Age_group, main=disease, gap=1.5, show.values=TRUE), OR
#' pyramid.plot(res$Male_1,res$Female_1,labels=res$Age_group, main=NULL, gap=50, show.values=TRUE)
#' @export

pop.prev.table <- function(data = persoon,
                           age = "demographics_member_age",
                           sex = "demographics_member_sex",
                           disease = "body_symptoms_twelvemonth_list_wheezing.and.tight.chest.at.night.or.after.exercise",
                           binwidth = 10,
                           verbose = FALSE,
                           drop.na = TRUE,
                           outname = TRUE, show.margins = FALSE){

  if (!require(plyr)){install.packages("plyr")
          library(plyr)}

  if (!require(reshape2)){install.packages("reshape2")
          library(reshape2)}

  if (outname == TRUE) assign(x="disease", disease, envir=.GlobalEnv)
  if (drop.na == TRUE) data = data[!is.na(data[, age]), ]
  if (is.null(disease)) {disease = rep(1, nrow(data))} else {disease = data[ ,disease]}
  df = data.frame(age = data[ ,age], sex = data[ ,sex], disease = disease)
  if (verbose == TRUE) message(str(df))
  #used the following from epicalc:pyramid by Virasakdi Chongsuvivatwong
  agegr <- cut(df$age,
               br = ((min(df$age, na.rm = TRUE)%/%binwidth):(max(df$age, na.rm = TRUE)%/%binwidth + (max(df$age, na.rm = TRUE)%%binwidth >  0)) * binwidth),
               include.lowest = TRUE)

  df$Age_group = agegr
  m = melt(df)
  res = dcast(m, Age_group ~ sex + disease, subset=.(!is.na(age)), margins=show.margins, drop=TRUE)
  res
}

