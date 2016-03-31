#' Pos Negify
#'
#' Links "positive" and "negative" to answers indicating "agree" and "disagree"
#'
#' @param df Data frame containing multiple choice data
#' @param negs Character vector referring to negative connotations
#' @param pos Character vector referring to positive connotations
#' @export

pos.negify <- function(df,
                       negs = c("Strongly disagree", "Disagree"),
                       pos = c("Agree", "Strongly agree")
){
  for (i in 1:length(df)){
    if (is.factor(df[,i])==TRUE) {df[,i] = as.character(df[,i])
                                  df[is.na(match(df[,i], negs))==FALSE, i] = "negative"
                                  df[is.na(match(df[,i],  pos))==FALSE, i]  = "positive"
                                  df[,i] = as.factor(df[,i])
    }
  }
  df
}
