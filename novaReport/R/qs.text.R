#' Text Output
#' 
#' This function creates a text output from questions and returns 
#' a vector containing the question name and its text grouped together
#' 
#' @param qs Questions data frame
#' @param varname Character vector referring to the name of the question
#' @param question.text Character string referring to the text of the question
#' @export

## function to make tex output from questions

qs.text <- function(qs, varname="question.name", question.text="question.text"){
  tl = unlist(lapply(1:nrow(qs), function(x) sprintf("\\item [{%s}] %s ", gsub("_", "\\\\_", qs[ ,varname][x]), qs[, question.text][x])))
  do.call('cat', list("\\begin{description}", tl , "\\end{description}"))
}

#' String Output
#' 
#' This function behaves the same as qs.text, but it manipulates a character vector of format strings
#' 
#' @param qs Questions data frame
#' @param varname Character vector referring to the name of the question
#' @param question.text Character string referring to the text of the question
#' @export

qs.text.string <- function(qs, varname="question.name", question.text="question.text"){
  tl = unlist(lapply(1:nrow(qs), function(x) sprintf("\\item [{%s}] %s ", gsub("_", "\\\\_", qs[ ,varname][x]), qs[, question.text][x])))
  sprintf("\\begin{description} %s \\end{description}", paste(tl, collapse =" " ))
}
