## function to make tex output from questions

qs.text <- function(qs, varname="question.name", question.text="question.text"){
  tl = unlist(lapply(1:nrow(qs), function(x) sprintf("\\item [{%s}] %s ", gsub("_", "\\\\_", qs[ ,varname][x]), qs[, question.text][x])))
  do.call('cat', list("\\begin{description}", tl , "\\end{description}"))
}

qs.text.string <- function(qs, varname="question.name", question.text="question.text"){
  tl = unlist(lapply(1:nrow(qs), function(x) sprintf("\\item [{%s}] %s ", gsub("_", "\\\\_", qs[ ,varname][x]), qs[, question.text][x])))
  sprintf("\\begin{description} %s \\end{description}", paste(tl, collapse =" " ))
}
