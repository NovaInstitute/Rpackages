
d.idx <- which(x[, section] == sec & x$question.type != intname & x[, question.type] != multiname)
if (verbose > 1)
  message("d.idx is : ", d.idx, "\n")
if (length(d.idx) > 0) {
  d.varnames = x[, question.name][d.idx]
  if (debug==TRUE) assign("d.varnames", d.varnames, envir=.GlobalEnv)
  if (verbose > 1)
    message("discrete vars in questionnaire:  ", d.varnames, " \n")
  d.cap = paste("Summary of discrete variables:", sec)
  if (verbose > 1)
    message("Caption is:  ", d.cap, " \n")
  d.lab = paste("d", sec)
  if (verbose > 1)
    message("Label is:  ", d.lab, " \n")
  if (verbose > 3) message("str(df) ", str(d.f), "\n")
  if (verbose > 1)
    message("Matches ", match(d.varnames, names(d.f)), "\n")
  if (verbose > 1)
    message("names d.f :", names(d.f), "\n")
  if (verbose > 1)
    message("as.integer(na.omit(match(d.varnames, names(d.f))))", as.integer(na.omit(match(d.varnames, names(d.f)))), "\n")

  dvar.idx = as.integer(na.omit(match(d.varnames, names(d.f))))
  if (length(na.omit(dvar.idx) > 0)) {
    if (verbose > 1)
      message("\n", "dvar.idx ", dvar.idx)
    dvars = data.frame(d.f[, na.omit(dvar.idx)])
    if (verbose > 1)
      message("dim dvars :", dim(dvars)[[1]], " x ", dim(dvars)[[2]])

    if(exists("dvars")){
      message("hold on, here we go for tableNominal")

      if (raw == TRUE){ discrete_table()
      } else {discrete_table_write()}
    }
  }
}