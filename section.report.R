##########################################################################################
# section.report                                                                         #
# function to make tables for discrete, numeric and multi-option variables               #
# This function works together with othjer functions:                                    #
# Its called by report.section and uses test, drop.na.col, drop.na.row, trashdot.titl    #
# AND cap.tN                                                                             #
# Needs a Clyral dataset and the questions with sections and types                       #
# (c) Nova Institute 2013                                                                #
##########################################################################################


section.report = function(x,
                          d.f = get("df", envir = sys.frame(which = -1)),
                          question.type = "question.type",
                          question.name = "question.name",
                          section = "section",
                          intname = get("intname", envir = sys.frame(which = -1)),
                          multiname = get("multiname", envir = sys.frame(which = -1)),
                          dirr = get("datadir", envir = sys.frame(which = 1)),
                          groupvar = get("groupvar", envir = sys.frame(which = 1)),
                          verbose = get("verbose", envir = sys.frame(which = 1)),
                          raw = TRUE,
                          debug = TRUE, ...) {

  if (verbose > 0) report_messages()

  # section
  if (verbose > 3)  message("Section is " , section, "\n")
  if (verbose > 1)  message(x[1, section][[1]])

  sec = gsub("[[:space:]]", "_", x[1, section][[1]])

  if (verbose > 0)  message("sec=", sec, "\n")

  # maak al die goed if(length( diskreet >0))


  # Discrete_report



  # numeric vars
  if (verbose > 0)
    message("Ons gaan numeries")
  n.idx <- which(x[, section] == sec & x[ ,question.type] == intname & x[, question.type] != multiname)
  if(verbose > 0) message("n.idx is ", n.idx, "\n", length(na.omit(n.idx)), "\n")

  if (length(na.omit(n.idx)) > 0) {
    n.varnames = x[, question.name][n.idx]
    if (verbose > 0)
      message("Numeric vars ", n.varnames, "\n")
    n.cap = paste("Summary of numeric variables: ", sec)
    n.lab = paste("n", sec)

if (length(na.omit(match(n.varnames, names(d.f)))) > 0){
      if (verbose > 1)
        message("Matches ", match(n.varnames, names(d.f)))
      nvars = data.frame(d.f[, na.omit(match(na.omit(n.varnames), names(d.f)))])
      if(dim(nvars)[[2]]==1) colnames(nvars) = n.varnames
      if (verbose > 1)
        message("dims nvars ", dim(nvars)[[1]], " x ", dim(nvars)[[2]], " en all(is.na(unlist(nvars))) is ", all(is.na(unlist(nvars))))
      if(all(is.na(unlist(nvars)))==FALSE){
        if (verbose > 1)  message("Dim nvars is ", dim(nvars)[[1]], " x " , dim(nvars)[[2]])
        if (verbose > 1)  message("groupvar is ", groupvar)
        if(is.na(match(groupvar, names(d.f)))) group <-  NA else group <- d.f[, groupvar]
        if (verbose > 1)  message("group is ", head(group), "\n")
        if (debug == TRUE) assign("group", group, envir=.GlobalEnv)
        if (debug == TRUE) assign("groupvar", groupvar, envir=.GlobalEnv)
        if (debug == TRUE) assign("d.f", d.f, envir=.GlobalEnv)
        if (debug == TRUE) assign("nvars", nvars, envir=.GlobalEnv)
        if (raw == FALSE) {
          message("raw == FALSE\n")
          capture.output(tableContinuous(vars = nvars,
                                       cap = ifelse(exists("n.cap"), n.cap, "") ,
                                       lab = ifelse(exists("n.lab"), n.lab, ""),
                                       group = ifelse(length(group) > 0, group, NA),
                                       caption.placement = "top"),
                       file = paste(dirr, sec, "N", ".tex", sep = ""))
        message("Numeric table success", "\n")
        } else {
          message("raw == FALSE\n")
          tableContinuous(vars = nvars,
                                cap = ifelse(exists("n.cap"), n.cap, ""),
                                lab = ifelse(exists("n.lab"), n.lab, ""),
                                group = ifelse(length(group) > 0, group, NA),
                                caption.placement = "top")
        }
      }
}

  }

  # Multi option
  m.idx <- which(x[, section] == sec & x[, question.type] == multiname)
  if (verbose > 0)
    message("m.idx gedefinieerd as ", head(m.idx))
  if (verbose > 0)
    message("names df: \n", names(d.f), "\nOK")
  if(verbose > 0) message("Lengte van m.idx ", length(as.integer(na.omit(m.idx))))

if (length(as.integer(na.omit(m.idx > 0)))) {
    m.varnames = x[, question.name][m.idx]
    if (verbose > 0)
      message("m.varnames is: ", paste(m.varnames, " "), "\n")
    m.cap = paste("Summary of multiple option variables:", sec, m.varnames)
    if (verbose > 0)
      message("m.cap is: ", paste(m.cap, " "), "\n")
    m.lab = gsub(" ", "", paste("m", sec,".", m.varnames, sep=""))
    if (verbose > 0)
      message("m.lab is: ", paste(m.lab, " "), "\n")
    # stack multiple option
    m.vars.idx = lapply(1:length(m.varnames), function(x) grep(m.varnames[x], names(d.f)))
    names(m.vars.idx) = fixname(m.varnames)
    m.vars = lapply(m.vars.idx, function(x) names(d.f[c(x)]))
    df.list = lapply(m.vars, function(x) cbind(d.f[x]))
    #lapply(df.list, cap.tN, lab = m.lab) # dit werk maar oorskryf elke keer die volgende table so kan net een m. per seksie doen
    if (raw == FALSE){mapply(cap.tN, x=df.list, lab=m.lab, cap = m.cap)
    } else {mapply(cap.tNN, x=df.list, lab=m.lab, cap = m.cap)}
}

  message("You are exiting section report\n")
}
