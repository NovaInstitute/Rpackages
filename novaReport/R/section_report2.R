#' section report II
#' 
#' 

section_report2 <- function(x, 
                            qss = qs, 
                            sec = "A", 
                            sectionvarname = "section", 
                            question.text="question.text",
                            qs.varname = "question.name", 
                            groupvar = "", 
                            multicol = "question.type", 
                            multiname = "M", 
                            debug = FALSE){
  if (debug == TRUE) message("sec = ", sec)
  if (any(qss[,sectionvarname, drop = TRUE] == sec)){ # as dit nie hiedie seksie is nie gaan ons nie voort nie
    idx <- qss[,sectionvarname, drop = TRUE] == sec
    if (debug == TRUE){
      assign("idx.1", idx, envir = .GlobalEnv)
      assign("qss.1", qss, envir = .GlobalEnv)}
    qss <- qss[idx, ,drop = FALSE]
    if (debug == TRUE) message("dim qss = ", paste(dim(qss), collapse = " by "))
    if (debug == TRUE){
      assign("x.1", x, envir = .GlobalEnv)
      assign("qs.1", qss, envir = .GlobalEnv)}

    # die veranderlike wat ons soek is die in die qs is en die groupvar behalwe as dit multi is : dan soek ons die wat begin met die naam in qs
    var.n <- c(as.character(qss[,qs.varname]), groupvar)

   if (all(!is.na(match(var.n, colnames(x))))) {
     x = x[,var.n]
   } else {
     if (any(qs[,multicol] == multiname)){
       mm.idx = lapply(as.character(qss[,qs.varname, drop = TRUE]), function(z) grep(z, colnames(x)))
       mm.idx = unlist(mm.idx[sapply(mm.idx, length) > 0])
       if (debug == TRUE) message("mm.idx is ", length(mm.idx), " lank")
       x <- x[ ,unique(c(mm.idx, match(groupvar, names(x)))), drop = FALSE]
     }
   }
   if (debug == TRUE) message(paste(dim(x), collapse = "  BY  "))
      do.call("cat",
              list(
                sprintf("\\section{%s} The variables and the associated questions were:", sec),
                sprintf("%s", qs.text.string(qss, varname=qs.varname, question.text=question.text)),
                discrete_table(x, lab = paste(sec, "_d"), groupvar = groupvar,
                               cap = paste("Categorical variables in section ", sec), verbose = FALSE),
                numeric_table(x, lab = paste(sec, "_n"), groupvar = groupvar,
                              cap = paste("Numeric variables in section ", sec), verbose = FALSE),
                multi_table(x, qs, verbose = FALSE)

                )
              )
    }
  }
#}
