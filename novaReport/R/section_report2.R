#' section report II
#' 
#' This function creates a report based on a specific section as well as a grouping variable. The numeric_table, 
#' discrete_table and multi_table functions are used to generate the LaTeX output
#' 
#' @param x Data frame containing the survey results, typically the DES
#' @param qss Data frame containing the questions and responses
#' @param sec Character vector referring to the section under consideration
#' @param sectionvarname Character vector containing the name of the section column
#' @param question.text Character vector containing the name of the question text column
#' @param qs.varname Character vector containing the question name column reference
#' @param groupvar Character vector containing the grouping variable under consideration
#' @param multicol Character vector containing the name of the question type column
#' @param multiname Character vector referring to the multiple choice questions
#' @param debug Logical to display function steps
#' @param forcegvar Logical that forces a large amount of grouping variables to be processed if TRUE. 
#' This will cause an ugly and unrefined table output
#' @param varSizeN Character vector that contains the percentage column size to be
#' attributed to the column containing variables. This is used to customize tables. 
#' Used in function discrete_table.
#' @param levSizeN Character vector that contains the percentage column size to be
#' attributed to the column containing levels. This is used to customize tables.
#' Used in function discrete_table.
#' @param varSizeC Character vector that contains the percentage column size to be
#' attributed to the column containing variables. This is used to customize tables.
#' Used in function numeric_table.
#' @param levSizeC Character vector that contains the percentage column size to be
#' attributed to the column containing levels. This is used to customize tables.
#' Used in function numeric_table.
#' @export

section_report2 <- function(x, 
                            qss = qs, 
                            sec = "A", 
                            sectionvarname = "section", 
                            question.text="question.text",
                            qs.varname = "question.name", 
                            groupvar = "", 
                            multicol = "question.type", 
                            multiname = "M", 
                            debug = FALSE, 
                            forcegvar = FALSE, varSizeN = "0.15", levSizeN = "0.05", 
                            varSizeC = "0.15", levSizeC = "0.15"){
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
                               cap = paste("Categorical variables in section ", sec), verbose = FALSE, 
                               forcegvar = forcegvar, varSizeN = varSizeN, levSizeN = levSizeN),
                numeric_table(x, lab = paste(sec, "_n"), groupvar = groupvar,
                              cap = paste("Numeric variables in section ", sec), verbose = FALSE, 
                              forcegvar = forcegvar, varSizeC = varSizeC, levSizeC = levSizeC),
                multi_table(x, qs, verbose = FALSE, multiname = "Multiple", cap = paste("Multiple choice variables in section ", sec))

                )
              )
  }
        if (forcegvar){ message("You have forced a large amount of grouping variables causing unwanted table output")}
  }
#}
