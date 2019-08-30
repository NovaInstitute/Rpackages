#' Multi-report
#'@export

multi_report <- function (x, 
                          single = TRUE,
                          vargroep_lbl = "een", 
                          var = NULL, 
                          groep = NULL, 
                          groeplab = NULL, 
                          out = TRUE, 
                          stats = c("all", "basic")[1], 
                          longt = FALSE, 
                          qs, 
                          qs_name = "question.name", 
                          qs_text = "question.text", 
                          drr = "~/tmp/", 
                          ncat = 4, 
                          nnum = 10, 
                          druk = FALSE, 
                          webify = FALSE, 
                          verbose = FALSE, 
                          wd = 15, 
                          md = TRUE, 
                          chi.digits = 4, 
                          debug = TRUE, 
                          varSizeN = "0.25", 
                          levSizeN = "0.15", 
                          voorspellende_veranderlikes = NULL, ...) {
  
  {
    if (!require(R.utils, quietly = TRUE, warn.conflicts = FALSE)) {
      install.packages("R.utils")
      library(R.utils)
    }
    if (!require(reporttools, quietly = TRUE, warn.conflicts = FALSE)) {
      install.packages("reporttools")
      library(reporttools)
    }
    if (!require(ggplot2, quietly = TRUE, warn.conflicts = FALSE)) {
      install.packages("ggplot2")
      library(ggplot2)
    }
    if (!require(reshape2, quietly = TRUE, warn.conflicts = FALSE)) {
      install.packages("reshape2")
      library(reshape2)
    }
    if (!require(gridExtra, quietly = TRUE, warn.conflicts = FALSE)) {
      install.packages("gridExtra")
      library(gridExtra)
    }
    if (!require(pander, quietly = TRUE, warn.conflicts = FALSE)) {
      install.packages("pander")
      library(pander)
    }
  }
  
  
  # Basiese huishouding -----------------------------------------------------
  
  if (!is.null(var)) {
    if (single) {
      message("Ek verkort var tot die eerste een") 
      var = as.character(var)[1]
    }
  } else {
    var <- names(x)
  }
  if (verbose)  message("\n\n\n\n var is:  ", var, "\n\n\n\n")
  if (!is.data.frame(x)) stop("x must be a data frame")
  if (!is.data.frame(qs)) stop("qs must be a data frame")
  if (!is.character(var)) stop("var must be coercible to a character")
  if (any(is.na(match(var, names(x)))))  stop("var must be a variable in x ")
  #if (any(is.na(match(groep, names(x))))) stop("groep must be a variable in x ")
  if (is.null(groep)) groep = 1
  if (is.null(groeplab)) groeplab = groep
  gp = x[, groep]
  if (all(is.na(gp))) message("\nGroep bestaan uit NAs. Dis 'n slegte idee. Kies dalk 'n ander groep\n")
  lb = capitalize(gsub("_|\\.", " ", var))
  if (identical(x[, var], gp)) groep = 1
  if (length(stats) > 1) stats = "all"
  if (is.na(match(qs_text, names(qs)))) stop("qs_text is nie 'n kolomnaam in qs nie")
  if (is.na(match(qs_name, names(qs)))) stop("qs_name is nie 'n kolomnaam in qs nie")
  
  chi2 <- NULL
  if (verbose) message("Groep: ", groep, "\nlength(unique(x[, var]): ", length(unique(x[, var])), "\nncat :", ncat)
  
  # Bou die vraagsin --------------------------------------------------------
  
  idx <- na.omit(match(var, qs[, qs_name]))
  if (length(idx) != 0) vraagteks = as.character(qs[idx, qs_text])
  natab = table(is.na(x[, var]), exclude = NULL)["TRUE"]
  nobtab = table(is.na(x[, var]))["FALSE"]
  nna = ifelse(is.na(natab), 0, natab)
  vr = gsub("_|\\.", " ", var)
  vraagsin = ""
  if (nobtab != 0 & !is.na(nobtab)) {
    if (length(idx) != 0) {
      vraagsin = sprintf("The variable \\emph{%s} contains the responses to the question \\emph{%s} and contains %i %s with %i missing %s", 
                         vr, 
                         vraagteks, 
                         nobtab, 
                         ifelse(nobtab == 1, "observation", "observations"), 
                         nna, 
                         ifelse(nna == 1, "value.", "values."))
    }
    else {
      vraagsin <- sprintf("The variable %s contains contains %i %s and %i missing %s", 
                          vr, 
                          nobtab, 
                          ifelse(nobtab == 1, "observation", "observations"), 
                          nna, 
                          ifelse(nna == 1, "value.", "values."))
    }
    
    verwyssin <- sprintf("The summary results are shown in Table %s and Figure %s.", 
                         ifelse(md == TRUE, paste("*", vr, "*", sep = ""), paste("\\ref{tab:lb", vr, "}", sep = "")), 
                         ifelse(md == TRUE, paste("*", vr, "*", sep = ""), paste("\\emph{", vr, "}", sep = ""))
    )
    num = is.numeric(x[, var])
    fc = is.factor(x[, var])
    ch = is.character(x[, var])
    
    # Maak die diagram --------------------------------------------------------
    
    # Die geen groep geval  --------------------------------------------------------
    if (groep == 1) {
      if (debug) message("Groep: ", groep, "\nlength(unique(x[, var]) :", length(unique(x[, var])), "\nncat :", ncat)
      # Die numeriese geen group geval  --------------------------------------------------------
      if (num) {
        if (length(unique(x[, var])) > nnum){
          p1 <- qplot(data = melt(x[, var]), 
                      geom = "boxplot", 
                      xlab = var, 
                      x = var, 
                      y = value, 
                      main = wrapper(paste("Values of ", lb), width = wd)) +
            theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11, face = "bold"))
          
          p2 <- qplot(data = data.frame(idx = 1:nrow(x), vr = x[, var]), 
                      x = vr, fill = 1, 
                      geom = "density", 
                      main = wrapper(paste("Density of ", lb), width = wd),
                      alpha = I(0.2)) + 
            theme(legend.position = "none") + 
            theme(axis.text = element_text(size = 9), 
                  axis.title = element_text(size = 11, face = "bold"))
          
          p3 <- suppressWarnings(
            qplot(data = data.frame(idx = 1:nrow(x), vr = x[, var]), 
                  x = vr, 
                  fill = 1, 
                  geom = "histogram", 
                  xlab = var, 
                  main = wrapper(paste("Histogram of ", lb), width = wd), alpha = I(0.9)) + 
              theme(legend.position = "none") +
              theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11, face = "bold"))
          )
          
          if (druk == TRUE) 
            pdf(file = paste(drr, var, ".pdf", sep = ""), width = 12)
          
          pp = function(main = paste("Values and distribution of ", var)){
            grid.arrange(p1, p3, p2, 
                         nrow = 2, 
                         ncol = 2, 
                         layout_matrix = rbind(c(1, 2), c(3, 3)), 
                         top = main)
          }
        }
        
        if (druk == TRUE) 
          dev.off()
        if (var %in% names(x)) {
          vrs <- x[, var, drop = FALSE]
          tab <- function() {
            tableContinuous2(vars = vrs, 
                             cap = paste(lb," "), 
                             lab = paste("lb", vr, sep = ""), 
                             longtable = longt, 
                             caption.placement = "top", 
                             html = webify, 
                             comment = FALSE, 
                             nams = gsub("_", " ", var), 
                             print.results = FALSE, 
                             table.placement = "!h")
          }
        }
      }
      
      # Die diskrete geen group geval --------------------------------------------------------
      if (fc | ch) {
        # ons maak net 'n grafiek vir meer as ncat veanderlikkes
        if (length(unique(x[, var])) > ncat){
          p <- ggplot(data.frame(vr = x[, var]), 
                      aes(x = vr, fill = 1), alpha = I(1/2)) + 
            labs(x = lb, y = "count") + 
            geom_bar(position = "dodge") + 
            theme(legend.position = "none") + 
            ggtitle(wrapper(paste("Frequency distrubution of",var), width = 2 * wd)) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
            theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11, face = "bold"))
          
          pp = function() grid.arrange(p, nrow = 1)
          if (druk == TRUE)  ggsave(file = paste(drr, var, ".pdf", sep = ""), width = 12)
        }
        
        if (var %in% names(x)) {
          vrs <- x[, var, drop = FALSE]
          tab <- function() {
            tableNominal2(vars = vrs, 
                          cap = paste(lb, "", sep = ""), 
                          lab = paste("lb", vr, sep = ""), 
                          levSizeN = levSizeN, 
                          cumsum = FALSE, 
                          longtable = longt, 
                          caption.placement = "top", 
                          html = webify, 
                          varSizeN = varSizeN, 
                          comment = FALSE, 
                          nams = gsub("_", " ", var), 
                          print.results = TRUE)
          }
        }
        if (stats == "all") {
          idx_temp <- match(x = var, table = names(x), -1)
          if (idx_temp > 0) {
            if (!is.null(voorspellende_veranderlikes)){
              vidxx <- match(voorspellende_veranderlikes, names(x))
              vidxx <- vidxx[!is.na(vidxx)]
            } else {vidxx <- integer()}
            
            if (length(vidxx) > 0) {
              chi2 <- chi.2.eksp(data = x, out.idx = idx_temp, 
                                 min.length = 0, max.levels = 10, ver.idx = vidxx, 
                                 p.cutoff = 100, outdir = drr, out.df = TRUE, 
                                 verbose = FALSE, xlsx = FALSE, net.name = "chi2eksp")
              
              if (!is.null(chi2)) {
                for (c in 1:ncol(chi2)) {
                  if (is.numeric(chi2[, c])) {
                    chi2[, c] <- round(x = chi2[, c], 
                                       digits = chi.digits)
                  }
                }
                dropidxx <- match(c("warnings", "outcome"), 
                                  table = names(chi2), nomatch = NA)
                dropidxx <- dropidxx[!is.na(dropidxx)]
                if (length(dropidxx) > 0) 
                  chi2 <- chi2[, -dropidxx]
                print(kable(chi2, format = "pandoc", 
                            digits = 5), comment = FALSE)
              }
            }
          }
        }
      }
    }
    # Die groep geval  --------------------------------------------------------
    if (groep != 1) {
      # Die numeriese groep geval -----------------------------------------------
      if (num) {
        if (length(unique(x[, var])) > nnum){
          p1 <- qplot(data = melt(data.frame(var = x[ ,var], group = gp)), 
                      geom = "boxplot", 
                      xlab = groeplab, 
                      x = group, 
                      y = value, 
                      main = wrapper(paste("Values of ", lb, " by ", groeplab), width = wd)) + 
            theme(legend.position = "bottom") + 
            theme(axis.text = element_text(size = 9), 
                  axis.title = element_text(size = 11, face = "bold"))
          
          p2 <- qplot(data = data.frame(var = x[, var], group = gp), 
                      x = var, 
                      geom = "density", 
                      xlab = var, 
                      fill = group, 
                      group = group, 
                      main = wrapper(paste("Density of ",  lb, " by ", groeplab), 
                                     width = wd), alpha = I(0.2)) + 
            theme(legend.position = "bottom") + 
            theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11, face = "bold"))
          
          if (druk == TRUE)  pdf(file = paste(drr, var, ".pdf", sep = ""), width = 12)
          
          pp = function() grid.arrange(p1, p2, nrow = 1, 
                                       top = wrapper(sprintf("Distribution of the variable %s", 
                                                             var), width = 2 * wd))
          if (druk == TRUE) dev.off()
        }
        
        if (var %in% names(x)) {
          vrs <- x[, var, drop = FALSE]
          tab <- function() {
            tableContinuous2(vars = vrs, 
                             group = gp, 
                             cap = paste(lb, "", sep = ""), 
                             lab = paste("lb",   vr, sep = ""), 
                             longtable = longt, 
                             html = webify, 
                             caption.placement = "top", 
                             comment = FALSE, 
                             nams = gsub("_", " ", var), 
                             print.results = FALSE)
          }
        }
      }
      if (fc | ch) {
        # Die diskrete groep geval -----------------------------------------------
        if (length(unique(x[, var])) > ncat){
          p <- qplot(data = data.frame(var = x[, var],  group = gp), x = var, geom = "bar", xlab = lb, 
                     facets = group ~ .,
                     main = paste(lb, "by", groeplab), fill = 1) + 
            theme(legend.position = "none") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 0)) + 
            theme(axis.text = element_text(size = 9),axis.title = element_text(size = 11, face = "bold"))
          
          pp = function() grid.arrange(p, nrow = 1)
          if (druk == TRUE)  ggsave(p, file = paste(drr, var, ".pdf", sep = ""), width = 12)
        }
        
        if (var %in% names(x)) {
          vrs <- x[, var, drop = FALSE]
          if (debug) assign("vrs", vrs, envir = .GlobalEnv)
          if (verbose) message("Diskrete groep geval: ons gaan nou tab maak
                               \nvar is ", var, "dim x is ", dim(x)[1], " by ",  dim(x)[2],
                               "\nvrs is ", str(vrs))
          tab <- function() {
            tableNominal2(vars = vrs, 
                          group = gp, 
                          cap = paste(lb, "", sep = ""), 
                          cumsum = FALSE, 
                          lab = paste("lb", vr, sep = ""), 
                          html = webify, 
                          longtable = longt, 
                          caption.placement = "top", 
                          varSizeN = varSizeN, 
                          levSizeN = levSizeN, 
                          comment = FALSE,
                          print.results = TRUE
            )
          }
        }
        
        if (stats == "all") {
          idx_temp <- match(var, table = names(x), -1)
          if (idx_temp > 0) {
            vidxx <- match(voorspellende_veranderlikes, 
                           names(x))
            vidxx <- vidxx[!is.na(vidxx)]
            if (debug == TRUE) 
              assign("voorspellende_veranderlikes", voorspellende_veranderlikes, 
                     envir = .GlobalEnv)
            if (debug == TRUE) 
              assign("x", x, envir = .GlobalEnv)
            if (debug == TRUE) 
              assign("idx_temp", idx_temp, envir = .GlobalEnv)
            if (length(vidxx) > 0) {
              chi2 <- chi.2.eksp(data = x, out.idx = idx_temp, 
                                 min.length = 0, max.levels = 10, ver.idx = vidxx, 
                                 p.cutoff = 0.05, outdir = drr, out.df = TRUE, 
                                 verbose = FALSE, xlsx = FALSE, net.name = "chi2eksp")
              if (!is.null(chi2)) {
                for (c in 1:ncol(chi2)) {
                  if (is.numeric(chi2[, c])) {
                    chi2[, c] <- round(x = chi2[, c], 
                                       digits = 5)
                  }
                }
                dropidxx <- match(c("warnings", "outcome"), 
                                  table = names(chi2), nomatch = NA)
                dropidxx <- dropidxx[!is.na(dropidxx)]
                if (length(dropidxx) > 0) 
                  chi2 <- chi2[, -dropidxx]
                print(kable(x = chi2, format = "pandoc", 
                            digits = chi.digits), comment = FALSE)
              }
            }
          }
        }
      }
    }
  }
  if (verbose)  message(var)
  if (out) {
    do.call("cat", 
            list(
              sprintf("%s", 
                      ifelse(md == TRUE, paste(" \n## ", lb, "\n", sep = ""), 
                             paste("\\subsection{",  lb, "}", sep = ""))), "\n", 
              ifelse(exists("vraagsin"), vraagsin, ""), "\n", 
              ifelse(exists("verwyssin"),  verwyssin, ""), "\n", 
              if (exists("tab", mode = "function")) tab() else "", "\n"))
    
    #if (exists("tab", mode = "function")) tab()
    if (exists("pp", mode = "function")) pp()
    cat("\\clearpage")
  }
}



# hulp funksie
wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}

