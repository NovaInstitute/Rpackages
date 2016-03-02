# multi_table

multi_table <- function(vars = vars, qs = NULL,
                        multicol = "question.type", multiname = "M",
                        question.name = "question.name", maxlevels = 20,
                        verbose = FALSE, ...){
  if (verbose == TRUE) message("\n\nJy is nou in multi_table")
  if (is.null(qs)) stop("qs missing")
  dots <- list(...)
  # Multi - opsie is per definisie diskreet
  # maak numeriese items met twee vlakke diskreet (tipies 0,1 vir ja nee)
  if (verbose == TRUE) message("names vars ", paste(names(vars), " "))
  to.factor.idx = which(sapply(vars, function(x) nlevels(as.factor(x))) == 2)
  if (verbose == TRUE) message("to.factor.idx ", paste(to.factor.idx, " "), "\n en sy lengte is ", length(to.factor.idx))
  if (length(to.factor.idx > 0)) for (i in 1:length(to.factor.idx)){
    if (verbose == TRUE) message("iterasie ", i, " met ", names(vars)[to.factor.idx[i]])
    vars[,to.factor.idx[i]] <- as.factor(vars[,to.factor.idx[i]])
  }
  if (verbose == TRUE) message("names(vars) is ",paste(names(vars), " "))
  if (verbose == TRUE) message("str(vars) is ",paste(str(vars), " "))
  # if there are no categorical variables there is no sense in continuing
  if(verbose == TRUE) message("Kom ons toets of daar diskrete veranderlikes is ")
  if (any_discrete(vars)){ if(verbose == TRUE) message("daar is diskrete veranderlikes ") # exit silently when there is discrete variables
    ## kies die veraderlikkes uit wat multi-keuse is
    # eers die vrae
    m.idx <- which(qs[, multicol] == multiname)
    #if (verbose == TRUE) message("qs is ", paste(str(qs), " "))
    if (verbose == TRUE) message("dim(qs) is ",paste(dim(qs), collapse = " by "), "\nmultiname is ", multiname, ", multicol is ", multicol, "\nm.idx is ", paste(m.idx, " "))
    if (length(m.idx) > 0){ if(verbose == TRUE) message("daar is multi-opsie vrae")# exit silently when there is no multi-option questions
      # implementeer die groep veranderlikke
      if(!is.na(match("groupvar", names(dots)))){
        if (!is.na(match(dots[["groupvar"]], names(vars)))) {
          groep =  vars[,dots[["groupvar"]]]
        } else {groep = 1}
      } else {groep = 1}
    }
      vars <- only_discrete(vars)
    if (verbose == TRUE) message("dim vars is ", paste(dim(vars), collapse = " by "))
    # selekteer die multi-opsie vrae
      multi.vars <- lapply(qs[m.idx, question.name], function(x) grep(x, names(vars)))
    if (verbose == TRUE) message("names(qs) is ", paste(names(qs), " "),"\nnames(vars) is ", paste(names(vars), " "),"\nmulti.vars is ", multi.vars)
      if (all(sapply(multi.vars , function(x) all(is.na(x)))) == FALSE){ # as multi-vars net NA is het dit gee sin om voort te gaan nie
        multi.vars = multi.vars[sapply(multi.vars, function(x) !all(is.na(x)))]
        varlist = lapply(na.omit(multi.vars), function(x) vars[,x])
        if (length(levels(as.factor(groep))) > 1) varlist = lapply(varlist, function(x) cbind(x, groep))
          varlist = lapply(varlist, function(x){
            z = data.frame(lapply(x, as.factor))
            z = z[, lapply(z, nlevels) <= maxlevels, drop=FALSE]
            dropzero = which(sapply(z, function(x) length(levels(x))) == 0)
            if (length(dropzero) > 0) z = z[ ,-dropzero, drop=FALSE]
            names(z) = gsub("_", " ", names(z))
            z})
        nie.leeg = lapply(varlist, ncol) != 0
        varlist = varlist[nie.leeg]

        if (!all(sapply(varlist, is.null))){ # Ons gaan die tabel maak. As varlist leeg is maak dit geen sin om aan te gaan nie
          require(reporttools)
          if (verbose == TRUE) message("Hier gaan discrete_table")
          for (i in 1:length(varlist)){
            tableNominal(vars = varlist[[i]],
                         cumsum = FALSE,
                         group = ifelse(levels(as.factor(groep)) == c("1"), NA, groep),
                         cap = ifelse(!is.na(match("cap", names(dots))), dots[["cap"]], ""),
                         lab = ifelse(!is.na(match("lab", names(dots))), dots[["lab"]], ""),
                         caption.placement = "top", table.placement = "!")
          }
          if (verbose == TRUE) message("Drukwerk is klaar")
        }
      }
    }
  }

