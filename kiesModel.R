# Generic model selection

# na aanleiding van http://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome

# x moet 'n matrik wees
# x <- data.frame(age = c(4,8,7,12,6,9,10,14,7),
#                 gender = c(1,   0,    1,    1,    1,    0,    1,    0,    0),
#                 bmi_p = c(0.86, 0.45, 0.99, 0.84, 0.85, 0.67, 0.91, 0.29, 0.88),
#                 m_edu = c(0,1,1,2,2,3,2,0,1),
#                 p_edu = as.factor(c(0,2,2,2,2,3,2,0,0)),
#                 f_color = c("blue", "blue", "yellow", "red", "red", "yellow", "yellow", "red", "yellow"),
#                 asthma = c(1,0,1,1,1,0,1,0,1))


# f_color <- as.factor(f_color)
# xfactors <- model.matrix(asthma ~ gender + m_edu + p_edu + f_color)[,-1]
# xz <- data.matrix(data.frame(age, bmi_p, xfactors))

# dink aan group lasso
#note alpha =1 for lasso only and can blend with ridge penalty down to alpha=0 ridge only

kiesModel <- function(x,
                      outcomename = "asthma",
                      type = "binomial",
                      alph = 1, # i.e. lasso 0 = ridge
                      max.factor.levels = 7,
                      data.capture.thresh = 0.95,
                      stratifier = list(NULL, "country")[[1]], 
                      form.uit = TRUE,
                      verbose = FALSE, debug = FALSE
                      ){
  x.orig = x 
  if(any(is.data.frame(x), is.matrix(x)) == FALSE) stop("x must be a matrix or data.frame")
  if(is.na(match(outcomename, colnames(x)))) stop("outcomename must be in x ")

  if (!require(glmnet)) install.packages("glmnet", dependencies = TRUE)
  message("type is: ", type)
  classlist = sapply(x, class)
  chars = which(classlist == "character")
  if (length(chars) > 0){
    if (verbose == TRUE) message("verander ", length(chars), " karakter vektore in faktore")
    for (i in 1:length(chars)){
      x[,chars[i]] <- as.factor(x[,chars[i]])
    }
  }
  # maak 0 en 1 veranderlikkes ook factor
  to.factor.idx = which(sapply(x, function(z) nlevels(as.factor(z))) == 2)
  if (verbose == TRUE) message("to.factor.idx is ", length(to.factor.idx), " lank")
  if (length(to.factor.idx > 0)) for (i in 1:length(to.factor.idx)){
    x[,to.factor.idx[i]] <- as.factor(x[,to.factor.idx[i]])
  }
  
  fctrs = which(sapply(x, is.factor)) ; if (verbose == TRUE) message(length(fctrs) , " veranderlikkes is faktore")
  if (length(fctrs) > 0){
    fctrs = fctrs[sapply(x[,fctrs], nlevels) > 1 ] ; if (verbose == TRUE) message(length(fctrs) , " veranderlikkes is faktore met >2 vlakke")
    fctrs = fctrs[sapply(x[,fctrs], nlevels) <= max.factor.levels ] ; if (verbose == TRUE) message(length(fctrs) , " veranderlikkes is faktore met < ", max.factor.levels ,"vlakke")
    fctrs = fctrs[sapply(x[,fctrs], FUN = function(x) prop.table(table(is.na(x)))["FALSE"] > data.capture.thresh)] ; if (verbose == TRUE) message(length(fctrs) , " veranderlikkes is faktore met > ", round(data.capture.thresh * 100)  ,"% data")
  }
  if (! is.null(stratifier)){
    if (is.na(match(stratifier, names(x)))) stop("Die veranderlike naam ", stratifier, " kom nie in x voor nie")
    # laat val alle veranderlikkes wat nie onderskeidend is tussen die strata
    idx = sapply(names(x[,fctrs]), function(i){
      # daar moet meer as een kategorie vir elke stratum wees
      tab = table(x[,i], x[,stratifier])
      test = all(apply(tab, 2, function(t) length(which(t != 0))) > 1)
      test
    })
    idx[stratifier] = TRUE 
    fctrs = fctrs[idx]
    if (verbose == TRUE) message("Ons stratifiseer volgens ", stratifier, ": Dus word net onderskeidende veranderlikkes gehou.\nDaar is nou ", length(fctrs))
    
  }
#   if (length(fctrs) > 0 ){
#     fctrs2 = fctrs[names(fctrs) %in% outcomename == FALSE] ; if (verbose == TRUE) message(length(fctrs) , " veranderlikkes is faktore nadat ", outcomename,  " verwyder is")
#     if (debug == TRUE) assign("fctrs2",fctrs2, envir = .GlobalEnv)
#     frm = as.formula(paste(outcomename , "~ ", paste(names(fctrs2), collapse = " + ")))
#     xfactors = model.matrix(frm, data = x)[,-1]
#     #fctrs = fctrs[outcomename %in% names(fctrs) == FALSE]
#   }
  
  ## Numerical 
  xnums = x[ ,sapply(x, is.numeric)]
  if (verbose == TRUE) message("Numereriese veranderlikkes se dimensies is ", paste(dim(xnums), collapse = " by "))
  xnums = xnums[ ,sapply(xnums, FUN = function(x) mean(x, na.rm=TRUE) != 0)]
  if (verbose == TRUE) message("Numereriese veranderlikkes met gemiddeld 0 vewyder\n dimensies van xnums nou ", paste(dim(xnums), collapse = " by "))

  if (length(fctrs) == 0){
    x <- xnums 
    if (verbose == TRUE) message("x bestaan net uit numeriese veranderlikkes en het dimensies van ", paste(dim(x), collapse = " by "))
    } else {
      x <- x[,c(names(xnums), names(fctrs))]
      if (verbose == TRUE) message("x bestaan uit numeriese en faktor veranderlikkes en het dimensies van ", paste(dim(x), collapse = " by "))
    }
  if (verbose == TRUE) message(str(x))
  
  if (any(z <- sapply(x, is.character))){
    if (verbose == TRUE) message("Dit lyk of daar nog karakter veanderlikkes is. Ek gaan hulle verwyder....\n Voetsek!!!!")
    idx = which(sapply(x, is.character) == TRUE)
    for (i in 1:length(idx)){
      x[,idx[i]] <- as.factor(x[,idx[i]])
    }
    if (verbose == TRUE) message("Mooi, nou's hulle weg")
  }
  
  x = data.matrix(x[,which(apply(x, 2, function(x) all(is.na(x))) == FALSE)])
  if (verbose  == TRUE) message("Ek verwyder leë rye. Dimensies van x is nou ", paste(dim(x), collapse = " by "))
  x = x[complete.cases(x), ]
  if (verbose  == TRUE) message("Ek hou net volledige rye. Dimensies van x is nou ", paste(dim(x), collapse = " by "))
  y = data.matrix(x[,colnames(x) %in% outcomename])
  if (verbose  == TRUE) message("Ek skep Y, sy dimensies is ", paste(dim(y), collapse = " by "))
  if (debug == TRUE) assign("Y1", y, envir = .GlobalEnv); message("Y1 uitgeskryf na .GlobalEnv")
  x = x[,colnames(x) %in% outcomename == FALSE]
  if (verbose  == TRUE) message("Ek verwyder ", outcomename, " uit x. Sy dimensies is nou ", paste(dim(x), collapse = " by "))
  if (debug == TRUE) assign("x1", x, envir = .GlobalEnv)
  
#   dropidx = which(is.na(Y))
#   if (length(dropidx) > 0){
#     y = Y[-dropidx, ]
#     x = x[-dropidx, ]
#     if (verbose  == TRUE) message("Daar was NA waardes in Y, ons moes hulle uitlaat. \nY is nou ", 
#                                   paste(dim(Y), collapse = " by "), " en x is ", paste(dim(x), collapse = " by "))
#   }
  
  X = data.matrix(x); if (verbose == TRUE) message("Die data.matrix X se dimensies is ", paste(dim(X), collapse = " by " ))
  Y = data.matrix(y) ; if (verbose == TRUE) message("Die data.matrix Y se dimensies is ", paste(dim(Y), collapse = " by " ))
  if (debug == TRUE) {assign("X2", X, envir = .GlobalEnv); message("Die data.matrix X2 word uitgeskryf na .GlobalEnv")}
  if (debug == TRUE) {assign("Y2", Y, envir = .GlobalEnv); message("Die data.matrix Y2 word uitgeskryf na .GlobalEnv")}

  glmmod <- glmnet(x = X, y = Y,alpha = alph,family = type)

  #plot variable coefficients vs. shrinkage parameter lambda.
  par(mfcol = c(1,2))
  plot(glmmod, xvar="lambda")
  grid()
  cv.glmmod <- cv.glmnet(x = X, y=as.numeric(Y),alpha = alph)
  plot(cv.glmmod)
  best_lambda <- cv.glmmod$lambda.min
  par(mfrow = c(1,1))
  glmmodbest<-glmnet(x = X, y = Y,alpha = alph,family = type, lambda = best_lambda)
  glmmodbest
}

printglmmodbest <- function(data = persons.ex, glmmodbest, outcomename = "symptoms._thirtydays", form.uit = FALSE, verbose = FALSE){
  if (!require(knitr)) install.packages("knitr", dependencies = TRUE)
  nms  = names(glmmodbest$beta[,1])
  nums = as.numeric(glmmodbest$beta[,1])
  sig.coefs = nms[glmmodbest$beta[,1] != 0]
  sig.coef.vals = nums[glmmodbest$beta[,1] != 0]
  disp = data.frame(sig.coefs, sig.coef.vals)
  if (any(sapply(data, class)=="factor")) factor.vars = which(sapply(data, class)[match(sig.coefs, names(data))] == "factor")
  sig.coefs[factor.vars] <- gsub("([[:print:]]*)", "factor\\(\\1\\)", sig.coefs[factor.vars] )
  if (verbose == TRUE) message("Betekenisvolle koëfisiente: ", paste(sig.coefs, " "))
  lm.frm = as.formula(paste(outcomename, " ~ ", paste(sig.coefs, collapse = " + ")))
  if (form.uit == TRUE) {
    assign("lm.frm", lm.frm, envir = .GlobalEnv); message("Ek stryf lm.frm uit want jy het gevra")
  }
  kable(disp)
}


# toets met tea <- read.table("http://factominer.free.fr/book/tea.csv", sep=";", header = TRUE)

kiesKlassifikasieModelle <- function(x,
                        outcomename = "asthma",
                        types = c("binomial","LDA"),
                        verbose = FALSE){
  require(caret)
  require(opm)
}

# classes <- which(lapply(getModelInfo(), function(x) !is.na(match("Classification", x$type))) == TRUE)

