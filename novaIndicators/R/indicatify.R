# Hulpfunksie
CI <- function (x, ci = 0.95, na.rm=TRUE, as.df = TRUE) {
 a <- mean(x, na.rm = na.rm)
 s <- sd(x, na.rm = na.rm)
 n <- ifelse(na.rm, na.omit(length(x)), length(x))
 error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
 if (as.df) {
  return(data.frame(PointEst = a, 
                    Lower = a - error, 
                    Upper = a + error))
 } else {
  return(c(PointEst = a, Lower = a - error, Upper = a + error))
 }
}

binCI <- function(x, 
                  vr, 
                  opsie, 
                  a = 0.05, 
                  na.rm = TRUE, 
                  verbose = FALSE, 
                  N = NULL, # for finite population correction
                  asDf = TRUE){
 
 n <- 0
 s <- 0
 if (!na.rm) {
  tab <- table(x[,vr], exclude = NULL)
 } else {
  tab <- table(x[,vr])
 }
 
 if (length(names(tab)) > 0) {
  if (opsie %in% names(tab)) {
   s <- tab[opsie]
  }
  
  n <- sum(tab)
  
  if (is.na(n)) {
   warning("No values to table in x[,vr].")
   n <- 0
  }
  
 } else {
  warning("x[,vr] is empty.")
 }
 
 if (verbose == TRUE) {message("n ", n)}
 if (require(Hmisc) == FALSE) {install.packages("Hmisc", dependencies = TRUE)}
 
 res <- binconf(x = s, alpha = a, n)
 
 # apply finite population correction
 if (!is.null(N)) {
  if (is.numeric(N)) {
   fpcf <- sqrt((N-n)/(N-1))
   res[, "Lower"] <- res[, "PointEst"] - ((res[, "PointEst"] - res[, "Lower"]) * fpcf)
   res[, "Upper"] <- res[, "PointEst"] + ((res[, "Upper"] - res[, "PointEst"]) * fpcf)
  }
 }
 
 if (asDf) {
  return(data.frame(round(res * 100,2)))
 }
 return(round(res * 100,2))
}

# 'pop.v' is used to get 'N' for finite population corrections
binCI.by <- function(x, 
                     vrr, 
                     opsie, 
                     by,  
                     na.rm = TRUE,
                     prop = TRUE, 
                     alpha = 0.05,
                     verbose = FALSE,
                     k = FALSE,
                     pop.v = NULL) {
 
 if (is.na(match(by, names(x)))) stop(by, " is not a columnname in x")
 if (is.na(match(vrr, names(x)))) stop(vrr, " is not a columnname in x")
 
 if (!prop) {
  if (!na.rm) {
   tab = table(x[,vrr], x[ ,by], exclude = NULL)
  } else {
   tab = table(x[,vrr], x[ ,by])
  }
  tab = apply(tab, 2, function(t) {
   binconf(x = t[[opsie]], n = sum(t), alpha = get("alpha")) * (sum(t))
  })
  rownames(tab) <- c("PointEst", "Lower", "Upper")    
  if (k == TRUE) {kable(tab); return(invisible(NULL))} else {return(tab)}    
 }  
 
 sl = split(x, x[,by])
 res = lapply(1:length(sl), function(i) {
  
  if (!is.null(pop.v)) {
   N <- pop.v[1,names(sl)[i]]
  } else {N <- NULL}    
  
  binCI(x = sl[[i]], 
        a = alpha, 
        N = N,
        vr = vrr,
        opsie = opsie)
 })
 res = t(do.call("rbind", res))
 
 rownames(res) <- c("PointEst", "Lower", "Upper")
 colnames(res) <- names(sl)
 
 if (k == TRUE) {kable(res)} else {return(res)}
}


# merge this and binCI.by eventually into one function
# 'pop.v' is used to get 'N' for finite population corrections
binCI.by2 <- function(x, 
                      vrr, 
                      opsie, 
                      by, 
                      na.rm = TRUE,
                      alpha = 0.05,
                      asPerc = TRUE,
                      pop.v = NULL) {
 
 if (is.na(match(by, names(x)))) stop(by, " is not a columnname in x")
 if (is.na(match(vrr, names(x)))) stop(vrr, " is not a columnname in x")
 
 if (!na.rm) {
  tab <- table(x[ ,by], x[,vrr], exclude = NULL)
  n <- table(x[,by], exclude = NULL)
 } else {
  tab <- table(x[ ,by], x[,vrr])
  n <- table(x[,by])
 }
 
 if (!all(names(n) == rownames(tab))) {
  n <- n[rownames(tab)]
 }
 
 if (opsie %in% x[[vrr]]) {
  res <- binconf(x = as.numeric(tab[,opsie]), 
                 n = as.numeric(n), 
                 alpha = alpha)  
  res <- data.frame(stringsAsFactors = FALSE, res)
  if (asPerc) {
   res[,c("PointEst","Lower","Upper")] <- round(res[,c("PointEst", "Lower","Upper")] * 100, 2)
  }    
  
 } else {
  res <- data.frame(stringsAsFactors = FALSE,
                    matrix(nrow = length(names(n)), ncol = 3, 
                           data = NaN, 
                           dimnames = list(NULL, c("PointEst", 
                                                   "Lower", 
                                                   "Upper"))))   
 }
 
 # if (!is.null(pop.v)) {
 #   if (all(names(n) %in% names(pop.v))) {
 #     pop.v <- pop.v[names(n)]
 #     fpcf <- sqrt((pop.v - n)/(pop.v - 1))
 #     res[, "Lower"] <- res[, "PointEst"] - ((res[, "PointEst"] - res[, "Lower"]) * fpcf)
 #     res[, "Upper"] <- res[, "PointEst"] + ((res[, "Upper"] - res[, "PointEst"]) * fpcf)
 #   } else {
 #     stop("Names of 'pop.v' do not agree with groups in variable 'by'.")
 #   }
 # }  
 
 res[[by]] <- names(n)
 res <- res[,c(by, setdiff(names(res), by))]
 
 return(res)
}


binCI.groups <- function(x, 
                         by, 
                         na.rm = TRUE,
                         alpha = 0.05,
                         Npop = NULL, # for finite population correction
                         asPerc = TRUE,
                         verbose = FALSE) {
 
 if (is.na(match(by, names(x)))) stop(by, " is not a columnname in x")
 
 if (!na.rm) {
  tab = table(x[ ,by], exclude = NULL)
 } else {
  tab = table(x[ ,by])
 }
 s <- as.numeric(tab)
 n <- sum(s)
 
 res <- binconf(x = s, alpha = alpha, n)
 
 if (!is.null(Npop)) {
  fpcf <- sqrt((Npop-n)/(Npop-1))
  res[, "Lower"] <- res[, "PointEst"] - ((res[, "PointEst"] - res[, "Lower"]) * fpcf)
  res[, "Upper"] <- res[, "PointEst"] + ((res[, "Upper"] - res[, "PointEst"]) * fpcf)
 }
 
 res <- data.frame(stringsAsFactors = FALSE, res)
 
 if (asPerc) {
  res <- sapply(X = res, FUN = function(m) {round(m*100, 2)})
  res <- data.frame(stringsAsFactors = FALSE, res)
 }
 
 res[[by]]<- names(tab)
 res <- res[,c(by, setdiff(names(res), by))]
 
 return(res)
}


mulCI<- function(x, 
                 vr, 
                 a = 0.05, 
                 na.rm=TRUE, 
                 opsie = NULL, 
                 verbose=FALSE){
 
 if (is.null(opsie)){
  if (!na.rm) {
   tab = table(x[,vr], exclude = NULL) 
  } else {
   tab = table(x[,vr]) 
  }
 } else {
  stop("")
 }
 
 if (verbose == TRUE) message("tab ", paste(tab, " "))
 if (require(MultinomialCI) == FALSE) {
  install.packages("MultinomialCI", dependencies = TRUE)
 }
 MultinomialCI::multinomialCI(as.vector(tab), alpha = a)
}


sumfun <- function(x = des, 
                   tp = "Type",
                   vr = "energy_coal_consumption_wintercurrentkg",
                   a = 0.05,
                   debug = FALSE){
 
 if (require(dplyr) == FALSE) {
  install.packages("dplyr", dependencies = TRUE)
  message("ek laai dplyr")
 }
 if (require(lazyeval) == FALSE) {
  install.packages("lazyeval", dependencies = TRUE)
  message("ek laai lazyeval")
 }
 if (require(multcomp) == FALSE) {
  install.packages("multcomp", dependencies = TRUE)
  message("ek laai multcomp")
 }
 
 if (is.null(x)) {warning("Argument received for parameter x was NULL. Returned NULL."); return(NULL)}
 
 xx = x %>% 
  group_by_(tp) %>% 
  select_(vr) %>%
  summarise_(mn = interp(~mean(var, na.rm=TRUE),
                         var = as.name(vr)),
             st.dev = interp(~sd(var2, na.rm=TRUE), 
                             var2 = as.name(vr)),
             n = interp(~length(var3), 
                        var3 = as.name(vr))) %>%
  mutate(se = st.dev/sqrt(n),
         confint.upper = mn + se * qt(p = 1-(a/2), df = n-1),
         confint.lower = mn - se * qt(p = 1-(a/2), df = n-1),
         txt = paste(round(mn, 2), 
                     " (", 
                     round(confint.lower,2), 
                     ", ", 
                     round(confint.upper, 2), 
                     ")", 
                     sep="")
  )
 an = aov(as.formula(paste(vr, "~", tp)), data =x)
 
 if (debug == TRUE){
  assign("vr",vr, envir = .GlobalEnv)
  assign("x",x, envir = .GlobalEnv)
  assign("tp",tp, envir = .GlobalEnv)
  assign("an",an, envir = .GlobalEnv)
  assign("xx",xx, envir = .GlobalEnv)
 }
 
 K <- diag(length(coef(an)))[-1, , drop=FALSE]
 if (K[1] > 0) rownames(K) <- names(coef(an))[-1]
 m.c = glht(an, linfct =  K)
 xx$effect[2:nrow(xx)] = an$coef[2:nrow(xx)]
 xx$pvalue[2:nrow(xx)] = summary(m.c)[9]$test$pvalues[1:nrow(xx)-1]
 #names(xx)[match("mn", names(xx))] <- "mean"
 return(xx)
}

################ Thermal comfort ################

comfify <- function(df = houseTS,
                    inside = "avgIndoor",
                    outside ="temperature",
                    hour = "hod",
                    res = c("thermal_comfort", "delta", "comf_refs")[1],
                    com.mean = 18.9, 
                    com.se.e = 0.255, 
                    com.range = 3.5,
                    floor = 17.5, 
                    nightfloor = 16, 
                    ceiling = 29.5,
                    nighthour.start = 22, 
                    nighthour.finish = 06) {
 
 df[[hour]] <- as.numeric(df[[hour]])
 df[["comfort.ref"]] <- com.mean + com.se.e * df[[outside]]
 df[["comfort.ref.high"]] <- ifelse((df[["comfort.ref"]] + com.range) < ceiling,
                                    df[["comfort.ref"]] + com.range, ceiling)
 dayidx <- ifelse(df[[hour]]  > nighthour.start |
                   df[[hour]] < nighthour.finish, "night", "day")
 df[["comfort.ref.low"]] <- NA_real_
 df[dayidx == "night" ,"comfort.ref.low"] <- ifelse(test = (df[dayidx == "night", "comfort.ref"] - com.range) > nightfloor, 
                                                    yes = df[dayidx == "night", "comfort.ref"] - com.range,
                                                    no = nightfloor)
 df[dayidx == "day" ,"comfort.ref.low"] <- ifelse(test = (df[dayidx == "day", "comfort.ref"] - com.range) > floor,
                                                  yes = df[dayidx == "day", "comfort.ref"] - com.range, 
                                                  no = floor)
 
 if (res == "comf_refs") {
  return(df[c("comfort.ref.low", "comfort.ref.high")])
 }
 
 thermal.comfort <- ifelse(test = df[[inside]] < df[["comfort.ref.low"]], 
                           yes = "too.cold",
                           no = ifelse(test = df[[inside]] > df[["comfort.ref.high"]],
                                       yes = "too.hot", 
                                       no = "comfortable"))
 
 if (res == "thermal_comfort") {
  return(thermal.comfort)
 } else {
  delta <- ifelse(thermal.comfort == "too.hot",
                  (df[[inside]] - df[["comfort.ref.high"]]),
                  ifelse(thermal.comfort == "too.cold",
                         (df[[inside]] - df[["comfort.ref.low"]]),
                         ifelse(thermal.comfort == "comfortable", 0, NA)))
  return(delta)
 }
}



# -------------------------------------------------- #
# this is a wrapper for the comfify function that gives you
# the option to choose which part of the data set you want to
# include in your analysis,
# i.e. do you want to analyse only fire episodes - excluding NA
# and non-fire episodes -
# or do you want to analyse fire episodes including NAs,
# or non-fire episodes only...
selectFireEps = function(x = NULL,
                         cols = c("date", "yr", "mo", "yrday", "hod", "Type", 
                                  "prepost", "fire", "B","K","S","L", "temperature"),
                         fireOpt = c(0,1),
                         verbose = FALSE, na.rm = FALSE, returnAll = TRUE) {
 
 if (na.rm == FALSE) { fireOpt <- c(fireOpt, NA) }
 
 frs <- which(x$fire %in% fireOpt)
 subset <- x[frs, cols]
 
 if (verbose) {
  message(paste("In selectFireEps(...) - dim(subset) is: ",
                nrow(subset), sep = ""))}
 if (nrow(subset) < 1) {
  warning("Resulting subset had 0 rows. Returning NULL.",
          immediate. = TRUE); return(NULL)}
 
 subset$ave.indoor.temp <- apply(subset[, c("B", "K", "L")], 1, mean, na.rm = T)
 subset$thermal.comfort <- comfify(df = subset,
                                   inside = "ave.indoor.temp",
                                   outside = "temperature",
                                   hour = "hod",
                                   res = "thermal.comfort")
 subset$thermal.comfort.delta <- comfify(df = subset,
                                         "ave.indoor.temp",
                                         "temperature",
                                         "hod",
                                         res = "delta")
 
 if (returnAll) {return(subset)} else {
  return(subset[, c("thermal.comfort", "thermal.comfort.delta")])}
}

# -------------------------------------------------- #
indoor_ambient_delta <- function(df = NULL,
                                 hours = c(1:24),
                                 months = NULL,
                                 campSeason = NULL,
                                 years = c(2013, 2014, 2015),
                                 tempname = "temperature",
                                 fireOpt = c(0,1),
                                 na.rm = FALSE) {
 
 if (is.null(df)) df <- {print("In indoor_ambient_delta(...): no data frame received."); return(NULL)}
 if (is.null(months) & is.null(campSeason)) {
  campSeason <- c("winter")
  print("In indoor_ambient_delta(...): No args received for months or campSeason. Defaulting to campSeason = winter.")
 }
 if (na.rm == FALSE) {fireOpt <- c(fireOpt, NA)}
 
 df$hod <- as.numeric(df$hod)
 df$mo <- as.numeric(df$mo)
 df$yr <- as.numeric(df$yr)
 df$campaign.season <- as.character(df$campaign.season)
 
 df <- df[df$hod %in% hours, ]
 if (is.null(months)) {df <- df[df$campaign.season %in% campSeason, ]} else {df <- df[df$mo %in% months, ]}
 df <- df[df$yr %in% years, ]
 df <- df[df$fire %in% fireOpt, ]
 df <- df[, c("date", "house", "prepost", "Type", "B", "K", "L", "S", "fire", tempname, "d_outd_B", "d_outd_K", "d_outd_L", "avgIndoor", "d_outd_avgIndoor", "hod", "mo", "yr", "yrday", "campaign", "campaign.season")]
 
 if (nrow(df) > 0)
 {
  df_means_all <- data.frame(colMeans(df[, -match(c("date", "house", "prepost", "Type", "campaign", "campaign.season"), names(df))], na.rm = TRUE), stringsAsFactors = FALSE)
  rownames(df_means_all) <- names(df)[-match(c("date", "house", "prepost", "Type", "campaign", "campaign.season"), names(df))]
  df_summary_selected <- summaryBy(data = df, formula = .~hod + mo + yr, FUN = mean, na.rm = TRUE)
  return(list(df, df_means_all, df_summary_selected))
 }
 
 return(NULL)
}

# -------------------------------------------------- #

#'@param popNumbers A vector contianing the population sizes for each of the 
#'groups in groupVar
#'@param capt Name of the indicator / caption to be added to the workbook table

warning("NB! tans stel dit negatiewe CILs net na 0 indien allowNegCIL == FALSE. Daar
is egter 'n ordentlike manier om CIs te bereken wanneer negatiewe CILs nie
moontlik is nie. Vind by CJP uit wat daardie fix is en werk dit hier in!")
indicatorMaker <- function(dfData, 
                           indicVar, 
                           indicOption = "yes", 
                           groupVar = NULL, 
                           allowNegCIL = FALSE,
                           popNumbers = NULL,
                           capt = "",
                           layout = c("vertical", "horizontal")[1],
                           writeToWB = FALSE,
                           sheetNm = capt,
                           startRow = 2,
                           assignToEnv = FALSE,
                           out.df.name = NULL,
                           returnRes = TRUE) {
 
 if (is.null(groupVar)) {
  dfData$grpx <- "all"
  groupVar <- "grpx"
 }
 
 df <- as.data.frame(binCI.by(x = dfData,
                              by = groupVar,
                              vrr = indicVar,
                              opsie = indicOption,
                              pop.v = popNumbers))
 ridx <- nrow(df)+1
 nn <- table(dfData[[groupVar]])
 for (grpnm in names(nn)) {
  df[ridx, grpnm] <- nn[[grpnm]]
 }
 rownames(df)[ridx] <- "n"
 
 df$stat <- rownames(df)
 df <- move.col(df = df, colName = "stat", colIdx = 1)
 rownames(df) <- NULL
 
 if (!allowNegCIL) {
  df <- melt(data = df, id.vars = "stat")
  idxx <- which(df$stat == "Lower" & df$value < 0)
  if (length(idxx) > 0) {df$value[idxx] <- 0}
  df <- dcast(data = df, formula = stat ~ variable)
  df <- df[match(x = c("PointEst", "Lower", "Upper", "n"), table = df$stat),]
 }
 
 if (layout == "horizontal") {
  df <- dcast(data = df, formula = .~stat)
  df <- df[,c("PointEst", "Lower", "Upper", "n")]
 } else {
  df <- rename.vars(data = df, from = "stat", to = "", info = FALSE)
 }
 
 if (writeToWB) {
  if (!(sheetNm %in% names(tempWorkbook$worksheets))) {
   addWorksheet(wb = tempWorkbook, sheetName = sheetNm)
  }
  iList <- list(df)
  names(iList) <- ifelse(!is.null(out.df.name), out.df.name, format_char(capt))
  tempWorkbook <- updateWorkbook(dfList = iList, 
                                 wb = tempWorkbook, 
                                 sn = sheetNm, 
                                 cn = capt, 
                                 rowNames = TRUE,
                                 startRow = startRow)
 }
 
 if (assignToEnv) {
  if (is.null(out.df.name)) {
   out.df.name <- fixname(capt)
  }
  assign(x = out.df.name, value = df, envir = .GlobalEnv)
 }
 
 if (returnRes) {return(df)} else {return(invisible(0))}
}

# for non-numeric questions/variables (e.g. 'yes'/'no' questions or 'house_type' var) 
warning("TODO: pass 'N' to binCI... functions for finite population corrections.")
indicatorMaker2 <- function(dfData, 
                            indicVar = NA_character_, 
                            indicOption = "yes", 
                            groupVar = NA_character_, 
                            popSize = NA_integer_,
                            allowNegCIL = FALSE) {
 
 if (is.na(indicVar) & is.na(groupVar)) {
  stop("Nothing specified for either 'indicVar' or 'groupVar'.")
 }    
 
 df <- NULL
 
 if (is.na(indicVar)) {
  
  df1 <- binCI.groups(x = dfData, by = groupVar, asPerc = TRUE)
  
  theCode <- paste("dplyr::summarise(dplyr::group_by(dfData, ", 
                   paste(groupVar, sep = ", ", collapse = ", "),
                   "), n = dplyr::n())", sep = "")
  df2 <- eval(parse(text = theCode))
  
  df <- merge.data.frame(x = df1, y = df2, by = groupVar, all = TRUE)
  rm(df1,df2)
 }
 
 if (is.na(groupVar)) {
  df <- binCI(x = dfData, vr = indicVar, opsie = indicOption, asDf = TRUE)
  df$n <- nrow(dfData[which(!is.na(dfData[[indicVar]])),]) # OR n of the option?? No, n of the data.
 }
 
 if (!is.na(indicVar) & !is.na(groupVar)) {
  
  df1 <- binCI.by2(x = dfData, vrr = indicVar, opsie = indicOption, by = groupVar, asPerc = TRUE)
  
  theCode <- paste("dplyr::summarise(dplyr::group_by(dfData, ", 
                   paste(groupVar, sep = ", ", collapse = ", "),
                   "), n = dplyr::n())", sep = "")
  df2 <- eval(parse(text = theCode))
  
  df <- merge.data.frame(x = df1, y = df2, by = groupVar, all = TRUE)
  rm(df1,df2)
 }
 
 if (is.nan(df$PointEst)) { return(df) }
 
 if (!allowNegCIL) {
  df$Lower[df$Lower < 0] <- 0 
 }

 # convert percentages to counts if user provided 'popSize'
 if (any(!is.na(popSize))) {
  if (!is.na(groupVar)) {
   popSize <- popSize[df[[groupVar]]] # make sure popSize is in the correct order
  }
  df$PointEst <- round((df$PointEst / 100) * popSize)
  df$Lower <- round((df$Lower / 100) * popSize)
  df$Upper <- round((df$Upper / 100) * popSize)
  df$n <- NULL     
 }  
 
 return(df)
}

# Calculates an aggregated, weighted indicator.
#'@param dfData A data frame containing the variable to be summarised plus at least
#'  one other variable (typically a variable indicating the population group) according 
#'  to which a weight will be assigned to each observation. There is no upper limit 
#'  on the number of weight variables.
#'@param indicVar Character. Name of the variable containing the observations to 
#'  be summarised.
#'@param indicOption Character. The response value in indicVar considered 'success' (in 
#'  binomial terms). Defaults to 'yes'.
#'@param dfWeights A data.frame containing a variable named 'weight' plus one variable
#'  for each of the weighting variables in dfData, containing the values of all possible
#'  responses to that particular variable. E.g. if the weighting variables in dfData are named
#'  'main_place' and 'subplace', with each main_place observation being one of 'waterval' 
#'  and 'Sefikile' and each subplace observation being one of 'ext_1', 'ext_2' and 'ext_3', then
#'  dfWeights should look like this:
#'    main_place  subplace  weight
#'    waterval    ext_1     0.3
#'    waterval    ext_2     0.1
#'    waterval    ext_3     0.2
#'    sefikile    ext_1     0.1
#'    sefikile    ext_2     0.1
#'    sefikile    ext_3     0.2
#'  If param 'outOpt' (see below) is set to 'count', dfWeights must also contain a variable
#'    named 'n' containing the counts (typically population sizes) to use for converting
#'    the percentages to counts.
#'@param outOpt Character. Indicates whether the result should be returned as percentages 
#'  or as counts. Defaults to percentage. Note that dfWeights must contain a variable named
#'  'n' if outOpt is set to 'count'.
#'@param alpha Numeric. Passed on to function Hmisc::binconf.
#'
indicatorMaker3 <- function(dfData, 
                            indicVar = NA_character_, 
                            indicOption = "yes", 
                            dfWeights,
                            outOpt = c("perc", "count")[1],
                            alpha = 0.05) {
 
 vnmsWeight <- setdiff(names(dfWeights), c("n", "weight"))
 
 if (!("weight" %in% names(dfWeights))) {
   stop("dfWeights must contain a variable named 'weight'.")
 }
 if (any(duplicated(dfWeights[,vnmsWeight]))) {
   stop("Duplicate entries for certian groups detected in dfWeights.")
 }
 if (sum(dfWeights$weight) != 1) {
   warning("sum(dfWeights$weight) != 1")
 }

 # assign weights to responses in data
 dfData <- merge.data.frame(x = dfData, 
                            y = dfWeights[,setdiff(names(dfWeights), "n")], 
                            by = vnmsWeight, 
                            all.x = TRUE)
 
 # calculate point estimate and conf intervals for each weight group
 dfSumm <- as.data.frame(
  eval(parse(text = sprintf("dplyr::summarise(dplyr::group_by(dfData, %s), 
                            n_succ =length(which(%s == '%s')), 
                            n_obs = length(which(!is.na(%s))))", 
                            paste(vnmsWeight, collapse = ","),
                            indicVar, 
                            indicOption, 
                            indicVar))))
 res <- binconf(x = dfSumm$n_succ, n = dfSumm$n_obs, alpha = 0.05, return.df = TRUE)
 res[,vnmsWeight] <- dfSumm[,vnmsWeight]
 res <- merge.data.frame(x = res, y = dfWeights, by = vnmsWeight, all = TRUE)
 res$n <- NULL
 if (any(is.na(res$PointEst))) {
   warning(sprintf("%d weight group(s) detected with no observations in dfData.",
                   length(which(is.na(res$PointEst)))))
 }
 
 # apply weights
 res$PointEst <- res$PointEst * res$weight
 res$Lower <- res$Lower * res$weight
 res$Upper <- res$Upper * res$weight
 res <- data.frame(
  PointEst = sum(res$PointEst),
  Lower = sum(res$Lower),
  Upper = sum(res$Upper))
 
 # add n responses
 res$n <- length(na.omit(dfData[[indicVar]]))

 # apply finite population correction
 if ("n" %in% names(dfWeights)) {
   N <- sum(dfWeights$n)
   fpcf <- sqrt((N-respTot)/(N-1))
   res$Lower <- res$PointEst - ((res$PointEst - res$Lower) * fpcf)
   res$Upper <- res$PointEst + ((res$Upper - res$PointEst) * fpcf)
 }
 
 if (outOpt == "perc") {
  res$PointEst <- round(res$PointEst *100, 2)
  res$Lower <- round(res$Lower *100, 2)
  res$Upper <- round(res$Upper *100, 2)
  return(res)
 }
 
 if (!("n" %in% names(dfWeights))) {
   stop("Param 'outOpt' set to 'count' but no variable named 'n' in dfWeights.")
 }
 
 # convert percentages to counts
 pop <- sum(dfWeights$n)
 res$PointEst <- res$PointEst * pop
 res$Lower <- res$Lower * pop
 res$Upper <- res$Upper * pop

 return(res)
}


# NB! tans stel dit negatiewe CILs net na 0 indien allowNegCIL == FALSE. Daar
# is egter 'n ordentlike manier om CIs te bereken wanneer negatiewe CILs nie
# moontlik is nie. Vind by CJP uit wat daardie fix is en werk dit hier in!
summaryX <- function(x, digits = 2, NAsToZero = FALSE, allowNegCIL = TRUE, combinedCI = FALSE) {
 if (NAsToZero) {
  x[which(is.na(x))] <- 0
 }
 
 smmry <- round(summary(x), digits = digits)
 nms <- names(smmry)
 smmry <- as.vector(smmry)
 df <- data.frame(matrix(data = smmry, 
                         nrow = 1, 
                         ncol = length(smmry)))
 names(df) <- nms
 df$n <- length(x[which(!is.na(x))])
 CIs <- CI(x = x, ci = 0.95, na.rm = T, as.df = TRUE)
 if (!allowNegCIL) {
  idxx <- which(CIs[["Lower"]] < 0)
  if (length(idxx) > 0) {CIs[idxx,"Lower"] <- 0}
 }
 if (!combinedCI) {
  df[["95% CI L."]] <- round(CIs[["Lower"]], digits = digits)
  df[["95% CI U."]] <- round(CIs[["Upper"]], digits = digits)
 } else {
  df[["95% CI"]] <- sprintf("(%s, %s)",  
                            round(CIs[["Lower"]], digits = digits),
                            round(CIs[["Upper"]], digits = digits))
 }
 df[["Std. Dev"]] <- round(sd(x, na.rm = TRUE), digits = digits)
 
 for (v in 1:ncol(df)) {
  df[[v]][which(is.nan(df[[v]]))] <- NA_real_
 }
 
 return(df)
}

# herskryf hierdie om die '...' argument te gebruik
summaryXby <- function(dfData, sumVar, groupVar = NULL, digits = 2, 
                       NAsToZero = FALSE, allowNegCIL = TRUE,
                       combinedCI = FALSE, orderBy = "Mean",
                       includeAll = FALSE, allNm = "ALL") {
 require(plyr)
 
 if (is.null(groupVar)) {
  return(summaryX(x = dfData[[sumVar]], 
                  digits = digits,
                  NAsToZero = NAsToZero,
                  allowNegCIL = allowNegCIL, 
                  combinedCI = combinedCI))
 }
 if (is.na(groupVar)) {
  return(summaryX(x = dfData[[sumVar]], 
                  digits = digits,
                  NAsToZero = NAsToZero,
                  allowNegCIL = allowNegCIL, 
                  combinedCI = combinedCI))
 }
 
 lsData <- split(dfData, f = dfData[[groupVar]])
 lsData <- lapply(X = lsData, FUN = function(df) {
  return(summaryX(x = df[[sumVar]], 
                  digits = digits,
                  NAsToZero = NAsToZero,
                  allowNegCIL = allowNegCIL, 
                  combinedCI = combinedCI))
 })
 
 dfRes <- do.call("rbind.fill", lsData)
 dfRes[[groupVar]] <- names(lsData); rm(lsData)
 
 dfRes <- dfRes[order(dfRes[[orderBy]], decreasing = TRUE),]
 rownames(dfRes) <- 1:nrow(dfRes)
 
 if (includeAll) {
  dfAll <- summaryX(x = dfData[[sumVar]], 
                    digits = digits,
                    NAsToZero = NAsToZero, 
                    allowNegCIL = allowNegCIL, 
                    combinedCI = combinedCI)
  dfAll[[groupVar]] <- allNm
  dfRes <- rbind(dfRes, dfAll); rm(dfAll)
  rownames(dfRes) <- 1:nrow(dfRes)
 }
 
 dfRes <- move.col(df = dfRes, colName = groupVar, colIdx = 1)
 
 return(dfRes)
}
