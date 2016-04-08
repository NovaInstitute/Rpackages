#' Calculate BM Management
#'
#' Make a quick BM management summary (requires the fixname function)
#'
#' @param x a (readable binary-mode) connection or a
#' character string giving the name of the file to load
#' @param verbose displays function messages when TRUE
#' @param debug performs assignments to the global environment for
#' debugging purposes when TRUE
#' @param COEF the coefficient used for Emission calculation purposes
#' @param naampos the name position
#' @return The output should be a melted data frame containing:
#' @return Number Households Actual
#' @return Percentage Coal Used by Household Actual
#' @return BASA Users Total end of vintage year Actual
#' @return Percentage Monitoring Loss Factor Actual
#' @return Percentage Nova BASA Users Retention Actual
#' @return Nova BASA Users Carried forward Actual
#' @return FACTOR VERS per Household existing Actual
#' @return FACTOR VERS per Household New Actual
#' @return Percentage New user start date Impact Actual
#' @return Low estimate Actual
#' @export

calculate.BM.manage <- function(x, verbose=FALSE, debug = FALSE,
                                COEF = 2.3515, naampos = NULL
                                   ){

  require(doBy)
  require(plyr)
  require(reshape2)

  COEF = 2.3515

  load(x)
  .env <- environment()
  if(verbose==TRUE){ message(length(ls()), " items in die ", as.character(environmentName(environment())) , " omgewing")}

  names(haq) <- tolower(names(haq))
  haq$response.id <- as.character(haq$response.id)
  haq[is.na(haq$response.id), "response.id"] <- haq[is.na(haq$response.id), "?..submission.id"]

  if (is.null(naampos) == FALSE){naam <- unlist(strsplit(x,'/'))[naampos]
  	} else {
  		nm = grep("\\.Rda", unlist(strsplit(x,'/')), value=T)
  		message("nm ", nm)
  		naam = gsub("[[:digit:]]|[[:punct:]]|all|[[:space:]]|\\.Rda", "", nm)
  		year = gsub("[[:alpha:]]|[[:punct:]]|[[:space:]]|all|\\.Rda", "", nm)}

  if(verbose==TRUE){ message("#########################################################\n Naam is ", naam)}
  if(verbose==TRUE){ message("#########################################################\n Jaar is ", year)}

  #jaar <- lapply(x,FUN=function(x)unlist(strsplit(x,'/'))[length(unlist(strsplit(x,'/')))] )
  #year <- melt(lapply(jaar, FUN=function(x) gsub('([[:alpha:]])+([[:space:]]*)([[:alpha:]])*([[:space:]]*)([[:digit:]]{4})([[:alpha:]])?all.Rda$','\\5', x)))[[1]]
  if(verbose==TRUE){ message("Year is ", year) }
  if(verbose==TRUE){ message("Objekte is ", paste(ls(), " "))}

  tw = ls()[grep('[[:digit:]]+', ls())]
  if(verbose==TRUE) message("tw is ", tw)
  en = gsub('[[:digit:]]+',"", tw)
  if(verbose==TRUE) message("en is ", en)
  if (length(tw) > 0 ){
  	lapply(1:length(tw), function(i) assign(en[i], get(tw[i]), envir = .env))
  	rm(list=tw)
  	}
  if(verbose==TRUE){ message("Objekte is ", paste(ls(), " "))}

  if (length(haq$town) > 0 & length(haq$town.0) > 0) haq = haq[,-grep("town.0", names(haq))]
  names(haq) <- fixname(names(haq))


  # bestaan uta
  if(verbose==TRUE) message("Kyk of uta bestaan: ", testObject(uta))
  if (testObject(uta) == FALSE & verbose==TRUE) message("Moenie worry nie, ons sal hom kry"); uta = get(ls()[grep("uta", ls())])
  if(verbose==TRUE) message("dim uta ", dim(uta)[1], " by ", dim(uta)[2])

  # Rekonstrueer pop
  rm(pop)
  pop = data.frame(Town = rownames(uta)[1:(length(uta[,1])-1)] ,
                                     population = uta[1:(length(uta[,1])-1), "Pop."])
  if (debug == TRUE) assign("pop", pop, envir=.GlobalEnv)
  if(verbose==TRUE) message("dim pop ", dim(pop)[1] , " by ", dim(pop)[2], " met name ", names(pop), " en kolom 1 is ", str(pop[,1]))

  # Decern the town variable: it may be town location or place
  if(length(haq$town) > 0) {haq$town = as.factor(as.character(haq$town)) ; if(verbose==TRUE) message("Ek herstel 'town' se faktorvlakke")}
  if(length(haq$mainplace.r) > 0) {haq$mainplace.r = as.factor(as.character(haq$mainplace.r)) ; if(verbose==TRUE) message("Ek herstel 'mainplace.r' se faktorvlakke")}

  placelist = c("mainplace.r", "town", "location", "location.1")

  if(verbose==TRUE) message("Plekname : ", ls()[na.omit(match(placelist, ls() ))], " met vlakke ", dimnames(table(haq[,ls()[na.omit(match(placelist, ls() ))]])))
  mlys = list(
    all(is.na(match(levels(as.factor(haq$mainplace.r)), pop$Town))) ,
    all(is.na(match(levels(as.factor(haq$location.1)), pop$Town))) ,
    all(is.na(match(levels(as.factor(haq$town)), pop$Town))) ,
    all(is.na(match(levels(as.factor(haq$location)), pop$Town)))
  )
  if(verbose==TRUE) message("mlys is ", mlys)
  t.idx = which(mlys==FALSE)
  if(verbose==TRUE){ message("t.idx ", t.idx ) }
  townvars = c("mainplace.r","location.1", "town", "location")[t.idx]
  if(length(townvars) == 1){ townvar <- townvars } else {
    townvar =  townvars[which(sapply(townvars, function(x) length(levels(as.factor(haq[, x]))))==length(pop$Town))][1]}
  if(verbose==TRUE){ message("townvar ", townvar ) }

  if(all(levels(haq[,townvar]) == levels(pop$Town))) haq[,townvar] = factor(as.character(haq[,townvar]),levels = levels(pop$Town))

  if(verbose==TRUE) message("Levels pop$Town " , paste(levels(pop$Town),  ' '))
  if(verbose==TRUE) message("Levels haq[,townvar] " , paste(levels(haq[ ,townvar]),  ' '))


  if(verbose==TRUE){ message("townvars ", townvars ) }

  # stel jare voor 2007 op 2007
  if(verbose==TRUE) message("Kyk of haq$year.of.initiation bestaan: ", "year.of.initiation" %in% names(haq))
  if ("year.of.initiation" %in% names(haq)  == FALSE & "bm.usage.start.year" %in% names(haq)){
  	if(verbose==TRUE) message("Moenie worry nie, ons sal hom kry"); haq$year.of.initiation = haq[,grep("bm.usage.start.year", names(haq))]
  	if(verbose==TRUE) message(grep("bm.usage.start.year", names(haq), value=TRUE), " haq$year.of.initiation", paste(table(haq$year.of.initiation), " "))
  	}
  myidx <- which(is.na(haq$year.of.initiation)==TRUE & is.na(haq$bmstartyearmonth)==FALSE)
  if (length(myidx) > 0) {
    haq[myidx, "year.of.initiation"] <- as.integer(substr(haq[myidx, "bmstartyearmonth"], 1, 4))
  }

  haq$year.of.initiation = unfactor(haq$year.of.initiation)
   if (verbose == TRUE) message(paste(names(table(haq$year.of.initiation)), " "))
   if (verbose == TRUE) message(paste(table(haq$year.of.initiation), " "))
  haq$year.of.initiation[which(as.integer(haq$year.of.initiation) <= 2007)] = 2007
  haq$annual.save = haq$annual.base - haq$annual.current
   if (verbose == TRUE) message("haq$annual.save: ", paste(summary(haq$annual.save), " "))
   if (verbose == TRUE) message("haq$annual.base: ", paste(summary(haq$annual.base), " "))
   if (verbose == TRUE) message("haq$annual.current: ", paste(summary(haq$annual.save), " "))
   if (verbose == TRUE) message("haq[,townvar] ", paste(table(haq[,townvar]), " "))

  aby = tapply(haq$annual.base, list(haq$year.of.initiation, haq[,townvar]), FUN = mean, na.rm = TRUE)
  #aby = aby[!is.na(aby)]
  apy = tapply(haq$annual.current, list(haq$year.of.initiation, haq[,townvar]), FUN = mean, na.rm = TRUE)
  #apy = apy[!is.na(apy)]
  acy = aby - apy
  er.y = acy * COEF
  er.y[is.na(er.y)] = 0
  #er.y = er.y[apply(er.y, 1, function(x) !all(x == 0)) , ]

  if(verbose==TRUE){ message("dim er.y ", dim(er.y) ) }
  if(verbose==TRUE){ message("dim pop ", dim(pop) ) }

  # make a matrix of observations
  bm.obs = tapply(haq$hh.bm.use,
                  list(haq[,townvar], haq$year.of.initiation),
                  function(x) table(x)["BM"])

  if(verbose==TRUE){ message("dim bm obs ", dim(bm.obs) ) }

  tot.obs = tapply(haq$response.id,
                   list(haq[,townvar]),
                   function(x) sum(table(x)))

  if(verbose==TRUE){ message("dim tot obs ", dim(tot.obs) ) }

  bm.props = apply(bm.obs, 2, function(x) x / tot.obs)

  #if(is.null(bm.props)) bm.props = as.matrix(t(bm.props))

  dmn =  list(levels(as.factor(haq$year.of.initiation)),c(levels(as.factor(as.character(haq[,townvar])))))
  if(verbose==TRUE){ message("dim names ", dmn ) }

  pop.matrix = matrix(rep(pop$population,times=length(dmn[[1]])),
                      nrow=length(dmn[[1]]),
                      byrow=TRUE)

  #if(length(dimnames(pop.matrix) == dmn))
  dimnames(pop.matrix) <- dmn
  if(debug == TRUE) assign("pop.matrix", pop.matrix, envir=.GlobalEnv)
  if(debug == TRUE) assign("bm.props", bm.props, envir=.GlobalEnv)
  if(debug == TRUE) assign("bm.obs", bm.obs, envir=.GlobalEnv)
  if(debug == TRUE) assign("tot.obs", tot.obs, envir=.GlobalEnv)
  if(debug == TRUE) assign("er.y", er.y, envir=.GlobalEnv)
  if(debug == TRUE) assign("aby", aby, envir=.GlobalEnv)
  if(debug == TRUE) assign("acy", acy, envir=.GlobalEnv)
  if(debug == TRUE) assign("apy", apy, envir=.GlobalEnv)

  if(is.null(dim(bm.props))) { res = pop.matrix * bm.props } else {res = pop.matrix * t(bm.props)}
  if(verbose==TRUE){ message("dim res ", dim(res)[1], " by ", dim(res)[2]) }
  if(debug == TRUE) assign("res", res, envir=.GlobalEnv)

  tot = sum(res, na.rm=TRUE)
  if(verbose==TRUE){ message("tot ", tot ) }

  if(is.null(dim(res))) { fin = res } else { res[is.na(res)] <- 0 ; fin = addmargins(na.omit(res)) }

  if(verbose==TRUE) message("Kyk of cl.al bestaan: ", "cl.alt" %in% ls())
  if ("cl.alt" %in% ls()  == FALSE ){
  	if(verbose==TRUE) message("Moenie worry nie, ons sal hom kry"); cl.alt = get(ls()[grep("cl.alt", ls())])
  	if(verbose==TRUE) message("cl.alt se dimensies: ", dim(cl.alt)[1], " by ", dim(cl.alt)[2])
  	}

  mon.loss.f = 1 - (cl.alt[2] / cl.alt[4])
  ER = cl.alt[2] / cl.alt[4] * (sum((res * acy), na.rm=T) * (2.3515 / 1000) )
  ERspes = cl.alt[2] / cl.alt[4] * (res * acy) * (2.3515 / 1000)

  if(verbose==TRUE){ message("mon.loss.f ", mon.loss.f) }

  fin.m = melt(fin)
  fin.m$vintage = year
  fin.m$project = naam
  fin.m

  res.m =  melt(res)
  res.m$vintage = year
  res.m$project = naam
  names(res.m)[c(1,2)] = c("Origin", "Main.Place")

  # Households Actual
  if(verbose==TRUE) message("Households = ", uta["TOTAL, MEAN", "Pop."])
  hha = round(uta["TOTAL, MEAN", "Pop."])

  # % Coal Used by Household Actual
  xuc = round(uta["TOTAL, MEAN", "XUcoal"], 4)
  if(verbose==TRUE) message("Coal Used by Household Actual = ", xuc)

  # BASA Users Total end of vintage year Actual
  BMNova = uta["TOTAL, MEAN", "BM users Nova"]
    if(verbose==TRUE) message("BASA Users Total end of vintage year Actual = ", BMNova)

  # % Monitoring Loss Factor Actual
  mon.loss.f = round(1 - (cl.alt[2] / cl.alt[4]),4)

  # FACTOR VERS per Household existing Actual
  idx = which(as.integer(rownames(ERspes)) < as.integer(as.character(year)))
  ER.old = weighted.mean(er.y[idx, ],res[idx, ] ,na.rm=TRUE)

  if(verbose==TRUE) message("ER.old created: dim = ", dim(ER.old))

  # FACTOR VERS per Household New Actual
  ER.new = weighted.mean(er.y[-idx,], res[-idx, ], na.rm=TRUE)
  # % New user start date Impact Actual

  if(verbose==TRUE)message("ER.new: dim = ", dim(ER.new))

  # Low estimate Actual
  lea = cl.alt[2]

  # kyk of BE.w en BE.s bestaan - indien nie maak hom

  if (length(grep("BE.w", ls())) == 0){
  	BE.w = CBc.w * COEF
  	BE.s = CBc.s * COEF
  	if(verbose==TRUE) message("kom ons toets: BE == BE.w + BE.s ", BE," ", BE.w," ", BE.s," ", BE == BE.w + BE.s)
  }

  # kyk of PE.w en PE.s bestaan - indien nie maak hom
  if (length(grep("PE.w", ls())) == 0){
  	PE.w = CPc.w * COEF
  	PE.s = CPc.s * COEF
  	if(verbose==TRUE) message("kom ons toets: BE == BE.w + BE.s ", PE == PE.w + PE.s)
  }

  # kyk of ER.w en ER.s bestaan - indien nie maak hom
  if (length(grep("ER.w", ls())) == 0){
  	ER.w = BE.w - PE.w
  	ER.s = BE.s - PE.s
  	if(verbose==TRUE) message("kom ons toets: ER == ER.w + ER.s ", ER == ER.w + ER.s)
  }

  result = data.frame("year" = year,
          "proj" = naam,
    	  "Households Actual" = round(hha),
          "Perc Coal Used by Household Actual" = xuc,
          "BASA Users Total end of vintage year Actual" = round(BMNova),
          "Perc.Monitoring Loss Factor Actual" = mon.loss.f,
          "FACTOR VERS per Household New Actual" = round(ER.new,2),
          "FACTOR VERS per Household existing Actual" = round(ER.old,2),
          "Low estimate Actual" = round(lea),
          Seasonal.ER = round(ER),
          Winter.ER = round(ER.w/1000),
          Summer.ER = round(ER.s/1000),
          summerdays.factor = round(1-mean(haq$summerdays.prop, na.rm=TRUE)/2,2),
          ER.per.new.user = ER.new,
          ER.per.old.user = ER.old)

  if(verbose==TRUE) message("result created: dim = ", dim(result))

  lst = list( Actual.Vars = result,
  		BM.User.per.Area = res,
  		ER.by.year.origin.mean = er.y
  		)
  lst

}

