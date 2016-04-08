#' Calculate ER AI
#'
#' ER Calculation function for Alternatve Ignition Methodology
#'
#' @param haq Data frame containing survey data (will be retreived from global environment)
#' @param pop Data frame containing town populations (will be retreived from global environment)
#' @param fundir The function directory as character vector
#' @param dropdir The dropbox directory as character vector
#' @param thisplace The directory of interenst for saving data as character vector
#' @param datadir The directory to save and retrieve data as character vector
#' @param basedir The directory containing base data as character vector
#' @param gendir The directory of the general data as character vector
#' @param dirr The directory containing R code as character vector
#' @param tabdir The directory to which tables should be saved as character vector
#' @param name The name to be attributed to the saved Rda file as character vector
#' @param projectname The project name that will be added to saved file names as character vector
#' @param write.date The date that files were written as character vector
#' @param reg.date The registration date as character vector
#' @param verbose Numerical that displays function messages when larger than 1
#' @param attend Data frame containing the attendance records
#' @param ... Further arguments passed to or from other methods
#' @references http://www.cdmgoldstandard.org/wp-content/uploads/2011/10/Alternative_Ignition.pdf
#' @export

calculateER.AI <- function(haq = get("haq", envir=.GlobalEnv),
                           pop = get("pop", envir=.GlobalEnv),
                           fundir = "/Users/christiaanpauw/Dropbox/Rfunctions/",
                           dropdir = "/Users/christiaanpauw/Dropbox/",
                           thisplace = "Verifikasie2012/EMMnorth/",
                           datadir = "/Users/christiaanpauw/Dropbox/Verifikasie2011/MalutiEast/data/",
                           basedir = "/Users/christiaanpauw/Dropbox/Verifikasie2011/MalutiEast/",
                           gendir = "/Users/christiaanpauw/Dropbox/Verifikasie2012/General_data/",
                           dirr = "Rscripts/",
                           tabdir = "tables/",
                           name = "M11",
                           projectname = "EMMnorth2011",
                           write.date = "Sep11",
                           reg.date = as.Date("2010-03-12"),
                           verbose = 2,
                           attend = att, ...
                           ){
  ## Load libraries and set option
  library(prettyR)
  library(gdata)
  library(Hmisc)
  library(doBy)
  library(plyr)
  library(reporttools)
  library(xtable)
  library(chron)
  library(TeachingDemos)
  library(doBy)
  library(ggplot2)
  options(digits=2)
  library(sp)
  library(Hmisc)
  assign("dropdir", dropdir, envir=.GlobalEnv)
  assign("basedir", basedir, envir=.GlobalEnv)
  assign("tabdir", tabdir, envir=.GlobalEnv)
  assign("projectname", projectname, envir=.GlobalEnv)

  if(verbose>1){ message("Dropdir = ", dropdir)
                 message("Basedir = ", basedir)}

  #source(paste(fundir,"AI.functions.R", sep=""))
  if (!require(novaAI) == FALSE){
          message("Installing novaAI package")
          devtools::install_github("NovaInstitute/Rpackages/novaAI")
          library(novaAI)
  }



  # Select only the populations of the appropriate suburbs
  pop = pop[match(levels(as.factor(haq[,"town"])),pop[,"Town"]), ]
  if(verbose>1) message("pop  = ", paste(names(pop), sep="; "))
  #pop["Town"]=drop.levels(pop["Town"])
  if(verbose>1) message("dim pop = ", dim(pop))

  ####### Baseline variables ########
  # Users in the target area
  uta=project.summary.UTA.raw(haq,
                          pop=get("pop",envir=sys.frame(which=0)),
                          Coal="coal.use",
                          var=contact,
                          coalvar="Yes",
                          group=get("haq", envir=environment(project.summary.UTA.raw))[,"town"],
                          BM=FALSE,
                          othervar="introduction.to.bm",
                          other=FALSE)
  if(verbose>1)message("UTA DONE")
  assign("uta", uta, envir = .GlobalEnv)


  cl.coal = cl.UTA(haq,
                   pop=get("pop",envir=sys.frame(which=0)),
                   Coal="coal.use",
                   coalvar="Yes",
                   var="hh.BM.use",
                   group=get("haq", envir=environment(cl.UTA))[,"town"],
                   othervar="introduction.to.bm") # 90% cl of uta
  if(verbose>1)message("cl.alt", exists("cl.alt"))
  assign("cl.coal", cl.coal, envir = .GlobalEnv)

  cl.alt = cl.Ualt(haq,
                   pop=get("pop",envir=sys.frame(which=0)),
                   Coal="coal.use",
                   coalvar="Yes",
                   var="hh.BM.use",
                   group=get("haq", envir=environment(cl.Ualt))[,"town"],
                 othervar="introduction.to.bm") # 90% cl of uta
  if(verbose>1)message("cl.alt", exists("cl.alt"))
  assign("cl.alt", cl.alt, envir = .GlobalEnv)

  Uother=project.summary.UTA.raw(haq,
                             pop=get("pop",envir=sys.frame(which=0)),
                             Coal="coal.use",
                             coalvar="Yes",
                             group=get("haq", envir=environment(project.summary.UTA))[,"town"],
                             BM=FALSE,
                             othervar="introduction.to.bm",
                             other=TRUE)
  assign("Uother", Uother, envir=.GlobalEnv)
  if(verbose>1) message("Uother = ", dim(Uother))

  ## Apply the test for cl.alt. It cl.alt>att then att
  cl.alt[1,"point estimate"] = ifelse(cl.alt[1,"point estimate"]>length(attend[,1]),
                                    length(attend[,1]),
                                    cl.alt[1,"point estimate"])
  assign("cl.alt", cl.alt, envir = .GlobalEnv)
  ########################## Survey date table ##################

  survey.date.table(haq)
  if(verbose>1) message("survey.date.table  = ", "DONE")
  ################## assemble basedata ####################
  # Select variables relevant to the baseline calculation for inspection

  basedata=assemble.basedata( beforevar  = "coal.units.winter.before.bm" ,
                              aftervar   = "coal.units.winter.after.bm",
                              currentvar = "current.coal.units.winter",
                              vars=c("response.id","town","surveyyear","year.of.initiation","bmstartyearmonth",
                                     "coal.buying.format", "coal.units.winter.before.bm","coal.units.winter.after.bm",
                                     "current.coal.units.winter"),
                             verbose=TRUE)
  assign("basedata", basedata, envir=.GlobalEnv)
  if(verbose>1)message(" ################################# basedata DONE ##########################")
  ############################### tables for report ############################

  baseline.tables(haq      = get("haq", envir=sys.frame(which=1)),
                  basedata = get("basedata", envir = sys.frame(which=1)),
                  uta      = get("uta", pos = sys.frame(which=1)),
                  cl.alt   = get("cl.alt", pos = sys.frame(which=1)),
                  Uother   = get("Uother", envir=sys.frame(which=1)),
                  basedir  = get("basedir", envir=sys.frame(which=1)),
                  tabdir   = get("tabdir", sys.frame(which=1))
                  )
  if(verbose>1)message("################################## baseline tables DONE ##################")

  #om die vraagtekens in program uit te haal
  label(haq$coal.use.in.summer) <-"Do you use coal in summer"
  label(haq$introduction.to.bm) <-"How did you learn about BM"

  freq.table.v(haq$introduction.to.bm,tables.dir=paste(basedir,tabdir,sep=""))
  freq.table.by(haq$coal.use.in.summer,haq$hh.BM.use,tables.dir=paste(basedir,tabdir,sep=""))

  #########################################################################################################################
  #############################################Sample from baseline coal use variables#####################################
  #########################################################################################################################

  baseline.sample(haq)
  if(verbose > 1) message("########################### Baseline sample DONE ######################")
  ############################################################################################################
  ######################################### Baseline by town and weighted mean #####################################
  ############################################################################################################

  baseline.vars()
  if(verbose > 1) message("########################### Baseline vars DONE ######################")

  ############################################################################################################
  ########################################## Baseline Emissions #################################################
  ############################################################################################################

  # Total baseline emissions
  ccontent=0.6544
  XO2=0.98
  COEF=ccontent*XO2*44/12
  assign("ccontent", ccontent, envir=.GlobalEnv)
  assign("XO2", XO2, envir=.GlobalEnv)
  assign("COEF", COEF, envir=.GlobalEnv)

  # Total baseline emissions
  BE=CBc*COEF
  BE.s=CBc.s*COEF
  BE.w=CBc.w*COEF
  assign("BE", BE, envir=.GlobalEnv)
  assign("BE.s", BE.s, envir=.GlobalEnv)
  assign("BE.w", BE.w, envir=.GlobalEnv)


  # Table of emission factor variables
  coef.tab=rbind(c("$w{}_{C}$", "$XO_{2}$", "$COEF$"),c(ccontent,XO2,round(COEF,4)))

  # Baseline emission table

  baseline.emissions.table()
  if(verbose > 1) message("########################### Baseline emissions DONE ######################")
  ##################################################################################################
  ######################################### Project emissions ##########################################
  ##################################################################################################

  # summary of project emission variables

  project.var.table()
  if(verbose > 1) message("project.var.table DONE")

  ####################### Samples: Project Scenario ##################

  project.sample()
  if(verbose > 1) message("############################ project sample DONE ############################")

  ############################################################################################################
  ######################################### Project by town and weighted mean #####################################
  ############################################################################################################

  project.tables()
  if(verbose > 1) message("############################ project tables DONE ############################")

  # Make Project table for report with formulae and results

  project.formula.table()

  ######################################################################################################
  ######################################### Emission reductions ############################################
  ######################################################################################################

  ER=BE-PE
  ER.w = BE.w-PE.w
  ER.s = BE.s-PE.s
  ER.cal <- ER.w+ER.s/2

  assign("ER",     ER,     envir=.GlobalEnv)
  assign("ER.w",   ER.w,   envir=.GlobalEnv)
  assign("ER.s",   ER.s,   envir=.GlobalEnv)
  assign("ER.cal", ER.cal, envir=.GlobalEnv)

  if(verbose > 1) message("ER = ", ER, "\n" , "BE = ", BE, "\n", "PE = ", PE , "\n")
  # Make ER table for report with winter and summer BE,PE and ER

  er.table()

  if(verbose > 1) message("############################ ER table DONE ############################")

  er.disc.table()
  if(verbose > 1) message("############################ ER.disc table DONE ############################")


  ##################################################################################################
  #################################### Summary of results #############################################
  ##################################################################################################
  addmargins(table(as.Date(as.integer(haq$bmstartyearmonth),origin=as.Date("1970-01-01")),haq$hh.BM.use))

  town = paste(levels(factor(haq$town)),collapse=".")
  assign("town", town, envir=.GlobalEnv)
  summry(file=paste(gendir,"bsagld-101214-CJP-PDD-spreistaat-basesummary.csv",sep=""))
  if(verbose > 1) message("############################ Summry DONE ############################")


  #### Save output ####

  write.csv(summry,file=paste(basedir,tabdir,projectname,"summry.csv",sep=""))
  write.csv(uta,file=paste(basedir,tabdir,projectname,"uta.csv",sep=""))
  write.csv(haq,file=paste(basedir,tabdir,projectname,"processed_data.csv",sep=""))
  save(summry,file=paste(basedir,tabdir,projectname,"summry.Rda",sep=""))

  lss = c("haq","min.pop", "approached",
          "town","pop",
          "uta","Uother","cl.alt",
          "CBy","CBy.w","CBy.s",
          "CBc.w","CBc.s","CBc",
          "CPc", "CPc.s", "CPc.w",
          "CPy", "CPy.s", "CPy.w",
          "BE","BE.w","BE.s",
          "PE","PE.w","PE.s",
          "ER","ER.w","ER.s",
          "ER.cal",
          "ccontent","XO2","COEF",
          "summry",
          "coalm","price.tab", "merc.tab")
  if (verbose > 3) message("lss: " , lss)
  daar = is.na(match(lss, ls(envir=.GlobalEnv)))==FALSE
  if (verbose > 3) message("daar: " , daar)
  save(list=lss[daar], file=paste(basedir,tabdir,projectname,"all.Rda",sep=""))

  message("ER is ", round(ER /1000), "\n", "\n", "                   Thats all folks!", "\n")
}
