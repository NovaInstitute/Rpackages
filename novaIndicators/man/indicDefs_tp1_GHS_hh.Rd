% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicatorDefTbls_GHS.R
\name{indicDefs_tp1_GHS_hh}
\alias{indicDefs_tp1_GHS_hh}
\title{indicDefs_tp1_GHS_hh}
\usage{
indicDefs_tp1_GHS_hh(dfHh, datasetNm = "GHSXXXX")
}
\arguments{
\item{dfHh}{accepts a data.frame that contains the dataset}

\item{datasetNm}{E.g. 'GHS2017' or 'GHS2016'}
}
\description{
Generates the indicator definitions/descriptions for type 1 ('yes'/'no' vars) indicators from the household dataset
}
\details{
The functions in this script prepare indicator definition tables for the General Household Survey (GHS).
Three different definition tables are generated:
  1. A table for indicators from 'yes'/'no' response variables (e.g. 'energy_coal_use'), 
     called "type 1" indicators. Generated by 'indicDefs_tp1_GHS_hh(...)'.
  2. A table for indicators from quantitative variables (e.g. 'energy_coal_consumption_wintercurrentunits),
     called "type 2" indicators
  3. A table for indicators generated from other indicators (e.g. 'TNNE_COAL_PCOMM_PMONTH_WINTR_SV_GHS2017' which
     is generated from 'KG_COAL_PCOALHH_PMONTH_WINTR_SV_GHS2017' and 'KG_COAL_PCOALHH_PMONTH_SUMMR_SV_GHS2017'),
     called "type 3" indicators.

Custom indicators can also be added to the tables using this script.

Example of use (not run):
  source("./global.R")
  load(paste(rdadir, "SV_GHS2017.Rda", sep = ""))
  
  dfIndicDefs_tp1 <- indicDefs_tp1(dfHh = dfHh_SV_GHS2017)
  write.xlsx(x = dfIndicDefs_tp1, file = "dfIndicDefs_tp1_GHS.xlsx")
  
  dfIndicDefs_tp2 <- indicDefs_tp2(dfHh = dfHh_SV_GHS2017)
  save(dfIndicDefs_tp2, file = "dfIndicDefs_tp2_GHS.Rda")
   
  dfIndicDefs_tp3 <- indicDefs_tp3()
  assign(x = "dfIndicDefs_tp3_GHS", value = dfIndicDefs_tp3, envir = .GlobalEnv)
}
