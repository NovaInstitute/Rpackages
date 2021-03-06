% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/template_indicatorDefTbls_DES.R
\name{indicDefs_tp1}
\alias{indicDefs_tp1}
\title{indicDefs_tp1}
\usage{
indicDefs_tp1(dfHh)
}
\arguments{
\item{dfHh}{The data frame/vector containing indicator definitions/descriptions for type 1 indicators.}
}
\value{
data.frame
}
\description{
Generates the indicator definitions/descriptions for type 1 ('yes'/'no' vars) indicators
}
\details{
Date created: 2018-03-22
Owner: NOVA Institute (Reg# 1994/002614/08)
This script prepares indicator definition tables for the Detailed Energy Survey (DES).
Three different definition tables are generated:
  1. A table for indicators from 'yes'/'no' response variables (e.g. 'energy_coal_use'), 
     called "type 1" indicators.
  2. A table for indicators from quantitative variables (e.g. 'energy_coal_consumption_wintercurrentunits),
     called "type 2" indicators
  3. A table for indicators generated from other indicators (e.g. 'TNNE_COAL_PCOMM_PMONTH_WINTR_SV_DES2017' which
     is generated from 'KG_COAL_PCOALHH_PMONTH_WINTR_SV_DES2017' and 'KG_COAL_PCOALHH_PMONTH_SUMMR_SV_DES2017'),
     called "type 3" indicators.

Custom indicators can also be added to the tables using this script.

Example of use (not run):
  source("./global.R")
  load(paste(rdadir, "SV_DES2017.Rda", sep = ""))
  
  dfIndicDefs_tp1 <- indicDefs_tp1(dfHh = dfHh_SV_DES2017)
  write.xlsx(x = dfIndicDefs_tp1, file = "dfIndicDefs_tp1_DES.xlsx")
  
  dfIndicDefs_tp2 <- indicDefs_tp2(dfHh = dfHh_SV_DES2017)
  save(dfIndicDefs_tp2, file = "dfIndicDefs_tp2_DES.Rda")
   
  dfIndicDefs_tp3 <- indicDefs_tp3()
  assign(x = "dfIndicDefs_tp3_DES", value = dfIndicDefs_tp3, envir = .GlobalEnv)

Generates the indicator definitions/descriptions for type 1 ('yes'/'no' vars) indicators
}
