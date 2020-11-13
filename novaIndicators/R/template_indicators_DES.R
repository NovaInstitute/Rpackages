#' Date created: ...
#' Owner: nova Institute (Reg# 1994/002614/08).

# ---------------------------------------- #
#' Script to prepare indicators from the DES-XXXX dataset for ...
# ---------------------------------------- #

source("./global.R")
#source(paste(sysdir, "DES/indicatorDefTbls_DES.R", sep = ""))
source(paste(novapackdir, "novaIndicators/indicatorsFrmTbl.R", sep = ""))

# ---------------------------------------- #
srcpth <- paste(sysdir, "/DES/indicators_SV_DES2017.R", sep = "")
load(paste(rdadir, "indicators.Rda", sep =""))
#indicators <-  new.env()

# ---------------------------------------- #
load(paste(rdadir, "SV_DES2017.Rda", sep = ""))
dfHousehold <- df_hh
dfStructures <- df_struc
rm(df_hh, df_hh_orig, df_struc, df_summer, df_winter, df_solid_fuel)

# ---------------------------------------- #
#' Calculate type 1 indicators ('yes'/'no' vars)

#dfIndicDefs <- indicDefs_tp1(dfHousehold)
#write.xlsx(x = dfIndicDefs, file = paste(tmpdir, "dfIndicDefs_tp1_DES.xlsx", sep = ""))
dfIndicDefs <- read.xlsx(paste(tmpdir, "dfIndicDefs_tp1_DES.xlsx", sep = ""))

lsIndics <- indicsFrmTbl_tp1(dfIndicDefs = dfIndicDefs, 
                             dfData = dfHousehold,
                             srcpth = srcpth,
                             nHhs = POP_HHS_SV_2017)
print(lsIndics[[sample(x = 1:length(lsIndics), size = 1)]])
for (indic in lsIndics) {
  assign(x = indic@name, value = indic, envir = indicators)
}

# ---------------------------------------- #
#' Calculate type 2 indicators (quantitative)

#dfIndicDefs <- indicDefs_tp2(dfHousehold)
#write.xlsx(x = dfIndicDefs, file = paste(tmpdir, "dfIndicDefs_tp2_DES.xlsx", sep = ""))
dfIndicDefs <- read.xlsx(paste(tmpdir, "dfIndicDefs_tp2_DES.xlsx", sep = ""))

lsIndics <- indicsFrmTbl_tp2(dfIndicDefs = dfIndicDefs,
                             dfData = dfHousehold, 
                             srcpth = srcpth)
print(lsIndics[[sample(x = 1:length(lsIndics), size = 1)]])
for (indic in lsIndics) {
  assign(x = indic@name, value = indic, envir = indicators)
}


# ---------------------------------------- #
#' Calculate type 3 indicators (indicator X indicator)

#dfIndicDefs <- indicDefs_tp3()
#write.xlsx(x = dfIndicDefs, file = paste(tmpdir, "dfIndicDefs_tp3_DES.xlsx", sep = ""))
dfIndicDefs <- read.xlsx(paste(tmpdir, "dfIndicDefs_tp3_DES.xlsx", sep = ""))

lsIndics <- indicsFrmTbl_tp3(dfIndicDefs = dfIndicDefs, indicators = indicators)

print(lsIndics[[sample(x = 1:length(lsIndics), size = 1)]])

for (indic in lsIndics) {
  assign(x = indic@name, value = indic, envir = indicators)
}

# ---------------------------------------- #
#save(indicators, file = paste(tmpdir, "indicators.Rda", sep = ""))
save(indicators, file = paste(rdadir, "indicators.Rda", sep = ""))


# ---------------------------------------- #
# DOODLES
# ---------------------------------------- #


