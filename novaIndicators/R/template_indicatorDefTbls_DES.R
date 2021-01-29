#' Date created: 2018-03-22
#' Owner: NOVA Institute (Reg# 1994/002614/08)

# ---------------------------------------- #
#' This script prepares indicator definition tables for the Detailed Energy Survey (DES).
#' Three different definition tables are generated:
#'   1. A table for indicators from 'yes'/'no' response variables (e.g. 'energy_coal_use'), 
#'      called "type 1" indicators.
#'   2. A table for indicators from quantitative variables (e.g. 'energy_coal_consumption_wintercurrentunits),
#'      called "type 2" indicators
#'   3. A table for indicators generated from other indicators (e.g. 'TNNE_COAL_PCOMM_PMONTH_WINTR_SV_DES2017' which
#'      is generated from 'KG_COAL_PCOALHH_PMONTH_WINTR_SV_DES2017' and 'KG_COAL_PCOALHH_PMONTH_SUMMR_SV_DES2017'),
#'      called "type 3" indicators.
#'
#' Custom indicators can also be added to the tables using this script.
#' 
#' Example of use (not run):
#'   source("./global.R")
#'   load(paste(rdadir, "SV_DES2017.Rda", sep = ""))
#'   
#'   dfIndicDefs_tp1 <- indicDefs_tp1(dfHh = dfHh_SV_DES2017)
#'   write.xlsx(x = dfIndicDefs_tp1, file = "dfIndicDefs_tp1_DES.xlsx")
#'   
#'   dfIndicDefs_tp2 <- indicDefs_tp2(dfHh = dfHh_SV_DES2017)
#'   save(dfIndicDefs_tp2, file = "dfIndicDefs_tp2_DES.Rda")
#'    
#'   dfIndicDefs_tp3 <- indicDefs_tp3()
#'   assign(x = "dfIndicDefs_tp3_DES", value = dfIndicDefs_tp3, envir = .GlobalEnv)
#'

# ---------------------------------------- #
#' Generates the indicator definitions/descriptions for type 1 ('yes'/'no' vars) indicators
# TODO: add custom type 1 indicator definitions to the end of this function

#'@title indicDefs_tp1
#'@name indicDefs_tp1
#'
#'@description Generates the indicator definitions/descriptions for type 1 ('yes'/'no' vars) indicators
#'
#'@param dfHh The data frame/vector containing indicator definitions/descriptions for type 1 indicators.
#'@return data.frame
#'
#'@export


indicDefs_tp1 <- function(dfHh) {
  
  indicTypes1 <- c("n_hhs", "perc_hhs")
  nmsYesNoVars <- names(dfHh)[sapply(X = dfHh, 
                                            FUN = function(x) {all(unique(x) %in% c(NA, "yes", "no"))})]
  nmsExcludeVars <- grep(pattern = "respondent|agreement_to_continue|air_quality|greening|bathtime|source|merchant|ignition|container|money|preference|accidents|txt", x = nmsYesNoVars, value = TRUE)
  nmsIndicVars <- c(setdiff(nmsYesNoVars, nmsExcludeVars), NA_character_)
  nmsFilterVars <- c(grep(pattern = "_use$", x = nmsIndicVars, value = TRUE), NA_character_)
  groupVars <- c(NA_character_, "house_type")
  
  lsOpts <- list(indicTypes1, nmsIndicVars, nmsFilterVars, groupVars)
  dfAllPossIndics <- expand.grid(lsOpts, stringsAsFactors = FALSE)
  names(dfAllPossIndics) <- c("stat", "indicVar", "filters", "groupVar")
  idxx <- which((is.na(dfAllPossIndics$indicVar) & is.na(dfAllPossIndics$groupVar)) |
                  (dfAllPossIndics$indicVar == dfAllPossIndics$filters) |
                  (dfAllPossIndics$indicVar == dfAllPossIndics$groupVar) |
                  (is.na(dfAllPossIndics$indicVar) & !is.na(dfAllPossIndics$filters) & !is.na(dfAllPossIndics$groupVar))) 
  #View(dfAllPossIndics[idxx,])
  dfAllPossIndics <- dfAllPossIndics[-idxx,]
  
  # ------------------- #
  # Add some variables to dfAllPossIndics that we will need to build the 'name' and 'descr' vars
  
  energyCarriers <- c("coal", "wood", "paraffin", "lpg", "electricity", 
                      "solid_fuel", "dirty_fuel", "dung")
  items <- c(energyCarriers, "waste")
  
  # season
  dfAllPossIndics$season <- NA_character_
  for (seas in c("summer","winter")) {
    idxx <- grep(pattern = seas, x = dfAllPossIndics$indicVar)
    dfAllPossIndics$season[idxx] <- seas
  }
  idxx1 <- grep(pattern = paste(energyCarriers, sep = "|", collapse = "|"), 
                x = dfAllPossIndics$indicVar)
  idxx2 <- which(is.na(dfAllPossIndics$season))
  dfAllPossIndics$season[intersect(idxx1,idxx2)] <- "any_season"
  unique(dfAllPossIndics$indicVar[which(is.na(dfAllPossIndics$season))])
  
  # purpose / use case
  dfAllPossIndics$purpose <- NA_character_
  purposes <- c("heating", "cooking", "bathing", "ironing", "light", "igniting", "washing", "socialising", "braai", "barbeque")
  nmsUtVars <- grep(pattern = paste(purposes, sep = "|", collapse = "|"), 
                    x = dfAllPossIndics$indicVar, value = TRUE)
  nmsUtVars <- gsub(pattern = "energy_|utility_|_in_winter|_in_summer", replacement = "", x = nmsUtVars)
  nmsUtVars <- gsub(pattern = paste(items, sep = "|", collapse = "|"), replacement = "", x = nmsUtVars)
  nmsUtVars <- gsub(pattern = "^_{1,}|_{1,}$", replacement = "", x = nmsUtVars)
  nmsUtVars <- gsub(pattern = "_{2,}", replacement = "_", x = nmsUtVars)
  nmsUtVars <- setdiff(nmsUtVars, c("light_consumptionweek"))
  nmsUtVars <- unique(nmsUtVars)
  purposeMap <- c("heating_the_house_on_the_inside|heating_in_winter|heating_in_summer", "heating_space_inside",
                  "cooking", "cooking",
                  "braai|barbeque", "barbeque",
                  "bathing", "bathing",
                  "heating_water_for_washing_dishes", "heating_water_dishes",
                  "heating_water_to_wash_floors_walls", "heating_water_cleaning",
                  "ironing", "ironing",
                  "washing_laundry", "heating_water_laundry",
                  "heating_outside", "heating_space_outside",
                  "socialising", "socialising",
                  "igniting", "igniting",
                  "light|lighting", "lighting",
                  "space_heating", "heating_space_anywhere",
                  "heating_additional", "heating_water_additional")
  for (p in seq.int(from = 1, to = length(purposeMap)-1, by = 2)) {
    idxx <- grep(pattern = purposeMap[p], x = dfAllPossIndics$indicVar)
    dfAllPossIndics$purpose[idxx] <- purposeMap[p+1]
  }
  # P.S. we can never have different indicVars map to the same purpose (e.g. 'other') in this
  # case, because that would lead to duplicate indicator names for different indicators.
  
  # 'item' var
  dfAllPossIndics$item <- NA_character_
  for (item in items) {
    idxx <- grep(pattern = item, x = dfAllPossIndics$indicVar)
    dfAllPossIndics$item[idxx] <- item
  }
  
  # x-using hhs var
  dfAllPossIndics$subset <- NA_character_
  dfAllPossIndics$subset_item <- NA_character_
  for (item in items) {
    idxx <- grep(pattern = item, x = dfAllPossIndics$filters)
    dfAllPossIndics$subset_item[idxx] <- item
  }
  idxx <- which((!is.na(dfAllPossIndics$filters)))
  dfAllPossIndics$subset[idxx] <- sprintf("%s-using", dfAllPossIndics$subset_item[idxx])
  
  # ------------------- #
  # now build the actual 'descr' var
  dfAllPossIndics$descr <- sprintf("%s %s households %s %s %s %s %s. Main place: %s. Dataset: %s.",
                                   ifelse(dfAllPossIndics$stat %in% c("n_hhs"), 
                                          "Number of", "Percentage of"),
                                   ifelse(is.na(dfAllPossIndics$subset), "", dfAllPossIndics$subset),
                                   case_when(dfAllPossIndics$item == "waste" ~ "burning", 
                                             dfAllPossIndics$item %in% energyCarriers ~ "using",
                                             is.na(dfAllPossIndics$item) ~ ""),
                                   ifelse(is.na(dfAllPossIndics$item), "", dfAllPossIndics$item),
                                   ifelse(is.na(dfAllPossIndics$purpose), "", 
                                          sprintf("for %s", dfAllPossIndics$purpose)),
                                   ifelse(is.na(dfAllPossIndics$season), "", 
                                          sprintf("in %s", dfAllPossIndics$season)),
                                   ifelse(is.na(dfAllPossIndics$groupVar), "", 
                                          sprintf(", given by %s", dfAllPossIndics$groupVar)),
                                   "Sharpeville",
                                   "DES2017")
  dfAllPossIndics$descr <- gsub(pattern = "[[:blank:]]{2,}", replacement = " ", x = dfAllPossIndics$descr)
  dfAllPossIndics$descr <- gsub(pattern = "([[:blank:]])([[:punct:]])", replacement = "\\2", x = dfAllPossIndics$descr)
  dfAllPossIndics$descr <- gsub(pattern = "_", replacement = " ", x = dfAllPossIndics$descr, fixed = TRUE)
  
  # ------------------- #          
  # build the 'name' var for dfAllPossIndics
  # N_PRFFNHHS_ELEC_ANYSEAS_BYHOUSETP_SV_DES2017
  
  idxx <- which(!is.na(dfAllPossIndics$filters))
  dfAllPossIndics$stat2 <- gsub(pattern = "hhs", replacement = "", x = dfAllPossIndics$stat)
  dfAllPossIndics$stat2 <- sprintf("%s%shhs", 
                                   dfAllPossIndics$stat2, 
                                   ifelse(is.na(dfAllPossIndics$subset_item), "", dfAllPossIndics$subset_item))
  unique(dfAllPossIndics$stat2)
  
  dfAllPossIndics$name <- sprintf("%s_%s_%s_%s_%s_SV_DES2017",
                                  dfAllPossIndics$stat2, 
                                  ifelse(is.na(dfAllPossIndics$item), "", dfAllPossIndics$item),
                                  ifelse(is.na(dfAllPossIndics$season), "", dfAllPossIndics$season),
                                  ifelse(is.na(dfAllPossIndics$purpose), "", dfAllPossIndics$purpose),
                                  ifelse(is.na(dfAllPossIndics$groupVar), "", sprintf("by%s",dfAllPossIndics$groupVar)))
  
  abbrevs <- c("summer", "summr",
               "winter", "wintr",
               "any_season", "anyseas",
               "solid_fuel", "solidf",
               "dirty_fuel", "dirtyf",
               "heating_space_inside", "spcheatins",
               "heating_water_additional", "wtrheatadd",
               "heating_water_dishes", "dishes",
               "heating_water_cleaning", "cleaning",
               "heating_water_laundry", "laundry",
               "heating_space_outside", "spcheatouts",
               "heating_space_anywhere", "spcheatanyw",
               "electricity", "elec",
               "paraffin", "prffn",
               "solid_fuel", "solidf",
               "dirty_fuel", "dirtyf",
               "house_type", "housetp")
  
  for (i in seq.int(from = 1, to = length(abbrevs) -1, by = 2)) {
    dfAllPossIndics$name <- gsub(pattern = abbrevs[i], replacement = abbrevs[i+1], 
                                 x = dfAllPossIndics$name, fixed = TRUE)
  }
  
  dfAllPossIndics$name <- gsub(pattern = "_{2,}", replacement = "_", x = dfAllPossIndics$name)
  dfAllPossIndics$name <- toupper(dfAllPossIndics$name)
  sample(x = dfAllPossIndics$name, size = 20)
  
  ## temporary fix
  warning("TODO: remove this")
  unique(grep(pattern = "formal|informal|multi|mixed|ceiling|rdp", x = dfAllPossIndics$indicVar, value = TRUE))
  
  exclude <- c("for_anything", "fridge",
               "tv", "mobile_phone", "tablet", "geyser", "computer", "laptop",
               "notebook", "desktop", "sound_system", "kettle", "freezer",
               "energy_dung_season", "energy_dung_volumne", 
               "previous", "consumption", "format", "other", "explain", "igniting",
               "device", "location", "garden", "heater", 
               "energy_electricity_lighting", "energy_wood_combination_coal",
               "waste_burning", "combustion_material" ,"relocate",
               "formal|informal|multi|mixed|ceiling|rdp")
  x <- grep(pattern = paste(exclude, sep = "|", collapse = "|"), x = dfAllPossIndics$indicVar, value = TRUE)
  dfAllPossIndics <- dfAllPossIndics[which(!(dfAllPossIndics$indicVar %in% x)),]
  
  idxx <- which(dfAllPossIndics$indicVar == "waste_removal")
  if (length(idxx) > 0) {
    dfAllPossIndics$name[idxx] <- gsub(pattern = "WASTE", 
                                       replacement = "SRVCWASTECOLL", 
                                       x = dfAllPossIndics$name[idxx], fixed = TRUE)
    dfAllPossIndics$descr[idxx] <- gsub(pattern = "burning waste", 
                                        replacement = "with access to waste collection services", 
                                        x = dfAllPossIndics$descr[idxx], fixed = TRUE)
  }
  
  length(unique(dfAllPossIndics$name))
  if (length(unique(dfAllPossIndics$name)) < nrow(dfAllPossIndics)) {
    sample(x = unique(dfAllPossIndics$name[duplicated(dfAllPossIndics$name)]), size = 10)
  }
  
  # ------------------- #
  # create the 'unit' var in dfAllPossIndics
  dfAllPossIndics$unit <- case_when(dfAllPossIndics$stat %in% c("n_hhs") ~ "(count)",
                                    dfAllPossIndics$stat %in% c("perc_hhs") ~ "%")
  
  # ------------------- #
  # create the 'indicOpt' var in dfAllPossIndics
  dfAllPossIndics$indicOpt <- NA_character_
  dfAllPossIndics$indicOpt[which(dfAllPossIndics$indicVar %in% nmsYesNoVars)] <- "yes"
  table(dfAllPossIndics$indicOpt, exclude = NULL)
  
  # ------------------- #
  # create the 'comment' var
  dfAllPossIndics$comment <- NA_character_
  
  # ------------------- #
  # prepare 'filters' for input to indicatorMaker2 
  idxx <- which(!is.na(dfAllPossIndics$filters))
  dfAllPossIndics$filters[idxx] <- sprintf("%s == 'yes'", dfAllPossIndics$filters[idxx])
  
  # ------------------- #
  # remove the vars from dfAllPossIndics that we no longer need
  dfAllPossIndics <- dfAllPossIndics[,c("name", "unit", "indicVar", "indicOpt",
                                        "filters", "groupVar", "descr", "comment")]
  
  return(dfAllPossIndics)
}

# ---------------------------------------- #
#' Generates the indicator definitions/descriptions for type 2 (quantitative) indicators
# TODO: add custom type 2 indicator definitions to the end of this function


#'@title indicDefs_tp2
#'@name indicDefs_tp2
#'@description Generates the indicator definitions/descriptions for type 2 (quantitative) indicators
#'@param dfHh The data frame/vector containing indicator definitions/descriptions for type 2 (quantitative) indicators
#'@return data.frame
#'
#'@export


indicDefs_tp2 <- function(dfHh) {
  
  varNms <- grep(pattern = "consumption|comsumption|units|current|litres|cost|volumne|volume", 
                 x = names(dfHh), value = TRUE)
  varNms <- setdiff(varNms, c("energy_paraffin_format_volume", "energy_paraffin_format_cost"))
  indicTypes2 <- c("kg", "litre", "rand", "n_units")
  filters <- grep(pattern = "energy_[[:print:]]{1,}use$", x = names(dfHh), value = TRUE)
  filters <- setdiff(filters, grep(pattern = "last_use$", x = filters, value = TRUE))
  filters <- c(filters, NA_character_)
  groupVars <- c(NA_character_, "house_type")
  dfAllPossIndics <- expand.grid(varNms, groupVars, filters, stringsAsFactors = FALSE)
  names(dfAllPossIndics) <- c("indicVar", "groupVar", "filters")
  
  idxx <- which((is.na(dfAllPossIndics$indicVar) & is.na(dfAllPossIndics$groupVar)) |
                  (dfAllPossIndics$indicVar == dfAllPossIndics$filters) |
                  (dfAllPossIndics$indicVar == dfAllPossIndics$groupVar) |
                  (is.na(dfAllPossIndics$indicVar) & !is.na(dfAllPossIndics$filters) & !is.na(dfAllPossIndics$groupVar))) 
  if (length(idxx) > 0) {
    #View(dfAllPossIndics[idxx,])
    dfAllPossIndics <- dfAllPossIndics[-idxx,]
  }
  
  seasons <- c("summer", "winter", "any_season")
  energyCarriers <- c("coal", "wood", "paraffin", "lpg", "electricity", "solid_fuel", "dirty_fuel", "candles", "dung")	
  items <- c(energyCarriers, "waste", "waste_garden")
  periods <- c("day", "week", "month", "season", "annum", "event")
  
  # ------------------- #
  # create 'period' var in dfAllPossIndics
  dfAllPossIndics$period <- NA_character_
  for (period in periods) {
    idxx <- grep(pattern = period, x = dfAllPossIndics$indicVar, fixed = TRUE)
    dfAllPossIndics$period[idxx] <- period
  }
  others <- c("currentunits|dung_volumne", "month", FALSE,
              "cost_", "month", TRUE,
              "cost_winter_paraffin|cost_summer_paraffin", "week", FALSE,
              "waste_[[:print:]]{0,}burning", "event", FALSE)
  for (k in seq.int(from = 1, to = length(others), by = 3)) {
    idxx <- grep(pattern = others[k], x = dfAllPossIndics$indicVar, fixed = others[k+2])
    dfAllPossIndics$period[idxx] <- others[k+1]
  }
  #View(dfAllPossIndics[which(is.na(dfAllPossIndics$period)),])
  
  # ------------------- #
  # create 'season' var in dfAllPossIndics
  dfAllPossIndics$season <- NA_character_
  idxx <- grep(pattern = "summer", x = dfAllPossIndics$indicVar, fixed = TRUE)
  dfAllPossIndics$season[idxx] <- "summer"
  idxx <- grep(pattern = "winter", x = dfAllPossIndics$indicVar, fixed = TRUE)
  dfAllPossIndics$season[idxx] <- "winter"
  idxx1 <- which(is.na(dfAllPossIndics$season))
  idxx2 <- grep(pattern = paste(energyCarriers, sep = "|", collapse = "|"), 
                x = dfAllPossIndics$indicVar)
  idxx <- intersect(idxx1, idxx2)
  if (length(idxx) > 0) {
    dfAllPossIndics$season[idxx] <- "any_season"
  }
  unique(dfAllPossIndics$indicVar[which(is.na(dfAllPossIndics$season))])
  
  # ------------------- #
  # create 'unit' var in dfAllPossIndics
  dfAllPossIndics$unit <- NA_character_
  idxx <- grep(pattern = "units|candles_consumptionweek|dung_volumne|waste", x = dfAllPossIndics$indicVar)
  dfAllPossIndics$unit[idxx] <- "units"
  idxx <- grep(pattern = "_kg", x = dfAllPossIndics$indicVar, fixed = TRUE)
  dfAllPossIndics$unit[idxx] <- "kg"
  idxx <- grep(pattern = "cost", x = dfAllPossIndics$indicVar, fixed = TRUE)
  dfAllPossIndics$unit[idxx] <- "rand"
  idxx <- grep(pattern = "litres|light_paraffin", x = dfAllPossIndics$indicVar)
  dfAllPossIndics$unit[idxx] <- "litres"
  unique(dfAllPossIndics$indicVar[which(is.na(dfAllPossIndics$unit))])
  
  # ------------------- #
  # create the 'item' var in dfAllPossIndics
  dfAllPossIndics$item <- NA_character_
  for (item in items) {
    idxx <- grep(pattern = item, x = dfAllPossIndics$indicVar)
    dfAllPossIndics$item[idxx] <- item
  }
  unique(dfAllPossIndics$indicVar[which(is.na(dfAllPossIndics$item))])
  
  # ------------------- #
  # x-using hhs var
  dfAllPossIndics$subset <- NA_character_
  dfAllPossIndics$subset_item <- NA_character_
  for (item in items) {
    idxx <- grep(pattern = item, x = dfAllPossIndics$filters)
    dfAllPossIndics$subset_item[idxx] <- item
  }
  idxx <- which((!is.na(dfAllPossIndics$filters)))
  dfAllPossIndics$subset[idxx] <- sprintf("%s-using", dfAllPossIndics$subset_item[idxx])
  table(dfAllPossIndics$subset_item, exclude = NULL)
  unique(dfAllPossIndics$subset)
  # 'subset' will be used for building 'descr'
  
  dfAllPossIndics$subset2 <- gsub(pattern = "-using", replacement = "hh", x = dfAllPossIndics$subset)
  dfAllPossIndics$subset2[is.na(dfAllPossIndics$subset2)] <- "hh"
  unique(dfAllPossIndics$subset2)
  # 'subset2' will be used for building 'name'
  
  # ------------------- #
  # generate the indicator names
  #KG_COAL_PHH_PMONTH_WINTR_SV_DES2017
  #KG_COAL_PUSINGHH_PMONTH_WINTR_BYHOUSETP_SV_DES2017
  dfAllPossIndics$name <- toupper(sprintf("%s_%s_P%s_%s_%s_%s_SV_DES2017",
                                          dfAllPossIndics$unit,
                                          dfAllPossIndics$item,
                                          dfAllPossIndics$subset2,
                                          ifelse(is.na(dfAllPossIndics$period), "", sprintf("p%s",dfAllPossIndics$period)),
                                          ifelse(is.na(dfAllPossIndics$season), "", dfAllPossIndics$season),
                                          ifelse(is.na(dfAllPossIndics$groupVar), "", 
                                                 sprintf("by%s",dfAllPossIndics$groupVar))))
  subs <- c("WINTER", "WINTR",
            "SUMMER", "SUMMR",
            "ANY_SEASON", "ANYSEAS",
            "BYHOUSE_TYPE", "BYHOUSETP",
            "WASTE_GARDEN", "WASTEGRDN",
            "WASTE_", "WASTEHH_",
            toupper(c("solid_fuel", "solidf",
                      "dirty_fuel", "dirtyf",
                      "electricity", "elec",
                      "paraffin", "prffn",
                      "solid_fuel", "solidf",
                      "dirty_fuel", "dirtyf")))
  
  for (i in seq.int(from = 1, to = length(subs), by = 2)) {
    dfAllPossIndics$name <- gsub(pattern = subs[i], 
                                 replacement = subs[i+1], 
                                 x = dfAllPossIndics$name, 
                                 fixed = TRUE)
  }
  dfAllPossIndics$name <- gsub(pattern = "_{2,}", replacement = "_", x = dfAllPossIndics$name)
  
  length(unique(dfAllPossIndics$name))
  if (length(unique(dfAllPossIndics$name)) != nrow(dfAllPossIndics)) {
    unique(dfAllPossIndics$name[duplicated(dfAllPossIndics$name)])
  }
  sample(x = dfAllPossIndics$name, size = 20)
  
  # ------------------- #
  # generate the 'descr' var to dfAllPossIndics
  dfAllPossIndics$descr <- sprintf("The amount of %s %s per %s household per %s %s, measured in %s %s. Main place: Sharpeville. Dataset: DES2017.",
                                   dfAllPossIndics$item,
                                   ifelse(dfAllPossIndics$item %in% energyCarriers, "used", "burned"),
                                   ifelse(is.na(dfAllPossIndics$subset), "", dfAllPossIndics$subset),
                                   dfAllPossIndics$period,
                                   ifelse(is.na(dfAllPossIndics$season), "", 
                                          sprintf("during %s", dfAllPossIndics$season)),
                                   dfAllPossIndics$unit,
                                   ifelse(is.na(dfAllPossIndics$groupVar), "", 
                                          sprintf(", given by %s", dfAllPossIndics$groupVar)))
  
  subs <- c("[[:blank:]]{2,}", " ",
            "lpg", "LPG",
            "house_type", "house type",
            "rand", "Rand",
            "([[:blank:]])([[:punct:]])", "\\2")
  for (i in seq.int(from = 1, to = length(subs) -1, by = 2)) {
    dfAllPossIndics$descr <- gsub(pattern = subs[i], 
                                  replacement = subs[i+1], 
                                  x = dfAllPossIndics$descr)
  }
  sample(x = dfAllPossIndics$descr, size = 10)
  
  # ---------------------------------------- #
  # add a 'comment' var
  dfAllPossIndics$comment <- NA_character_  
  
  # ---------------------------------------- #
  # finish off the 'filters' var for input to the conveyor belt
  idxx <- which(!is.na(dfAllPossIndics$filters))
  dfAllPossIndics$filters[idxx] <- sprintf("%s == 'yes'", dfAllPossIndics$filters[idxx])
  unique(dfAllPossIndics$filters)
  
  # ---------------------------------------- #
  # remove the variables from dfAllPossIndics that we no longer need
  dfAllPossIndics <- dfAllPossIndics[,c("name", "unit", "indicVar", "filters", "groupVar", "descr", "comment")]
  
  # ---------------------------------------- #
  return(dfAllPossIndics)
}

# ---------------------------------------- #
#' Generates the indicator definitions/descriptions for type 3 (indicator X indicator) indicators
# TODO: add custom type 3 indicator definitions to the end of this function

#'@title indicDefs_tp3
#'@name indicDefs_tp3
#'@description Generates the indicator definitions/descriptions for type 3 (indicator X indicator) indicators
#'@export

indicDefs_tp3 <- function() {
  
  dfIndicDefs <- data.frame(stringsAsFactors = FALSE,
                            name = c("TNNE_COAL_PCOMM_PMONTH_WINTR_SV_DES2017",
                                     "TNNE_COAL_PCOMM_PMONTH_SUMMR_SV_DES2017",
                                     "TNNE_WOOD_PCOMM_PMONTH_WINTR_SV_DES2017",
                                     "TNNE_WOOD_PCOMM_PMONTH_SUMMR_SV_DES2017",
                                     "KLITRS_PRFFN_PCOMM_PMONTH_WINTR_SV_DES2017",
                                     "KLITRS_PRFFN_PCOMM_PMONTH_SUMMR_SV_DES2017",
                                     "TNNE_COAL_PCOMM_PANNM_SV_DES2017",
                                     "TNNE_WOOD_PCOMM_PANNM_SV_DES2017",
                                     "KLITRS_PRFFN_PCOMM_PANNM_SV_DES2017",
                                     "KG_COAL_PCOALHH_PANNM_SV_DES2017",
                                     "KG_WOOD_PWOODHH_PANNM_SV_DES2017",
                                     "LITRES_PRFFN_PPRFFNHH_PANNM_SV_DES2017"),
                            unit = c(rep("tonne",4), "kilolitres", "kilolitres", 
                                     "tonne", "tonne", "kilolitres", "kg", "kg", "litres"),
                            indic1 = c("KG_COAL_PCOALHH_PMONTH_WINTR_SV_DES2017",
                                       "KG_COAL_PCOALHH_PMONTH_SUMMR_SV_DES2017",
                                       "KG_WOOD_PWOODHH_PMONTH_WINTR_SV_DES2017",
                                       "KG_WOOD_PWOODHH_PMONTH_SUMMR_SV_DES2017",
                                       "LITRES_PRFFN_PPRFFNHH_PWEEK_WINTR_SV_DES2017",
                                       "LITRES_PRFFN_PPRFFNHH_PWEEK_SUMMR_SV_DES2017",
                                       "TNNE_COAL_PCOMM_PMONTH_WINTR_SV_DES2017",
                                       "TNNE_WOOD_PCOMM_PMONTH_WINTR_SV_DES2017",
                                       "KLITRS_PRFFN_PCOMM_PMONTH_WINTR_SV_DES2017",
                                       "KG_COAL_PCOALHH_PMONTH_WINTR_SV_DES2017",
                                       "KG_WOOD_PWOODHH_PMONTH_WINTR_SV_DES2017",
                                       "LITRES_PRFFN_PPRFFNHH_PWEEK_WINTR_SV_DES2017"),
                            indic2 = c("N_HHS_COAL_WINTR_SV_DES2017",
                                       "N_HHS_COAL_SUMMR_SV_DES2017",
                                       "N_HHS_WOOD_WINTR_SV_DES2017",
                                       "N_HHS_WOOD_SUMMR_SV_DES2017",
                                       "N_HHS_PRFFN_WINTR_SV_DES2017",
                                       "N_HHS_PRFFN_SUMMR_SV_DES2017",
                                       "TNNE_COAL_PCOMM_PMONTH_SUMMR_SV_DES2017",
                                       "TNNE_WOOD_PCOMM_PMONTH_SUMMR_SV_DES2017",
                                       "KLITRS_PRFFN_PCOMM_PMONTH_SUMMR_SV_DES2017",
                                       "KG_COAL_PCOALHH_PMONTH_SUMMR_SV_DES2017",
                                       "KG_WOOD_PWOODHH_PMONTH_SUMMR_SV_DES2017",
                                       "LITRES_PRFFN_PPRFFNHH_PWEEK_SUMMR_SV_DES2017"),
                            operation = c(rep("round((VAR1 * VAR2) / 1000, 2)", 6), 
                                          rep("round((VAR1 *4 + VAR2 *8))",3),
                                          rep("round(VAR1 *4 + VAR2 *8,2)",2),
                                          "round((VAR1 *N_WEEKS_PM *4) + (VAR2 * N_WEEKS_PM *8),2)"),
                            descr = NA_character_)
  
  dfIndicDefs$item <- c("coal", "coal", "wood", "wood", "paraffin", "paraffin", 
                        rep(c("coal", "wood", "paraffin"),2))
  dfIndicDefs$period <- c(rep("pmonth",6), rep("pannum",6))
  dfIndicDefs$season <- c(rep(c("winter", "summer"),3), rep(NA_character_,6))
  
  dfIndicDefs$usergroup <- NA_character_
  for (grp in c("PCOMM", "PCOALHH", "PWOODHH", "PPRFFNHH")) {
    idxx <- grep(pattern = grp, x = dfIndicDefs$name, fixed = TRUE)
    dfIndicDefs$usergroup[idxx] <- switch(grp,
                                          PCOMM = "community",
                                          PCOALHH = "coal-using household",
                                          PWOODHH = "wood-using household",
                                          PPRFFNHH = "paraffin-using household")
  }
  
  dfIndicDefs$descr <- sprintf("Amount of %s used per %s %s%s, measured in %s. Main place: Sharpeville. Dataset: DES2017.",
                               dfIndicDefs$item, 
                               dfIndicDefs$usergroup,
                               gsub(pattern = "^p", replacement = "per ", x = dfIndicDefs$period),
                               ifelse(is.na(dfIndicDefs$season), "", sprintf(" during %s", dfIndicDefs$season)),
                               dfIndicDefs$unit)

  # ------------------- #
  # add 'comment' var
  dfIndicDefs$comment <- NA_character_
  
  # ------------------- #
  # remove vars from dfIndicDefs that we no longer need  
  dfIndicDefs <- dfIndicDefs[,c("name", "unit", "indic1", "indic2", "operation", "descr", "comment")]
  
  return(dfIndicDefs)
  
}

