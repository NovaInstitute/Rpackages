

indicatorsToDataframe <- function(indicators) {

  lsdfs <- lapply(X = indicators, FUN = function(indic) {

    require(tidyr)
    require(dplyr)
    require(plyr)
    require(novaUtils)
    
    nm <- indic@name
    message(nm)

    # if (!("val" %in% slotNames(indic))) {
    #   return(NULL)
    # }
    if (is.null(names(indic@val))) {
      meanNm <- 1
    } else {
      meanNm <- intersect(c("Mean", "PointEst"), names(indic@val))
    }
    mean <- indic@val[[meanNm]]
    #message(mean)

    confIntv <- NA_character_
    if (!is.null(names(indic@val))) {
      if (length(intersect(c("95% CI L.", "Lower"), names(indic@val))) > 0) {
        confIntv <- sprintf("(%s,%s)",
                            indic@val[[intersect(c("95% CI L.", "Lower"), names(indic@val))]],
                            indic@val[[intersect(c("95% CI U.", "Upper"), names(indic@val))]])

      }
    }
    commnts <- ifelse(length(indic@comments) == 0, NA_character_, indic@comments)
    descr <- ifelse(length(indic@description) == 0, NA_character_, indic@description)

    nmsGroupCols <- setdiff(names(indic@val), c("PointEst", "Lower", "Upper", "n",
                                                "Mean", "95% CI L.", "95% CI U.",
                                                "Min.", "1st Qu.", "Median",
                                                "3rd Qu.", "Max.", "NA's", "Std. Dev"))
    if (length(nmsGroupCols) > 0) {

      dfRes <- data.frame(stringsAsFactors = FALSE,
                          subplace = indic@val[[nmsGroupCols]],
                          name = nm,
                          mean = paste(as.numeric(mean), confIntv, sep = " "), check.names = FALSE)
       
      dfRes$subplace <- toupper(dfRes$subplace)
      
      dfRes <- dfRes %>% spread(subplace, mean)
      dfRes$unit <- indic@unit
      dfRes[["All towns"]] = ""
      dfRes$description <- descr

    } else {

      dfRes <- data.frame(stringsAsFactors = FALSE,
                          name = nm,
                          unit = indic@unit,
                          "All towns" = paste(as.numeric(mean), confIntv, sep = " "),
                          description = descr, check.names = FALSE)
    }

    names(dfRes)[1] <- "name"

    return(dfRes)
  })

  library(gdata)
  table(sapply(X = lsdfs, FUN = ncol))
  dfIndics <- do.call("rbind.fill", lsdfs)

  dfIndics <- rename.vars(data = dfIndics, from = "name", to = "Indicator name")  
  dfIndics <- move.col(dfIndics, colName = "All towns", colIdx = 2)
  dfIndics <- move.col(dfIndics, colName = "unit", colIdx = 3)
  dfIndics <- move.col(dfIndics, colName = "description", colIdx = ncol(dfIndics))

  names(dfIndics) <- toupper(fixname(names(dfIndics)))
  
  #dfIndics <- dfIndics[order(dfIndics$name),]
  rownames(dfIndics) <- 1:nrow(dfIndics)

  # write.xlsx(x = dfIndics, file = paste(outputdir, "indicators_",
  #                                       gsub(pattern = "[[:punct:]]|[[:blank:]]",
  #                                            replacement = "",
  #                                            x = Sys.time()),
  #                                       ".xlsx",
  #                                       sep = ""))
  
  return(dfIndics)

}

# sirkelgrafiek-indikatore
# N_HHS_COAL_ANYSEAS_BYHOUSETP_SV_DES2017
# N_HHS_WOOD_ANYSEAS_BYHOUSETP_SV_DES2017
# N_HHS_PRFFN_ANYSEAS_BYHOUSETP_SV_DES2017
# N_HHS_ELEC_ANYSEAS_BYHOUSETP_SV_DES2017
# N_HHS_BYHOUSETP_SV_DES2017
# N_HHS_SOLIDF_ANYSEAS_BYHOUSETP_SV_DES2017
# KG_COAL_PCOALHH_PANNM_SV_DES2017
# KG_WOOD_PWOODHH_PANNM_SV_DES2017
# PERC_HHS_SRVCWASTECOLL_SV_DES2017
# PERC_HHS_SRVCWASTECOLL_SV_GHS2017
# N_HHS_WASTE_SV_DES2017
# N_HHS_WASTE_SV_GHS2017
