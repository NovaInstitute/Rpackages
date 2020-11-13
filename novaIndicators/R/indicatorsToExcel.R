

indicatorsToExcel <- function(indicators, outputdir) {

  lsdfs <- lapply(X = indicators, FUN = function(indic) {

    nm <- indic@name
    message(nm)

    if (!("val" %in% slotNames(indic))) {
      return(NULL)
    }
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
        confIntv <- sprintf("(%s, %s)",
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
                          name = nm,
                          mean = as.numeric(mean),
                          conf_int = confIntv,
                          comments = commnts,
                          description = descr)


      for (colnm in nmsGroupCols) {
        dfRes$name <- sprintf("%s_%s", dfRes$name, indic@val[[colnm]])
      }
      dfRes$name <- toupper(dfRes$name)

    } else {

      dfRes <- data.frame(stringsAsFactors = FALSE,
                          name = nm,
                          mean = mean,
                          conf_int = confIntv,
                          comments = commnts,
                          description = descr)
    }

    names(dfRes)[1] <- "name"

    return(dfRes)
  })

  table(sapply(X = lsdfs, FUN = ncol))
  dfIndics <- do.call("rbind", lsdfs)
  dfIndics <- dfIndics[order(dfIndics$name),]
  rownames(dfIndics) <- 1:nrow(dfIndics)

  write.xlsx(x = dfIndics, file = paste(outputdir, "indicators_",
                                        gsub(pattern = "[[:punct:]]|[[:blank:]]",
                                             replacement = "",
                                             x = Sys.time()),
                                        ".xlsx",
                                        sep = ""))

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
