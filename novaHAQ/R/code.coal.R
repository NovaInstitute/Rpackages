#' Unfactor
#'
#' Helper function for code.coal that creates a numeric vector from a factor
#'
#' @param x Data that will be converted
#' @export

unfactor <- function(x) {
    if (is.factor(x)) {
        as.numeric(unclass(levels(x)))[as.numeric(x)]
    } else {
        x
    }
}

#' Code Coal
#'
#' The main use of this function is to calculate the amount of savings to be attributed to
#' the BM method of lighting fires. The function modifies the provided data frame by adding
#' additional cost saving variables and assigning the completed data frame to the global environment
#'
#' @param haq Data frame containing survey data
#' @param verbose Logical to display function messages
#' @param units Character vector referring to the variables that contain coal units used in
#' winter and summer before and after BM
#' @param current.var Character vector referring to the variable of current coal used in summer
#' @param summercoal.var The coal used in summer as character vector
#' @param summersavevar The variable containing data relating to coal and BM use during summer as
#' character vector
#' @param wintersavevar The variable containing data relating to coal and BM use during winter as
#' character vector
#' @param summersaveval The value relating to no savings during summer as character vector
#' @param wintersaveval The value relating to no savings during winter as character vector
#' @param estimate.summer Logical that makes use of summer estimation when TRUE
#' @param summer.est.var The summer estimation variable as character vector
#' @param summer.est.value The summer estimation value as character vector
#' @param summer.yes.value The value corresponding to yes for summer coal use as character vector
#' @param summer.no.value The value corresponding to no for summer coal use as character vector
#' @param estimate.winter Logical that makes use of winter estimation when TRUE
#' @param winter.est.var The winter estimation variable as character vector
#' @param winter.est.value The winter estimation value as character vector
#' @export

code.coal <- function(haq,
                      verbose = FALSE,
                      units = c("coal.units.winter.before.bm",
                                "coal.units.summer.before.bm",
                                "coal.units.winter.after.bm",
                                "coal.units.summer.after.bm"),
                      current.var = "current.coal.units.summer",
                      summercoal.var = "coal.use.in.summer",
                      summersavevar = "bm.saved.coal.summer", wintersavevar = "bm.saved.coal.winter",
                      summersaveval = "No", wintersaveval = "No",
                      estimate.summer = FALSE,
                      summer.est.var = "bm.saved.coal.summer",
                      summer.est.value = "It is not yet summer, or I only started recently and cannot tell exactly",
                      summer.yes.value = "Yes",
                      summer.no.value = "No",
                      estimate.winter = FALSE,
                      winter.est.var = "bm.saved.coal.winter",
                      winter.est.value = "I have only started recently and cannot tell exactly") {

    suppressMessages(require(gdata))
    # Non BM user cannot have a use before and afterBM
    nonbmidx = which(haq$hh.BM.use != "BM" & haq$hh.BM.use != "BM other" & haq$hh.BM.use != "BM old")
    if (verbose == TRUE)
        message("nonbmidx=  ", length(nonbmidx))
    if (verbose == TRUE)
        message("unit names: ", paste(names(haq)[match(units, names(haq))], sep = " "), " ")
    haq[nonbmidx, match(units, names(haq))] = NA

    # If someone say they did not save it means that the use before and after BM is the same and the savings is zero and eff = 1
    nosummersaveidx = which(haq[,summersavevar] == summersaveval)
    if (length(nosummersaveidx) > 0){
      haq[nosummersaveidx,units[2]] = haq[nosummersaveidx, current.var]
      haq[nosummersaveidx,units[4]] = haq[nosummersaveidx, current.var]
    }

    nowintersaveidx = which(haq[,wintersavevar] == wintersaveval)
    if (length(nowintersaveidx) > 0){
      haq[nowintersaveidx,units[2]] = haq[nowintersaveidx, current.var]
      haq[nowintersaveidx,units[4]] = haq[nowintersaveidx, current.var]
    }

        # Calculate average winter eef to use to give the average proportional saving to the users who are not confident to give
    # their savings
    eef.raw = with(haq[which(haq$hh.BM.use == "BM"), ], unfactor(haq$coal.units.winter.before.bm)/unfactor(haq$coal.units.winter.after.bm))
    eef = mean(eef.raw[which(eef.raw < 2)], na.rm = TRUE)
    if (verbose == TRUE)
        message("eef ", eef)

    # Calculate average summer eef to use to give the average proportional saving to the users who are not confident to give
    # their savings
    eef.s.raw = with(haq[which(haq$hh.summer.BM.use == "BM"), ], unfactor(haq$coal.units.summer.before.bm)/unfactor(haq$coal.units.summer.after.bm))
    eef.s = mean(eef.s.raw[which(eef.s.raw < 2)], na.rm = TRUE)
    if (verbose == TRUE)
        message("eef.s ", round(eef.s,2) )

    # if someone has winter eef but no summer but they use coal in summer the summer eef is the same as winter eef
    fxdx =  which(is.na(haq$eef.s)  & !is.na(haq$eef) & haq$hh.summer.BM.use == "BM")
    if (length(fxdx) > 0) haq[fxdx, "eef.s"] <- haq[fxdx, "eef"]

    # energy efficiency factor
    haq$eef = unfactor(haq$coal.units.winter.before.bm)/unfactor(haq$coal.units.winter.after.bm)
    haq$eef.s = unfactor(haq$coal.units.summer.before.bm)/unfactor(haq$coal.units.summer.after.bm)
    # make sure there are no Inf (i.e. x/0) responses
    haq[which(haq$eef.s == Inf), c("eef.s")] = NA

    # There are cases where the user could not yet estimate summer use
    if (estimate.summer == TRUE) {
        if (verbose == TRUE)
            message("\nMaking use of summer estimation")
        # index of applicable records: Their summer before and after and current
        # is NA. This only goes for BM.summer.users. The others have to get zeros
        s.e.idx = which((haq[, summer.est.var] == summer.est.value | is.na(haq[, summer.est.var])) &
          is.na(haq[ ,current.var]) == TRUE &
          is.na(haq[ ,units[2]]) ==  TRUE &
          is.na(haq[ ,units[4]]) ==  TRUE &
          haq$hh.summer.BM.use == "BM")

        if (length(s.e.idx) > 0) {
          if (verbose == TRUE)
            message("Estimating summer use form means for ", length(s.e.idx), " cases")
            haq[s.e.idx, current.var] = mean(haq[which(haq$hh.summer.BM.use == "BM"), current.var],
                na.rm = TRUE)
            haq[s.e.idx, units[4]] = mean(haq[which(haq$hh.summer.BM.use == "BM"), current.var], na.rm = TRUE)
            haq[s.e.idx, units[4]] = haq[s.e.idx, units[4]] * eef.s
            haq[s.e.idx, "estimate"] = TRUE
        }

        # index of those who have a summer current and only need to apply eef to get a before
        s.c.ok.idx = which((haq[, summer.est.var] == summer.est.value | is.na(haq[, summer.est.var]))&
          is.na(haq[ ,current.var]) == FALSE &
          is.na(haq[, units[2]]) == TRUE &
          is.na(haq[, units[4]]) == TRUE &
          haq$hh.summer.BM.use == "BM")
        if (verbose==TRUE) message("Summer OK index contains ", length(s.c.ok.idx), " cases" )

        if (length(s.c.ok.idx) > 0) {
          message("Estimating ", length(s.c.ok.idx), " records using mean eef for summer")
            haq[s.c.ok.idx, units[4]] = haq[s.c.ok.idx, current.var]  # assume a flat baseline
            haq[s.c.ok.idx, units[2]] = haq[s.c.ok.idx, units[4]] * eef.s  # assume average summer eef
            haq[s.c.ok.idx, "estimate"] = TRUE
          message("Summer estimation applied to ",length(s.c.ok.idx), " records")
          message(units[4], " has " , nobs(haq[, units[4]]), " observations")
          message(units[2], " has " , nobs(haq[, units[2]]), " observations")
          message(current.var, " has " , nobs(haq[ ,current.var]), " observations\n")
        }

        # For those who have before and after but not current: apply a flat baseline

        # For those who have everything except coal.units.summer.before.bm.0
        s.one.ok.idx = which((haq[, summer.est.var] == summer.est.value | is.na(haq[, summer.est.var]))&
          is.na(haq[ ,current.var]) == FALSE &
          is.na(haq[ ,units[2]]) == TRUE &
          is.na(haq[ ,units[4]]) == FALSE &
          haq$hh.summer.BM.use == "BM")
        if (verbose==TRUE) message("Summer one OK index", head(s.one.ok.idx) )

        if (length(s.one.ok.idx) > 0) {
          message("Estimating ", length(s.one.ok.idx), " records using mean eef")
          haq[s.one.ok.idx, units[2]] = haq[s.one.ok.idx, units[4]] * eef.s  # assume average summer eef
          haq[s.one.ok.idx, "estimate"] = TRUE
          message("Summer estimation applied to ",length(s.one.ok.idx), " records")
          message(units[4], " has " , nobs(haq[, units[4]]), " observations")
          message(units[2], " has " , nobs(haq[, units[2]]), " observations")
          message(current.var, " has " , nobs(haq[ ,current.var]), " observations\n")
        }

        # for those who only miss summer: fix it outside the function with which(haq$hh.summer.BM.use=="BM" & is.na(haq$bm.saved.coal.summer))

        # as summervar = "No" is voor en na en huidig dieselfde en eef.s = 1
        s.c.no.idx = which(haq[, summer.est.var] == summer.no.value &
          is.na(haq[ ,current.var]) == FALSE &
          is.na(haq[ ,units[2]]) == TRUE &
          is.na(haq[ ,units[4]]) == TRUE &
          haq$hh.summer.BM.use == "BM")
        if (verbose==TRUE) message("Summer No index", head(s.c.no.idx) )

        if (length(s.c.no.idx) > 0) {
          if (verbose==TRUE) message("Estimating ", length(s.c.no.idx), " records using flat baseline approach on current use")
          haq[s.c.no.idx, units[4]] = haq[s.c.no.idx, current.var]  # assume a flat baseline
          haq[s.c.no.idx, units[2]] = unfactor(haq[s.c.no.idx, units[4]]) * 1  # assume average summer eef
          haq[s.c.no.idx, 'eef.s'] = 1
          haq[s.c.no.idx, "estimate"] = TRUE
          message("Summer estimation applied to ",length(s.c.no.idx), " records")
          message(units[4], " has " , nobs(unfactor(haq[, units[4]])), " observations")
          message(units[2], " has " , nobs(unfactor(haq[, units[2]])), " observations")
          message(current.var, " has " , nobs(unfactor(haq[ ,current.var])), " observations\n")
        }

      message("leaving summer estimation \n")
    }

    if (estimate.winter == TRUE) {
        if (verbose == TRUE)
            message("Making use of winter estimation")
        # index of applicable records: Their winter before and after BM is currently NA
        w.e.idx = which(haq[, winter.est.var] == winter.est.value &
          haq$hh.BM.use == "BM" & is.na(haq$current.coal.units.winter) == TRUE &
          is.na(haq[, units[1]]) == TRUE &
          is.na(haq[, units[3]]) == TRUE)

          if (length(w.e.idx) > 0){
            message("Winter coal non-existent, use mean values: ", length(w.e.idx))
            haq[w.e.idx, "current.coal.units.winter"] = mean(haq$current.coal.units.winter[which(haq$hh.BM.use == "BM")], na.rm = TRUE)
            haq[w.e.idx, units[3]] = mean(haq$current.coal.units.winter[which(haq$hh.BM.use == "BM")], na.rm = TRUE)
            haq[w.e.idx, units[1]] = haq[w.e.idx, units[3]] * eef
            haq[w.e.idx, "estimate"] = TRUE
          }
         # Hier moet ons nou die w.c.ok.idx maak
        w.c.ok.idx = which(haq[, winter.est.var] == winter.est.value &
          is.na(haq$current.coal.units.winter) == FALSE &
          is.na(haq[, units[1]]) == TRUE &
          is.na(haq[, units[3]]) == TRUE &
          haq$hh.BM.use == "BM")

        if (length(w.c.ok.idx) > 0) {
        message("Winter coal OK so use mean eef: ", length(w.c.ok.idx))
          haq[w.c.ok.idx, units[3]] = haq[w.c.ok.idx, "current.coal.units.winter"]  # assume a flat baseline
          haq[w.c.ok.idx, units[1]] = haq[w.c.ok.idx, units[3]] * eef  # assume average winter eef
          haq[w.c.ok.idx, "estimate"] = TRUE
          message("Winter estimation applied to ",length(w.c.ok.idx), " records \n")
        }
      message("Leaving winter estimation \n")
    }

    # In rare cases where no current use exist but use after BM is given. Conservatively assume flat baseline

    haq[which(haq$current.coal.units.winter == 0 & haq$hh.BM.use == "BM"), "current.coal.units.winter"] = haq[which(haq$current.coal.units.winter ==
        0 & haq$hh.BM.use == "BM"), "coal.units.winter.after.bm"]
    ### winter Convert to to kg - only for legitimate BM users
    haq$winter.before.bm.kg <- unfactor(haq$coal.units.winter.before.bm) * haq$kg
    if (verbose == TRUE)
      message("winter.before.bm.kg mean = ", summary(haq$winter.before.bm.kg)[4], " nobs= ", nobs(haq$winter.before.bm.kg) , " of whom " ,
              table(is.na(haq$winter.before.bm.kg), haq$hh.BM.use)[1], " use BM")

    haq$winter.before.bm.kg[which(haq$hh.BM.use != "BM")] = NA
    if (verbose == TRUE)
      message("winter.before.bm.kg after setting non-BM to NA mean = ", summary(haq$winter.before.bm.kg)[4], " nobs= ", nobs(haq$winter.before.bm.kg) , " of whom " ,
              table(is.na(haq$winter.before.bm.kg), haq$hh.BM.use)[1], " use BM")

    haq$totalwinter.before.bm.kg <- 4 * haq$winter.before.bm.kg

    haq$summer.before.bm.kg <- unfactor(haq$coal.units.summer.before.bm) * haq$kg
    if (verbose == TRUE)
      message("kg", mean(haq$kg, na.rm = TRUE),
              "\ncoal.units.summer.before.bm", mean(haq$coal.units.summer.before.bm, na.rm = TRUE),
              "\nsummer.before.bm.kg mean = ", summary(haq$summer.before.bm.kg)[4],
              " nobs = ", nobs(haq$summer.before.bm.kg) , " of whom " ,
              table(is.na(haq$summer.before.bm.kg), haq$hh.BM.use)[1], " use BM")

    haq$summer.before.bm.kg[which(haq$hh.BM.use != "BM")] = NA
    if (verbose == TRUE)
      message("summer.before.bm.kg after setting non-BM to NA mean = ", summary(haq$summer.before.bm.kg)[4], " nobs = ", nobs(haq$summer.before.bm.kg) , " of whom " ,
              table(is.na(haq$summer.before.bm.kg), haq$hh.BM.use)[1], " use BM")

    haq$totalsummer.before.bm.kg <- 8 * haq$summer.before.bm.kg
    if (verbose == TRUE)
        message("units before to kg \n")

    haq$winter.save.initial.kg <- unfactor(haq$coal.saved.initial.winter) * haq$kg
    haq$totalwinter.save.initial.kg <- 4 * haq$winter.save.initial.kg
    if (verbose == TRUE)
      message("winter.save.initial.kg ", summary(haq$winter.save.initial.kg)[4], " ", nobs(haq$winter.save.initial.kg))

    haq$summer.save.initial.kg <- unfactor(haq$coal.saved.initial.summer) * haq$kg
    haq$totalsummer.save.initial.kg <- 8 * haq$summer.save.initial.kg
    if (verbose == TRUE)
        message("summer.save.initial.kg ", summary(haq$summer.save.initial.kg)[4], " ", nobs(haq$summer.save.initial.kg))

    # define proportion of winter days - only for legitimate BM users
    haq$winterdays.prop <- haq$winterdays.used/fullwinter
    if (verbose == TRUE)
        message("haq$winterdays.prop ", summary(haq$winterdays.prop)[4], " ", nobs(haq$winterdays.prop) , "\n")

    # Current winter use in kg: Only for legitimate BM users who are confident to give their use
    haq$winter.current.kg <- ifelse(haq$hh.BM.use == "BM" | haq$hh.BM.use == "Historic BM",
                                    unfactor(haq[ ,grep("current.coal.units.wint",names(haq))[1]]) * haq$kg,
                                    NA)
    haq$winter.current.kg[which(haq$bm.saved.coal.winter == "Nobody in this house uses BM")] <- NA
    haq$totalwinter.current.kg <- 4 * haq$winter.current.kg  # Full year winter baseline
    if (verbose == TRUE)
        message("winter.current.kg mean = ", summary(haq$winter.current.kg)[4], " nobs= ", nobs(haq$winter.current.kg) , " of whom " ,
                table(is.na(haq$winter.current.kg), haq$hh.BM.use)[1], " use BM")

    ### summer

    haq$summerdays.prop <- haq$summerdays.used/fullsummer
    if (verbose) message("mean summerdays.prop: ", mean(haq$summerdays.prop, na.rm = TRUE))
    haq$summer.current.kg <- ifelse(haq$hh.summer.BM.use == "BM" | haq$hh.summer.BM.use == "Historic BM",
                                    unfactor(haq [ , grep("current.coal.units.sum", names(haq))[1]]) *  haq$kg,
                                    NA)
    haq$totalsummer.current.kg <- 8 * haq$summer.current.kg
    if (verbose == TRUE)
        message("summer current.kg mean = ", summary(haq$summer.current.kg)[4], " nobs = ", nobs(haq$summer.current.kg),
                " of whom ", table(is.na(haq$summer.current.kg), haq$hh.BM.use)[1], " use BM")

    haq$summer.current.kg[which(haq$hh.BM.use != "BM")] = NA
    if (verbose == TRUE)
      message("summer current.kg after mean = ", summary(haq$summer.current.kg)[4], " nobs = ", nobs(haq$summer.current.kg),
              " of whom ", table(is.na(haq$summer.current.kg), haq$hh.BM.use)[1], " use BM")

    haq$winter.after.bm.kg <- unfactor(haq$coal.units.winter.after.bm) * haq$kg
    if (verbose == TRUE)
      message("winter.after.bm.kg ", summary(haq$winter.after.bm.kg)[4], " ", nobs(haq$winter.after.bm.kg))

    haq$winter.after.bm.kg[which(haq$hh.BM.use != "BM")] = NA
    if (verbose == TRUE)
      message("winter.after.bm.kg set to NA for non-BM users")
      message("winter.after.bm.kg ", summary(haq$winter.after.bm.kg)[4], " ", nobs(haq$winter.after.bm.kg))
    haq$totalwinter.after.bm.kg <- 4 * haq$winter.after.bm.kg

    haq$summer.after.bm.kg <- unfactor(haq[ ,units[4]]) * haq$kg
    haq$totalsummer.after.bm.kg <- 8 * haq$summer.after.bm.kg
    if (verbose == TRUE)
        message("summer.after.bm.kg ", summary(haq$summer.after.bm.kg)[4], " ", nobs(haq$summer.after.bm.kg), "\n")

    ### WINTERBASELINE - only for legitimate BM users
    haq$winterbase <- haq$winter.before.bm.kg/haq$winter.after.bm.kg * haq$winter.current.kg
    haq$winterbase[which(haq$bm.saved.coal.winter == "Nobody in this house uses BM")] <- NA
    haq$total.winterbase <- 4 * haq$winterbase
    haq$winter.save = haq$winterbase - haq$winter.current.kg
    if (verbose == TRUE)
        message("winterbase mean = ", summary(haq$winterbase)[4], " nobs= ", nobs(haq$winterbase), " and ", table(is.na(haq$winterbase), haq$hh.BM.use)[1,1] , " use BM" )

    # Comparison of reported initial winter savings and calculated winter savings
    haq$calculated.winter.save <- haq$winter.before.bm.kg - haq$winter.after.bm.kg
    haq$total.calculated.winter.save <- 4 * haq$calculated.winter.save
    haq$diff1 <- haq$winter.save.initial.kg - haq$calculated.winter.save

    # Scenario of rising baseline
    haq$diff2 <- haq$winter.current.kg - haq$winter.after.bm.kg
    haq$winter.save <- ifelse(haq$diff2 > 0, haq$calculated.winter.save, haq$winter.save)
    haq$total.winter.save <- 4 * haq$winter.save

    # Conservartive winter baseline that incorporates disqualification of rising baseline
    haq$winterconbase <- haq$winter.current.kg + haq$winter.save
    haq$winterconbase.year <- 4 * haq$winterdays.prop * haq$winterconbase
    if (verbose == TRUE)
      message("winterconbase.year mean = ", summary(haq$winterconbase.year)[4], " nobs = ", nobs(haq$winterconbase.year))

    haq$winter.current.year <- 4 * haq$winterdays.prop * haq$winter.current.kg
    haq$winter.save.year <- 4 * haq$winterdays.prop * haq$winter.save
    if (verbose == TRUE)
      message("winter.current.year mean = ", summary(haq$winter.current.year)[4], " nobs = ", nobs(haq$winter.current.year),
              " of whom ", table(is.na(haq$winter.current.year), haq$hh.BM.use)[1], " use BM" )

    haq$winter.current.year[which(haq$hh.BM.use != "BM")] = NA
    if (verbose == TRUE)
      message("winter.current.year after setting non-B to NA: mean = ", summary(haq$winter.current.year)[4],
              " nobs = ", nobs(haq$winter.current.year), "\n")


    # SUMMER BASELINE - only for legitimate BM users

    haq$summerbase <- ifelse(haq$summer.after.bm.kg != 0, haq$summer.before.bm.kg/haq$summer.after.bm.kg * haq$summer.current.kg,
        0)
    haq$summerbase[which(haq$bm.saved.coal.summer == "Nobody in this house uses BM")] <- NA
    haq$total.summerbase <- 8 * haq$summerbase
    if (verbose == TRUE)
      message("summerbase mean = ", summary(haq$summerbase)[4], " nobs= ", nobs(haq$summerbase), " and ", table(is.na(haq$summerbase), haq$hh.BM.use)[1,1] , " use BM" )

    haq$summer.save = ifelse(is.na(haq$summerbase) == FALSE | is.na(haq$summer.current.kg) == FALSE, haq$summerbase - haq$summer.current.kg,
        NA)
    if (verbose == TRUE)
      message("summersave mean = ", summary(haq$summer.save)[4], " nobs= ", nobs(haq$summer.save), " and ", table(is.na(haq$summer.save), haq$hh.BM.use)[1,1] , " use BM" )

    # Comparison of reported initial summer savings and calculated summer savings
    haq$calculated.summer.save <- haq$summer.before.bm.kg - haq$summer.after.bm.kg
    haq$total.calculated.summer.save <- 8 * haq$calculated.summer.save
    haq$diff3 <- haq$summer.save.initial.kg - haq$calculated.summer.save

    # Scenario of rising baseline
    haq$diff4 <- haq$summer.current.kg - haq$summer.after.bm.kg
    haq$summer.save <- ifelse(haq$summer.current.kg > haq$summer.after.bm.kg, haq$calculated.summer.save, haq$summer.save)
    haq$total.summer.save <- 8 * haq$summer.save

    # Conservartive summer baseline that incorporates disqualification of rising baseline
    haq$summerconbase <- haq$summer.current.kg + haq$summer.save
    haq$summerconbase.year <- 8 * haq$summerdays.prop * haq$summerconbase
    if (verbose == TRUE)
        message("summer.current.kg mean = ", summary(haq$summer.current.kg)[4] ,  " nobs = " ,nobs(haq$summer.current.kg),
                " and " , table(is.na(haq$summer.current.kg), haq$hh.BM.use)[1] , " use BM")

        message("summer.save after removing rising baseline mean = ", summary(haq$summer.save)[4] ,  " nobs = ", nobs(haq$summer.save), " ")

        message("summerconbase.year mean = ", summary(haq$summerconbase.year)[4], " nobs =  " , nobs(haq$summerconbase.year), " and ",
                table(is.na(haq$summerconbase.year), haq$hh.BM.use)[1], " use BM \n")

    # If a BM user do not use coal in summer, she therefore cannot use BM in summer, thus hh.summer.BM.use = 'BM no summer'
    # for such a use, the value of summerconbase.year=0

    haq$hh.summer.BM.use = as.character(haq$hh.summer.BM.use)
    nsidx = which(haq$hh.BM.use == "BM" &
      (haq[, summercoal.var] == "NO" |
       haq[, summercoal.var] == "No" |
       haq[, summercoal.var] == 2 )
                  )
    haq[nsidx, "hh.summer.BM.use"] <- "BM no summer"
    haq[nsidx, "summerconbase.year"] <- 0
    if (verbose == TRUE)
      message("summerconbase.year has ", nobs(haq$summerconbase.year), " observations after ",
              length(nsidx) , " zeros were added of whom ",
              table(is.na(haq$summerconbase.year), haq$hh.BM.use)[1], " use BM \n" )

    haq$summer.current.year <- 8 * haq$summerdays.prop * haq$summer.current.kg
    haq$summer.save.year <- 8 * haq$summerdays.prop * haq$summer.save
    # If a BM user do not use coal in summer, she therefore cannot use BM in summer, thus hh.summer.BM.use = 'BM no summer'
    # for such a use, the value of summercurrent.year=0
    if (verbose == TRUE)
      message("summer.current.year mean = ", summary(haq$summer.current.year)[4], " nobs = ", nobs(haq$summer.current.year))

    # the same goes for summer.current.year
    scidx = which(haq$hh.BM.use == "BM" & (haq[ ,summercoal.var] == "NO" | haq[ ,summercoal.var] == "No" | haq[ ,summercoal.var] == 2))
    haq[scidx, "summer.current.year"] <- 0
    if (verbose == TRUE)
      message("summer current year set to zero for ", length(scidx), " records" )

    # define anual baseline coal use
    haq$summerconbase.year[which(is.na(haq$winterconbase.year) == TRUE & haq$hh.BM.use == "BM")] <- NA
    if (verbose == TRUE)
      message("summerconbase year is NA where there is no winterconbase")
      message("summerconbase.year mean = ", summary(haq$summerconbase.year)[4], " nobs = ", nobs(haq$summerconbase.year))

    haq$summerconbase.year[which(haq$hh.BM.use != "BM")] <- NA


    if (verbose == TRUE)
      message("summerconbase.year mean = ", summary(haq$summerconbase.year)[4], " nobs = ", nobs(haq$summerconbase.year))

    haq$annual.base = haq$winterconbase.year + haq$summerconbase.year
    if (verbose == TRUE)
      message("annual.base mean = ", summary(haq$annual.base)[4], " nobs = ", nobs(haq$annual.base))

    # define anual project coal use
    haq$summer.current.year[which(is.na(haq$winter.current.year) == TRUE & haq$hh.BM.use == "BM")] <- NA
    if (verbose == TRUE)
      message("summer.current mean set to NA where winter.current.year is missing: nobs = ", nobs(haq$summer.current.year))
    haq$annual.current = haq$winter.current.year + haq$summer.current.year
    if (verbose == TRUE)
      message("annual.current mean = ", summary(haq$annual.current)[4], " nobs = ", nobs(haq$annual.current))

    # CALCULATE PROPORTIONAL SAVINGS
    haq$proportion.save.winter <- haq$winter.save/haq$winter.before.bm.kg
    haq$proportion.save.summer <- haq$summer.save/haq$summer.before.bm.kg

    assign("haq", haq, envir = .GlobalEnv)
}
