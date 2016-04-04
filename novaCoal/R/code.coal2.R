# Helper function
unfactor <- function(x) {
    if (is.factor(x)) {
        as.numeric(unclass(levels(x)))[as.numeric(x)]
    } else {
        x
    }
}

# code.coal

code.coal <- function(haq, 
                      verbose = FALSE, 
                      units = c("coal.units.winter.before.bm", "coal.units.summer.before.bm", "coal.units.winter.after.bm", 
    "coal.units.summer.after.bm"), 
                      estimate.summer = FALSE, 
                      summer.est.var = "bm.saved.coal.summer", 
                      summer.est.value = "It is not yet summer, or I only started recently and cannot tell exactly", 
                      estimate.winter = FALSE, 
                      winter.est.var = "bm.saved.coal.winter", 
                      winter.est.value = "I have only started recently and cannot tell exactly") {
    
    # Non BM user cannot have a use before and afterBM
    nonbmidx = which(haq$hh.BM.use != "BM" & haq$hh.BM.use != "BM other" & haq$hh.BM.use != "BM old")
    if (verbose == TRUE) 
        message("nonbmidx=  ", length(nonbmidx))
    if (verbose == TRUE) 
        message("unit names ", names(haq)[match(units, names(haq))])
    haq[nonbmidx, match(units, names(haq))] = NA
    
    # Calculate average winter eef to use to give the average proportional saving to the users who are not confident to give
    # their savings
    eef.raw = with(haq[which(haq$hh.BM.use == "BM"), ], unfactor(haq$coal.units.winter.before.bm)/unfactor(haq$coal.units.winter.after.bm))
    eef = mean(eef.raw[which(eef.raw < 4)], na.rm = TRUE)
    if (verbose == TRUE) 
        message("eef")
    
    # Calculate average summer eef to use to give the average proportional saving to the users who are not confident to give
    # their savings
    eef.s.raw = with(haq[which(haq$hh.summer.BM.use == "BM"), ], unfactor(haq$coal.units.summer.before.bm)/unfactor(haq$coal.units.summer.after.bm))
    eef.s = mean(eef.s.raw[which(eef.s.raw < 4)], na.rm = TRUE)
    if (verbose == TRUE) 
        message("eef.s")
    
    # energy efficiency factor
    
    haq$eef = unfactor(haq$coal.units.winter.before.bm)/unfactor(haq$coal.units.winter.after.bm)
    haq$eef.s = unfactor(haq$coal.units.summer.before.bm)/unfactor(haq$coal.units.summer.after.bm)
    # make sure there are no Inf (i.e. x/0) responses
    haq[which(haq$eef.s == Inf), c("eef.s")] = NA
    
    # There are cases where the use could not yet estimate summer use be
    if (estimate.summer == TRUE) {
        if (verbose == TRUE) 
            message("Making use of summer estimation")
        # index of applicable records: Their summer before and after and current is NA
        s.e.idx = which(haq[, summer.est.var] == summer.est.value & is.na(haq$current.coal.units.summer) == TRUE & is.na(haq$coal.units.summer.before.bm) == 
            TRUE & is.na(haq$coal.units.summer.after.bm) == TRUE & haq$hh.summer.BM.use == "BM")
        
        if (length(s.e.idx > 0)) {
            haq[s.e.idx, "current.coal.units.summer"] = mean(haq$current.coal.units.summer[which(haq$hh.summer.BM.use == "BM")], 
                na.rm = TRUE)
            haq[s.e.idx, "coal.units.summer.after.bm"] = mean(haq$current.coal.units.summer[which(haq$hh.summer.BM.use == 
                "BM")], na.rm = TRUE)
            haq[s.e.idx, "coal.units.summer.after.bm"] = haq[s.e.idx, "coal.units.summer.after.bm"] * eef.s
            haq[s.e.idx, "estimate"] = TRUE
        }
        
        # undex of those who have a summer current and only need to apply eef to get a before
        s.c.ok.idx = which(haq[, summer.est.var] == summer.est.value & is.na(haq$current.coal.units.summer) == FALSE & is.na(haq$coal.units.summer.before.bm) == 
            TRUE & is.na(haq$coal.units.summer.after.bm) == TRUE & haq$hh.summer.BM.use == "BM")
        
        if (length(s.c.ok.idx) > 0) {
            haq[s.c.ok.idx, "coal.units.summer.after.bm"] = haq[s.c.ok.idx, "current.coal.units.summer"]  # assume a flat baseline
            haq[s.c.ok.idx, "coal.units.summer.before.bm"] = haq[s.c.ok.idx, "coal.units.summer.after.bm"] * eef.s  # assume average summer eef
            haq[s.c.ok.idx, "estimate"] = TRUE
        }
        
    }
    
    if (estimate.winter == TRUE) {
        if (verbose == TRUE) 
            message("Making use of winter estimation")
        # index of applicable records: Their winter before and after BM is currently NA
        w.e.idx = which(haq[, winter.est.var] == winter.est.value & haq$hh.BM.use == "BM" & is.na(haq$current.coal.units.winter) == 
            TRUE & is.na(haq$coal.units.winter.before.bm) == TRUE & is.na(haq$coal.units.winter.after.bm) == TRUE)
        
        haq[w.e.idx, "current.coal.units.winter"] = mean(haq$current.coal.units.winter[which(haq$hh.BM.use == "BM")], na.rm = TRUE)
        haq[w.e.idx, "coal.units.winter.after.bm"] = mean(haq$current.coal.units.winter[which(haq$hh.BM.use == "BM")], na.rm = TRUE)
        haq[w.e.idx, "coal.units.winter.before.bm"] = haq[w.e.idx, "coal.units.winter.after.bm"] * eef
        haq[w.e.idx, "estimate"] = TRUE
    }
    
    # In rare cases where no current use exist but use after BM is given. Conservatively assume flat baseline
    
    haq[which(haq$current.coal.units.winter == 0 & haq$hh.BM.use == "BM"), "current.coal.units.winter"] = haq[which(haq$current.coal.units.winter == 
        0 & haq$hh.BM.use == "BM"), "coal.units.winter.after.bm"]
    ### winter Convert to to kg - only for legitimate BM users
    haq$winter.before.bm.kg <- unfactor(haq$coal.units.winter.before.bm) * haq$kg
    haq$totalwinter.before.bm.kg <- 4 * haq$winter.before.bm.kg
    haq$summer.before.bm.kg <- unfactor(haq$coal.units.summer.before.bm) * haq$kg
    haq$totalsummer.before.bm.kg <- 8 * haq$summer.before.bm.kg
    if (verbose == TRUE) 
        message("units before to kg")
    
    haq$winter.save.initial.kg <- unfactor(haq$coal.saved.initial.winter) * haq$kg
    haq$totalwinter.save.initial.kg <- 4 * haq$winter.save.initial.kg
    if (verbose == TRUE) 
      message("winter.save.initial.kg ", summary(haq$winter.save.initial.kg), " ", nobs(haq$winter.save.initial.kg))
    
    haq$summer.save.initial.kg <- unfactor(haq$coal.saved.initial.summer) * haq$kg
    haq$totalsummer.save.initial.kg <- 8 * haq$summer.save.initial.kg
    if (verbose == TRUE) 
        message("summer.save.initial.kg ", summary(haq$summer.save.initial.kg), " ", nobs(haq$summer.save.initial.kg))
    
    # define proportion of winter days - only for legitimate BM users
    haq$winterdays.prop <- haq$winterdays.used/fullwinter
    if (verbose == TRUE) 
        message("haq$winterdays.prop ", summary(haq$winterdays.prop), " ", nobs(haq$winterdays.prop))
    
    # Current winter use in kg: Only for legitimate BM users who are confident to give their use
    haq$winter.current.kg <- ifelse(haq$hh.BM.use == "BM" | haq$hh.BM.use == "Historic BM", 
                                    unfactor(haq[ ,grep("current.coal.units.wint",names(haq))[1]]) * haq$kg, 
                                    NA)
    haq$winter.current.kg[which(haq$bm.saved.coal.winter == "Nobody in this house uses BM")] <- NA
    haq$totalwinter.current.kg <- 4 * haq$winter.current.kg  # Full year winter baseline
    if (verbose == TRUE) 
        message("winter.current.kg", summary(haq$winter.current.kg), " ", nobs(haq$winter.current.kg))
    
    ### summer
    
    haq$summerdays.prop <- haq$summerdays.used/fullsummer
    haq$summer.current.kg <- ifelse(haq$hh.summer.BM.use == "BM" | haq$hh.summer.BM.use == "Historic BM", 
                                    unfactor(haq [ , grep("current.coal.units.sum", names(haq))[1]]) *  haq$kg,
                                    NA)
    haq$totalsummer.current.kg <- 8 * haq$summer.current.kg
    if (verbose == TRUE) 
        message("summer current.kg", summary(haq$summer.current.kg), " ", nobs(haq$summer.current.kg))
    
    haq$winter.after.bm.kg <- unfactor(haq$coal.units.winter.after.bm) * haq$kg
    haq$totalwinter.after.bm.kg <- 4 * haq$winter.after.bm.kg
    if (verbose == TRUE) 
      message("winter.after.bm.kg", summary(haq$winter.after.bm.kg), " ", nobs(haq$winter.after.bm.kg))
    
    haq$summer.after.bm.kg <- unfactor(haq$coal.units.summer.after.bm) * haq$kg
    haq$totalsummer.after.bm.kg <- 8 * haq$summer.after.bm.kg
    if (verbose == TRUE) 
        message("summer.after.bm.kg", summary(haq$summer.after.bm.kg), " ", nobs(haq$summer.after.bm.kg))
    
    ### WINTERBASELINE - only for legitimate BM users
    haq$winterbase <- haq$winter.before.bm.kg/haq$winter.after.bm.kg * haq$winter.current.kg
    haq$winterbase[which(haq$bm.saved.coal.winter == "Nobody in this house uses BM")] <- NA
    haq$total.winterbase <- 4 * haq$winterbase
    haq$winter.save = haq$winterbase - haq$winter.current.kg
    if (verbose == TRUE) 
        message("winterbase ", summary(haq$winterbase), " ", nobs(haq$winterbase))
    
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
      message("winterconbase.year ", summary(haq$winterconbase.year), " ", nobs(haq$winterconbase.year))
    
    haq$winter.current.year <- 4 * haq$winterdays.prop * haq$winter.current.kg
    haq$winter.save.year <- 4 * haq$winterdays.prop * haq$winter.save
    if (verbose == TRUE) 
      message("winter.current.year ", summary(haq$winter.current.year), " ", nobs(haq$winter.current.year))
    
    # SUMMER BASELINE - only for legitimate BM users
    
    haq$summerbase <- ifelse(haq$summer.after.bm.kg != 0, haq$summer.before.bm.kg/haq$summer.after.bm.kg * haq$summer.current.kg, 
        0)
    haq$summerbase[which(haq$bm.saved.coal.summer == "Nobody in this house uses BM")] <- NA
    haq$total.summerbase <- 8 * haq$summerbase
    haq$summer.save = ifelse(is.na(haq$summerbase) == FALSE | is.na(haq$summer.current.kg) == FALSE, haq$summerbase - haq$summer.current.kg, 
        NA)
    
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
        message("summer.current.kg has ", nobs(haq$summer.current.kg), " observations \n")
        message("summer.save ", nobs(haq$summer.save), " observations \n")
        
        message("summerconbase.year has ", nobs(haq$summerconbase.year), " observations \n")
    
        message("BM old ", nobs(which(is.na(haq$summer.after.bm.kg)  & haq$hh.BM.use =="BM old")), " observations for summer\n")
    
    # If a BM user do not use coal in summer, she therefore cannot use BM in summer, thus hh.summer.BM.use = 'BM no summer'
    # for such a use, the value of summerconbase.year=0
    
    haq$hh.summer.BM.use = as.character(haq$hh.summer.BM.use)
    haq[which(haq$hh.BM.use == "BM" & (haq$coal.use.in.summer == "NO" | haq$coal.use.in.summer == "No" | haq$coal.use.in.summer == 2 )), "hh.summer.BM.use"] <- "BM no summer"
    haq[which(haq$hh.BM.use == "BM" & (haq$coal.use.in.summer == "NO" | haq$coal.use.in.summer == "No" | haq$coal.use.in.summer == 2 )), "summerconbase.year"] <- 0
    if (verbose == TRUE) 
      message("summerconbase.year has ", nobs(haq$summerconbase.year), " observations after no summer")
    
    haq$summer.current.year <- 8 * haq$summerdays.prop * haq$summer.current.kg
    haq$summer.save.year <- 8 * haq$summerdays.prop * haq$summer.save
    # If a BM user do not use coal in summer, she therefore cannot use BM in summer, thus hh.summer.BM.use = 'BM no summer'
    # for such a use, the value of summercurrent.year=0
    if (verbose == TRUE) 
      message("summer.current.year ", summary(haq$summer.current.year), " ", nobs(haq$summer.current.year))
    
    # the same goes for summer.current.year
    haq[which(haq$hh.BM.use == "BM" & (haq$coal.use.in.summer == "NO" | haq$coal.use.in.summer == "No" | haq$coal.use.in.summer == 2)), "summer.current.year"] <- 0
    
    # define anual baseline coal use
    haq$summerconbase.year[which(is.na(haq$winterconbase.year) == TRUE & haq$hh.BM.use == "BM")] <- NA
    #haq$summerconbase.year[which(haq$hh.BM.use != "BM")] <- NA
    if (verbose == TRUE) 
      message("summerconbase.year ", summary(haq$summerconbase.year), " ", nobs(haq$summerconbase.year))
    
    haq$annual.base = haq$winterconbase.year + haq$summerconbase.year
    if (verbose == TRUE) 
      message("annual.base ", summary(haq$annual.base), " ", nobs(haq$annual.base))
    
    # define anual project coal use
    haq$summer.current.year[which(is.na(haq$winter.current.year) == TRUE & haq$hh.BM.use == "BM")] <- NA
    haq$annual.current = haq$winter.current.year + haq$summer.current.year
    if (verbose == TRUE) 
      message("annual.current ", summary(haq$annual.current), " ", nobs(haq$annual.current))
    
    # CALCULATE PROPORTIONAL SAVINGS
    haq$proportion.save.winter <- haq$winter.save/haq$winter.before.bm.kg
    haq$proportion.save.summer <- haq$summer.save/haq$summer.before.bm.kg
    
    
    
    assign("haq", haq, envir = .GlobalEnv)
    
} 
