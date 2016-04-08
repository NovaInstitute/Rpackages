#' Alternative Ignition Data Intergity Test Function
#'
#' This function is used to test data before running ER calculation (needs the fixname function)
#'
#' @param haq Data frame containing survey data
#' @param pop Data frame containing the populations for different locations
#' @export

test.AI <- function(haq,
                    pop){

if (!require(novaUtils) == FALSE){
        message("Installing package novaUtils")
        devtools::install_github("NovaInstitute/Rpackages/novaUtils")
        library(novaUtils)
}

  names(pop) = novaUtils::fixname(names(pop))
  mt = "\t\t\t\t\t\t\t\t"
  # test pop
  pop.is = exists("pop")
  message("Does pop exist? ", pop.is)
  pop.town = all(match(levels(haq$town), pop$town))
  #message("Do we have a population for every town? ", pop.town)

  # test att
  att.is = exists("att")
  #message("Does attendance record exist? ", att.is)

  ## test haq

  # COAL
  # All coal
  coal.miss = all(is.na(haq$contact.coal.use)==FALSE)
  #message("Does all the records have a coal use? ", coal.miss)

  # BM
  bm.miss = all(is.na(haq$hh.BM.use)==FALSE)
  #message("Does all the records have a BM? ", bm.miss)

  # correct BM Old
  # cirrect BM Other


  # Baseline
  ann.base.NA =  all(is.na(haq$annual.base))==FALSE
  #message("Is Annual baseline coal use a numeric vector? ", ann.base.NA)

  ann.base.ok = all(na.omit(haq$annual.base==(haq$winterconbase.year + haq$summerconbase.year)))
  #message("Is Annual baseline coal use the sum of winter and summer? ", ann.base.ok)

  w.s.base.NA = all(is.na(haq[which(is.na(haq$winterconbase.year)==TRUE),"summerconbase.year"])==TRUE)
  #message("Is baseline #NA winter = #NA summer? ", w.s.base.NA)

  # Project
  ann.current.NA =  all(is.na(haq$annual.current))==FALSE
  #message("Is Annual current coal use a numeric vector? ", ann.current.NA)

  ann.current.ok = suppressWarnings(all(haq$annual.current[!is.na(haq$annual.current)] ==
    (haq$winter.current.year[!is.na(haq$winter.current.year)] + haq$summer.current.year[!is.na(haq$summer.current.year)]))
                                    )
  if(ann.current.ok == FALSE){
    message("Nobs annual.current: ", nobs(haq$annual.current))
    message("Nobs winter.current: ", nobs(haq$winter.current.year))
    message("Nobs summer.current: ", nobs(haq$summer.current.year))
  }
  #message("Is Annual current coal use the sum of winter and summer? ",mt, ann.current.ok)

  w.s.current.NA = all(is.na(haq[which(is.na(haq$winter.current.year)==TRUE),"summer.current.year"])==TRUE)
  #message("Is current #NA winter = #NA summer? ", mt, w.s.current.NA)

  #eef
  vars = c(pop.town,
           att.is,
           coal.miss,
           bm.miss,
           ann.base.NA,
           ann.base.ok,
           w.s.base.NA,
           ann.current.NA,
           ann.current.ok,
           w.s.current.NA
           )

  rye = c("Do we have a population for every town? ",
          "Does attendance record exist? ",
          "Does all the records have a coal use? ",
          "Does all the records have a BM? ",
          "Is Annual baseline coal use a numeric vector? ",
          "Is Annual baseline coal use the sum of winter and summer? ",
          "Is baseline #NA winter = #NA summer? ",
          "Is Annual current coal use a numeric vector? ",
          "Is Annual current coal use the sum of winter and summer? ",
          "Is current #NA winter = #NA summer? "
          )

  data.frame(test = vars,
             row.names = rye)

}
