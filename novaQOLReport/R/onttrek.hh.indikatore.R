#' CI
#'
#' Creates statistical informative output for numeric data based on a confidence interval
#'
#' @param x Data frame containing numeric data
#' @param ci The confidence interval as a numeric
#' @param na.rm Logical that removes NA values when TRUE
#' @export

CI <- function (x, ci = 0.95, na.rm=TRUE)
{
  a <- mean(x, na.rm=TRUE)
  s <- sd(x, na.rm=TRUE)
  n <- na.omit(length(x))
  error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
  return(data.frame(PointEst = a, Lower = a - error, Upper = a + error))
}

#' Extract HH Indicators
#'
#' Extract the final QOL indicators out of the datasets possible per town. Used for procuring
#' the total tons of coal and wood burned at homes per annum
#'
#' @param hh Data frame containing survey information
#' @param workpop.min The minimum working age for a member as Numeric
#' @param workpop.max The maximum working age for a member as Numeric
#' @param imm.max The age at which someone is "mature"?? (I have no idea what this var is)
#' @param outfile Character vector containing the output directory for writing a xlsx file
#' @param write Logical whether to write an xlsx file or not
#' @param bag.weight Numeric indicating the bag weight attributed to each person
#' @param debug Logical that initializes global environment assignments for debugging process
#' @param verbose Logical to display function messages
#' @param orden Logical that prescribes a numerical order (Not actually used in this function)
#' @details Further arguments are all character vectors referring to survey variables contained in the QOL
#' dataset
#' @export

onttrek.hh.indikatore <- function(hh = qol,
                                  workpop.min = 14,
                                  workpop.max = 65,
                                  imm.max = 15,
                                  outfile="/Users/christiaanpauw/tmp/a.xlsx",
                                  write = FALSE,
                                  bag.weight=50,
                                  debug = FALSE, verbose = FALSE,
                                  demographics_member_age = "demographics_member_age",
                                  demographics_member_sex = "demographics_member_sex",
                                  personal_education_completed = "personal_education_completed",
                                  noschoolvar = "No school education completed",
                                  respondent_info_address_householdsonstand = "respondent_info_address_householdsonstand",
                                  respondent_household_membernumber = "respondent_household_membernumber",
                                  sol_energy_electricity_failure_90 = "sol_energy_electricity_failure_90",
                                  swb_satisfaction_general = "swb_satisfaction_general",
                                  swb_satisfaction_work = "swb_satisfaction_work",
                                  sol_income_household_total = "sol_income_household_total",
                                  sol_household_health_general = "sol_household_health_general",
                                  swb_satisfaction_food = "swb_satisfaction_food",
                                  swb_satisfaction_discovery = "swb_satisfaction_discovery",
                                  swb_satisfaction_water = "swb_satisfaction_water",
                                  swb_satisfaction_waste = "swb_satisfaction_waste",
                                  swb_satisfaction_air = "swb_satisfaction_air",
                                  swb_satisfaction_house = "swb_satisfaction_house",
                                  sol_water_source_main = "sol_water_source_main",
                                  sol_water_available_30days = "sol_water_available_30days",
                                  sol_sanitation_toilet_type = "sol_sanitation_toilet_type",
                                  sol_sanitation_toilet_flushsystemactive = "sol_sanitation_toilet_flushsystemactive",
                                  sol_waste_household_collection = "sol_waste_household_collection",
                                  sol_waste_household_collectionfailure = "sol_waste_household_collectionfailure",
                                  sol_energy_cooking_all_wood = "sol_energy_cooking_all_wood",
                                  sol_energy_cooking_all_coal = "sol_energy_cooking_all_coal",
                                  sol_energy_cooking_all_cattle.manure...animal.dung = "sol_energy_cooking_all_cattle.manure...animal.dung",
                                  sol_energy_cooking_all_paraffin = "sol_energy_cooking_all_paraffin",
                                  sol_energy_cooking_maincarrier = "sol_energy_cooking_maincarrier",
                                  sol_energy_heating_carrier = "sol_energy_heating_carrier",
                                  sol_energy_heating_all_wood = "sol_energy_heating_all_wood",
                                  sol_energy_heating_all_coal = "sol_energy_heating_all_coal",
                                  sol_energy_heating_all_cattle.manure...animal.dung = "sol_energy_heating_all_cattle.manure...animal.dung",
                                  sol_energy_heating_all_paraffin = "sol_energy_heating_all_paraffin",
                                  #sol_energy_heating_carrier = "sol_energy_heating_carrier",
                                  poverty.income.cat = "poverty.income.cat",  low.pov.lab = "Lower bound poverty line -",
                                  sol_safety_perception = "sol_safety_perception",
                                  sol_safety_crime_victim = "sol_safety_crime_victim",
                                  sol_safety_crime_victimlist_street.robbery = "sol_safety_crime_victimlist_street.robbery",
                                  sol_safety_crime_victimlist_home.robbery = "sol_safety_crime_victimlist_home.robbery",
                                  sol_safety_crime_victimlist_home.burglary..excluding.home.robbery = "sol_safety_crime_victimlist_home.burglary..excluding.home.robbery.",
                                  body_symptoms_twelvemonth_list_no.complaints.at.all = "body_symptoms_twelvemonth_list_no.complaints.at.all",
                                  body_symptoms_twelvemonth_list_coughing.sputum.but.no.weight.loss.or.fever.or.night.sweats = "body_symptoms_twelvemonth_list_coughing.sputum.but.no.weight.loss.or.fever.or.night.sweats",
                                  body_symptoms_twelvemonth_list_three.plus.l.s.p.d = "body_symptoms_twelvemonth_list_three.plus.l.s.p.d",
                                  body_symptoms_year_list_painful..swollen.and.stiff.joints = "body_symptoms_year_list_painful..swollen.and.stiff.joints",
                                  body_disease_treatment_asthma = "body_disease_treatment_asthma",
                                  body_disease_prevalence_cancer = "body_disease_prevalence_cancer",
                                  body_disease_treatment_cancer = "body_disease_treatment_cancer",
                                  demographics_member_parent_motheralive = "demographics_member_parent_motheralive",
                                  matriclist = paste(levels(persoon[,personal_education_completed])[c(1,2,3,8,18, 19,20:22)], collapse="|"),
                                  unempvar = "Unemployed, looking",
                                  unempvar.a = "Unemployed, will accept",
                                  asthmalike = "body_symptoms_twelvemonth_list_wheezing.and.tight.chest.at.night.or.after.exercise",
                                  body_nurtition_fruitveg_frequency = "body_nurtition_fruitveg_frequency",
                                  body_nutrition_protein_frequency = "body_nutrition_protein_frequency",
                                  house_insulation_ceiling = "house_insulation_ceiling", house_type = "house_type", inf.house.val = "Caravan|tent|Other|Shack|backyard",
                                  orden = FALSE
                                  ){
  require(Hmisc)
  require(data.table)
  names(persoon) <- gsub("[[:space:]]+", ".", tolower(names(persoon)))
  names(hh) <- gsub("[[:space:]]+", ".", tolower(names(hh)))
  workpop.idx = which(persoon[, demographics_member_age] > workpop.min & persoon[, demographics_member_age] < workpop.max)
  adult.pop.idx = which(persoon[, demographics_member_age] > 19)
  imm.idx = which(persoon[, demographics_member_age] > (imm.max-1))
  imm.n = length(imm.idx)
  adult.pop.n = length(adult.pop.idx)
  workpop.n = length(workpop.idx)
  matriculated.n = length(grep(matriclist, persoon[, personal_education_completed]))
  illiterate = length(grep(noschoolvar, persoon[imm.idx, personal_education_completed]))
  imm.age.idx  = which(persoon[, demographics_member_age]< imm.max)

  mode.lang = names(sort(-table(persoon$demographics_member_language_first)))[1]

  # EMPLOYMENT
  full.time.employ <- data.frame(round(binconf(table(persoon$sol_occupation)["Paid, full-time employment"], n= workpop.n) *100, 2))
  part.time.employ <- data.frame(round(binconf(table(persoon$sol_occupation)[grep("Part-time employed", names(table(persoon$sol_occupation)), value=T)], n = workpop.n) *100, 2))
  unemploy.look <- data.frame(round(binconf(table(persoon$sol_occupation)[unempvar], n = workpop.n) * 100, 2))
  unemploy.accept <- data.frame(round(binconf(table(persoon$sol_occupation)[unempvar.a], n = workpop.n) *100 + unemploy.look,2))
  wftab <- table(persoon$sol_occupation, persoon$workforce)
  #fulltime.study <- data.frame(round(binconf(wftab[grep("School pupil", names(table(persoon$sol_occupation)), value=T), "TRUE"], n = workpop.n) *100 ,2))

  # DEMOGRAFIE
  age = round(CI(persoon[,demographics_member_age]), 2)
  childcount = table(cut(persoon[,demographics_member_age], c(0,15)))
  oldcount = table(cut(persoon[,demographics_member_age], c(0,64,120)))[2][[1]]
  peoplecount = sum(table(persoon[,demographics_member_age]))
  child.perc = data.frame(round(binconf(childcount, n = peoplecount) * 100, 2))
  old.perc = data.frame(round(binconf(oldcount, n = peoplecount) * 100, 2))
  age.median = data.frame(PointEst = round(median(persoon[ ,demographics_member_age], na.rm=TRUE), 2), Lower = NA, Upper = NA)
  hh.number = round(CI(hh[, respondent_household_membernumber], na.rm=TRUE), 2)
  hh.number.median = data.frame(PointEst = round(median(hh[ ,respondent_household_membernumber], na.rm=TRUE), 2), Lower = NA, Upper = NA)
   if (!is.na(match("respondent_info_address_householdsonstand", names(hh)))){
   	hh.stand = round(CI(hh[, respondent_info_address_householdsonstand], na.rm=TRUE), 2)
   	hh.stand.median = data.frame(PointEst = round(median(hh[ ,respondent_info_address_householdsonstand], na.rm=TRUE), 5), Lower = NA, Upper = NA)
  }
  female = data.frame(round(binconf(table(persoon[, demographics_member_sex])["Female"], n = sum(table(persoon[, demographics_member_sex]))) * 100, 2))
  male = data.frame(round(binconf(table(persoon[,demographics_member_sex])["Male"], n = sum(table(persoon[ ,demographics_member_sex]))) * 100, 2))

  if (!is.na(match("demographics_member_language_first", names(persoon)))){
  	dom.lang = data.frame(round(binconf(table(persoon$demographics_member_language_first)[names(sort(-table(persoon$demographics_member_language_first)))[1]] , n = sum(table(persoon$demographics_member_language_first))) * 100, 2))
  }

  work.pop.matric = data.frame(round(binconf(matriculated.n, n = workpop.n) *100, 2))
  iliterate.adult = data.frame(round(binconf(illiterate, n = imm.n) * 100, 2))
  work.age.prop = data.frame(round(binconf(workpop.n, n=nrow(persoon)) * 100,2))
  immunised.prop = data.frame(round(binconf(table(persoon$body_immunisation)["Yes"], n = sum(table(persoon$body_immunisation))) * 100, 2))

  # HEALH & WELL
  if (!is.na(match(swb_satisfaction_general, names(hh)))) satisfaction_general = round(CI(hh[ ,swb_satisfaction_general], na.rm=TRUE), 2)
   if (!is.na(match(swb_satisfaction_work, names(hh)))) satisfaction_work    = round(CI(hh[ ,swb_satisfaction_work], na.rm=TRUE), 2)
  if (!is.na(match(sol_income_household_total, names(hh)))){
  	if(is.numeric(hh[ ,sol_income_household_total])){
  		income_household_total = round(CI(hh[ ,sol_income_household_total], na.rm=TRUE), 2)
  		income_household_total.media = data.frame(PointEst = round(median(hh[ ,sol_income_household_total], na.rm=TRUE), 2), Lower = NA, Upper = NA)
  	}
  }
  #salary.hh = data.frame(round(binconf(table(qol$sol_income_sources_salary.from.work)["Yes"], n=nrow(qol)) * 100, 2))
  if (is.numeric(hh[, sol_household_health_general])){
    md = median(as.numeric(levels(as.factor(kwa[, "sol_household_health_general"]))))
    g.idx = hh[, sol_household_health_general] > md
    hh[, sol_household_health_general] = as.character(hh[, sol_household_health_general])
    hh[, sol_household_health_general] = "No"
    hh[g.idx, sol_household_health_general] = "Yes"
  }
  health.hh = data.frame(round(binconf(table(hh[ ,sol_household_health_general])["Yes"], n = nrow(hh)) * 100, 2))
  if (!is.na(match(swb_satisfaction_food, names(hh))))  satisfaction_food = round(CI(hh[ ,swb_satisfaction_food], na.rm=TRUE), 2)
  if (!is.na(match(swb_satisfaction_discovery, names(hh))))  satisfaction_discovery = round(CI(hh[ ,swb_satisfaction_discovery], na.rm=TRUE), 2)
  never.doctor = data.frame(round(binconf(table(persoon$sol_services_medical_last)["Never"], n= sum(table(persoon$sol_services_medical_last))) * 100, 2))
  no.complain = data.frame(round(binconf(table(persoon[ ,body_symptoms_twelvemonth_list_no.complaints.at.all])["Yes"], n = sum(table(persoon[ ,body_symptoms_twelvemonth_list_no.complaints.at.all]))) * 100, 2))
  bronch = data.frame(round(binconf(table(persoon[ ,body_symptoms_twelvemonth_list_coughing.sputum.but.no.weight.loss.or.fever.or.night.sweats])["Yes"] ,
  n = sum(table(persoon[ ,body_symptoms_twelvemonth_list_coughing.sputum.but.no.weight.loss.or.fever.or.night.sweats]))) * 100, 2))
  asma = data.frame(round(binconf(table(persoon[ ,asthmalike])["Yes"] , n = sum(table(persoon[ ,asthmalike]))) * 100, 2))
  smoke = data.frame(round(binconf(table(persoon$body_exposure_smoking_active)["Yes"] , n = sum(table(persoon$body_exposure_smoking_active))) * 100, 2))
  diah = data.frame(round(binconf(table(persoon[ ,body_symptoms_twelvemonth_list_three.plus.l.s.p.d])["Yes"] , n = sum(table(persoon[ ,body_symptoms_twelvemonth_list_three.plus.l.s.p.d]))) * 100, 2))

 if (!is.na(match(body_symptoms_year_list_painful..swollen.and.stiff.joints, names(persoon)))) arth = data.frame(round(binconf(table(persoon[ ,body_symptoms_year_list_painful..swollen.and.stiff.joints])["Yes"] , n = sum(table(persoon[ ,body_symptoms_year_list_painful..swollen.and.stiff.joints]))) * 100, 2))

 if (!is.na(match(body_disease_treatment_asthma, names(persoon)))) asthma.tr = data.frame(round(binconf(table(persoon[ ,body_disease_treatment_asthma])["Yes"] , n = sum(table(persoon[ ,body_disease_treatment_asthma], exclude = NULL))) * 100, 2))

 if (!is.na(match(body_disease_prevalence_cancer, names(persoon)))) cancer = data.frame(round(binconf(table(persoon[ ,body_disease_prevalence_cancer])["Yes"] , n = sum(table(persoon[ ,body_disease_prevalence_cancer], exclude = NULL))) * 100, 2))

 if (!is.na(match(body_disease_treatment_cancer, names(persoon)))) cancer.tr = data.frame(round(binconf(table(persoon[ ,body_disease_treatment_cancer])["Yes"] , n = sum(table(persoon[ ,body_disease_treatment_cancer], exclude = NULL))) * 100, 2))


  veg = data.frame(round(binconf(table(persoon[, body_nurtition_fruitveg_frequency])["Every day"] , n = sum(table(persoon[, body_nurtition_fruitveg_frequency]))) * 100, 2))
  protien = data.frame(round(binconf(table(persoon[ ,body_nutrition_protein_frequency] )[grep("Every day", names(table(persoon[ ,body_nutrition_protein_frequency])))] , n = sum(table(persoon[ , body_nutrition_protein_frequency]))) * 100, 2))
  if (!is.na(match(demographics_member_parent_motheralive, names(persoon)))) {
  	maternal.o = data.frame(round(binconf(x = length(which(persoon[ ,demographics_member_parent_motheralive] == "No")) , n = sum(table(persoon$demographics_member_parent_motheralive)))  *  100, 2))
  both.o = data.frame(round(binconf(length(which(persoon$demographics_member_parent_motheralive == "No" & persoon$demographics_member_parent_fatheralive == "No")), n = sum(table(persoon$demographics_member_parent_motheralive))) *100, 2))
  }

  restricted.activity = data.frame(round(binconf(sum(persoon$body_morbidity_rad, na.rm=TRUE) , n = ( workpop.n * 6*22.5)) * 100, 2)) # in persent werksdae van totale populasie verloor

  # SERVICES
  if (!is.na(match(swb_satisfaction_water, names(hh))))  water = round(CI(hh[ ,swb_satisfaction_water], na.rm=TRUE), 2)
  if (!is.na(match(swb_satisfaction_waste, names(hh))))  waste = round(CI(hh[ ,swb_satisfaction_waste], na.rm=TRUE), 2)
  if (!is.na(match(swb_satisfaction_air, names(hh))))  air = round(CI(hh[ ,swb_satisfaction_air], na.rm=TRUE), 2)
  if (!is.na(match(swb_satisfaction_house, names(hh))))  house = round(CI(hh[ ,swb_satisfaction_house], na.rm=TRUE), 2)
  if (!is.na(match(house_insulation_ceiling, names(struktuur)))){
  	celtab = table(struktuur[ ,house_insulation_ceiling])
  	ceiling = data.frame(round(binconf(sum(celtab[grep("Ceiling in some rooms|Yes, there is a ceiling in some rooms", names(celtab))]) , n = sum(celtab)) * 100, 2))
  }
  message("\n\n\n\n", dim(hh), "\n\n\n")
  tap.yard = data.frame(round(binconf(sum(table(hh[ ,sol_water_source_main])[grep("Piped water in the house|Piped water into the house|Tap/borehole in yard|Stand pipe in yard|Borehole", names(table(hh[ ,sol_water_source_main])))]) , n = sum(table(hh[, sol_water_source_main])))  * 100 , 2))
  tap.dwelling = data.frame(round(binconf(sum(table(hh[ ,sol_water_source_main])[grep("Piped water in the house|Piped water into the house", names(table(hh[ ,sol_water_source_main])))]) , n = sum(table(hh[ ,sol_water_source_main])))  * 100 , 2))
  water.unavail = round(CI(hh[ ,sol_water_available_30days], na.rm=TRUE), 2)
  santab = table(hh[ ,sol_sanitation_toilet_type])
  toilet = data.frame(round(binconf(sum(santab[grep("Flushed to piped sewer|Flushed to piped sewer system|Flushed to septic tank system", names(santab))]) , n = sum(table(hh[ ,sol_sanitation_toilet_type]))) * 100, 2))
  if (!is.na(match(sol_sanitation_toilet_flushsystemactive, names(hh)))) flush = data.frame(round(binconf(table(hh[ ,sol_sanitation_toilet_flushsystemactive])["Yes"] , n = sum(table(hh[ ,sol_sanitation_toilet_flushsystemactive]))) * 100, 2))
  waste.rem.serv = data.frame(round(binconf(table(hh[ ,sol_waste_household_collection])["Yes"] , n = sum(table(hh[ ,sol_waste_household_collection]))) *100, 2))
  waste.fail.tab = table(hh[ ,sol_waste_household_collectionfailure])
  waste.fail.m = data.frame(round(binconf(waste.fail.tab[grep("More than monthly|More than once a month", names(waste.fail.tab))] , n = sum(waste.fail.tab)) *100, 2))
  if (verbose == TRUE){
  	if (debug == TRUE) assign("hh1", hh, 	envir = .GlobalEnv)
    message(paste(dim(hh),collapse = " by ")," " ,sol_energy_cooking_all_wood," ", sol_energy_cooking_all_coal," ", sol_energy_cooking_all_cattle.manure...animal.dung," ", sol_energy_cooking_all_paraffin," ", sol_energy_cooking_maincarrier)
  }
  dirty.cook.idx = which(hh[ ,sol_energy_cooking_all_wood]=="Yes" |
                                                     hh[ ,sol_energy_cooking_all_coal]=="Yes" |
                                                     hh[ ,sol_energy_cooking_all_cattle.manure...animal.dung]=="Yes" |
                                                     hh[ ,sol_energy_cooking_all_paraffin]=="Yes")
   if (verbose == TRUE) message("dirty cook idx is ", length(dirty.cook.idx), " lank")
  dirty.cook = data.frame(round(binconf(length(dirty.cook.idx) ,
                                      n = sum(table(hh[ ,sol_energy_cooking_maincarrier]))) *100, 2))

  if (verbose == TRUE){
    message(paste(dim(hh),collapse = " by ")," " ,sol_energy_heating_all_wood," ", sol_energy_heating_all_coal," ", sol_energy_heating_all_cattle.manure...animal.dung," ", sol_energy_heating_all_paraffin," ", sol_energy_heating_carrier)
  }
  dirty.heat = data.frame(round(binconf(length(which(hh[ ,sol_energy_heating_all_wood]=="Yes" | hh[ ,sol_energy_heating_all_coal]=="Yes" | hh[ ,sol_energy_heating_all_cattle.manure...animal.dung]=="Yes" | hh[ ,sol_energy_heating_all_paraffin]=="Yes")) , n = sum(table(hh[ ,sol_energy_heating_carrier]))) *100, 2))
  housetab = table(struktuur[, house_type])
  informal.house = data.frame(round(binconf(sum(housetab[grep(inf.house.val, names(housetab))]) , n = sum(housetab)) * 100, 2))
  if (!is.na(match(sol_energy_electricity_failure_90, names(hh)))) electricity_failure_90 = data.frame(round(binconf(x = length(which(hh[,sol_energy_electricity_failure_90] > 3)), n = length(!is.na(hh[,sol_energy_electricity_failure_90]))) * 100, 2))

  # SAFETY
  if (!is.na(match(sol_safety_perception, names(hh)))) safety_perception = data.frame(round(binconf(table(hh[ ,sol_safety_perception])["Yes"] , n = nrow(hh)) * 100, 2))
  if (!is.na(match(sol_safety_crime_victim, names(hh))))  crime_victim = data.frame(round(binconf(table(hh[ ,sol_safety_crime_victim])["Yes"] , n = nrow(hh)) * 100, 2))
  if (verbose == TRUE){
    message(sol_safety_crime_victimlist_street.robbery, " ")
  }
  if (!is.na(match(sol_safety_crime_victimlist_street.robbery, names(hh)))) street.robbery = data.frame(round(binconf(table(hh[, sol_safety_crime_victimlist_street.robbery])["Yes"] , n = nrow(hh)) * 100, 2))
 if (!is.na(match(sol_safety_crime_victimlist_home.robbery, names(hh))))  home.robbery = data.frame(round(binconf(table(hh[, sol_safety_crime_victimlist_home.robbery])["Yes"] , n = nrow(hh)) * 100, 2))
  if (!is.na(match(sol_safety_crime_victimlist_home.burglary..excluding.home.robbery, names(hh)))) home.burglary = data.frame(round(binconf(table(hh[ ,sol_safety_crime_victimlist_home.burglary..excluding.home.robbery])["Yes"] , n = nrow(hh)) * 100, 2))

  # ADDITIONAL
  if (!is.na(match(sol_income_household_total, names(persoon)))){
  	if (is.numeric(persoon[, sol_income_household_total])){
  		q1 <- data.frame(PointEst = as.integer(quantile(persoon[ ,sol_income_household_total], na.rm=T)[2]), Lower = NA, Upper = NA)
  		lower.poverty.child <- data.frame(with(persoon[which(persoon[ ,demographics_member_age] < 16), ],  binconf(x=sum(table(child.poverty)[1:3]), n=sum(table(child.poverty)))))
  		lower.poverty <- data.frame(with(persoon,  binconf(x=sum(table(poverty.income.cat)[1:grep(low.pov.lab, levels(poverty.income.cat))]), n=sum(table(poverty.income.cat)))))
  		food.dolar.poverty <- data.frame(round(binconf(sum(table(persoon$poverty.income.cat)[c("Food poverty","One twentyfive $ -")]) , n = sum(table(persoon$poverty.income.cat))) * 100, 2))
  	}
  }

  # sit hom bymekaar
  if (verbose == TRUE) message(paste("Formals:\n", formals(), " "))
  # maak 'n lys ban die uitsette sonder die formele argumente en hulpveranderlikkes. hulle kenmerk is dat hulle data frames is
  varlys = ls()[-match(c("persoon", "adult.pop.idx", "adult.pop.n","write", "illiterate", "bag.weight", "hh", "workpop.min", "workpop.max", "imm.max", "outfile", "workpop.idx", "matriculated.n", "workpop.n", "imm.age.idx", "mode.lang"), ls())]
  classlist = sapply(varlys, function(x) class(get(x)) == "data.frame")
  classlist = classlist[which(classlist==TRUE)]
  if (debug == TRUE) assign("classlist", classlist, envir = .GlobalEnv)
  if (debug == TRUE) assign("varlys", varlys, envir = .GlobalEnv)
  if (!any(classlist)) stop("daar moet data frames wees. Sit debug = TRUE en inspekteer classlist")
  varlys = varlys[match(names(classlist), varlys)]
  if (verbose == TRUE) message(paste(varlys, " "))
  if (debug == TRUE) assign("varlys", varlys, envir = .GlobalEnv)

  lys = lapply(varlys, function(x) get(x))
  names(lys) <- varlys
  if (verbose == TRUE) message(paste(sapply(lys, class)), " \n")
  if (debug == TRUE) assign("lys", lys, envir = .GlobalEnv)
  if (require(data.table) == FALSE) message("JY MOET data.table INSTALLEER, MY OU!!")
  res = rbindlist(lys, fill = TRUE)
  res = data.frame(lapply(unclass(res), cbind))
  res$range = paste("(", as.character(res$Lower), ", ", as.character(res$Upper),")", sep = "")
  rownames(res) = varlys

   # order
   if (orden == TRUE){
   	orde = c(1, 2, 32, 8, 16)
   }

  if (write == TRUE) {
   require(openxlsx)
   write.xlsx(res, file=outfile,  row.names = TRUE)
  }
 res
}
