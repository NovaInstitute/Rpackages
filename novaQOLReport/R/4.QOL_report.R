#' Quality of life
#'
#' Quality of life survey processing
#'
#' @export

novaQOL <- function(){
rm(list=ls())

# paaie en name -----------------------------------------------------------
#hier <- "~/Dropbox/Sasol QOL/Working/Questionnaire_Development/Sampling/SampleFrame/"
datadir <- "C:/Users/Willem/Desktop/Nova Packages/NovaQOLReport/novaQOLReport/data/"
tabdir  <- "C:/Users/Willem/Desktop/Nova Packages/NovaQOLReport/Tabelle/"
grafdir <- "C:/Users/Willem/Desktop/Nova Packages/NovaQOLReport/Grafieke/"
multig  <- paste(grafdir, "Multi/", sep="")
multit <- paste(tabdir, "Multi/", sep="")
fundir  <- "C:/Users/Willem/Desktop/Nova Packages/NovaQOLReport/novaQOLReport/R/"
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
#  biblioteke en funksies -------------------------------------------------
source(paste(fundir, "make.formula.c2m.R", sep = ""))
source(paste(fundir, "report.table.R", sep = ""))
source(paste(fundir, "hkIndex.R", sep = ""))
source(paste(fundir, "eentweedrie.R", sep = ""))
source(paste(fundir, "multi.opsie.tabel.R", sep = ""))
source(paste(fundir, "chi.chaid.R", sep = ""))
source(paste(fundir, "swb.bar.R", sep = ""))
source(paste(fundir, "swb.toets.groep.R", sep = ""))
source(paste(fundir, "bartown.R", sep = ""))
source(paste(fundir, "onttrek.hh.indikatore.R", sep = ""))
source(paste(fundir, "pop.prev.table.R", sep = ""))
source(paste(fundir, "pos.negify.R", sep = ""))
#source('~/Dropbox/Rfunctions/chi.2.eksp.R')
#source('~/Dropbox/Rfunctions/afh.tab.R')
#source('~/Dropbox/Rfunctions/tableNominal2.R')
#source('~/Dropbox/Rfunctions/popProject.R')
#source("~/Dropbox/Rfunctions/withWarnings.R")
library(novaSurvey)
library(novaReport)
# if (!require(CHAID)){ install.packages("CHAID")
#         library(CHAID)
#         }
if (!require(partykit)){ install.packages("partykit")
library(partykit)
}
# if (!require(xlsx)){ install.packages("xlsx")
# library(xlsx)
# }
if (!require(vcd)){ install.packages("vcd")
library(vcd)
}
if (!require(FactoMineR)){ install.packages("FactoMineR")
library(FactoMineR)
}
if (!require(reporttools)){ install.packages("reporttools")
library(reporttools)
}
if (!require(Hmisc)){ install.packages("Hmisc")
library(Hmisc)
}
if (!require(ggplot2)){ install.packages("ggplot2")
library(ggplot2)
}
if (!require(reshape2)){ install.packages("reshape2")
library(reshape2)
}
if (!require(doBy)){ install.packages("doBy")
library(doBy)
}
if (!require(plotrix)){ install.packages("plotrix")
library(plotrix)
}

# stel latex op  ---------------------------------------------------------------
Sys.setenv( PATH=paste(Sys.getenv("PATH"),"/usr/texbin",sep=":") )
options(xdvicmd='open')

# laai data ---------------------------------------------------------------
load(paste(datadir, "/huishouding.Rda", sep=""))
load(paste(datadir, "/persoon.Rda", sep=""))
load(paste(datadir, "/struktuur.Rda", sep=""))
load(paste(datadir, "/swb.Rda", sep=""))

######################################################################
# Skep indekse en kategorie"e
#######################################################################
# SWB
qol$QIX = hkIndeks(swb.pca, cuts = c(75, 70, 65) ,  dimensies = 3)
qol$QIX.s <- scale(qol$QIX)
lb="QoLIndex"
capture.output(tableContinuous(vars=list(qol$QIX.s),group  = qol$place, print.pval="anova",
cap = paste("",lb), lab = paste(lb, ".town", sep=""), cumsum=FALSE), file = paste(tabdir, "QIX.tex" ))

#######################################################################
# maak multi-opsie tabelle en masse
#######################################################################
multiq <- qs[which(qs$question.type=="Multiple"), "question.name"]
lapply(multiq  ,function(x)  multi.opsie.tabel(qol, x, tabdir = multit, graphdir = multig))

multiqp <- qs.person[which(qs.person$question.type=="Multiple"), "question.name"]
multiqp[2] <- "body_disease_diagnosed_twelvemonth"
lapply(multiqp  ,function(x)  multi.opsie.tabel(persoon, x, tabdir = multit, graphdir = multig))

multiqs <- qs.struc[which(qs.struc$question.type=="Multiple"), "question.name"]
lapply(multiqs  ,function(x)  multi.opsie.tabel(struktuur, x, tabdir = multit, graphdir = multig))

# toets LSM vs. Veldwerker
Qolfw <- ggplot(data = qol, aes(x =QIX.s, color=place)) + geom_density() + facet_wrap(~fieldworker, ncol = 4)
ggsave(Qolfw, file=paste(grafdir, "QOLidxFW.pdf", sep=""))

# Skep generiese kategorie"e deur hierargiese tros-analise
hcc  <- hclust(dist(qol))
pdf(file=paste(grafdir,"clust.pdf", sep=""), width=14)
plot(hcc)
ihcc <- rect.hclust(hcc, k = 8)
names(ihcc) = LETTERS[1:length(ihcc)]
for (i in 1:length(ihcc)){
  qol[match(names(ihcc[[names(ihcc)[i]]]), row.names(qol)), "tros"] <- names(ihcc)[i]
}
dev.off()

# tabelle vir werksmag
capture.output(tableNominal2(vars=list(wf$sol_occupation), group=wf$place, cumsum = FALSE, longtable = FALSE, cap="Occupation in economically active age by town", lab="WF.occupation", print.pval = "chi2", font.size="small"), file = paste(tabdir, "WF.occupation.tex"))
capture.output(tableNominal2(vars=list(wf$potential.work), group=wf$place, cumsum = FALSE, longtable = FALSE, cap = "Work potential in economically active age by town", lab="WF.potential.work", print.pval = "chi2", font.size="small"), file = paste(tabdir, "WF.potential.work.tex"))
save(wf, file=paste(datadir,"wf.Rda", sep=""))

# maak geannoteerde swb stel vir grafieke
#swbp <- cbind(swb, qol[, c("place","fieldworker","LSM","dirty.fuel","tros")])
#swbm <- melt(swbp,  id.var=c("place","fieldworker","LSM","dirty.fuel","tros"))

#######################################################################
# maak een, tree en drierigting tabelle en masse
#######################################################################

eentweedrie(x = persoon[,-na.omit(match(c("pilot", "lat", "lon", "suburb", "hh.id", "pers.id"), names(persoon)))],groep = "place", groeplab = "persoon.town", moederskip = FALSE, graph = TRUE, graphdir = paste(grafdir, "n.graf/", sep=""))
eentweedrie(x = persoon[,-na.omit(match(c("pilot", "lat", "lon", "suburb", "hh.id", "pers.id"), names(persoon)))],groep = "LSM", groeplab = "persoon.LSM", moederskip = FALSE, graph = TRUE, graphdir = paste(grafdir, "n.graf/", sep=""))

eentweedrie(x = struktuur,
            drops=c("rooms_none_ceiling_bedroom.3..if.there.are.3.bedrooms.", "rooms_ceiling_bedroom.2..if.there.are.2.bedrooms." ,"rooms_ceiling_bedroom", "rooms_none_ceiling_bedroom","rooms_none_ceiling_bedroom.2..if.there.are.2.bedrooms.", "rooms_ceiling_bedroom.3..if.there.are.3.bedrooms." ) ,
            groep = "place", groeplab = "town", moederskip = FALSE, graph = TRUE, graphdir = paste(grafdir, "n.graf/", sep=""))

eentweedrie(x = qol,groep = "place", groeplab = "town", moederskip = FALSE, graph = TRUE, graphdir = paste(grafdir, "n.graf/", sep=""))
eentweedrie(x = qol, groep = "sol_energy_coal_ignition", groeplab = "BM", een = FALSE)

#######################################################################
# maak positief - negatief swb tabelle en masse
#######################################################################
swbn = grep("swb", names(qol), value = TRUE)
lapply(swbn, function(x) swb.bar(data=qol, var = x, group="place", graphdir = grafdir))

# geslag per dorp
windows()
barplot(prop.table(table(persoon$demographics_member_sex, persoon$place),2),
  legend.text=c("Female", "Male"),
  col = c("pink", "blue"), ylab = "Proportion", main = "Sex distribution per town", sub="errorbar = 95%CI",
  args.legend = list(y = 1.15))
pt = prop.table(table(persoon$demographics_member_sex, persoon$place),2)[1,]
tab = addmargins(table(persoon$demographics_member_sex, persoon$place),1)
res.list = apply(tab, 2, function(x) prop.test(x[1], x[3]))
sig.dff = lapply(res.list, function(x) 0.50 < min(x$conf.int) | 0.5 > max(all(x$conf.int < 50)))
center = sapply(res.list, function(x) x["estimate"])
lower = sapply(res.list, function(x) x["conf.int"][[1]][[1]])
upper = sapply(res.list, function(x) x["conf.int"][[1]][[2]])
errbar(c(0.7, 1.9, 3.1, 4.3), y=pt, yplus=upper, yminus=lower, add= TRUE)
abline(h=0.5, col="red")
dev.copy2pdf( file=paste(grafdir, "sex.town.bar.pdf", sep=""))
dev.off()

# Druk proporsie-grafieke per dorp
bartown(persoon, vr="body_immunisation", out=TRUE, ablevel=FALSE, ypos = 1.2, maintxt = "Immunisation status per town")
bartown(persoon, vr="doctor.ever", out=TRUE, ablevel=FALSE, ypos = 1.2, maintxt = "Lifetime doctors visit per town")
bartown(persoon, vr="body_disease_diagnosed_twelvemonth_ulcers", out=TRUE, ablevel=FALSE, ypos = 1.2, maintxt = "Ulcers in past 12 months per town")

bartown(persoon, vr="body_disease_diagnosed_ever_asthma", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Lifetime asthma diagnosis per town")
bartown(persoon, vr="body_disease_diagnosed_ever_arthritis", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Lifetime arthritis diagnosis per town")
bartown(persoon, vr="body_disease_diagnosed_ever_high.blood.pressure", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Lifetime HBP diagnosis per town")
bartown(persoon, vr="body_disease_diagnosed_ever_diabetes", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Lifetime diabetes diagnosis per town")

bartown(persoon, vr="body_morbidity_prevalence", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Six month morbidity per town")
bartown(persoon, vr="body_morbidity_thirtydays", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "30 day morbidity per town")
bartown(persoon, vr="sol_absence_care_prevalence", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "6 absence: care for other member")
bartown(persoon, vr="body_morbidity_rad", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "30 day morbidity per town")

bartown(persoon, vr="body_symptoms_twelvemonth_list_no.complaints.at.all", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "No compomplaints (1 yr) per town")
bartown(persoon, vr="body_symptoms_twelvemonth_list_coughing.sputum.but.no.weight.loss.or.fever.or.night.sweats", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Bronchitis-like symptoms (1 yr) per town")

bartown(persoon, vr="body_symptoms_twelvemonth_list_severe.and.long.lasting.headache", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Severe, long-lasting headaches per town")

bartown(persoon, vr="body_symptoms_twelvemonth_list_three.plus.l.s.p.d", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Diarhea symptoms per town")
bartown(persoon, vr="body_symptoms_twelvemonth_list_wheezing.and.tight.chest.at.night.or.after.exercise", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Asthma-like symptoms per town")
bartown(persoon, vr="body_symptoms_twelvemonth_list_painful..swollen.and.stiff.joints", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Arthritis-like symptoms per town")

bartown(qol, vr="sol_energy_cooking_all_electricity", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Any electricity use for cooking per town")
bartown(qol, vr="sol_energy_cooking_all_coal", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Any coal use for cooking per town")
bartown(qol, vr="sol_energy_cooking_all_wood", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Any wood use for cooking per town")
bartown(qol, vr="sol_energy_cooking_all_paraffin", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Any paraffin use for cooking per town")

bartown(qol, vr="sol_energy_cooking_all_electricity", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Any electricity use for heating per town")
bartown(qol, vr="sol_energy_cooking_all_wood", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Any wood use for heating per town")
bartown(qol, vr="sol_energy_cooking_all_coal", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Any coal use for heating per town")
bartown(qol, vr="sol_energy_cooking_all_paraffin", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Any paraffin use for heating per town")

bartown(qol, vr="sol_household_health_general", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "General health perception  per town", coll = c("darkgreen","red"))
bartown(persoon, vr="sol_household_health_general", place  = "body_morbidity_thirtydays",out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "General health perception by 30d mordibity", coll = c("darkgreen","red"))
bartown(qol, vr="sol_household_health_general", place = "sol_energy_heating_carrier", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "General perception of health by heating energy carrier", coll = c("darkgreen","red"))
bartown(qol, vr="sol_household_health_general", place = "sol_energy_coal_ignition", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "General perception of health per coal ignition", coll = c("darkgreen","red"))

mosaicplot(table(persoon$"sol_household_health_general", persoon$"body_morbidity_thirtydays"), main = "General perception of health by 30 day mordibity", shade=TRUE, type="pearson")

bartown(qol, vr="sol_income_sources_salary.from.work", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Salary per town")
bartown(qol, vr="sol_income_sources_pension", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Pension per town")

bartown(qol, vr="sol_safety_crime_victimlist_street.robbery", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Street robbery per town (only crime victims)")

bartown(qol, vr="sol_environment_air_perception", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Perception of air quality by town")
#bartown(qol, vr="sol_sanitation_toilet_flushsystemactive", out=TRUE, ablevel=FALSE, ypos = 1.15, maintxt = "Flush system working")

## Inkomste plot:
# proporsies
# pt = prop.table(table(qol[,c("povery.income.cat", "place")]),2)
# pp = qplot(data=melt(pt), x=place, y=value, fill=factor(povery.income.cat, levels=povery.income.cat), geom="bar", stat="identity", main="Income poverty by town", ylab="Proportion")
# pp + scale_fill_discrete(name = "Poverty category")
# ggsave(filename=paste(grafdir, "income.poverty.proportions", "bar.pdf", sep=""))

# geprojekteer
# hh.pop <- data.frame(eMbalenhle = 35404, Lebohang = 8908, eMzinoni = 10310,  KwaDela = 982)*(1+1.34/100)^2
# pers.pop <- unlist(summaryBy(respondent_household_membernumber ~ place, data=qol, FUN=mean, na.rm=T)[,2] * hh.pop)
# pers.gewig <-  round(pers.pop[match(persoon$place, names(pers.pop))] / table(persoon$place)[match(persoon$place, names(table(persoon$place)))])
# names(pers.gewig) <- NULL
# hh.gewig <- round(hh.pop[match(qol$place, names(hh.pop))] / table(qol$place)[match(qol$place, names(table(qol$place)))])
#
# abs.pov.nr.m <- melt(sapply(popProjek(var="povery.income.cat", pop=pers.pop), function(x) x[,"PointEst"]))
# ppa <- qplot(data=abs.pov.nr.m, x=V2, y=value, fill=V1, geom="bar", stat="identity", main="Projected income poor individuals by town", ylab="Proportion")
# ppa + scale_fill_discrete(name = "Poverty category")
# ggsave(filename=paste(grafdir, "income.poverty.absolute", "bar.pdf", sep=""))

# projekteer die aantal shacks

popProjek(df=qol, var="has.inf")

#######################################################################
# Anova tipe
#######################################################################
iqol <- qol[,c(names(which(sapply(qol, class) == "integer")),"place", "fieldworker")]
frms <- lapply(1:(length(iqol)-2), function(i) as.formula(paste(names(iqol)[i], " ~ place")))
names(lrs) = names(iqol[1:(length(iqol)-2)])
lrs <- lapply(frms, function(x) if (any(with(iqol, xtabs(x, data=iqol))==0)){ paste("leeg")} else {lm(x, data=iqol)})

###########################################################################
# Kaarte
###########################################################################


######################################################################
# Maak bevolkingspiramide per dorp: ouderdom en geslag
######################################################################

dorp.p <- by(data=persoon,INDICES = persoon$place, FUN = function(x) pop.prev.table(data=x, disease=NULL))
pdf(file=paste(grafdir, "pop.piram.pdf", sep=""))
#jpeg(file=paste(grafdir, "pop.piram.jpg", sep=""), quality=100)
par(mfrow=c(2,2))
lapply(names(dorp.p), function(x) pyramid.plot(dorp.p[[x]][, "Male_1"], dorp.p[[x]][ ,"Female_1"] ,labels=dorp.p[[x]][ ,"Age_group"], main=x, gap=max(unlist(dorp.p[[x]]))*0.21, show.values=TRUE, unit="n", lxcol="blue", rxcol="pink"))
dev.off()


#######################################################################
# Afhanklikhede tussen kategoriese veranderlikkes                     #
#######################################################################
# als in een met chi.chaid(qol, naam = "HH_chi2_uitkomste")
## Huishouding vlak ##
swbi <- grep("swb", names(qol), value = TRUE)
for (i in 1:length(swbi)){
	qol[,swbi[i]] <- ifelse(qol[,swbi[i]] <= 5, "z.Neg", "a.Pos")
	qol[,swbi[i]] <- as.factor(qol[,swbi[i]])
}
chi.chaid(qol, naam = "HH_chi2_uitkomste")

outidx <- as.integer(which(sapply(qol, class) == "factor"))
drops <- grep("sol_waste_household_recycle_plastic|container", names(qol))
if (length(drops) > 0) { outidx <- outidx[-match(drops, outidx)] } else {cat("geen drops")}

chi.2.eksp(data = qol,
           plot=TRUE,
           p.cutoff= 0.01,
           out.idx = outidx,
           outdir = datadir,
           xlsx.name = "HH_chi2_uitkomste.xlsx", verbose = FALSE)

spdf <- lapply(spdf, function(x){
  transform(x,  n = sapply(1:length(x[,1]),
                           function(zz) sum(!is.na(qol[,x[zz,"exposure"]]))),
            cc  = sapply(1:length(x[,1]),
                         function(zz) length(
                           which(complete.cases(qol[,na.omit(
                             match(c(x[zz ,"exposure"], x[zz,"outcome"]),
                                   names(qol)))]
                                     ))==TRUE)))})

#chaid.qol.list <- lapply(spdf, make.formula.c2m)
#qol.chaidlist <- lapply(chaid.qol.list, function(x) chaid(x, data=qol))
#qol.shortform <- lapply(qol.chaidlist, get.short.form.chaid, type = "right")

save(qol, spdf, chaid.qol.list, qol.chaidlist, qol.shortform, file=paste(datadir, "sdpf.Rda", sep=""))

## Persoon vlak ##
# chi.chaid(persoon, naam = "PERS_chi2_uitkomste")

persoon <- persoon[,!duplicated(names(persoon))]
outidx <- as.integer(which(sapply(persoon, class) == "factor" ))
order.idx <- as.integer(which(sapply(persoon, function(x) all(class(x) == c("ordered", "factor"))) == TRUE))
outidx <- c(outidx, order.idx)
drops <- grep("sol_waste_household_recycle_plastic|container", names(persoon))
if (length(drops) > 0) {outidx <- outidx[-match(drops, outidx)]} else {cat("geen drops")}


chi.2.eksp(data = persoon, net.name="net",out.list.name = "persoon.spdf", out.raw=TRUE,
           plot=TRUE,
           p.cutoff= 0.025,
           out.idx = outidx,
           outdir = datadir,
           xlsx.name = "PERS_chi2_uitkomste.xlsx", verbose=TRUE)

p.spdf <- lapply(persoon.spdf, function(x){
  transform(x,  n = sapply(1:length(x[,1]),
                           function(zz) sum(!is.na(persoon[,x[zz,"exposure"]]))),
            cc  = sapply(1:length(x[,1]),
                         function(zz) length(
                           which(complete.cases(persoon[,na.omit(
                             match(c(x[zz ,"exposure"], x[zz,"outcome"]),
                                   names(persoon)))]
                                     ))==TRUE)))})

#### Maak die afhanklikheids tablelle: qol laaste

lapply(p.spdf, function(x) afh.tab(x, tabdir=tabdir, alpha=0.01))
lapply(spdf, function(x) afh.tab(x, tabdir=tabdir, alpha = 0.01))


p.chaid.qol.list <- lapply(p.spdf, make.formula.c2m)
#p.qol.chaidlist <- lapply(p.chaid.qol.list, function(x) chaid(x, data=persoon))
#p.qol.shortform <- lapply(p.qol.chaidlist, get.short.form.chaid, type = "right")
#save(persoon, p.spdf, p.chaid.qol.list, p.qol.chaidlist, p.qol.shortform, file=paste(datadir, "PERS.sdpf.Rda", sep=""))

#lapply(1:length(qol.shortform), function(i) {
#  pdf(file=paste(grafdir, names(qol.chaidlist[i]),"Mosaic.pdf", sep=""),width=21, height=18)
#  mosaic(as.formula(qol.shortform[[i]]), data=qol, gp=shading_Friendly)
#  dev.off()})

lapply(seq_along(p.qol.chaidlist), function(i) {
  pdf(file=paste(grafdir, names(p.qol.chaidlist[i]),"PERS.CHIAD.pdf", sep=""),width=14, height=12)
  plot(p.qol.chaidlist[[i]], main = names(p.qol.chaidlist[i]))
  message(names(p.qol.chaidlist[i]))
  dev.off()
})

#######################################################################
# Analise van multi-opsie groepe
#######################################################################



#######################################################################
# Projeksie na populasie met popProjek
#######################################################################
# Maak namelys van veranderlikkes
qn <- qs$question.name[which(qs$question.type == "Single")][-grep("respondent|lsm|kwadela", qs$question.name[which(qs$question.type == "Single")])]
pn <- qs.person$question.name[grep("Single|Multiple", qs.person$question.type)]
pn <- pn[-grep("relationship_pcg|body_morbidity_cause", pn)]
pn <- pn[na.omit(match(names(persoon), pn))]

# maak die tabelle
lapply(pn, function(x) popProjek(df=persoon, var=x, pop=pers.pop, verbose=T, debug=T))
lapply(qn, function(x) popProjek(var=x, pop=hh.pop))

# ekstras
popProjek(var="incomecat")
popProjek(var="per.capita.income.cat")
popProjek(df=persoon, var="child.poverty", pop=pers.pop)

#######################################################################
# maak indikator opsomming
#######################################################################
indik <- onttrek.hh.indikatore(write=TRUE)
save(indik, file=paste(datadir, "indicators.Rda", sep=""))
}
