#' Emission Response
#'
#' Function to create Wesselton combination Winter 2014 ER calculation short form
#'
#' @export

haqER <- function(){
rm(list=ls())
# load("/Users/nova/Dropbox/Verifikasie2014/HAQ-Wesselton/data/M14Nov23volledig-skoondata.Rda")
# load("/Users/nova/Dropbox/verifikasie2014/HAQ-Wesselton/ImplementationData/Attendance and Invitation/HAQ-WesseltonCombined2012_2013-impdata.Rda")
# pop = read.csv(paste("/Users/nova/Dropbox/Verifikasie2014/HAQ-Wesselton/data/populations.live.csv"))

load("/Users/nova/Dropbox/Verifikasie2014/HAQ-Wesselton/data/M14Nov23volledig-skoondata.Rda")
load("/Users/nova/Dropbox/verifikasie2014/HAQ-Wesselton/ImplementationData/Attendance and Invitation/HAQ-WesseltonCombined2012_2013-impdata.Rda")
pop = read.csv(paste("/Users/nova/Dropbox/Verifikasie2014/HAQ-Wesselton/data/populations.live.csv"))

library(gdata)

##Quality Control - delete record with bmstartyearmonth  2015-04-15 , resulting in negative winterdays used
#dropidx <- which(haq$response.id == "907de848-4067-40f9-8cd3-a46c811bd59a")
#if (length(dropidx) > 0) haq <- haq[-dropidx, ]
####Stel rekord se winterbase. winterconbase en winterconbase.year na NA - respondent se geheue van units before en untis after nie stabiel.
#haq[which(haq$response.id == "101a8acb-30ba-4ae3-a63e-49fd82cfd5a9"), c("winterbase","winterconbase","winterconbase.year")] <- NA

haq$contact.coal.use = haq$coal.use
haq$response.id = haq$submission.id

##haal  Ext uit - geen BM users "
haq <- haq[haq$town!=c("Nederland Park"),]
haq <- haq[haq$town!=c("Sakhile Ext 2"),]
haq <- haq[haq$town!=c("Tsakane Ext 8"),]




meth = "new"
haq$town <- as.character(haq$town)

test.AI(haq, pop)

calculateER.AI(dropdir = "/Users/nova/Dropbox/",
               gendir = "/Users/nova/Dropbox/Verifikasie2014/General_data/",
               write.date = "Nov04",
               reg.date = as.date("2010/02/12"),
               fundir = "/Users/nova/Dropbox/Rfunctions/",
               basedir = "/Users/nova/Dropbox/Verifikasie2014/HAQ-Wesselton/",
               tabdir = "tables/",
               projectname = "HAQ-Wesselton2014-2015", verbose=2)

# calculateER.AI(dropdir = "/Users/nova/Dropbox/",
#                gendir = "/Users/nova/Dropbox/Verifikasie2014/General_data/",
#                write.date = "Nov04",
#                reg.date = as.date("2010/02/12"),
#                fundir = "/Users/nova/Dropbox/Rfunctions/",
#                basedir = "C:/Users/Willem/Desktop/Nova Packages/NovaHAQ/",
#                tabdir = "tables/",
#                projectname = "HAQ-Wesselton2014-2015", verbose=2)

# Sit nou hier is test.ER in
BE - PE == ER
round(BE.s + BE.w) == round(BE)
round(PE.s + PE.w) == round(PE)

CPy * cl.alt[4] == CPc
CPy.w * cl.alt[4] == CPc.w
CPy.s * cl.alt[4] == CPc.s
round(CPc.w + CPc.s) == round(CPc)


round(CPy.s + CPy.w) == round(CPy)



########
calculate.BM.manage("/Users/nova/Dropbox/Verifikasie2014/HAQ-Wesselton/tables/HAQ-Wesselton2014-2015all.Rda")
x = calculate.BM.manage("/Users/nova/Dropbox/Verifikasie2014/HAQ-Wesselton/tables/HAQ-Wesselton2014-2015all.Rda")
x$BM.User.per.Area
addmargins(x$BM.User.per.Area)

}
