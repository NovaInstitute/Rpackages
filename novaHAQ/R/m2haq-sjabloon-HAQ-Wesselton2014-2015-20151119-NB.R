#' HAQ Wesselton
#'
#' This function is used in conjunction with the functions contained in the novaHAQ package
#' to create the necessary data files, tables and xlsx files for evaluating the Basa Magogo
#' method and the savings related to it.
#'
#' @export

haqWesselton <- function(){
rm(list=ls())

# Define paths
dropdir   <- '/Users/nova/Dropbox/'
thisplace <-'/Users/nova/Dropbox/Verifikasie2014/HAQ-Wesselton/'
basedir  <- "Wesselton/"
gendir   <- '/Users/nova/Dropbox/Verifikasie2014/General_data'
dir      <- "Rscripts/"
dirr     <- "Rscripts/"
tabdir   <- "tables/"
dataname <- "M14"
project.startyear <- 2010
impyear  <- 2014
town     <- "Wesselton"
projectname <- paste(town,impyear,sep="")
filename    <- "HAQ-Wesselton2014-2015_haq-inspektion-final.csv"
#cb.filename <- "HAQ_-_Wesselton_Monitoring_2014_Codebook_oldway.csv" - Reeds gehardloop in "Skoonmaak van data"
coalm.filename <- "HAQ-Wesselton-Coalmerchant Survey 2014-2015 - 20151104.csv"
datadir     <- paste(thisplace,"data/",sep="")
#merc.script <- "merchants_recode_NFS2012.R"
contact = "coal.use"

#load packages
library(Hmisc)
library(prettyR)
library(xtable)
library(chron)
library(plyr)

############## Set up dates #############
season.dates(impyear=impyear)

##############  ¡¡¡¡¡¡!!!!!!!¡¡¡¡¡¡###################
# If the respondent stated using the technique after the end of the period the winterdays.used value is negative

############### Import data  ###########
# Questionnares & CodeBook
haq = read.csv(paste(thisplace,"Data/",filename, sep=""))

## YEARS
haq$year.of.initiation <- unfactor(haq$year.of.initiation)
haq[which(haq$year.of.initiation < 2000 & haq$introduction.to.bm == "Demonstration"), "year.of.initiation"] <- 2012

###het nie begin maand gehad nie
haq[which(haq$bm.usage.start.month == 0), "bm.usage.start.month"] <- 6


# Simplify the levels of haq$hh.coal.ignition and fix 'bnm' to 'bm'
levels(haq$hh.coal.ignition)=c("BM","Bottom Up","Hybrid BM")
colnames(haq)[grep("bnm",colnames(haq))] <- "bm.demo.invitation."

#
# Subset to remove 'not at home' etc.
min.pop=sum(table(haq$town))
if(verbose>1) message("min.pop  = ", min.pop)
approached = length(haq[,1]) # capture number of houses approached before cleaning uo data set
idx = which(haq[,contact]=="Yes" |   haq[,contact]=="No")
if(length(idx)>0) haq = haq[idx,] # use only records where an interview could take place
if(verbose>1) message("haq shortended to  = ", length(haq[,1]))

#

###### Fix fieldworker error (e.g. very high values)
#haq[which(haq$current.coal.units.winter>6 & haq$fieldworker=="Mapoloane Mokoena (da510202-473a-4f87-b310-89f2dbeb1dc0)"),"current.coal.units.winter"]=haq[which(haq$current.coal.units.winter>6 & haq$fieldworker=="Mapoloane Mokoena (da510202-473a-4f87-b310-89f2dbeb1dc0)"),"coal.units.winter.after.bm"]
#### Disqualify or correct corrupt records
# drop.idx <- which(haq$response.id=="d0137efd-0403-40d5-8379-969d7ad01d7d")
# if(length(drop.idx) > 1) haq = haq[-drop.idx,] # user giving coal use as 800 units (probably meant kg?)
# haq[which(haq$year.of.initiation==1020),"year.of.initiation"] = 2010 # lisdeksia?
# haq[which(haq$year.of.initiation==1 |haq$year.of.initiation==2 |haq$year.of.initiation==3),"year.of.initiation"]=2010 # Conservative option is to set to the most recent year

haq$coal.units.winter.before.bm <- unfactor(haq$coal.units.winter.before.bm )
haq$coal.units.winter.after.bm <- unfactor(haq$coal.units.winter.after.bm )
haq$coal.units.summer.before.bm <- unfactor(haq$coal.units.summer.before.bm )
haq$coal.units.summer.after.bm <- unfactor(haq$coal.units.summer.after.bm )
haq$current.coal.units.winter <- unfactor(haq$current.coal.units.winter )
haq$current.coal.units.summer <- unfactor(haq$current.coal.units.summer )
haq$coal.saved.initial.summer <- unfactor(haq$coal.saved.initial.summer )

#### Define the formats needed immediately
initial.labels(haq=haq)

#### code BM use
code.bm(haq, coal.use="coal.use", contact="contact", verbose=TRUE)


############### Define format weights ##################
#if(length(haq$location.1)>0) haq$town <- haq$location.1

# Preproces
clm = read.csv(paste(thisplace,"Data/",coalm.filename,sep=""), stringsAsFactors=FALSE)
if (length(which(clm$Date == impyear)) > 0) clm <- clm[which(clm$Date == impyear), ]

names(clm)[which(names(clm) == "Main.Place")] <- "Town"
names(clm)[which(names(clm) == "Submission.Id")] <- "Response.ID"
names(clm)[which(names(clm) == "Trade Name")] <- "Trade.Name"

##0 waardes word vervang met NA om gemiddelde reg te kry.
clm[clm==0] <- NA

clm$date.r = as.Date(clm$Received, format= "%d-%m-%Y %H:%M:%S")

Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
### Print coal merchant survey tables
coalm.tables(subplace = FALSE,csv = clm,debug=TRUE,verkort=FALSE,
             selectvar = haq$main.place,
             fn.s=paste(datadir,sep="") ,
             tabdirr=paste(thisplace,tabdir,sep="")
)

haq$main.place <- as.character(haq$main.place)
haq[which(haq$main.place == "HAQ-EMMsouth"), "main.place"] <- "EMMSouth"
haq$main.place <- as.factor(haq$main.place)


haq$kg <- paste(haq$main.place, haq$coal.buying.format, sep="")#Create new variable that is a combination of town and coal buying format
haq$kg <- sub('[[:alpha:]]+NA', NA, haq$kg)
haq$kg <- coalm.sum[match(tolower(haq$kg), tolower(paste(coalm.sum$Town, gsub("\\.", " ", coalm.sum$Format), sep=""))) ,"$mean$"]

##het nie gewigte gehad - geen steenkool om te meet - gebruik waardes se gemiddeldes wat ons het
haq[which(haq$coal.buying.format == "Big Bag" & is.na(haq$kg)), "kg"] <- 46.22
haq[which(haq$coal.buying.format == "Large drum" & is.na(haq$kg)), "kg"] <- 50.00 ###conservative
haq[which(haq$coal.buying.format == "Small Bag" & is.na(haq$kg)), "kg"] <- 36.98 ###80% of Big bag
haq[which(haq$coal.buying.format == "Tin" & is.na(haq$kg)), "kg"] <- 16.54875

#Create an ftable of town, format and weight to be written to tex file
kg.idx = which(is.na(haq$kg)==FALSE)
Kg = as.factor(haq$kg[kg.idx])
Town = haq$main.place[kg.idx]
Format = haq$coal.buying.format[kg.idx]
kg.t <- ftable(Kg~Town+Format)
kg.t.l <- capture.output(xtableFtable(kg.t, label = "kgtown",
                       caption = "Applied format weights by town"))
writeLines(kg.t.l, con = paste(thisplace,tabdir,"kgTown.tex",sep=""))


haq$date.r = as.Date(haq$received, format= "%d-%m-%Y %H:%M:%S")
haq$received = as.Date(haq$received, format= "%d-%m-%Y %H:%M:%S")

## YEARS
if(nchar(gsub('([[:digit:]]{1,4})-([[:digit:]]{1,2})-([[:digit:]]{1,4}) ([[:digit:]]{2}):([[:digit:]]{2}):([[:digit:]]{2})', '\\1', haq$received[[1]])) < 4){
  haq$received <- as.POSIXct(haq$received, format= "%d-%m-%Y %H:%M:%S")
}

bm.years(haq, verbose=TRUE)

##CREATE COAL USE AND SAVE VARIABLES

code.coal(haq, estimate.summer=TRUE,
          summer.est.var = "memory.coal.units.summer.before.bm",
          winter.est.var = "memory.coal.units.winter.before.bm",
          estimate.winter=TRUE,
          winter.est.value="No",
          summer.est.value="No",
          verbose = TRUE)

### Inspect high eef
# winter
high.idx.w <- which(haq$eef>4)
u.idx = c(grep("unit", names(haq)), grep("eef", names(haq)), grep("kg", names(haq)))
if(length(high.idx.w) > 0) haq[high.idx.w,  u.idx]

# summer
high.idx.s <- which(haq$eef.s>4)
if(length(high.idx.s) > 0) haq[high.idx.s, u.idx]

###eef missing values
###As op skerm onder wil sien
print(haq[which(is.na(haq$eef) & haq$hh.BM.use == "BM"),])
##As in view formaat wil sien
View(haq[which(is.na(haq$eef)),])

haq[which(is.na(haq$eef)), "eef"] <- 1.5
haq[which(is.na(haq$eef.s)), "eef.s"] <- 1.5

# missingeef <- is.na(haq$eef)
# haq[missingeef, "eef"] <- 1.5
# missingeefs <- is.na(haq$eef.s)
# haq[missingeefs, "eef.s"] <- 1.5

#LABELS AND FORMATS
haq.labels(haq)

# Save
save(haq,
     approached,
     min.pop,
     coalm,
     merc.tab,
     price.tab,
     file=paste(datadir,dataname,gsub(" ","",(substr(date(),5,10))),"volledig-skoondata.Rda",sep=""))

### Print coal merchant survey tables
##source(paste(thisplace,dirr,merc.script,sep=""))
##merchant.tables(datadir=datadir)
}
