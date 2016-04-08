#' Basa Magogo coding
#'
#' This function is used to refine and manipulate any data contained in the data frame that relates to
#' BM usage. Once the function has been carried out the the edited data frame is assigned to the global environment
#'
#' @param haq Data frame that contains the BM survey data
#' @param coal.use Character vector referring to the variable specifying whether a person
#' uses coal or not
#' @param contact Character vector referring to the variable containing contact information. Contains
#' either "yes" or "no"
#' @param verbose Logical taht displays function messages when TRUE
#' @export

code.bm <- function(haq=haq,
                    coal.use = "coal.use",
                    contact = "contact",
                    verbose = FALSE){
# Recode multi.firemaker..coal.ignition
haq$multi.firemaker..coal.ignition=as.character(haq$multi.firemaker..coal.ignition)
haq$multi.firemaker..coal.ignition[which(haq$multi.firemaker..coal.ignition=="Everyone uses BM")] = "BM"
haq$multi.firemaker..coal.ignition[which(haq$multi.firemaker..coal.ignition=="Everyone uses Hybrid BM")] = "BM"
haq$multi.firemaker..coal.ignition[which(haq$multi.firemaker..coal.ignition=="Some use BM and others Bottom up")] = "BM"
haq$multi.firemaker..coal.ignition[which(haq$multi.firemaker..coal.ignition=="Everyone uses Bottom up")] = "Bottom Up"
haq$multi.firemaker..coal.ignition=as.factor(haq$multi.firemaker..coal.ignition)

if(verbose==TRUE) message("Multi firemakers formated")

# Create households BM use variable that combines all firemakers. If one uses BM the household is considered a BM using household  BM
bm.idx = which(haq$hh.coal.ignition=="BM" |
  haq$hh.coal.ignition=="Hybrid BM" |
  haq$multi.firemaker..coal.ignition=="BM" |
  haq$multi.firemaker..coal.ignition=="Hybrid BM")

haq[bm.idx, "hh.BM.use"] = "BM"

# Hybrid BM is BM
haq[which(haq$hh.firemaker.id=="Myself only" & haq$hh.coal.ignition=="Hybrid BM"),"hh.BM.use"] = "BM"

if(verbose==TRUE) message("Multi firemakers unified")

# In cases where there is only one firemaker who uses bottom up it is bottom up
haq[which(haq$hh.firemaker.id=="Myself only" & haq$hh.coal.ignition=="Bottom Up"),"hh.BM.use"] = "Bottom Up"
# set to "Bottom Up" if they say no one uses BM
haq[which(haq$hh.firemaker.id=="Different people" & haq$bm.saved.coal.winter=="Nobody in this house uses BM"),"hh.BM.use"] = "Bottom Up"
# If some use BM and some don't, it is still a BM using household
haq[which(haq$hh.firemaker.id=="Different people" & haq$multi.firemaker..coal.ignition=="Some use BM and others Bottom up"),"hh.BM.use"] = "BM"
# Same applies if some use BM Hybrid
haq[which(haq$hh.firemaker.id=="Different people" & haq$multi.firemaker..coal.ignition=="BM Hybrid"),"hh.BM.use"] = "BM"
haq[which(haq$hh.firemaker.id=="Different people" & haq$multi.firemaker..coal.ignition=="Everyone uses Hybrid BM"),"hh.BM.use"] = "BM"


if(verbose==TRUE) message("Multi firemakers coded")

# set missing values for hh.BM.use to "Bottom Up" to be conservative
haq[which(is.na(haq$hh.BM.use)==TRUE & is.na(haq$bm.saved.coal.winter)==TRUE & haq[ ,coal.use]=="Yes"),"hh.BM.use"]="Bottom Up"

if(verbose==TRUE) message("Missing values set to Bottom Up")

# In cases where the household doesn't use coal it is "I don't use coal"
haq[which(haq[ ,coal.use]=="No"),"hh.BM.use"] = "I don\'t use coal"
haq[which(haq$historical.coal.use=="Yes" & haq[ ,coal.use]=="No"),"hh.BM.use"] = "I don\'t use coal anymore"

if(verbose==TRUE) message("I don't use coal ")

# Summer BM use is the same as hh.BM.use but has two additional categories: "BM no summer", and "Historic BM no summer"
haq$hh.summer.BM.use = haq$hh.BM.use
haq$hh.summer.BM.use = as.character(haq$hh.summer.BM.use)
haq[which(haq$hh.BM.use == "BM" & haq$coal.use.in.summer == "No"),"hh.summer.BM.use"] = "BM no summer"
haq$hh.summer.BM.use = as.factor(haq$hh.summer.BM.use)

if(verbose==TRUE) message("summer BM coded")

####### Format bm.saved.coal.winter for complete tables
haq$bm.saved.coal.winter = as.character(haq$bm.saved.coal.winter)
haq[which(haq$hh.BM.use=="Bottom Up"),"bm.saved.coal.winter"] = "Nobody in this house uses BM"
haq[which(haq$hh.BM.use=="I don\'t use coal"),"bm.saved.coal.winter"] = "I don\'t use coal"
haq$bm.saved.coal.winter = as.factor(haq$bm.saved.coal.winter)

if(verbose==TRUE) message("bm.saved.coal.winter coded")

### If someone uses Hybrid BM and they do not save, then bm.saved.coal.summer is not "Nobody in this house uses BM" but "No"
haq[which(haq$coal.use.in.summer=="Yes" & (haq$multi.firemaker..coal.ignition=="Hybrid BM") & haq$bm.saved.coal.winter=="No"),"bm.saved.coal.summer"] = "No"
### If no-one used BM the ignition method is "Bottom up"
haq[which(haq$bm.saved.coal.summer=="Nobody in this house uses BM"),"hh.BM.use"] = "Bottom Up"

### A Bottom Up user cannot report savings because of BM since she does not use BM. Sometimes the fieldworker marks "No" instead of "Nobody in this house uses BM"
# It should always be "Nobody in this house uses BM". Make sure it is by:
# Winter
haq[which(haq$hh.BM.use=="Bottom Up"),"bm.saved.coal.winter"] = "Nobody in this house uses BM"
haq$bm.saved.coal.summer = as.character(haq$bm.saved.coal.summer)
# Summer
haq[which(haq$hh.BM.use == "Bottom Up"),"bm.saved.coal.summer"] = "Nobody in this house uses BM"
haq$bm.saved.coal.summer = as.factor(haq$bm.saved.coal.summer)

# Mark users who converted becuase of other reasons
haq$hh.BM.use=as.character(haq$hh.BM.use)
haq[which(haq[,"introduction.to.bm"]=="Radio"|
  haq[,"introduction.to.bm"]=="Billboard"|
  haq[,"introduction.to.bm"]=="Newspaper/magazine"|
  haq[,"introduction.to.bm"]=="Other"),"hh.BM.use"]="BM other"

haq[which(haq$year.of.initiation < project.startyear), "hh.BM.use"] = "BM old"

bu.idx = which(is.na(haq$hh.coal.ignition) & haq$multi.firemaker..coal.ignition == "Bottom Up")
if (length(bu.idx) > 0 ) haq[bu.idx, "hh.BM.use"] <- "Bottom Up"

if(verbose==TRUE) message("Nou gaan ons almal wat nie tuis was nie uitsnoei" )
keep.idx = which(haq[, contact]=="Yes" | haq[, contact]=="No")
if(verbose==TRUE) message("keep.idx is ", length(keep.idx), " Dim haq is ", dim(haq) )

if(length(keep.idx) > 0 ) haq = haq [keep.idx, ]
if(verbose==TRUE) message("Dim haq is ", dim(haq) )

assign("haq",haq,envir=.GlobalEnv)

}
