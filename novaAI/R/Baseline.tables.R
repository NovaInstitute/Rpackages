#' Baseline Tables
#'
#' Function creates tables for report
#'
#' @param haq Data frame containing HAQ survey data
#' @param basedata Data frame containing baseline data
#' @param uta Data frame containing UTA data
#' @param cl.alt Data frame containing 90 percent confidence
#' interval data for the estimation of BM users attributable to the project
#' activity in the Target Area
#' @param cl.coal Data frame containing 90 percent confidence interval data
#' for number of coal users in the Target Area
#' @param Uother Data frame of data describing coal users converted by other sources
#' @param basedir The baseline directory as character vector
#' @param tabdir Directory to which tables are saved within the baseline directory
#' as character vector
#' @param verbose Displays function messages when TRUE
#' @export

baseline.tables=function(haq,
                         basedata,
                         uta,
                         cl.alt = get("cl.alt", pos = sys.frame(which = 1)),
                         cl.coal = get("cl.coal", pos = sys.frame(which = 1)),
                         Uother,
                         basedir,
                         tabdir,
                         verbose = TRUE){

if(verbose==TRUE) message("haq ", exists("haq"), "\n",
                          "uta ", exists("uta"), "\n" ,
                          "cl.alt", exists("cl.alt"), cl.alt,"\n" )
 ###### Baseline scenario ######
# Coal user
print(
  xtable(uta[,c(1:3,5)],
         align="lrrrr",
         display=c("s","d","d","f","d"),
         caption="Coal users present in the Target Area",
         label="UTA",
         title="Town",
         digits=4),
  hline.after=c(-1,0,nrow(uta)-1,nrow(uta)),
  file=paste(basedir,tabdir,"UTA.tex",sep="")
  )
message("Coal users written")

# Confidence interval number of Coal users
print(
  xtable(cl.coal ,
         caption="90 percent confidence interval for number of coal users in the Target Area",
         title="Town",
         label="CIUTA",
         align="lx{1cm}rx{1cm}rx{1cm}r",
         display=c("s","f","fg","f","fg","f","fg"),
         digits=4),
  file=paste(basedir,tabdir,"CIUTA.tex",sep=""))
message("cl.coal written")

# BM users
print(
  xtable(uta[,1:6],
         caption="BM users present in the Target Area",
         label="Ualt",
         align="lrrrrrr",
         display=c("s","fg","fg","f","f","fg","fg"),
         digits=4,
         title="Town"),
  hline.after=c(-1,0,nrow(uta)-1,nrow(uta)), file=paste(basedir,tabdir,"Ualt.tex",sep="")
  )
message("uta written")

# CI. for above
print(
  xtable(cl.alt,
         caption="90 percent confidence interval for the estimation of BM users attributable to the project activity in the Target Area",
         label="UaltCL",
         align="lx{1cm}rx{1cm}rx{1cm}r",
         display=c("s","f","fg","f","fg","f","fg"),
         title="Town", digits=4),
  hline.after=c(-1,0,nrow(cl.alt)-1, nrow(cl.alt)),
  file=paste(basedir,tabdir,"UaltCL.tex",sep="")
  )

message("cl.alt written")

# Other BM users
print(
  xtable(Uother,
         align="lrrrrrx{1cm}x{1cm}x{1cm}x{1cm}",
         display=c("s","f","f","f","f","f","f","f","f","f"),
         caption="Coal users conveted by other sources",
         label="Uother",
         title="Town",
         digits=c(0,0,0,4,4,4,0,0,0,0)),
  hline.after=c(-1,0,nrow(Uother)-1,nrow(Uother)),
  file=paste(basedir,tabdir,"Uother.tex",sep="")
  )
message("Uother written")


###### Baseline emissions ######
#Sample from baseline coal use variables
basedata$"BM start date"=as.character(as.Date(as.numeric(basedata$"BM start date"),origin="1970-01-01"))


latex(basedata[sample(1:length(basedata[,1]),5),c(2,5:10)],
      col.just=c("l","l","r","x{1.6cm}","x{1.2cm}","x{1.2cm}","x{1.2cm}","r"),
      display=c("d","s","s","s","f","f","f","f"),
      caption="Sample from baseline coal use variables",
      multicol=FALSE, numeric.dollar = FALSE,
  label="basedata",title="record.nr.",
  rowlabel = "ID",
  digits=4,
  hline.after=c(-1,0,5) ,
  file=paste(basedir,tabdir,"basesample.tex",sep="")
  )
message("basedata sameple printed")

## Winter eef
if(is.na(match("bm.saved.coal.winter.0",names(haq)))==TRUE){
winter.eef=summaryBy(coal.units.winter.before.bm+coal.units.winter.after.bm+eef~town +coal.buying.format, data=haq[which(haq$hh.BM.use=="BM"),], FUN=meann)
} else {
  winter.eef=summaryBy(coal.units.winter.before.bm.0+coal.units.winter.after.bm.0+eef~town +coal.buying.format.r, data=haq[which(haq$hh.BM.use=="BM"),], FUN=meann)
}
colnames(winter.eef)=c("Town", "Format","Units pre (mean)","Units pre (n)","Units pre (NA)","Units post (mean)","Units post (n)","Units post (NA)", "eef (mean)","eef (n)", "eef (NA)")
winter.eef=winter.eef[,-c(5,8)]

latex(winter.eef,
         caption = "Summary of winter $eef$ values per town and format" ,
         display=c("s","s","f","d","f","d","f","d","d"),
         col.just=c("x{1.2cm}","x{1.2cm}","x{1.1cm}","x{1.2cm}","x{1.1cm}","x{1.1cm}","x{1.1cm}","x{1.1cm}","x{1.1cm}"),
         label="winterEEF",
         multicol = FALSE, numeric.dollar = FALSE,
         title="", rowname=NULL,
         digits=4,
  file=paste(basedir,tabdir,"winterEEF.tex",sep="")
  )
 message("winter.eef sameple printed")
}

#' Baseline Sample
#'
#' Creates sample tables for inspection
#'
#' @param haq Data frame containing HAQ survey data
#' @export

baseline.sample=function(haq){
## Conversion from bags to kg: winter and summer sample for inspection
# Winter
  if(is.na(match("bm.saved.coal.winter.0",names(haq)))==TRUE){
ws=haq[sample(which(haq$hh.BM.use=="BM"),5),c("current.coal.units.winter",
                                              "kg",
                                              "winter.current.kg",
                                              "totalwinter.current.kg",
                                              "winterdays.used",
                                              "winter.current.year",
                                              "hh.BM.use")]
                                                             } else {
ws=haq[sample(which(haq$hh.BM.use=="BM"),5),c("current.coal.units.winter.0",
                                              "kg",
                                              "winter.current.kg",
                                              "totalwinter.current.kg",
                                              "winterdays.used",
                                              "winter.current.year",
                                              "hh.BM.use")]
                                                                      }

names(ws)=gsub("\\.","\\ ",names(ws))
names(ws)=gsub("0","",names(ws))

latex(ws,
      caption = "Application of format weights: Winter (sample)" ,
            col.just   = c("x{1.4cm}","r","x{1.4cm}","x{2cm}","x{2cm}","x{2cm}","x{1.5cm}"),
            multicol = FALSE, numeric.dollar = FALSE,
            label   = "wweight",
            digits = 3,
            rowlabel = "ID",
            file=paste(basedir,tabdir,"wweight.tex",sep=""))

message("wweight printed")

# Summer
  if(length(which(haq$hh.summer.BM.use=="BM"))>0){
  if(is.na(match("bm.saved.coal.winter.0",names(haq)))==TRUE){
sss=haq[sample(which(haq$hh.summer.BM.use=="BM"),5,replace = TRUE),names(haq)[na.omit(match(c("current.coal.units.summer",
                                                                                              "kg",
                                                                                              "summer.current.kg",
                                                                                              "totalsummer.current.kg",
                                                                                              "summerdays.used",
                                                                                              "summer.current.year",
                                                                                              "hh.BM.use"),names(haq)))]]
                                                              }else{
sss=haq[sample(which(haq$hh.summer.BM.use=="BM"),5,replace = TRUE),names(haq)[na.omit(match(c("current.coal.units.summer.0",
                                                                                              "kg",
                                                                                              "summer.current.kg",
                                                                                              "totalsummer.current.kg",
                                                                                              "summerdays.used",
                                                                                              "summer.current.year",
                                                                                              "hh.BM.use"),names(haq)))]]
                                                                      }

names(sss)=gsub("\\.","\\ ",names(sss))
names(sss)=gsub("0","\\ ",names(sss))

latex(sss,
      caption = "Application of format weights: Summer (sample)" ,
      display = c(rep("d",6),"s"),
      col.just   = c("x{2cm}","r","x{1.4cm}","x{2cm}","x{2cm}","x{2cm}","x{1.5cm}"),
      multicol = FALSE, numeric.dollar = FALSE,
      label   = "sweight",
      digits = 4,
      rowlabel = "ID",
      file=paste(basedir,tabdir,"sweight.tex",sep="")
  )
  message("sweight printed")
  }
## Sample of baseline calculation
# Winter
eefsample=haq[sample(which(haq$hh.BM.use=="BM"),5,replace = TRUE),c("eef", "winter.current.kg","winterbase","totalwinter.current.kg","winterconbase.year","winterdays.used")]
names(eefsample)=gsub("\\.","\\ ",names(eefsample))
names(eefsample)=gsub("(winter)(*)","\\1\\ ",names(eefsample))

latex(eefsample,
      caption = "Calculation of seasonal project and baseline coal consumption (sample)",
      display = c("d","f",rep("d",5)),
      col.just = c("r","x{1.4cm}","x{1.4cm}","x{1.4cm}","x{2cm}","x{2cm}","x{1.5cm}"),
      multicol =FALSE,numeric.dollar = FALSE,
      label = "seef",
      digits = 4,
      title = "ID",
      file=paste(basedir, tabdir,"seef.tex",sep="")
         )
  message("eef sample printed")
# Summer
  if(length(which(haq$hh.summer.BM.use=="BM"))>1){
s.eefsample=haq[sample(which(haq$hh.summer.BM.use=="BM"),5,replace=TRUE),names(haq)[na.omit(match(c("eef.s", "summer.current.kg","summerbase","totalsummer.current.kg","summerconbase.year","summerdays.used"),names(haq)))]]
names(s.eefsample)=gsub("\\.","\\ ",names(s.eefsample))
names(s.eefsample)=gsub("(summer)(*)","\\1\\ ",names(s.eefsample))

latex(s.eefsample,
         caption="Calculation of summer project and baseline coal consumption (sample)" ,
         display=c("d","f",rep("d",5)),
         col.just=c("r","x{1.4cm}","x{1.4cm}","x{1.4cm}","x{2cm}","x{2cm}","x{1.5cm}"),
         multicol = FALSE, numeric.dollar = FALSE,
         label="sseef",
         rowlabel="ID",
         digits=4,
  file=paste(basedir,tabdir,"sseef.tex",sep="")
  )
message("s.eef sample printed")
}
## Annual

yearbase=haq[sample(which(haq$hh.BM.use=="BM"),5),names(haq)[na.omit(match(c("hh.summer.BM.use","winterconbase.year","summerconbase.year","annual.base"),names(haq)))]]
names(yearbase)=gsub("\\.","\\ ",names(yearbase))
names(yearbase)=gsub("(winter)(*)","\\1\\ ",names(yearbase))
names(yearbase)=gsub("(summer)(*)","\\1\\ ",names(yearbase))

latex(yearbase,
      caption="Calculation of annual baseline coal consumption (sample)" ,
      display=c("d","s",rep("d",3)),
      col.just=c("l","l","x{2cm}","x{2cm}","x{2cm}"),
      multicol = FALSE, numeric.dollar = FALSE,
      label="yearbase",
      rowlabel="ID",
      digits=4,
  file=paste(basedir,tabdir,"yearbase.tex",sep="")
  )
  message("yearbase sample printed")
# Summary
label(haq$annual.base) = "Conservatively calculated annual coal baseline in kg"

basevars=as.list(haq[which(haq$hh.BM.use=="BM"), match(c("winterconbase.year","summerconbase.year","annual.base"), names(haq))])
basenams=Hmisc::label(haq[which(haq$hh.BM.use=="BM"),match(c("winterconbase.year","summerconbase.year","annual.base"), names(haq))])

capture.output(tableContinuous(vars = basevars,
                               nams = basenams,
                               stats = c("n", "min", "mean", "max", "na"),
                               cap = "Baseline coal use", lab = "baseline",
                               longtable = FALSE,prec = 2),
               file=paste(basedir,tabdir,"basesummary.tex",sep=""))

}
