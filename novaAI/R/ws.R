#' Winter Savings
#'
#' Function to make winter savings table in final report
#'
#' @param x An R object (Not used in this function)
#' @note This function will require the BMbuurt and haq data frames to be present in the global
#' invironment in order to work
#' @export

ws=function(x){
year=rep("2008", length(BMbuurt[,1]))

projection=round(BMbuurt$point.BM)

m=ldply(by(haq$total.winter.save[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")], haq$town[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")],mean, na.rm=TRUE))

stddev=ldply(by(haq$total.winter.save[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")], haq$town[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")],sd, na.rm=TRUE))

tonnes=projection*m[,2]

co2=tonnes*2.21
co2bmb=BMbuurt$town.winter.ER

tab=cbind(year, projection, m, stddev, round(tonnes/1000,2), co2, round(co2bmb/1000),2)[,c(3,1,2,4,6,7,9)]

colnames(tab)=c("Town","Year","BM users", "Mean coal saved(kg)", "Stddev","Total coal saved (t)","CO2 saved(t)")

tabb=xtable(tab,caption="Summarised coal savings per town:Winter", label="Summary_coal_W", digits=2,align=c("l","l",rep("r",length(tab)-1)),display=c("s","d","d",rep("f",length(tab)-2))  )

print(tabb,file=paste(tabdir,"winterfinalsummary.tex",sep=""),caption.placement="top", caption.lot="winterfinalsummary",floating.environment="sidewaystable")
}

#' Summer Savings
#'
#' Function to make summer savings table in final report
#'
#' @param x An R object (Not used in this function)
#' @note This function will require the BMbuurt and haq data frames to be present in the global
#' invironment in order to work
#' @export

ss=function(x){
year=rep("2008", length(BMbuurt[,1]))

s.projection=round(BMbuurt$point.BM.s)

m.s=ldply(by(haq$total.summer.save[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))], haq$town[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))],mean, na.rm=TRUE))

s.stddev=ldply(by(haq$total.summer.save[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))], haq$town[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))],sd, na.rm=TRUE))

s.tonnes=s.projection*m.s[,"V1"]

co2.s=s.tonnes*2.21
co2bmb.s=BMbuurt$town.summer.ER

tab.s=cbind(year, s.projection, m.s, s.stddev, round(s.tonnes/1000,2), co2.s, round(co2bmb.s/1000,2))[,c(3,1,2,4,6,7,9)]

colnames(tab.s)=c("Town","Year","BM users", "Mean coal saved(kg)", "Stddev","Total coal saved(t)","CO2 saved(t)")

tabb.s=xtable(tab.s,caption="Summarised coal savings per town: Summer", label="Summary_coal_S", digits=2,align=c("l","l",rep("r",length(tab.s)-1)),display=c("s","d","d",rep("f",length(tab.s)-2)) )

print(tabb.s,file=paste(tabdir,"summerfinalsummary.tex",sep=""),caption.placement="top", caption.lot="summerfinalsummary",floating.environment="sidewaystable")
}
