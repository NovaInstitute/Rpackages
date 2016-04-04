########################################################################################################################
                              ##########           SUMMER             ##########
########################################################################################################################

########## ALL TOWNS TOGETHER: SUMMER BASELINE ############
summer.coal.base.all=function(x){
n = sum(BMbuurt$BM.summer)
point.BM = sum(BMbuurt$point.BM.s,na.rm=TRUE)
mean.base = mean(haq$summerconbase.year[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))], na.rm=TRUE)
stddev =      sd(haq$summerconbase.year[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))], na.rm=TRUE)
total.base = sum(BMbuurt$town.summer.coal.base,na.rm=TRUE)
total.base.co2 =  sum(BMbuurt$town.summer.BE,na.rm=TRUE)
tab=cbind(n, round(point.BM), round(mean.base,2), round(stddev,2), round(total.base/1000,2),round(total.base.co2/1000,2))
rownames(tab)="All towns: Summer baseline 2008"
colnames(tab)=c("n","BM users", "Mean coal baseline(kg)", "Stddev", "Total coal baseline(t)","CO2 baseline (t)")
tab
}

########## ALL TOWNS TOGETHER: SUMMER PROJECT ############
summer.coal.project.all=function(x){
n = sum(BMbuurt$BM.summer)
point.BM = sum(BMbuurt$point.BM.s,na.rm=TRUE)
mean.current = mean(haq$summer.current.year[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))], na.rm=TRUE)
stddev =         sd(haq$summer.current.year[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))], na.rm=TRUE)
total.current = sum(BMbuurt$town.summer.coal.project,na.rm=TRUE)
total.current.co2 =  sum(BMbuurt$town.summer.PE,na.rm=TRUE)
tab=cbind(n, round(point.BM), round(mean.current,2), round(stddev,2), round(total.current/1000,2),round(total.current.co2/1000,2))
rownames(tab)="All towns: Summer projet 2008"
colnames(tab)=c("n","BM users", "Mean coal use(kg)", "Stddev", "Total coal use(t)","Project CO2(t)")
tab
}

############### ALL TOWNS: SUMMER SAVE ##################
summer.coal.summary.all=function(x){
n = sum(BMbuurt$BM.summer)
point.BM = sum(BMbuurt$point.BM.s,na.rm=TRUE)
mean.save = mean(haq$total.summer.save[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))], na.rm=TRUE)
stddev =      sd(haq$total.summer.save[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))], na.rm=TRUE)
total.save = sum(BMbuurt$town.summer.coal.save,na.rm=TRUE)
total.co2 =  sum(BMbuurt$town.summer.ER,na.rm=TRUE)
tab=cbind(n, round(point.BM), round(mean.save,2), round(stddev,2), round(total.save/1000,2),round(total.co2/1000,2))
rownames(tab)="All towns: Summer saving 2008"
colnames(tab)=c("n","BM users", "Mean coal saved(kg)", "Stddev", "Total coal saved(t)","CO2 saved (t)")
tab
}

########## SUMMERB P E  ############
summer.bpe=function() {
b=summer.coal.base.all()
p=summer.coal.project.all()
e=summer.coal.summary.all()
colnames(b)=c("n","BM users", "Mean coal (kg)", "Stddev", "Total coal (t)","CO2(t)")
colnames(p)=c("n","BM users", "Mean coal (kg)", "Stddev", "Total coal (t)","CO2(t)")
colnames(e)=c("n","BM users", "Mean coal (kg)", "Stddev", "Total coal (t)","CO2(t)")
tab=rbind(b,p,e)
tab}

########## TOWN by TOWN: SUMMER BASELINE ############
summer.coal.base=function(x){
n = BMbuurt$BM.user
point.BM = BMbuurt$point.BM
mean.base = BMbuurt$summer.coal.base
stddev =ldply(by(haq$summerconbase.year,haq$town,function(x){sd(x, na.rm=TRUE)}))[[2]]
l=ldply(by(haq$summerconbase.year[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))],haq$town[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))],function(x){length(x)}))[[2]]
total.base = BMbuurt$town.summer.coal.base
total.base.co2 =  BMbuurt$town.summer.BE
total.base.co22=ldply(by(haq$summer.baseline.emissions,haq$town,function(x){mean(x,na.rm=TRUE)}))[[2]]
tab=cbind(n, l,round(point.BM), round(mean.base,2), round(stddev,2), round(total.base/1000,2),round(point.BM*total.base.co22/1000,2),round(total.base.co2/1000,2))
rownames(tab)=levels(haq$town)
colnames(tab)=c("n","saved?","BM users", "Mean coal baseline(kg)", "Stddev", "Total coal baseline(t)","Total CO2(t)HAQ","CO2 baseline (t)")
tab
}



############### TOWN BY TOWN: SUMMER SAVE##################
summer.coal.summary.town=function(x){
n = BMbuurt$BM.summer
point.BM = BMbuurt$point.BM.s
mean.save = BMbuurt[,match(c("summer.coal.save"),names(BMbuurt))]
stddev = ldply(by(haq$total.summer.save[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))], haq$town[which(haq$coal.use.in.summer.r=="YES" & (haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM"))],sd, na.rm=TRUE))[[2]]
total.save = BMbuurt$town.summer.coal.save
total.co2 = BMbuurt$town.summer.ER
tab=cbind(n, round(point.BM), round(mean.save,2), round(stddev,2), round(total.save/1000,2),round(total.co2/1000,2))
rownames(tab)=rownames(BMbuurt)
colnames(tab)=c("n","BM users", "Mean coal saved(kg)", "Stddev", "Total coal saved(t)","CO2 saved (t)")
tab
}

########################################################################################################################
                              ##########           WINTER             ##########
########################################################################################################################

############### ALL TOWNS TOGETHER: WINTER SAVE ##################
winter.coal.summary.all=function(x){
n = sum(BMbuurt$BM.users,na.rm=TRUE)
point.BM = sum(BMbuurt$point.BM,na.rm=TRUE)
mean.save = mean(haq$total.winter.save[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")], na.rm=TRUE)
stddev =      sd(haq$total.winter.save[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")], na.rm=TRUE)
total.save = sum(BMbuurt$town.winter.coal.save,na.rm=TRUE)
total.co2 = sum(BMbuurt$town.winter.ER,na.rm=TRUE)
tab=cbind(n, round(point.BM), round(mean.save,2), round(stddev,2), round(total.save/1000,2),round(total.co2/1000,2))
rownames(tab)="All towns: Winter saving 2008"
colnames(tab)=c("n","BM users", "Mean coal saved(kg)", "Stddev", "Total coal saved(t)","CO2 saved (t)")
tab
}

########## WINTER.PROJECT ############
winter.coal.project.all=function(x){
n = sum(BMbuurt$BM.users,na.rm=TRUE)
point.BM = sum(BMbuurt$point.BM,na.rm=TRUE)
mean.current = mean(haq$winter.current.year[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")], na.rm=TRUE)
stddev =  sd(haq$winter.current.year[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")], na.rm=TRUE)
total.current = sum(BMbuurt$town.winter.coal.project,na.rm=TRUE)
total.current.co2 =  sum(BMbuurt$town.winter.PE,na.rm=TRUE)
tab=cbind(n, round(point.BM), round(mean.current,2), round(stddev,2), round(total.current/1000,2),round(total.current.co2/1000,2))
rownames(tab)="All towns: Winter project 2008"
colnames(tab)=c("n","BM users", "Mean coal use(kg)", "Stddev", "Total coal use(t)","Project CO2(t)")
tab
}
########## WINTER BASELINE ############
winter.coal.base.all=function(x){
n = sum(BMbuurt$BM.users,na.rm=TRUE)
point.BM = sum(BMbuurt$point.BM,na.rm=TRUE)
mean.base = mean(haq$winterconbase.year[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")], na.rm=TRUE)
stddev =  sd(haq$winterconbase.year[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")], na.rm=TRUE)
total.base = sum(BMbuurt$town.winter.coal.base,na.rm=TRUE)
total.base.co2 =  sum(BMbuurt$town.winter.BE,na.rm=TRUE)
tab=cbind(n, round(point.BM), round(mean.base,2), round(stddev,2), round(total.base/1000,2),round(total.base.co2/1000,2))
rownames(tab)="All towns: Winter baseline 2008"
colnames(tab)=c("n","BM users", "Mean coal use(kg)", "Stddev", "Total coal use(t)","Project CO2(t)")
tab
}

########## WINTER BE PE ER  ############
winter.bpe=function() {
b=winter.coal.base.all()
p=winter.coal.project.all()
e=winter.coal.summary.all()
colnames(b)=c("n","BM users", "Mean coal (kg)", "Stddev", "Total coal (t)","CO2(t)")
colnames(p)=c("n","BM users", "Mean coal (kg)", "Stddev", "Total coal (t)","CO2(t)")
colnames(e)=c("n","BM users", "Mean coal (kg)", "Stddev", "Total coal (t)","CO2(t)")
tab=rbind(b,p,e)
tab}


############### TOWN BY TOWN: WINTER ################## 
winter.coal.summary.town=function(x){
n = BMbuurt$BM.users
point.BM = BMbuurt[,match(c("point.BM"),names(BMbuurt))]
mean.save = BMbuurt[,match(c("winter.coal.save"),names(BMbuurt))]
stddev = ldply(by(haq$total.winter.save[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")], haq$town[which(haq$hh.BM.use=="BM"|haq$hh.BM.use=="Historic BM")],sd, na.rm=TRUE))[[2]]
total.save = BMbuurt$town.winter.coal.save
total.co2 = BMbuurt$town.winter.ER
tab=cbind(n, round(point.BM), round(mean.save,2), round(stddev,2), round(total.save/1000,2),round(total.co2/1000,2))
rownames(tab)=rownames(BMbuurt)
colnames(tab)=c("n","BM users", "Mean coal saved(kg)", "Stddev", "Total coal saved(t)","CO2 saved (t)")
tab
}
########################################################################################################################
                              ##########           WINTER             ##########
########################################################################################################################

################# ALL TOWN: ANNUAL#####################
summary.all=function(x){
	rbind(winter.coal.summary.all(),summer.coal.summary.all())
	}

################ BASELINE, PROJECT AND ER #####################

BPE.town=function(){
	tab=summer.bpe()[,5:6]+winter.bpe()[,5:6]
	rownames(tab)=c("Baseline", "Project", "Reduction")
	tab
	}
