########## GS Project Summary  ###########
# Make Summary from haq data set according to GS formulae

# n Pop. XUcoal XUalt Coal users BMusers BMother BM Nova BE PE ER ER/hh mean.days.used

GS.summary=function(df,Coal="hh.coal.use.r",var="hh.coal.use.r",
                    pop=pop,group=haq$town,othervar="introduction.to.bm.r",
                    altvar="hh.BM.use", coal=TRUE,BM=TRUE,other=TRUE, annual.base="annual.base",COEF=COEF){

	# Begin met UTA
	base=project.summary.UTA(df,Coal,var,pop,group,othervar,altvar,coal,BM,other)
	base=data.frame(base[-length(base[,1]),])

	# Werk lae skatting uit: eers prop.alt en dan low.estimate
	base$prop.alt=ldply(apply(base[,c("XUalt","XUother","XUcoal")],1,function(x){(x[1]-x[2])*x[3]}))$V1
	base$low.alt.prop =ldply(apply(base[,c("Pop.","n","prop.alt")],1,function(x){strat90(x[1],x[2],x[3])[4]}))$V1
	base$low.alt =as.numeric(base$low.alt.prop)*as.integer(base$Pop.)

	# BE
	#base$annual.base.emmision=
	be=ldply(tapply(df$annual.base[which(is.na(group)==FALSE)],group[which(is.na(group)==FALSE)],mean,na.rm=TRUE)*COEF)[,2]
	pe=ldply(tapply(df$annual.current[which(is.na(group)==FALSE)],group[which(is.na(group)==FALSE)],mean,na.rm=TRUE)*COEF)[,2]

	base$mean.BE=be
	base$mean.PE=pe
	# PE
	#base$mean.ER=base$BE-base$PE
	# ER
	base$mean.ER=base$mean.BE-base$mean.PE
	base
	}
