#' Population variance per Strate with 90 percent CI
#'
#' Function creates a table of estimated proportion variance per strata, including
#' intervals and a 90 percent confidence interval
#'
#' @param Households Data frame containing household survey data
#' @param Sample Data frame containing sample information
#' @param pointest Data frame containing point test information
#' @param debug Performs the necessary global assignments when TRUE
#' @export

strat90=function(Households,Sample,pointest, debug=TRUE){
	strat<-data.frame(cbind(Households,Sample,pointest))

strat$p.strvar<-(1-strat$Sample/strat$Households)*strat$pointest*(1-strat$pointest)/(strat$Sample-1)#estimated proportion variance per strata
str.tot<-(sum(strat$Households))#overall population size
str.prop<-sum(strat$Households/str.tot * strat$pointest)#estimated population proportion
str.var<-sum((strat$Households/str.tot)^2*(1-strat$Sample/strat$Households)*strat$pointest*(1-strat$pointest)/(strat$Sample-1))#estimated population variance
str.stderr<-sqrt(str.var)#standard error # degrees of freedom !!!!
str.conf.lower <- str.prop-1.645 * str.stderr#gebruik TDist
str.conf.upper <- str.prop+1.645 * str.stderr#gebruik TDist
e.value = 1.645 * str.stderr
  out.str =  list(strat, str.tot, str.prop, str.var, str.stderr,e.value)
  assign("out.str", out.str, envir=.GlobalEnv)
if(debug==TRUE) {
  assign("p.strvar", strat$p.strvar, envir=.GlobalEnv)
  assign("str.prop", str.prop, envir=.GlobalEnv)
  assign("str.var", str.var, envir=.GlobalEnv)
  assign("str.stderr", str.stderr, envir=.GlobalEnv)
  assign("prop", strat$Households/str.tot*strat$pointest, envir=.GlobalEnv)
  assign("var", (strat$Households/str.tot)^2*(1-strat$Sample/strat$Households)*strat$pointest*(1-strat$pointest)/(strat$Sample-1), envir=.GlobalEnv)
}
x=rbind(str.tot,str.prop,str.var,str.conf.lower,str.conf.upper,e.value)
row.names(x)=c("N","Estimated Proportion","Variance","90% Conf. Interval (low)","90% Conf. Interval (High)","Accuracy")
x
	}
