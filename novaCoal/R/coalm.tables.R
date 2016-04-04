##################################################################################################
##                                   Coal merchant survey                                       ##
## Script to subset coal merchant survey for sepcific towns and create merc.tab an price.tab    ##
## (c) Nova Instituut 2013                                                                      ##
##################################################################################################


coalm.tables <- function(csv=NULL,
                         fn="~/Downloads/Coal_merchant_survey_20110620-100836.csv",
                         manual.edit=NULL,
                         manual.dest="Bolata",
                         selectvar=haq$town,
                         fn.s=paste(basedir,tabdir,projectname,"coalm.Rda",sep=""),
                         tabdirr=paste(dropdir,basedir,tabdir,sep=""),
                         loc=haq$town,
                         format=haq$coal.buying.format, debug = FALSE, verkort = TRUE, subplace = TRUE
                         ){
# load data and subset for the relevant towns
require(doBy)  
message("fn= ", fn)  
message("selectvar= ", paste(levels(as.factor(selectvar)), " "), "\n")
message("loc= ", paste(levels(as.factor(loc)), " "))
message("format= ", levels(as.factor(format)))
message("csv option ", is.null(csv))
  
if(is.null(csv)) {message("csv option chosen")
  coalm = read.csv(fn)
  message("coalm read") }else{ message("using data from workspace" )
                               coalm = csv} 

#drop empty column
dropidx = which(lapply(coalm, function(x) all(is.na(x))) == TRUE)
if (length(dropidx) > 0) coalm = coalm[-dropidx]

# Fix names
#names(coalm) = gsub("_0", "", names(coalm))

#coalm = coalm[,which(drop.idx==FALSE)]
message("records in coalm: ", length(coalm[,1]))

if(length(manual.edit)>0) {idx <- na.omit( which(sapply(coalm$Delivery.suburbs,"%in%",manual.edit)))
                           coalm$Town <- as.character(coalm$Town)
                           coalm$Town[idx] <-  manual.dest
                           message("idx is ",idx)
                           message("coalm$Town is now ", manual.dest)}

message("coalm$Town", levels(as.factor(coalm$Town)))
if (verkort == TRUE) coalm <- coalm[which(is.na(match(coalm$Town,levels(as.factor(selectvar))))==FALSE),]
message("coalm shortend to ", length(coalm[,1]))

# make indicies 
bigbag =   grep("Big.bag.weight", names(coalm))
smallbag = grep("Small.bag.weight", names(coalm))
tin =      grep("Tin.weight", names(coalm))
drum =     grep("Drum.weight", names(coalm))
wheelbarrow = grep("Wheelbarrow.weight", names (coalm))
message("coalm indexed")

# Define function to get merchant mean
merchant.mean = function(x = 1:length(format.idx),
                       data=coalm,
                       format.idx=bigbag,
                       nr.measured="Big.bag.number"){
  message("You are inside merchant mean")
  if(length(format.idx) > 0){
    message("There is a format such as ", nr.measured, ". The indicies are ",paste(format.idx, " " ))
    message("data[x, format.idx] ", data[x, format.idx])
   if(all(is.na(data[x,format.idx]))==FALSE){
     message("at least one is not empty")
     s = sum(data[x, format.idx], na.rm=TRUE)
     message("s ", s)
     n = data[x, grep(gsub("_0", "", nr.measured), names(data))]
     message("n ", n)
     meann = s/n
     message("meann ", meann)} else {message("meann set to NA because it had no values") 
                                     meann = NA}
   meann} else {meann = NA}
  return(mean(meann, na.rm=TRUE))
  message("you are leaving merchant mean")
  }

# Work out the mean weight per format type for every merchant
coalm$Big.bag.mean=sapply(1:length(coalm[,1]),function(x) merchant.mean(x,
                                                                        format.idx = bigbag,
                                                                        nr.measured="Big.bag.number" ))
names(coalm$Big.bag.mean)="Big.bag.mean"
message("Big bag mean ", coalm$Big.bag.mean)

if(length(grep("Small.bag.number",names(coalm)))>0){
  coalm$Small.bag.mean=sapply(1:length(coalm[,1]),function(x){merchant.mean(x,
                                                                            format.idx=smallbag,
                                                                            nr.measured="Small.bag.number")})
  names(coalm$Small.bag.mean)="Small.bag.mean"
  message("Small bag mean", coalm$Small.bag.mean)
                                                      }

if(length(grep("Tin.number",names(coalm)))>0){
  coalm$Tin.mean=sapply(1:length(coalm[,1]),function(x){merchant.mean(x,
                                                                      format.idx=tin,
                                                                      nr.measured="Tin.number")})
  names(coalm$Tin.mean)="Tin.mean"
  message("Tin mean", coalm$Tin.mean)
                                                      }

if(length(grep("Drum.number",names(coalm)))>0) {coalm$drum.mean=sapply(1:length(coalm[,1]),function(x){merchant.mean(x,format.idx=drum, nr.measured="Drum.number")})
names(coalm$drum.mean)="Drum.mean"
message("Drum mean", coalm$Drum.mean)}

if(length(grep("Wheelbarrow.number",names(coalm)))>0) {coalm$wheelbarrow.mean=sapply(1:length(coalm[,1]),function(x){merchant.mean(x,format.idx=wheelbarrow, nr.measured="Wheelbarrow.number")})
                                                names(coalm$wheelbarrow.mean)="Wheelbarrow.mean"
                                                message("Wheelbarrow mean", coalm$Wheelbarrow.mean)}

bag.price=function(x,
                   data=coalm,
                   format.price=c("Bag.price","Small.bag.price","Tin.price","Drum.price","Wheelbarrow.price")[1],
                   mean.weight=c("Big.bag.mean","Small.bag.mean","Tin.mean","Drum.mean","Wheelbarrow.mean")[1]){
  message("format price: ", format.price, " mean.weight: ", mean.weight)
  message("names(data) ", paste(names(data), " "))
  data[,format.price]/data[,mean.weight]}

if(length(grep("Big.bag.number",names(coalm)))>0){
  coalm$Big.bag.price.kg = sapply(1, bag.price)
  names(coalm$Big.bag.price.kg) <- "Big bag price/kg"
  message("Big bag price kg", coalm$Big.bag.price.kg)
}

if(length(grep("Small.bag.number",names(coalm)))>0){
  coalm$Small.bag.price.kg = sapply(1,bag.price,
                                    format.price="Small.bag.price",
                                    mean.weight="Small.bag.mean")
  names(coalm$Small.bag.price.kg) <- "Small bag price/kg"
  message("Small bag price kg", coalm$Small.bag.price.kg)
}

if(length(grep("Drum.number",names(coalm)))>0){
  coalm$Drum.price.kg=sapply(1,bag.price,format.price="Drum.price",mean.weight="drum.mean")
  names(coalm$Drum.price.kg) <- "Drum price/kg"
  message("Drum price kg", coalm$Drum.price.kg)
}

if(length(grep("Tin.number",names(coalm)))>0){coalm$Tin.price.kg=sapply(1, bag.price, format.price="Tin.price", mean.weight="Tin.mean")
names(coalm$Tin.price.kg) <- "Tin price/kg"
message("Tin price kg", coalm$Tin.price.kg)}

if(length(grep("Wheelbarrow.number",names(coalm)))>0){coalm$Wheelbarrow.price.kg=sapply(1, bag.price, format.price="Wheelbarrow.price", mean.weight="wheelbarrow.mean")
                                              names(coalm$Wheelbarrow.price.kg) <- "Wheelbarrow price/kg"
                                              message("Wheelbarrow price kg", coalm$Wheelbarrow.price.kg)}

assign("coalm",coalm,envir=.GlobalEnv)
message("coalm assigend")

merc.col = na.omit(match(c("Trade.Name","Delivery.suburbs","Big.bag.mean","Small.bag.mean","Tin.mean"),names(coalm)))
merc.tab=coalm[,merc.col, drop = FALSE]
names(merc.tab)=gsub("_0","",names(merc.tab))
names(merc.tab)=gsub("\\."," ",names(merc.tab))
message("merc tab created")

price.tab <- coalm[,grep("Trade.Name|price",names(coalm))]
names(price.tab)=gsub("_0","",names(price.tab))
names(price.tab)=gsub("\\."," ",names(price.tab))
message("price tab created")

test.na = function(x){length(which(is.na(x)==TRUE))==length(x)}
drop.na.col = function(x){test=apply(x,2,test.na)
                          test}

drop.idx.merc = drop.na.col(data.frame(merc.tab))
if (length(drop.idx.merc) > 0) merc.tab = merc.tab[,which(drop.idx.merc==FALSE), drop = FALSE]

m.i <- sapply(merc.tab,is.factor)
message("m.i", m.i)
merc.tab[m.i] <- lapply(merc.tab[m.i],function(x)gsub("/", " ",x))
numvar = ncol(merc.tab)
numvar = numvar+1
message("numvar ",numvar)
if (numvar > 3){col.add =  paste(rep("x{1.2cm}",numvar-3),sep="",collapse="")} else {col.add = ""} 
all = paste("r","p{4.5cm}","p{4.5cm}", col.add,sep="")
message(all, "\ndim van merc tab is  ", paste(dim(merc.tab), " "))
if (debug == TRUE) assign("merc.tab", merc.tab, .GlobalEnv)

print(
  xtable(merc.tab
         ,align=all
         ,display=c("d","s","s","f","f","f","f")[1:(numvar)]
         ,caption="Weights per coal merchant surveyed"
         ,label="merc")
  ,file=paste(tabdirr,"merc.tex",sep="")
  )
message("merc.tab printed")

assign("merc.tab",merc.tab,envir=.GlobalEnv)
message("merc.tab assigend")

p.i <- sapply(price.tab,is.factor)
if(length(p.i)>0) price.tab[p.i] <- sapply(price.tab[p.i],function(x)gsub("/", " ",x))
p.numvar = ncol(price.tab)+1
p.col.add = paste(rep("x{1.2cm}",p.numvar-2),sep="",collapse="")
message(p.col.add)
p.all = paste("r","p{2.5cm}", p.col.add,sep="")

message("Starting to print pricetab", "P.Numvar = " , p.numvar)
try(print(
  xtable(price.tab,
         align=p.all,
         display=c("d","s","f","f","f","f", "f", "f")[1:(p.numvar)],
         caption="Price per kg by coal merchant surveyed",
         label="price"),
  file=paste(tabdirr,"price.tex",sep="")
  ))
message("price.tab printed")

assign("price.tab",price.tab,envir=.GlobalEnv)
message("price.tab assigend")

# Unhash to see the merchants per town
# tapply(haq$coal.merchant[which(haq$hh.BM.use=="BM")],haq$town[which(haq$hh.BM.use=="BM")],table.by)

# get weighted mean for kg
Big.Bag = weighted.mean(x=coalm$Big.bag.mean[which(is.na(coalm$Big.bag.mean)==FALSE)],
                        w=coalm$Big.bag.number[which(is.na(coalm$Big.bag.mean)==FALSE)])

Small.Bag = if(length(grep("Small.bag.number",names(coalm)))>0){weighted.mean(x=coalm$Small.bag.mean[which(is.na(coalm$Small.bag.mean)==FALSE)],
                                                                                w=coalm$Small.bag.number[which(is.na(coalm$Small.bag.mean)==FALSE)])}else{NA}

Tin = if(length(grep("Tin.number",names(coalm)))>0){weighted.mean(x=coalm$Tin.mean[which(is.na(coalm$Tin.mean)==FALSE)],
                    w=coalm$Tin.number[which(is.na(coalm$Tin.mean)==FALSE)])}else{NA}

Large.drum = if(length(grep("Drum.number",names(coalm)))>0){weighted.mean(x=coalm$drum.mean[which(is.na(coalm$drum.mean)==FALSE)],
                            w=coalm$Drum.number[which(is.na(coalm$drum.mean)==FALSE)])}else{NA}

Wheelbarrow = if(length(grep("Wheelbarrow.number",names(coalm)))>0){weighted.mean(x=coalm$wheelbarrow.mean[which(is.na(coalm$wheelbarrow.mean)==FALSE)],
                                                                          w=coalm$Wheelbarrow.number[which(is.na(coalm$wheelbarrow.mean)==FALSE)])}else{NA}

weightz <- data.frame(Big.Bag = Big.Bag,
                      Small.Bag = Small.Bag,
                      Tin = Tin,
                      "Large\ drum" = Large.drum,
                      wheelbarrow = Wheelbarrow)

message("weightz created", weightz)
assign("weigthz",weightz,envir=.GlobalEnv)

idx <- na.omit(match(levels(format),gsub("\\."," ",colnames(weightz))))
message("Format= ", table(format),"colnames(weightz)= ", colnames(weightz)  ,"Weightz idx= ", idx)
weightz <- weightz[,idx]
assign("weightz", weightz, envir=.GlobalEnv)
message("Weightz shortend ", paste(weightz, " "))

message("format= ", str(format)  )
f.idx <- na.omit(match(gsub("\\."," ",colnames(weightz)),
                       levels(format)
                       )
                 )
message("f.idx= ",f.idx)
message("levels(format)[f.idx]=",levels(format)[f.idx])

town=matrix(
  sapply(levels(as.factor(loc)),FUN=function(x)paste(x,levels(format)[f.idx],sep=""))
  ,ncol=1)[,1]
message("town",paste(town, " "))

kg=unlist(rep(weightz,length(levels(as.factor(loc)))))
message("kg",paste(kg, " "))

gewigte <- data.frame(town=town,kg=kg)
message("gewigte created", str(gewigte))

gewigte$kg <- as.numeric(gewigte$kg)

# create objects in global environment

assign("gewigte2", gewigte, envir=.GlobalEnv)
message("gewigte2 assigend")

summarise.coalm <- function(coalm=coalm){
  require(reshape2)
  require(plyr)
  coalm.m = melt(coalm, id.vars=c("Response.ID", "Town"),
                 measure.vars=grep("weight_[[:digit:]]$", names(coalm), value=TRUE))  # consider Trade.Name
  message("coalm melted" , paste(names(coalm.m), collapse=" " ))
  if (length(grep("container",as.character(coalm.m$variable))) > 0) coalm.m = coalm.m[-grep("container",as.character(coalm.m$variable)),]
  coalm.m$variable = gsub('.weight_[[:digit:]]','',coalm.m$variable)
  coalm.m <- coalm.m[!is.na(coalm.m$value), ]
  message("Nr of values in coalm.m ", length(which(is.na(coalm.m$value)==FALSE)))
  coalm.m = coalm.m[which(is.na(coalm.m$value)==FALSE),]
  message("dimensions of coalm.m ", paste(dim(coalm.m), collapse=" by "))
  coalm.m$value = as.numeric(coalm.m$value)
  message("dimensions of coalm.m ", paste(dim(coalm.m), collapse=" by "))
  coalm.sum = summaryBy(formula=value~Town+variable,data=coalm.m,FUN=function(x){c(n=length(x),
                                                                                     min=min(x),
                                                                                     mean=mean(x,na.rm=TRUE),
                                                                                     max=max(x))})
  colnames(coalm.sum) = c("Town", "Format", "$n$", "$min$" , "$mean$", "$max$")
  if (subplace == TRUE){
    message("cp1")
    print(names(coalm))
    m1 <- melt(coalm[ ,-grep("Deliv", names(coalm))], id.vars=c("Trade.Name","Response.ID", "Town"),  measure.vars=grep("weight_[[:digit:]]$", names(coalm), value=TRUE))
    m2 <- melt(coalm[,c("Trade.Name","Response.ID", "Town", grep("Deliv", names(coalm), value=TRUE))], id.vars=c("Trade.Name","Response.ID", "Town"))       
    colnames(m2)[4:5] <- c( "variable2", "value2" )
    dm <- merge(m1, m2)
    if (any(!is.na(dm$value))) dm <- dm[!is.na(dm$value), ]
    if (any(!is.na(dm$value2))) dm <- dm[!is.na(dm$value2), ]
    dm <- dm[which(dm$value2 > 0), ]
    dm <- dm[ ,-match("value2", names(dm))]
    if (length(grep(".container.weight", dm$variable)) > 0) dm <- dm[-grep(".container.weight", dm$variable), ]
    dm$variable <- gsub(".weight_[[:digit:]]+$", "", dm$variable)
    dm$variable2 <- gsub("Delivery.places_", "", dm$variable2)
    gewigte <- dcast(dm, Trade.Name +  variable + variable2 ~., fun.aggregate=mean)
    gewigte$town <- gsub("\\.", " ", paste(gewigte$variable2, gewigte$variable, sep=""))
    names(gewigte)[4] <- "kg"
    assign("gewigte",gewigte,envir=.GlobalEnv)
    message("gewigte assigend")
  }
  coalm.sum
}
message("now lets summarise. Dinentions of coalm is ", paste(dim(coalm), collapse=" by "))
if (debug == TRUE) assign("coalm.d", coalm, envir=.GlobalEnv)
coalm.sum = summarise.coalm(coalm)
assign("coalm.sum", coalm.sum, envir=.GlobalEnv)
message("coal.sum created")

latex(coalm.sum,
      digits=4,
      rowname=NULL,
      file=paste(tabdirr,"coalm.summary.tex",sep=""),
      label="coalm.summary",
      caption="Summary of coal  merchant survey results"
      )

# drop empty column to enhance readibility
dropidx = which(apply(coalm,2, function(x) sum(is.na(x)==FALSE))==0)
if (length(dropidx) > 0) if(is.na(dropidx)==FALSE) coalm = coalm[,-dropidx]

# now write out a coalm.xlsx 
try(require(openxlsx))
    
if("package:openxlsx" %in% search()){    
  openxlsx::write.xlsx(x=list(coalm, coalm.sum, price.tab, merc.tab),file=paste(tabdirr, projectname, "CoalMerchantResults.xlsx", sep=""))
  }


save(coalm,merc.tab,price.tab,coalm.sum,file=paste(fn.s,"coalm.Rda",sep=""))
}
