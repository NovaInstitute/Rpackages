#' Allocate Weights
#'
#' This function is used to allocate weights of various coal formats (such as a big or tin)
#' to different areas examined in the survey data
#'
#' @param x Data frame containing survey data
#' @param formaat Character vector of the formats under consieration
#' @param loc Character vector referring to the variable containing different areas
#' @param debug Logical that assigns objects to the global environment for debugging purposes
#' when TRUE
#' @param verbose Logical that displays function messages when TRUE
#' @examples suburb.lst = lapply(levels(haq$town), FUN=function(x) {split(coalm,f=coalm[,x])[[2]] } )
#' names(suburb.lst) = levels(haq$town)
#' for (i in 1: length(suburb.lst)){
#'   suburb.lst[[i]][,"area"] <- names(suburb.lst)[[i]]
#'   }
#' res = lapply(suburb.lst, allocate.weights, formaat = haq$coal.buying.format)
#' @details Make use of coalm.tables if you want the answer per suburb
#' @export





allocate.weights <- function(x,
                             formaat = get("format", pos=1),
                             loc = "area",
                             debug = FALSE,
                             verbose = FALSE){

message("loc = ", loc )
message("x[ ,loc]", x[loc])
if (verbose ==TRUE) message("str(x): ", str(x))
# get weighted mean for kg
Big.Bag = if(length(grep("Big.bag.number_0",names(x)))>0){weighted.mean(x=x$Big.bag.mean[which(is.na(x$Big.bag.mean)==FALSE)],
                        w=x$Big.bag.number_0[which(is.na(x$Big.bag.mean)==FALSE)])}else{NA}

Small.Bag = if(length(grep("Small.bag.number_0",names(x)))>0){weighted.mean(x=x$Small.bag.mean[which(is.na(x$Small.bag.mean)==FALSE)],
                                                                                w=x$Small.bag.number_0[which(is.na(x$Small.bag.mean)==FALSE)])}else{NA}

Tin = if(length(grep("Tin.number_0",names(x)))>0){weighted.mean(x=x$Tin.mean[which(is.na(x$Tin.mean)==FALSE)],
                                                                    w=x$Tin.number_0[which(is.na(x$Tin.mean)==FALSE)])}else{NA}

Drum = if(length(grep("Drum.number_0",names(x)))>0){weighted.mean(x=x$drum.mean[which(is.na(x$drum.mean)==FALSE)],
                                                                      w=x$Drum.number_0[which(is.na(x$drum.mean)==FALSE)])}else{NA}

weightz <- data.frame(Big.Bag = Big.Bag,
                      Small.Bag = Small.Bag,
                      Tin = Tin,
                      "Large\ drum" = Drum )

message("weightz created", weightz)
assign("weigthz",weightz,envir=.GlobalEnv)

idx <- na.omit(match(levels(formaat),gsub("\\."," ",colnames(weightz))))
message("Format= ", table(formaat)," colnames(weightz)= ", colnames(weightz)  ," Weightz idx= ", idx)
weightz <- weightz[,idx]
message("Weightz shortend ", weightz)

message("format= ", str(formaat)  )
f.idx <- na.omit(match(gsub("\\."," ",colnames(weightz)),
                       levels(formaat)
)
)
message("f.idx= ",f.idx)
message("levels(formaat)[f.idx]=",levels(formaat)[f.idx])

town=matrix(
  sapply(levels(as.factor(x[,loc])),FUN=function(x)paste(x,levels(formaat)[f.idx],sep=""))
  ,ncol=1)[,1]
message("town",town)

kg=unlist(rep(weightz,length(levels(as.factor(x[,loc])))))
message("kg",kg)

gewigte <- data.frame(town=town,kg=kg)
message("gewigte created", str(gewigte))

gewigte$kg <- as.numeric(gewigte$kg)

# create objects in global environment
if (debug==TRUE){
  assign(paste(levels(as.factor(x[,loc])),".","gewigte", sep=""), gewigte, envir=.GlobalEnv)
  message("gewigte assigend")
}

# Sometimes a user refers to a big bag as a small bag. This can be seen where
# there is only one bag size that is sold. In such a case big and small bag
# sizes are the same. Same goes for drums. Default 4 tins per bag

if (is.na(gewigte["Small.Bag","kg"])) gewigte["Small.Bag","kg"] <- gewigte["Big.Bag","kg"] * 0.6 # assume 60% of a big bag
if (is.na(gewigte["Tin","kg"])) gewigte["Tin","kg"] <- gewigte["Big.Bag","kg"] / 4
if (is.na(gewigte["Large.drum","kg"]))gewigte["Large.drum","kg"] <- gewigte["Big.Bag","kg"]

gewigte
}
