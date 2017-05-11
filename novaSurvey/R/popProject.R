#' Projects a sample population onto population
#' 
#' @param df Data frame containing data to be used
#' @param var Character string that specifies the variable under consideration
#' @param pop Data frame containing populations
#' @param latex Logical that outputs latex table if TRUE
#' @param xlsx Logical that writes xlxs tables if TRUE
#' @param dirr Directory that output will be stored in
#' @param verbose Logical displaying message is TRUE
#' @param debug Logical Initializes debugging if TRUE
#' @export
 

popProjek <- function(df = qol, var = "sol_energy_coal_ignition", 
                      place = "place", 
                      pop = data.frame(eMbalenhle = 35404, Lebohang = 8908, eMzinoni = 10310,  KwaDela = 982),
                      latex = TRUE, xlsx = TRUE,
                      dirr= tabdir,
                      verbose = FALSE, debug = FALSE, c.loc="bottom"){
        
        
        if (!require("Hmisc")) {
                message("Loading Hmisc")
                install.packages("Hmisc", dependencies = TRUE)
                if (!require("Hmisc")) stop("Load Hmisc manually")
        }
        if(!require("reshape2")){
                message("Loading reshape2")
                install.packages("reshape2", dependencies = TRUE)
                if(!require("reshape2")) stop("Load reshape2 manually")
        }
        if(!require("openxlsx")){
                message("Loading openxlsx")
                install.packages("openxlsx")
                if(!require("openxlsx")) stop("load xlsx manually")
                
        }
        
   TNAMES = levels(as.factor(df[,place]))
   idx = nlevels(as.factor(df[,var])) + 1 # vir die NA
   if (verbose == TRUE) message("TNAMES is ", paste(TNAMES, " "), "\nidx is; ", idx)
   aa = tapply(df[,var], df[,place], function(x) addmargins(table(x, exclude=NULL)))
   if (debug == TRUE) assign("aa", aa, envir=.GlobalEnv)
   reslys = lapply(aa, function(z) t(mapply(binconf, x= z[1:idx],n= z["Sum"])))
   if (debug == TRUE) assign("reslys", reslys, envir=.GlobalEnv)
   reslys  = lapply(reslys, function(x) {dimnames(x)[[2]] <- c("PointEst","Lower","Upper")
   	x})
   bb = reslys[[1]]
   if (debug == TRUE) assign("bb", bb, envir=.GlobalEnv)
   names(pop) =   tolower(gsub("[[:space:]]+", "\\.", names(pop)))
   NMS = gsub("[[:space:]]+", "\\.", tolower(levels(as.factor(df[,place]))))
   lt = lapply(NMS, function(x) as.numeric(rep(pop[x], times=(ncol(bb) * nrow(bb)))))
   names(lt) = NMS
   if (debug == TRUE) assign("lt", lt, envir=.GlobalEnv)
   names(reslys) = gsub("[[:space:]]+", "\\.", tolower(names(reslys)))
   reslys = reslys[NMS]
   res = mapply(maal, reslys, lt, SIMPLIFY=FALSE)
   if (debug == TRUE) assign("ress", res, envir=.GlobalEnv)
   res = lapply(res, function(x) round(x))
   
   xm = melt(res)
   if (length(grep("Var1", names(xm))) > 0) names(xm)[grep("Var1", names(xm))] = "X1"
   if (length(grep("2", names(xm))) > 0) names(xm)[grep("2", names(xm))] = "X2"
   xdc=dcast(xm, L1+X1~X2)
   xdc$L1[-match( unique(xdc$L1), xdc$L1)] = ""# die eerste kollom is bevat herhaling
   names(xdc)[1:2] = c(place, var)
   names(xdc) = gsub("_", " ", names(xdc))
   
   if (latex == TRUE){
    lbl = paste(gsub("_", " ", var), "_proj", sep="")
   	ttl = gsub("_", " ", var)
    cp = paste("Projection of \\emph{", ttl,"}", sep="" )
   	rgp = rep(times = length(TNAMES), x = idx )
    fn = paste(dirr, ttl, ".proj.tex", sep="")
    if (verbose == TRUE) message("rgp ", rgp)
   	latex(xdc, rowlabel="", rowname=NULL, n.rgroup=rgp, rgroup=TNAMES ,caption=cp, label=lbl, caption.loc=c.loc, file=fn, title=ttl)
    if (verbose == TRUE) message("ek skryf hom by ", fn)
   }
   
   if (xlsx == TRUE){
    lbl = paste(gsub("_", " ", var), "_proj", sep="")
   	ttl = gsub(" ", "_", var)
    cp = paste("Projection of \\emph{", ttl,"}", sep="" )
   	rgp = rep(times = length(TNAMES), x = idx )
    fn = paste(dirr, ttl, ".proj.xlsx", sep="")
    if (verbose == TRUE) message("rgp ", rgp)
   	openxlsx::write.xlsx(xdc, rowlabel="", rowname=NULL, n.rgroup=rgp, rgroup=TNAMES ,caption=cp, label=lbl, caption.loc=c.loc, file=fn, title=ttl)
    if (verbose == TRUE) message("ek skryf hom by ", fn)
   }
   
   xdc
  }
  
## gebruik dalk die "samplingbook" pakket se Sprop
 
  
#' Help function
#' 
#' @param x A number
#' @param y A number
#' @export

maal <- function(x,z){ x * z }

#toetsdata

toetsdata.popProjek <- function(){
        set.seed(1)
        df = data.frame(sol_energy_coal_ignition = sample(c("Y","N"), size = 100, replace = TRUE),
                        place = sample(c("eMbalenhle", "Lebohang", "eMzinoni",  "KwaDela"), 100, TRUE))
        dirr = tempdir()
        
        res = list(df = df, dirr = dirr)
        res
}

# Dink daaraan om Freq, % en Pop kolomme ook in te sit en cl van die % ook te gee 