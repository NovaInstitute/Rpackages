#' Summary
#'
#' Function to summarise ER calculation results
#'
#' @param file The file path to the base summary csv
#' @export

summry <- function(file="~/Desktop/Nova/DEE/GS/PDD/ROFWERK/bsagld-101214-CJP-PDD-spreistaat-basesummary.csv") {
summry=read.csv(file=file,header=TRUE)

summry[,"Value"]=NULL
summry[1,"Value"]=round(uta["TOTAL, MEAN","XUcoal"],4)
summry[2,"Value"]=round(uta["TOTAL, MEAN","Pop."])
summry[3,"Value"]=round(uta["TOTAL, MEAN","XUalt"],4)
summry[4,"Value"]=round(Uother["TOTAL, MEAN","XUother"],4) # same as uta["TOTAL, MEAN", "BM users other"] / uta["TOTAL, MEAN", "BM users"]
summry[5,"Value"]=round(mean(haq$eef,na.rm=TRUE),2)
summry[6,"Value"]=round(mean(haq$eef.s,na.rm=TRUE),2)
summry[7,"Value"]=round(CBy,2)
summry[8,"Value"]=round(CBc/1000,2)
summry[9,"Value"]=round(CPy,2)
summry[10,"Value"]=round(CPc/1000,2)
summry[11,"Value"]=round(ccontent,4)
summry[12,"Value"]=round(XO2,2)
summry[13,"Value"]=round(COEF,4)
summry[14,"Value"]=round(BE/1000,2)
summry[15,"Value"]=round(PE/1000,2)
summry[16,"Value"]=0
summry[17,"Value"]=round(ER/1000,2)
summry[18,"Value"]=round(Ualt,0)
summry[19,"Value"]=paste(town,collapse=";")

summry[,2]=as.character(summry[,2])
summry[,3]=as.character(summry[,3])
summry[,4]=as.character(summry[,4])
summry[,5]=as.character(summry[,5])
summry[,6]=as.character(summry[,6])

assign("summry",summry,envir=.GlobalEnv)

}
