#' Project Formula Table
#'
#' Function to make project formula table and ER table for report. This function retrieves the needed
#' data from the global environment such as the base and table directories and the HAQ data frame.
#'
#' @export

project.formula.table=function(){

  Winter=c(round(as.numeric(projectbytownwinter["TOTAL, MEAN","winter current year sum"]),2), #sum current
           round(cl.alt[1,"point prop."],2),  #XUalt
           round(as.numeric(projectbytownwinter["TOTAL, MEAN","winter current year n"])) , # n_alt
           length(which(is.na(haq$winter.current.kg[which(haq$hh.BM.use=="BM")]))), # n.NA
           uta["TOTAL, MEAN","n"] - length(which(is.na(haq$winter.current.kg[which(haq$hh.BM.use=="BM")]))), # n
           round(CPy.w,1), # CPy
           round(CPc.w,1) # CPc
           )
  Summer=c(round(as.numeric(projectbytownsummer["TOTAL, MEAN","summer current year sum"]),2),
           round(cl.alt[1,"point prop."],2),
           round(as.numeric(projectbytownsummer["TOTAL, MEAN","summer current year n"])),
           round(as.numeric(projectbytownsummer["TOTAL, MEAN","summer current year NA "]), 2),
           uta["TOTAL, MEAN","n"],
           round(CPy.s,1),
           round(CPc.s,1)
           )
  Annual=c(round(as.numeric(projectbytownyear["TOTAL, MEAN","annual current sum"]),2),
           round(cl.alt[1,"point prop."],2),
           round(as.numeric(projectbytownyear["TOTAL, MEAN","annual current n"])) ,
           round(as.numeric(projectbytownyear["TOTAL, MEAN","annual current NA "])),
           uta["TOTAL, MEAN","n"],
           round(CPy,1),
           round(CPc,1)
           )

  Season=matrix(rbind( Winter, Summer, Annual),ncol=7)
  latex(Season,colheads=c("$\\sum_{i}\\sum_{j}C_{P,i,j,y}\\times\\mathit{FC}_{i}$",
                          "$XU_{alt,y}$",
                          "$n_{alt}$",
                          "$n.NA$",
                           "$n$",
                          "$\\bar{C}_{P,y}$",
                          "${C}_{P,c}$"),
        rowname=c("Winter","Summer", "Annual"),
        n.rgroup = c(2,1),
        digits=3,
        label  = "Average-project-for",
        caption = "Average seasonal and annual project coal consumption",
        file=paste(basedir,tabdir,"ProjectFormulaTab.tex",sep=""))

  # Project Emission
  project.emission.tab=rbind(c(round(CPc.w/1000,2),round(COEF,4),round(PE.w/1000,2)),
                             c(round(CPc.s/1000,2),round(COEF,4),round(PE.s/1000,2)),
                             c(round(CPc/1000,2),round(COEF,4),round(PE/1000,2))
                             )
  PE.df=data.frame(project.emission.tab)
  colnames(PE.df)=c("$C_{P,y}$(tonnes coal)", "$COEF$", "$PE$(tonnes $CO_{2}$)")
  rownames(PE.df)=c("Winter", "Summer", "Annual")

  assign("project.emission.tab",project.emission.tab,envir=.GlobalEnv)

  latex(PE.df,
    colheads = c("$C_{P,y}$(tonnes coal)", "$COEF$", "$PE$(tonnes $CO_{2}$)"),
    caption  = "Project Emissions",
    label    = "ProjectEmissions",
    title    = projectname,
    n.rgroup = c(2,1),
    file     = paste(basedir,tabdir,"ProjectEmissionTab.tex",sep="")
    )

}
