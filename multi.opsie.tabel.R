## Multi-opsie tabel ##

multi.opsie.tabel <- function(df = des,
                                x = "energy_lpg_utility",
                                yescat = "Yes",
                                nocat = "No",
                                tabeleer = TRUE,
                                abs.perc = FALSE,
                                tex = TRUE,
                                tabdir = get("tabdir", envir=.GlobalEnv),
                                graphdir = get("grafdir", envir=.GlobalEnv),
                                fn = paste(x, "h.bar.", sep=""),
                                verbose=FALSE, fplace = "!ht"){
  suppressMessages(require(gdata))
  suppressMessages(require(Hmisc))
  ttl = capwords(gsub("\\.|_", " ", x))
  if (verbose == TRUE) message("Titel is : ", ttl)
if (tabeleer == TRUE) {
   #if (verbose == TRUE) message("x is ", x, "\nnames df is ", paste(names(df)," "))
  d.f = df[,grep(x, names(df))]
   if (verbose == TRUE) message("d.f is ", dim(d.f)[[1]], " by ", dim(d.f)[[2]], "\nVan die klas ", class(d.f))
  cct = addmargins(table(complete.cases(d.f), exclude = NULL))
  na = cct[grep("FALSE", names(cct))]
  n.obs = cct[grep("TRUE", names(cct))]
  n = cct[grep("Sum", names(cct))]
   message(paste(na, " "))
  tab = as.data.frame(t(sapply(d.f, function(y) table(y))))
   if (verbose == TRUE) message("tab is ", dim(tab)[[1]], " by ", dim(tab)[[2]], "\nVan die klas ", class(tab))

  rownames(tab) = gsub(paste("^", x ,sep=""), "", rownames(tab))
  rownames(tab) = gsub(paste("^", "_" ,sep=""), "", rownames(tab))
  rownames(tab) = gsub("\\.", " ", rownames(tab))
  rownames(tab) = sapply(rownames(tab), initCap)

  No = length(which(df[,grep(x, names(df))]==nocat))
   if (verbose == TRUE) message("No is ", str(No))
  Yes = length(which(df[,grep(x, names(df))]==yescat))
   if (verbose == TRUE) message("Yes is ", Yes)
   if (abs.perc == TRUE) {
   	tab = transform(tab, np.p= No / length(df[,1]) *100, yes.p = Yes / length(df[,1])*100 )
   	names(tab) = c("No", "Yes", "$\\%No\\, of\\, all$", "$\\%Yes \\, of\\, all$")
   	}
   if (abs.perc == FALSE) {tab = transform(tab, no.p= round(No / (No + Yes),2)*100, yes.p= round(Yes / (No + Yes),2)*100 )
   	names(tab) = c("No", "Yes", "$\\%No$", "$\\%Yes$")
   	}
  assign(paste(x, ".tab", sep=""), tab, envir=.GlobalEnv)
  if (tex == TRUE) {
    latex(tab,
          file=paste(tabdir, ttl, ".tex", sep=""),
          digits=4,
          caption = paste("Multiple choice results for:", ttl),
          title = ttl,
          where = fplace,
          insert.bottom = paste("\\centering n = ", n, "; n.obs = ",n.obs, "; \\#NA = ", na))
  }
}

z = sapply(d.f, function(y) table(y)[yescat])
z[is.na(z)] = 0
names(z) = gsub(paste(yescat, "$", sep=""), "", names(z))
names(z) = gsub("\\.NA|\\.1", "", names(z))
names(z) = gsub(paste(x, "_", sep=""), "", names(z))
names(z) = gsub("\\.$", "", names(z))
names(z) = gsub("\\.", " ", names(z))
names(z) = sapply(names(z), initCap)
if (verbose == TRUE) message(sapply(names(z[which(z==1)]), function(x) paste(x, "\n")))

pdf("a4", file=paste(graphdir, fn,".pdf",sep=""))
par(mai =  c(1.360, 3.5, 1.093, 0.1))
barplot(c(z/length(df[,1])),
        horiz=TRUE, names.arg=c(names(z)),
        las=1 ,
        main = paste("Multiple choice results for:\n", ttl), sub = paste("n = ", n, "; #NA = ", na),
        cex.lab=0.5, xlim=c(0,1))
par(mai =  c(1.360, 1.093, 1.093, 0.560))
dev.off()

}

########################## Hulp funksies ##########################

######
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                                 {s <- substring(s, 2); if(strict) tolower(s) else s},
                                 sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }

######
initCap <- function(z) {sub(pattern=substring(text=z,first=1, last=1),
           replacement=toupper(substring(z,first=1, last=1)[1]),
           x=z)}

######
multi.opsie.punt <- function(df = des,
                                x = "energy_lpg_utility",
                                yescat = "Yes",
                                nocat = "No", lm = 0.02, abs.perc = TRUE){
  d.f = df[,grep(x, names(df))]
  cct = addmargins(table(complete.cases(d.f)))
  na = cct[1]
  n.obs = cct[2]
  n = cct[3]
  tab = as.data.frame(t(sapply(d.f, function(y) table(y))))

  rownames(tab) = gsub(paste("^", x ,sep=""), "", rownames(tab))
  rownames(tab) = gsub(paste("^", "_" ,sep=""), "", rownames(tab))
  rownames(tab) = gsub("\\.", " ", rownames(tab))
  rownames(tab) = sapply(rownames(tab), initCap)

  No = length(which(df[,grep(x, names(df))]==nocat))
  Yes = length(which(df[,grep(x, names(df))]==yescat))
    if (abs.perc == TRUE) {
   	tab = transform(tab, np.p= No / length(df[,1]) *100, yes.p = Yes / length(df[,1])*100 )
   	names(tab) = c("No", "Yes", "$\\%No\\, of\\, all$", "$\\%Yes \\, of\\, all$")
   	}
   if (abs.perc == FALSE) {tab = transform(tab, no.p= round(No / (No + Yes),2)*100, yes.p= round(Yes / (No + Yes),2)*100 )
   	names(tab) = c("No", "Yes", "$\\%No$", "$\\%Yes$")
   	}
   tab = tab[which(tab[,4] > lm*100), ]
   tab = tab[order(tab$Yes, decreasing = TRUE),]
   rn = rownames(tab)

   do.call("cat", list(
   sprintf("The %s that %s chosen by more that %.2f percent of %s %s given below: \\begin{itemize}",
    ifelse(nrow(tab) > 1, "options", "option"),
    ifelse(nrow(tab) > 1, "were", "was"),
    lm*100,
    ifelse(abs.perc == TRUE, "all respondents", "respondents who answered the question"),
    ifelse(nrow(tab) > 1, "are", "is")),

   sprintf("\\item The option %s  was chosen by %.0f percent ", paste("\\emph{", rn, "}", sep="") , tab[,4]),

   "\\end{itemize}"))
  }

############################### voorbeeld data #################################

varlist <- structure(list(`character(0)` = structure(list(sewe_A = structure(c(NA,  2L, 2L, 2L, NA, 1L, NA, 2L, 1L, 1L), .Label = c("0", "1"), class = "factor"),
                                                          sewe_B = structure(c(1L, 1L, 2L, 1L, NA, 2L, 2L, 1L, 2L,NA), .Label = c("0", "1"), class = "factor"),
                                                          sewe_C = structure(c(NA,    2L, 1L, NA, 2L, NA, 2L, NA, NA, NA), .Label = c("0", "1"), class = "factor"),
                                                          groep = structure(c(2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L ), .Label = c("F", "M"), class = "factor")),
                                                     .Names = c("sewe_A",  "sewe_B", "sewe_C", "groep"), class = "data.frame", row.names = c(NA, -10L)),
                          `character(0)` = structure(list(agt_B = structure(c(2L,   NA, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L), .Label = c("0", "1"), class = "factor"),
                                                          agt_C = structure(c(2L, 1L, 2L, NA, 1L, NA, 2L, 2L, 1L, 1L ), .Label = c("0", "1"), class = "factor"),
                                                          groep = structure(c(2L,  1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L), .Label = c("F", "M"), class = "factor")),
                                                     .Names = c("agt_B",  "agt_C", "groep"), class = "data.frame", row.names = c(NA, -10L ))),
                     .Names = c("character(0)", "character(0)"))