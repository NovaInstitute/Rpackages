#' Chi2 CHAID
#'
#' Pairwise Chi2 tests together with CHAID
#'
#' @details Relies on chi.2.exp and spdf function
#' @param x Data frame containing survey information
#' @param naam Character vector containing the name attributed to the output data
#' @param datadirr Character vector referring to the location of the output directory for generated data
#' @param grafdirr Directory in which graphs will be stored as Character vector
#' @param tabdirr Directory in which tables will be stored as Character vector
#' @param p.cutofff The cutoff probability value as a Numeric
#' @param plott Logical whether to plot data or not
#' @param nett.naam Character vector containing the name of the gml network diagram
#' @export

chi.chaid <- function(x, naam = "HH_chi2_uitkomste", datadirr = datadir, grafdirr = grafdir,
                      tabdirr = tabdir, p.cutofff = 0.01, plott = TRUE,
                      nett.naam = "HH_chi2_uitkomste.net"){

chi.2.eksp(data = x,
           plot=plott,
           p.cutoff= p.cutofff,
           out.idx = as.integer(which(sapply(x, class) == "factor")),
           outdir = datadirr,
           xlsx.name = paste(naam, sep=""), net.name = nett.naam, verbose = FALSE)
message("spdf gemaak")

lapply(get("spdf", envir = .GlobalEnv), function(x) afh.tab(x, tabdir=tabdirr))
message("Afhanklikheidstabelle gemaak")

chaid.qol.list <- lapply(spdf, make.formula.c2m)
message("Fromule-lus gemaak")
qol.chaidlist <- lapply(chaid.qol.list, function(i) chaid(i, data=x))
#qol.shortform <- lapply(qol.chaidlist, get.short.form.chaid, type = "right")

save(x, spdf, chaid.qol.list, qol.chaidlist,
     #qol.shortform,
     file=paste(datadir, naam, ".Rda", sep=""))

lapply(seq_along(qol.chaidlist), function(i) {
  pdf(file=paste(grafdirr, names(qol.chaidlist[i]),"CHIAD.pdf", sep=""),width=21, height=18)
  plot(qol.chaidlist[[i]], main = names(qol.chaidlist[i]))
  message(names(qol.chaidlist[i]))
  dev.off()
})
assign("qol.chaidlist", qol.chaidlist, envir=.GlobalEnv)
assign("qol.shortform", qol.shortform, envir=.GlobalEnv)
}
