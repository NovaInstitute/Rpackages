#' One, Two & Three
#'
#' Function for one, two and three way LaTeX tables and graphs
#'
#' @param x Data frame containing survey data
#' @param drops Character vector of the names of columns to be dropped
#' @param een Logical that generates one way LaTeX tables when TRUE
#' @param eendir Character vector of the directory in which the one way LaTeX tables should be saved
#' @param twee Logical that generates two way LaTeX tables when TRUE
#' @param tweedir Character vector of the directory in which the two way LaTeX tables should be saved
#' @param n.tweedir Character vector of the directory in which the numeric two way LaTeX tables
#' should be saved
#' @param driedir Character vector of the directory in which the three way LaTeX tables should be saved
#' @param groep Character vector that indicates the grouping variable
#' @param groeplab The label of the grouping variable as character vector
#' @param moederskip Logical that creates tables with additional information
#' @param p.type.n The plot type as character vector
#' @param graph Logical that creates and saves a qplot when TRUE
#' @param graphdir Character vector of the directory in which the graphs should be saved
#' @param verbose Logical that displays function messages
#' @param ... Arguments that are passed to/from other functions
#' @export

eentweedrie <- function(x = qol,
                        drops = c("sol_waste_notcollected_inside_recycled", "sol_waste_notcollected_outside_other", "sol_waste_notcollected_outside_other", "sol_income_sources_explainother", "respondent_info_contact_phonenumber"),
                        een = TRUE,
                        eendir = tabdir,
                        twee = TRUE,
                        tweedir = paste(tabdir, "tweerigting/", sep = ""),
                        n.tweedir = paste(tabdir, "n.tweerigting/", sep = ""),
                        driedir = paste(tabdir, "drierigting/", sep = ""),
                        groep = "place",
                        groeplab = NULL,
                        moederskip = TRUE,
                        p.type.n = "box",
                        graph = FALSE,
                        graphdir = "~/Dropbox/Sasol QOL/Working/Analysis/QualityOfLifeSurvey/Report/Grafieke/",
                        verbose = FALSE, ...) {
    require(reporttools)
    require(ggplot2)

        if (file.exists(n.tweedir) == FALSE) dir.create(n.tweedir)
        if (file.exists(tweedir) == FALSE) dir.create(tweedir)
        if (file.exists(driedir) == FALSE) dir.create(driedir)
        if (file.exists(graphdir) == FALSE) dir.create(graphdir)

    assign("tableNominal2", tableNominal2, envir = .GlobalEnv)
    z <- function() match("tableNominal2", ls(envir=.GlobalEnv))
    if (is.na(z())) stop("Jy kort tableNominal2.R. Laai hom appart en probeer weer")

    if (is.null(groeplab))
        groeplab = groep
    # Kategories #
    fidx <- names(x)[sapply(x, is.factor)]
    message("drops is \n", paste(drops, " "), "\nfidx lengte: ", length(fidx))
    dropidx <- as.integer(na.omit(match(drops, fidx)))
    if (length(dropidx) > 0)
        fidx <- fidx[-dropidx]
    if (verbose == TRUE) message("fidx lengte: ", length(fidx))

    # enkel
    if (een == TRUE){
    for (i in 1:length(fidx)){
      lb = names(x[match(fidx[i], names(x))])
      lb = gsub("_", " ", lb)
      if (verbose == TRUE) message("\nOK, hier begin ",lb)
      capture.output(
        tableNominal2(
            vars = list(x[, fidx[i]]),
            cap = paste(" ", lb),
            lab = paste(lb,  sep = ""),
            cumsum = FALSE, longtable = FALSE, caption.placement="top"),
        file = paste(eendir, gsub(" ","_",lb), ".enkel",".tex", sep = ""))
      if (verbose == TRUE) message(lb, "is in ", paste(eendir, gsub(" ","_",lb), ".enkel",".tex", sep = ""))
    }
    }

    # gegroepeer per dorp
    if (twee == TRUE){
      for (i in 1:length(fidx)) {
        lb = names(x[match(fidx[i], names(x))])
        lb = gsub("_", " ", lb)
        if (verbose == TRUE) message("\nOK, hier begin ",lb)
        capture.output(tableNominal2(vars = list(x[, fidx[i]]), nms = lb,
                                    group = x[, groep],
                                    print.pval = "chi2",
                                    cap = paste(" ", lb),
                                    lab = paste(lb, ".", groeplab, sep = ""),
                                    cumsum = FALSE, longtable = FALSE, caption.placement="top"),
                       file = paste(tweedir, gsub(" ","_",lb), ".", groeplab, ".tex", sep = ""))
        if (verbose == TRUE) message(paste(tweedir, gsub(" ","_",lb), ".", groeplab, ".tex", sep = ""), " geskryf \n")
      }

    # kategoriese moederskip
    if (moederskip == TRUE) {
        for (i in 1:length(fidx)) {
            lb1 = names(x[match(fidx[i], names(x))])
            lb1 = gsub("_", " ", lb1)
            lb = "Moerderskip"
            capture.output(
              tableNominal(
                vars = list(x[, fidx[i]]),
                group = x[, groep],
                print.pval = "chi2",
                cap = paste(" ", lb1),
                lab = paste(lb1, ".town", sep = ""),
                cumsum = FALSE, longtable = TRUE),
              file = paste(tweedir, lb, ".", groeplab, ".tex", sep = ""), append = TRUE)
        }
    }
    }

    # Numeries #
    if (verbose == TRUE) message("\n Ons gaan numeries")
    nidx <- names(x)[sapply(x, is.numeric)]
    dropidx <- as.integer(na.omit(match(drops, nidx)))
    if (length(dropidx) > 0)
        nidx <- nidx[-dropidx]
    # gegroepeer per dorp
    for (i in 1:length(nidx)) {
        lb = names(x[match(nidx[i], names(x))])
        lb = gsub("_", " ", lb)
        if (verbose == TRUE) message("lb: ", lb)
        if (verbose == TRUE) message("groep: ", groep)
        if (length(x[, nidx[i]]) > 0){
          capture.output(
            tableContinuous(
              vars = list(x[, nidx[i]]),
              group = x[, groep],
              print.pval = "anova",
              cap = paste(" ", lb),
              lab = paste(lb, ".town", sep = ""),
              cumsum = FALSE, longtable=FALSE, caption.placement="top"),
            file = paste(n.tweedir, gsub(" ", "_", lb), ".", groeplab, ".tex", sep = ""))
          if (verbose == TRUE) message(paste(n.tweedir, gsub(" ", "_", lb), ".", groeplab, ".tex", " geskryf met label \n", lb,sep = ""))
        }

   if (graph == TRUE) {
    	if (p.type.n == "density"){pt = qplot(x[, nidx[i]],  geom="density", color=x[, groep], main = paste("Density plot of ",lb) , xlab= lb)
    	ggsave(pt, file = paste(graphdir, gsub(" ", "_", lb), ".", groeplab, ".pdf", sep = ""))} else {
    		pt = qplot(y = x[, nidx[i]],  geom="boxplot", x = x[, groep], main = paste("Comparison of ",lb, " by ",groep) , xlab= lb)
    	ggsave(pt, file = paste(graphdir, gsub(" ", "_", lb), ".", "box." ,groeplab, ".pdf", sep = ""))}
    }
    }

    ## numeriese moederskip gegroepeer per dorp
    if (moederskip == TRUE) {
        for (i in 1:length(nidx)) {
            lb = names(x[match(nidx[i], names(x))])
            lb = gsub("_", " ", lb)
            capture.output(
              tableContinuous(
                vars = list(x[, nidx[i]]),
                group = x[, groep],
                print.pval = "anova",
                cap = paste(" ", lb),
                lab = paste(lb, ".town", sep = ""),
                cumsum = FALSE, longtable=TRUE, caption.placement="top"),
              file = paste(n.tweedir, gsub(" ", "_", lb), ".", groeplab, ".tex", sep = ""), append = TRUE)
        }
    }

    message("That all folks!")
}

