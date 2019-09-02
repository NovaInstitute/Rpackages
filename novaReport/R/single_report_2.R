voorspellende_veranderlikes <-    c("respondent_address_town",
                                    "respondent_age",
                                    "respondent_sex",
                                    "bidy_disease_prevalence_cholesterol",
                                    "water.supply",
                                    "water_collect_container",
                                    "sol_water_treatment",
                                    "toilets.available.in.yard",
                                    "refuse.collection",
                                    "frequency.of.refuse.removal",
                                    "failure.to.remove.refuse",
                                    "recycling",
                                    "vegetables.and.fruit",
                                    "fruit.and.veg.source",
                                    "proteins",
                                    "sol_energy_cooking_main",
                                    "sol_energy_heating_main",
                                    "sol_energy_coal_ignition",
                                    "household.health.experience",
                                    "access.to.medical.care.through.employment",
                                    "income_salary.from.work",
                                    "income_pension",
                                    "income_disability.grant",
                                    "income_child.grant",
                                    "income_income.from.own.business",
                                    "income_any.other.source.of.income",
                                    "income_we.have.no.income.at.all",
                                    "income_i.do.not.want.to.disclose.it",
                                    "dirty.fuel",
                                    "pilot",
                                    "body_exposure_alcohol",
                                    "body_exposure_drugs",
                                    "body_morbidity_month",
                                    "body_symptoms_twoweek_list_a.running.nose.with.body.aches",
                                    "body_symptoms_twoweek_list_a.running.nose.without.body.aches", 
                                    "body_symptoms_twoweek_list_coughing.sputum.but.no.weight.loss.or.fever.or.night.sweats",
                                    "body_symptoms_twoweek_list_itching.or.burning.eyes",
                                    "body_symptoms_twoweek_list_yellow.or.green.nasal.mucous..draining.into.throat",
                                    "body_symptoms_year_list_coughing.blood..and.losing.weight.with.fever.and.sweating.at.night",
                                    "body_symptoms_year_list_coughing.sputum.but.no.weight.loss.or.fever.or.night.sweats",
                                    "demographic_member_sex",
                                    "personal_education_completed",
                                    "sol_occupation",
                                    "body_disease_diagnosed_ever_asthma",
                                    "body_disease_diagnosed_ever_cancer",
                                    "body_disease_diagnosed_ever_hiv",
                                    "body_disease_diagnosed_ever_tuberculosis..tb.",
                                    "body_disease_diagnosed_fourtnight_bronchitis",
                                    "body_disease_diagnosed_fourtnight_sinusitus",
                                    "body_disease_diagnosed_year_pneumonia",
                                    "body_immunisation",
                                    "body_disease_treatment_asthma",
                                    "body_disease_prevalence_tb",
                                    "body_disease_prevalence_pneumonia",
                                    "respondent_address_town",
                                    "respondent_address_subplace")

# hulpfunksie
wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}

#' Single Report
#' 
#' Report per individual variable with subsection heading. 
#' 
#' @param x Data frame typically DES data
#' @param var Character vector containing the variable in x that you wish to plot
#' @param groep Character vector containing variable in x that grouping should be done by
#' @param groeplab Character vector containing a chosen lable for the graphs
#' @param out Logical that generates the desired output
#' @param stats Character vector that decides whether the index of the variable that 
#' needs to be 'predicted' should be determined
#' @param qs Data frame that contains a variable name, section, the question text and type 
#' @param qs_name Character vector referring to the question name
#' @param qs_text Character vector referring to the question text
#' @param drr Character vector containing the directory
#' @param druk Logical that determines whether the output will genarate a pdf and save it to the given directory
#' @param webify Logical whether to create an html document
#' @param verbose Logical to display function messages
#' @param wd Integer giving the target column for wrapping lines in the output
#' @param md Logical that formats summary results
#' @param chi.digits Numerical giving the number of decimal places or significant digits to be used after making 
#' use of the chi.2.eksp function
#' @param debug Logical that assigns voorspellende_veranderlikes, x and idx_temp to their corresponding names in the
#' @param varSizeN Character vector that contains the percentage column size to be
#' attributed to the column containing variables. This is used to customize tables.
#' @param levSizeN Character vector that contains the percentage column size to be
#' attributed to the column containing levels. This is used to customize tables.
#' global environment
#' @param ... Arguments to be passed to/from calling function
#' @note The idea is to lapply a single report over the subsection and subsection_report lapply over the sections
#' @export

single_report <- function (x, var = c("een"), 
                           groep = NULL, 
                           groeplab = NULL, 
                           out = TRUE, 
                           stats = c("all", "basic")[1], 
                           longt = FALSE, 
                           qs, 
                           font.size=10,
                           qs_name = "question.name", 
                           qs_text = "question.text", 
                           drr = "~/tmp/", 
                           ncat = 4, 
                           nnum = 10, 
                           druk = FALSE, 
                           webify = FALSE, 
                           verbose = FALSE, 
                           wd = 15, 
                           md = TRUE, 
                           chi.digits = 4, 
                           debug = TRUE, 
                           varSizeN = "0.5", 
                           levSizeN = "0.5", 
                           voorspellende_veranderlikes = NULL, 
                           plotOok = TRUE,
                           clearpage = FALSE, ...) {
        
        {if (require(R.utils, quietly = TRUE, warn.conflicts = FALSE) == 
             FALSE) {
                install.packages("R.utils")
                library(R.utils)
        }
                if (require(reporttools, quietly = TRUE, warn.conflicts = FALSE) == 
                    FALSE) {
                        install.packages("reporttools")
                        library(reporttools)
                }
                if (require(ggplot2, quietly = TRUE, warn.conflicts = FALSE) == 
                    FALSE) {
                        install.packages("ggplot2")
                        library(ggplot2)
                }
                if (require(reshape2, quietly = TRUE, warn.conflicts = FALSE) == 
                    FALSE) {
                        install.packages("reshape2")
                        library(reshape2)
                }
                if (require(gridExtra, quietly = TRUE, warn.conflicts = FALSE) == 
                    FALSE) {
                        install.packages("gridExtra")
                        library(gridExtra)
                }
                if (require(pander, quietly = TRUE, warn.conflicts = FALSE) == 
                    FALSE) {
                        install.packages("pander")
                        library(pander)
                }}
        
        
        # Basiese huishouding -----------------------------------------------------
        
        if (length(var) > 1) message("Ek verkort var tot die eerste een")
        var = as.character(var)[1]
        if (!is.data.frame(x)) stop("x must be a data frame")
        if (!is.data.frame(qs)) stop("qs must be a data frame")
        if (!is.character(var)) stop("var must be coercible to a character")
        if (any(is.na(match(var, names(x)))))  stop("var must be a variable in x ")
        if (any(is.na(match(groep, names(x))))) stop("groep must be a variable in x ")
        if (is.null(groep)) groep = 1
        if (is.null(groeplab)) groeplab = groep
        gp = x[, groep]
        if (all(is.na(gp))) message("\nGroep bestaan uit NAs. Dis 'n slegte idee. Kies dalk 'n ander groep\n")
        lb = capitalize(gsub("_|\\.", " ", var))
        if (identical(x[, var], gp)) groep = 1
        if (length(stats) > 1) stats = "all"
        if (is.na(match(qs_text, names(qs)))) stop("qs_text is nie 'n kolomnaam in qs nie")
        if (is.na(match(qs_name, names(qs)))) stop("qs_name is nie 'n kolomnaam in qs nie")
        
        idx <- na.omit(match(var, qs[, qs_name]))
        if (length(idx) != 0) vraagteks = as.character(qs[idx, qs_text])
        natab = table(is.na(x[, var]), exclude = NULL)["TRUE"]
        nobtab = table(is.na(x[, var]))["FALSE"]
        nna = ifelse(is.na(natab), 0, natab)
        vr = gsub("_|\\.", " ", var)
        vraagsin = ""
        verwyssin <- ""
        tabLbl <- sprintf("tbl%s", gsub("[[:blank:]]{1,}", "", capwords(vr)))
        
        # KOM ONS BEGIN MET 'N OPSKRIF
        cat(sprintf("%s", 
                    ifelse(md == TRUE, paste(" \n## ", lb, "\n", sep = ""), 
                           paste("\\subsection{",  lb, "}", sep = ""))))
        
        chi2 <- NULL
        if (verbose) message("Groep: ", groep, "\nlength(unique(x[, var]): ", length(unique(x[, var])), "\nncat :", ncat)

        # DOEN NOU DIE RES
        
        if (nobtab != 0 & !is.na(nobtab)) {
                
                # Bou die vraagsin --------------------------------------------------------
                
                if (length(idx) != 0) {
                        vraagsin = sprintf("The variable \\emph{%s} contains the responses to the question \\emph{%s} and contains %i %s with %i missing %s", 
                                           vr, 
                                           vraagteks, 
                                           nobtab, 
                                           ifelse(nobtab == 1, "observation", "observations"), 
                                           nna, 
                                           ifelse(nna == 1, "value.", "values."))
                } else {
                        vraagsin <- sprintf("The variable %s contains contains %i %s and %i missing %s", 
                                            vr, 
                                            nobtab, 
                                            ifelse(nobtab == 1, "observation", "observations"), 
                                            nna, 
                                            ifelse(nna == 1, "value.", "values."))
                }
                
                # verwyssin <- sprintf("The summary results are shown in Table %s.", 
                #                     ifelse(md, paste("*", vr, "*", sep = ""), paste("\\ref{tab:lb", vr, "}", sep = ""))
                #                     )
                verwyssin <- sprintf("The summary results are shown in Table %s.", paste("\\ref{tab:", tabLbl, "}", sep = ""))
                num = is.numeric(x[, var])
                fc = is.factor(x[, var])
                ch = is.character(x[, var])
                
                # Maak die diagram --------------------------------------------------------
                
                # Die geen groep geval  --------------------------------------------------------
                if (groep == 1) {
                        if (debug) message("Groep: ", groep, 
                                           "\nlength(unique(x[, var]) :", length(unique(x[, var])), 
                                           "\nncat :", ncat)
                        # Die numeriese geen group geval  --------------------------------------------------------
                        if (num) {
                                if (length(unique(x[, var])) > nnum){
                                        p1 <- qplot(data = melt(x[, var]), 
                                                    geom = "boxplot", 
                                                    xlab = var, 
                                                    x = var, 
                                                    y = value, 
                                                    main = wrapper(paste("Values of ", lb), width = wd)) +
                                                theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11, face = "bold"))
                                        
                                        p2 <- qplot(data = data.frame(idx = 1:nrow(x), vr = x[, var]), 
                                                    x = vr, fill = 1, 
                                                    geom = "density", 
                                                    main = wrapper(paste("Density of ", lb), width = wd),
                                                    alpha = I(0.2)) + 
                                                theme(legend.position = "none") + 
                                                theme(axis.text = element_text(size = 9), 
                                                      axis.title = element_text(size = 11, face = "bold"))
                                        
                                        p3 <- suppressWarnings(
                                                qplot(data = data.frame(idx = 1:nrow(x), vr = x[, var]), 
                                                      x = vr, 
                                                      fill = 1, 
                                                      geom = "histogram", 
                                                      xlab = var, 
                                                      main = wrapper(paste("Histogram of ", lb), width = wd), alpha = I(0.9)) + 
                                                        theme(legend.position = "none") +
                                                        theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11, face = "bold"))
                                        )
                                        
                                        if (druk == TRUE) 
                                                pdf(file = paste(drr, var, ".pdf", sep = ""), width = 12)
                                        
                                        pp = function(main = paste("Values and distribution of ", var)){
                                                grid.arrange(p1, p3, p2, 
                                                             nrow = 2, 
                                                             ncol = 2, 
                                                             layout_matrix = rbind(c(1, 2), c(3, 3)), 
                                                             top = main)
                                        }
                                }
                                
                                if (druk == TRUE) 
                                        if(!is.null(dev.list())){
                                                dev.off()
                                        }
                                if (var %in% names(x)) {
                                        vrs <- x[, var, drop = FALSE]
                                        tab <- function() {
                                                tableContinuous2(vars = vrs, 
                                                                 cap = paste(lb," "), 
                                                                 lab = paste("tab:", tabLbl, sep = ""), 
                                                                 longtable = longt, 
                                                                 caption.placement = "top", 
                                                                 html = webify, 
                                                                 comment = FALSE, 
                                                                 nams = gsub("_", " ", var), 
                                                                 print.results = TRUE, 
                                                                 table.placement = "!h")
                                        }
                                }
                        }
                        
                        # Die diskrete geen group geval --------------------------------------------------------
                        if (fc | ch) {
                                # ons maak net 'n grafiek vir meer as ncat veanderlikkes
                                if (length(unique(x[, var])) > ncat){
                                        p <- ggplot(data.frame(vr = x[, var]), 
                                                    aes(x = vr, fill = 1), alpha = I(1/2)) + 
                                                labs(x = lb, y = "count") + 
                                                geom_bar(position = "dodge") + 
                                                theme(legend.position = "none") + 
                                                ggtitle(wrapper(paste("Frequency distrubution of",var), width = 2 * wd)) + 
                                                theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
                                                theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11, face = "bold"))
                                        
                                        pp = function() grid.arrange(p, nrow = 1)
                                        if (druk == TRUE)  ggsave(file = paste(drr, var, ".pdf", sep = ""), width = 12)
                                }
                                
                                if (var %in% names(x)) {
                                        vrs <- x[, var, drop = FALSE]
                                        tab <- function() {
                                                tableNominal2(vars = vrs, 
                                                              cap = paste(lb, "", sep = ""), 
                                                              lab = paste("tab:", tabLbl, sep = ""), 
                                                              levSizeN = levSizeN, 
                                                              cumsum = FALSE, 
                                                              longtable = longt, 
                                                              caption.placement = "top", 
                                                              html = webify, 
                                                              varSizeN = varSizeN,
                                                              font.size = font.size,
                                                              comment = FALSE, 
                                                              nams = gsub("_", " ", var), 
                                                              print.results = FALSE)
                                        }
                                }
                                if (stats == "all") {
                                        idx_temp <- match(x = var, table = names(x), -1)
                                        if (idx_temp > 0) {
                                                if (!is.null(voorspellende_veranderlikes)){
                                                        vidxx <- match(voorspellende_veranderlikes, names(x))
                                                        vidxx <- vidxx[!is.na(vidxx)]
                                                } else {vidxx <- integer()}
                                                
                                                if (length(vidxx) > 0) {
                                                        chi2 <- chi.2.eksp(data = x, out.idx = idx_temp, 
                                                                           min.length = 0, max.levels = 10, ver.idx = vidxx, 
                                                                           p.cutoff = 100, outdir = drr, out.df = TRUE, 
                                                                           verbose = FALSE, xlsx = FALSE, net.name = "chi2eksp")
                                                        
                                                        if (!is.null(chi2)) {
                                                                for (c in 1:ncol(chi2)) {
                                                                        if (is.numeric(chi2[, c])) {
                                                                                chi2[, c] <- round(x = chi2[, c], 
                                                                                                   digits = chi.digits)
                                                                        }
                                                                }
                                                                dropidxx <- match(c("warnings", "outcome"), 
                                                                                  table = names(chi2), nomatch = NA)
                                                                dropidxx <- dropidxx[!is.na(dropidxx)]
                                                                if (length(dropidxx) > 0) 
                                                                        chi2 <- chi2[, -dropidxx]
                                                                print(kable(chi2, format = "pandoc", 
                                                                            digits = 5), comment = FALSE)
                                                        }
                                                }
                                        }
                                }
                        }
                }
                # Die groep geval  --------------------------------------------------------
                if (groep != 1) {
                        # Die numeriese groep geval -----------------------------------------------
                        if (num) {
                                if (verbose) message("die numeriese groep geval")
                                if (length(unique(x[, var])) > nnum){
                                        p1 <- qplot(data = melt(data.frame(var = x[ ,var], group = gp)), 
                                                    geom = "boxplot", 
                                                    xlab = groeplab, 
                                                    x = group, 
                                                    y = value, 
                                                    main = wrapper(paste("Values of ", lb, " by ", groeplab), width = wd)) + 
                                                theme(legend.position = "bottom") + 
                                                theme(axis.text = element_text(size = 9), 
                                                      axis.title = element_text(size = 11, face = "bold"))
                                        
                                        p2 <- qplot(data = data.frame(var = x[, var], group = gp), 
                                                    x = var, 
                                                    geom = "density", 
                                                    xlab = var, 
                                                    fill = group, 
                                                    group = group, 
                                                    main = wrapper(paste("Density of ",  lb, " by ", groeplab), 
                                                                   width = wd), alpha = I(0.2)) + 
                                                theme(legend.position = "bottom") + 
                                                theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11, face = "bold"))
                                        
                                        if (druk == TRUE)  pdf(file = paste(drr, var, ".pdf", sep = ""), width = 12)
                                        
                                        pp = function() grid.arrange(p1, p2, nrow = 1, 
                                                                     top = wrapper(sprintf("Distribution of the variable %s", 
                                                                                           var), width = 2 * wd))
                                        if (druk == TRUE) dev.off()
                                        
                                } else {
                                        if (verbose) message("Ons gaan x[, var] 'n factor maak!")
                                        x[, var] <- as.factor(x[, var])
                                        fc <- is.factor(x[, var])
                                        gp = x[,var]
                                }
                                
                                if (var %in% names(x)) {
                                        if (length(unique(x[, var])) > nnum){
                                                vrs <- x[, var, drop = FALSE]
                                                tab <- function() {
                                                        tableContinuous2(vars = vrs, 
                                                                         group = gp, 
                                                                         cap = paste(lb, "", sep = ""), 
                                                                         lab = paste("tab:", tabLbl, sep = ""), 
                                                                         longtable = longt, 
                                                                         html = webify, 
                                                                         caption.placement = "top", 
                                                                         comment = FALSE, 
                                                                         nams = gsub("_", " ", var), 
                                                                         print.results = TRUE)
                                                }
                                        }
                                }
                        }
                        if (fc | ch) {
                                # Die diskrete groep geval -----------------------------------------------
                                if (length(unique(x[, var])) > ncat){
                                        p <- qplot(data = data.frame(var =abbreviate(x[, var], minlength = 28),  group = gp), x = var, geom = "bar", xlab = lb, 
                                                   facets = group ~ .,
                                                   main = paste(lb, "by", groeplab), fill = 1) + 
                                                theme(legend.position = "none") + 
                                                theme(axis.text.x = element_text(angle = 90, hjust = 0)) + 
                                                theme(axis.text = element_text(size = 9,  face = "bold"),axis.title = element_text(size = 11, face = "bold"))
                                        
                                        pp = function() grid.arrange(p, nrow = 1)
                                        if (druk == TRUE)  ggsave(p, file = paste(drr, var, ".pdf", sep = ""), width = 12)
                                }
                                
                                if (var %in% names(x)) {
                                        vrs <- x[, var, drop = FALSE]
                                        if (debug) assign("vrs", vrs, envir = .GlobalEnv)
                                        if (verbose) message("Diskrete groep geval: ons gaan nou tab maak
                               \nvar is ", var, "dim x is ", dim(x)[1], " by ",  dim(x)[2],
                                                             "\nvrs is ", str(vrs))
                                        tab <- function() {
                                                tableNominal2(vars = vrs, 
                                                              group = gp, 
                                                              cap = paste(lb, "", sep = ""), 
                                                              cumsum = FALSE, 
                                                              lab = paste("tab:", tabLbl, sep = ""), 
                                                              html = webify, 
                                                              longtable = longt, 
                                                              caption.placement = "top", 
                                                              varSizeN = varSizeN, 
                                                              levSizeN = levSizeN, 
                                                              comment = FALSE,
                                                              print.results = TRUE
                                                )
                                        }
                                }
                                
                                if (stats == "all") {
                                        idx_temp <- match(var, table = names(x), -1)
                                        if (idx_temp > 0) {
                                                vidxx <- match(voorspellende_veranderlikes, 
                                                               names(x))
                                                vidxx <- vidxx[!is.na(vidxx)]
                                                if (debug == TRUE) 
                                                        assign("voorspellende_veranderlikes", voorspellende_veranderlikes, 
                                                               envir = .GlobalEnv)
                                                if (debug == TRUE) 
                                                        assign("x", x, envir = .GlobalEnv)
                                                if (debug == TRUE) 
                                                        assign("idx_temp", idx_temp, envir = .GlobalEnv)
                                                if (length(vidxx) > 0) {
                                                        chi2 <- chi.2.eksp(data = x, out.idx = idx_temp, 
                                                                           min.length = 0, max.levels = 10, ver.idx = vidxx, 
                                                                           p.cutoff = 0.05, outdir = drr, out.df = TRUE, 
                                                                           verbose = FALSE, xlsx = FALSE, net.name = "chi2eksp")
                                                        if (!is.null(chi2)) {
                                                                for (c in 1:ncol(chi2)) {
                                                                        if (is.numeric(chi2[, c])) {
                                                                                chi2[, c] <- round(x = chi2[, c], 
                                                                                                   digits = 5)
                                                                        }
                                                                }
                                                                dropidxx <- match(c("warnings", "outcome"), 
                                                                                  table = names(chi2), nomatch = NA)
                                                                dropidxx <- dropidxx[!is.na(dropidxx)]
                                                                if (length(dropidxx) > 0) 
                                                                        chi2 <- chi2[, -dropidxx]
                                                                print(kable(x = chi2, format = "pandoc", 
                                                                            digits = chi.digits), comment = FALSE)
                                                        }
                                                }
                                        }
                                }
                        }
                }
        }
        if (verbose)  message(var)
        if (out) {
                do.call("cat", 
                        list(
                                "\n", 
                                ifelse(exists("vraagsin"), vraagsin, ""), "\n", 
                                ifelse(exists("verwyssin"),  verwyssin, ""), "\n"#, if (exists("tab", mode = "function")) tab() else "", "\n"
                        )
                )
                assign("tab", tab, envir = .GlobalEnv)
                if (exists("tab", mode = "function")) tab()
                if (exists("pp", mode = "function") & plotOok) pp()
                if (clearpage) cat("\n\\clearpage\n")
        }
}



# hulp funksie
wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}






# single_report <- function(x, 
#                           var = c("een"), 
#                           groep = NULL, 
#                           groeplab =NULL, 
#                           out = TRUE, 
#                           stats = c("all", "basic")[1],
#                           qs, 
#                           qs_name = "question.name", 
#                           qs_text = "question.text",
#                           drr = "~/tmp/", 
#                           druk = FALSE, 
#                           webify = FALSE, 
#                           verbose = FALSE, 
#                           wd = 15, 
#                           md = TRUE, chi.digits = 4, debug = TRUE, 
#                           varSizeN = "0.25",
#                           levSizeN = "0.15",
#                           ...){
#   
#   if(require(R.utils, quietly = TRUE, warn.conflicts = FALSE) == FALSE){ install.packages("R.utils"); library(R.utils) }
#   if(require(reporttools, quietly = TRUE, warn.conflicts = FALSE) == FALSE){ install.packages("reporttools"); library(reporttools) }
#   if(require(ggplot2, quietly = TRUE, warn.conflicts = FALSE) == FALSE){ install.packages("ggplot2"); library(ggplot2) }
#   if(require(reshape2, quietly = TRUE, warn.conflicts = FALSE) == FALSE){ install.packages("reshape2"); library(reshape2) }
#   if(require(gridExtra, quietly = TRUE, warn.conflicts = FALSE) == FALSE){ install.packages("gridExtra"); library(gridExtra) }
#   if(require(pander, quietly = TRUE, warn.conflicts = FALSE) == FALSE){ install.packages("pander"); library(pander) }
# 
#   #z <- function() match("tableNominal2", ls(envir=.GlobalEnv))
#   #if (is.na(z())) stop("Jy kort tableNominal2.R. Laai hom appart en probeer weer")
# 
#   # maak seker alles is daar
#   if (length(var) > 1) message("Ek verkort var tot die eerste een")
#   var = as.character(var)[1]
#   if (!is.data.frame(x)) stop("x must be a data frame")
#   if (!is.data.frame(qs)) stop("qs must be a data frame")
#   if (!is.character(var)) stop("var must be coercible to a character")
#   if (any(is.na(match(var, names(x))))) stop("var must be a variable in x ")
#   if (any(is.na(match(groep, names(x))))) stop("groep must be a variable in x ")
#   if (is.null(groep)) groep = 1
#   if (is.null(groeplab)) groeplab = groep
#   gp = x[,groep]
#   if (all(is.na(gp))) message("\nGroep bestaan uit NAs. Dis 'n slegte idee. Kies dalk 'n ander groep\n")
#   lb = capitalize(gsub("_|\\.", " ", var))
#   if (identical(x[,var], gp)) groep = 1
#   if (length(stats) > 1) {stats = "all"}
# 
#   chi2 <- NULL
#   
#   # teks wat vraag gee en aantal waarnemings (nobs en #NA)
#   idx = na.omit(match(var, qs[,qs_name]))
#   if (length(idx) != 0) vraagteks = as.character(qs[idx, qs_text])
#   natab = table(is.na(x[,var]))["TRUE"]
#   nobtab = table(is.na(x[,var]))["FALSE"]
#   nna = ifelse(is.na(natab), 0, natab)
#   vr = gsub("_|\\.", " ", var)
#   vraagsin = ""
#   if (nobtab != 0 & !is.na(nobtab)){
#     if (length(idx) != 0){
#       vraagsin = sprintf("The variable %s contains the responses to the question \\emph{%s}. It contains %i %s and %i missing %s" ,
#                          vr, vraagteks, nobtab, ifelse(nobtab==1 ,"observation", "observations"), nna, ifelse(nna==1, "value.", "values."))
#     } else {
#       vraagsin = sprintf("The variable %s contains contains %i %s and %i missing %s" ,
#                          vr, nobtab, ifelse(nobtab==1 ,"observation", "observations"), nna, ifelse(nna==1, "value.", "values."))
#     }
# 
#     # teks met verwysing na tabel met opsommende statestiek.
#     verwyssin = sprintf("The summary results are shown in Table %s and Figure %s.",
#                         ifelse(md == TRUE, paste("*",vr,"*",sep=""), paste("\\ref{tab:lb", vr, "}", sep = "")), ifelse(md == TRUE, paste("*",vr,"*",sep=""), paste("\\emph{", vr, "}", sep = "")) # of paste("\\ref{tab:lb", vr, "}", sep = "")
#                         )
# 
#     num = is.numeric(x[, var])
#     fc = is.factor(x[,var])
#     ch = is.character(x[,var])
# 
#     # geen groep geval
#     if (groep == 1){
#       # numeriese geen groep geval
#       if (num){
#         p1 <- qplot(data = melt(x[,var]), geom="boxplot", xlab = var, x = var, y = value, main = wrapper(paste("Values of ", lb), width = wd)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=11,face="bold"))
#         p2 <- qplot(data = data.frame(idx = 1:nrow(x), vr = x[,var]), x = vr  , fill = 1, geom="density",
#                     main = wrapper(paste("Density of ", lb), width = wd), alpha = I(0.2)) + theme(legend.position="none") + theme(axis.text=element_text(size=9), axis.title=element_text(size=11,face="bold"))
#         p3 <- suppressWarnings(qplot(data = data.frame(idx = 1:nrow(x), vr = x[,var]), x = vr , fill = 1, geom="histogram",
#                     xlab = var,  main = wrapper(paste("Histogram of ", lb), width = wd), alpha = I(0.9)) + theme(legend.position="none") + theme(axis.text=element_text(size=9), axis.title=element_text(size=11,face="bold")))
#         if (druk == TRUE) pdf(file = paste(drr,var,".pdf",sep=""), width = 12)
#         pp = function(main = paste("Values and distribution of ", var)){
#           grid.arrange(p1, p3, p2, nrow=2, ncol = 2, 
#                        layout_matrix=rbind(c(1,2), c(3,3)), 
#                        top = main)
#           }
#         if (druk == TRUE) dev.off()
#         
#         if (var %in% names(x)) {
#           vrs <- list(x[, var])
#           tab <- function() {tableContinuous2(vars = vrs, cap = paste(lb, " "), lab = paste("lb",vr,  sep = ""), 
#                                               longtable = FALSE, caption.placement="top", html = webify, comment = FALSE, 
#                                               nams = gsub("_", " ", var), print.results = TRUE, table.placement = "!h")}
#         }
#       }
# 
#       # kategoriese geen groep geval
#       if (fc|ch){
#         p <- ggplot(data.frame(vr = x[,var]), aes(x=vr, fill = 1), alpha = I(1/2)) + labs(x = lb, y = "count") +
#           geom_bar(position = "dodge") + 
#           theme(legend.position="none") +
#           ggtitle(wrapper(paste("Frequency distrubution of", var), width =2*wd )) +
#           theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=11,face="bold"))
#         
#         pp = function() grid.arrange(p, nrow = 1)
#         if (druk == TRUE) ggsave(file = paste(drr,var,".pdf",sep=""), width = 12)
#         
#         if (var %in% names(x)) {
#           vrs <- list(x[, var])
#           tab <- function() {tableNominal2(vars = vrs, cap = paste(lb,"", sep=""), 
#           lab = paste("lb", vr,  sep = ""), 
#           levSizeN = levSizeN, cumsum = FALSE, longtable = FALSE, caption.placement="top", html = webify, varSizeN = varSizeN, 
#           comment = FALSE, nams = gsub("_", " ", var), print.results = FALSE)}
#         }
#         
#         if (stats == "all") {
#           # bepaal die indeks van die veranderlike wat 'voorspel' moet word
#           idx_temp <- match(var, table = names(x), -1)
#           if (idx_temp > 0) {
#             # bepaal die indekse van die voorspellende veranderlikes, indien enige
#             vidxx <- match(voorspellende_veranderlikes, names(x))
#             vidxx <- vidxx[!is.na(vidxx)]
#             
#             if (length(vidxx) > 0) {
#               chi2 <- chi.2.eksp(data = x, 
#                                  out.idx = idx_temp, 
#                                  min.length = 0, max.levels = 10, 
#                                  ver.idx = vidxx, 
#                                  p.cutoff = 100, 
#                                  outdir = drr, out.df = TRUE, 
#                                  verbose = FALSE, xlsx = FALSE, 
#                                  net.name = "chi2eksp")
#               
#               if (!is.null(chi2)) {
#                 for (c in 1:ncol(chi2)) {
#                   if (is.numeric(chi2[, c])) {
#                     chi2[, c] <- round(x = chi2[, c], digits = chi.digits)
#                   }
#                 }
#                 dropidxx <- match(c("warnings", "outcome"), table = names(chi2), nomatch = NA)
#                 dropidxx <- dropidxx[!is.na(dropidxx)]
#                 if (length(dropidxx) > 0) chi2 <- chi2[, -dropidxx]
#                 print(kable(chi2, format = "pandoc", digits = 5), comment = FALSE)
#               }
#             } 
#           }
#         }
#       }
#     }
# 
#     if (groep != 1){
#       # numeriese groep geval
#       if (num){
#         p1 <- qplot(data = melt(data.frame(var = x[,var], group =gp)), geom="boxplot",
#                     xlab = groeplab, x = group, y = value, main = wrapper(paste("Values of ", lb, " by ", groeplab), width = wd)) + theme(legend.position="bottom") + theme(axis.text=element_text(size=9), axis.title=element_text(size=11,face="bold"))
#         p2 <- qplot(data = data.frame(var = x[,var], group =gp), x = var , geom="density",
#                     xlab = var, fill = group,  group = group, main = wrapper(paste("Density of ", lb, " by ", groeplab), width = wd), alpha = I(0.2)) + theme(legend.position="bottom") + theme(axis.text=element_text(size=9), axis.title=element_text(size=11,face="bold"))
#         if (druk == TRUE) pdf(file = paste(drr,var,".pdf",sep=""), width = 12)
#         pp = function() grid.arrange(p1, p2, nrow = 2, 
#                                      top = wrapper(sprintf("Distribution of the variable %s", var), width = 2*wd))
#         if (druk == TRUE) dev.off()
#         
#         if (var %in% names(x)) {
#           vrs <- list(x[, var])
#           tab <- function() {tableContinuous2(vars = vrs, group = gp, cap = paste(lb, "",sep=""), 
#                                              lab = paste("lb",vr,  sep = ""), 
#                                              longtable = FALSE, html = webify, caption.placement="top",comment = FALSE, 
#                                              nams = gsub("_", " ", var), print.results = FALSE)}
#         }
#       }
# 
#       # kategoriese groep geval
#       if (fc|ch){
#         p <- qplot(data = data.frame(var = x[,var], group =gp), x = var, geom = "bar", xlab = lb,
#                    facets = group ~ ., main = paste(lb, "by", groeplab), fill = 1) +
#           theme(legend.position="none") +
#           theme(axis.text.x=element_text(angle = 90, hjust = 0)) + 
#           theme(axis.text=element_text(size=9), axis.title=element_text(size=11,face="bold"))
#         pp = function() grid.arrange(p, nrow=1)
#         if (druk == TRUE) ggsave(p, file = paste(drr,var,".pdf",sep=""), width = 12)
#         
#         if (var %in% names(x)) {
#           vrs <- list(x[, var])
#           tab <- function() {tableNominal2(vars = vrs, group = gp, cap = paste(lb, "", sep=""), cumsum = FALSE, 
#                                            lab = paste("lb", vr,  sep = ""), html = webify, longtable = FALSE, 
#                                            caption.placement="top", varSizeN = varSizeN, levSizeN = levSizeN, comment = FALSE, 
#                                            nams = gsub("_", " ", var), print.results = FALSE)}
#         }
#         
#         if (stats == "all") {
#           # bepaal die indeks van die veranderlike wat voorspel moet word
#           idx_temp <- match(var, table = names(x), -1)
#           
#           if (idx_temp > 0) {
#             # bepaal die indekse van die voorspellende veranderlikes, indien enige
#             vidxx <- match(voorspellende_veranderlikes, names(x))
#             vidxx <- vidxx[!is.na(vidxx)]
#             if (debug == TRUE) assign("voorspellende_veranderlikes", voorspellende_veranderlikes, envir = .GlobalEnv)
#             if (debug == TRUE) assign("x", x, envir = .GlobalEnv)
#             if (debug == TRUE) assign("idx_temp", idx_temp, envir = .GlobalEnv)
#             
#             if (length(vidxx) > 0) {
#               chi2 <- chi.2.eksp(data = x, 
#                                  out.idx = idx_temp, 
#                                  min.length = 0, 
#                                  max.levels = 10, 
#                                  ver.idx = vidxx, 
#                                  p.cutoff = 0.05, 
#                                  outdir = drr, 
#                                  out.df = TRUE, 
#                                  verbose = FALSE, 
#                                  xlsx = FALSE, 
#                                  net.name = "chi2eksp")
#               
#               if (!is.null(chi2)) {
#                 for (c in 1:ncol(chi2)) {
#                   if (is.numeric(chi2[, c])) {
#                     chi2[, c] <- round(x = chi2[, c], digits = 5)
#                   }
#                 }
#                 dropidxx <- match(c("warnings", "outcome"), table = names(chi2), nomatch = NA)
#                 dropidxx <- dropidxx[!is.na(dropidxx)]
#                 if (length(dropidxx) > 0) chi2 <- chi2[, -dropidxx]
#                 print(kable(x = chi2, format = "pandoc", digits = chi.digits), comment = FALSE)
#               }
#             } 
#           }
#         }
#       }
#     }
#     # gepaste tabel en grafiek
#     # model seleksie vir gekose veranderlikes
#   }
# 
#   if (verbose == TRUE) message(var)
#   
#   if (out == TRUE){
#     do.call("cat", list(sprintf("%s", ifelse(md == TRUE, paste("\n## ", lb,"\n", sep=""), paste("\\subsection{",lb,"}", sep=""))),"\n",
#                         ifelse(exists("vraagsin"), vraagsin, ""),"\n",
#                         ifelse(exists("verwyssin"), verwyssin, ""), "\n",
#                         if (exists("tab", mode = "function")) tab() else "", "\n"
#                         )
#             )
#     if (exists("tab", mode = "function")) tab()
#     if (exists("pp", mode = "function")) pp() 
#     
#   }
#    
# }
# 
# # maak funksie wat binne 'n seksie kyk of iets 'n Multi-opsie is.
# # As dit is maak dan een multi-opsie tabel anders gaan deur en maak enkel tabelle met singe_report
# 
# 
# 
# 



