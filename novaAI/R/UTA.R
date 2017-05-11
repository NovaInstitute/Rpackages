#' Project summary UTA
#'
#' Function to calculate UTA
#'
#' @param df Data frame containing survey data
#' @param Coal The variable associated with whether a household uses coal
#' @param coalvar The value of the coal use variable of importance
#' @param var The variable associated with whether a household uses coal
#' @param pop Data frame containing population data
#' @param group The grouping variable
#' @param othervar Any other variable to be considered
#' @param altvar The alternative variable that could also be present
#' @param coal If FALSE the function omits coal users from the table
#' @param BM If If FALSE the function omits BM users from the table
#' @param other If FALSE the function omits other users from the table
#' @export

project.summary.UTA = function(df, Coal = "hh.coal.use.r", coalvar = "YES", var = "hh.coal.use.r",
                               pop = pop, group = haq$town, othervar = "introduction.to.bm.r",
                               altvar = "hh.BM.use", coal = TRUE, BM = TRUE, other = TRUE) {

    group = as.character(group)
    group = as.factor(group)
    n = ldply(by(df, group, function(x) length(x[, 1])))[[2]]
    prop.Coal = ldply(by(df, group, function(x) {
        length(x[which(x[, Coal] == coalvar), 1])/length(x[, 1])
    }))[[2]]
    prop.other = ldply(by(df, group, function(x) {
        length(x[which(x[, altvar] == "BM old" | x[, othervar] == "Radio" | x[, othervar] == "Billboard" | x[, othervar] == "Newspaper/magazine" |
            x[, othervar] == "Other" | x[, othervar] == "other"), 1])/length(x[which(x[, altvar] == "BM" | x[, altvar] == "BM other" | x[, altvar] == "BM old"), 1])
    }))[[2]]
    prop.alt = ldply(by(df, group, function(x) {
        length(x[which(x[, altvar] == "BM" | x[, altvar] == "BM other" | x[, altvar] == "BM old"), 1])/length(x[which(x[, Coal] == coalvar), 1])
    }))[[2]]  #/prop.Coal
    idx = match(levels(group), pop[, "Town"])
    popl = pop[idx, 2]
    point.Coal = as.numeric(prop.Coal) * as.integer(popl)
    point.BM = as.numeric(prop.alt) * as.numeric(prop.Coal) * as.integer(popl)
    point.BM.other = as.numeric(prop.other) * point.BM
    tab = cbind(n, as.integer(popl), prop.Coal, prop.alt, prop.other, round(point.Coal), round(point.BM), round(point.BM.other), round(point.BM -
        point.BM.other))
    rownames(tab) = paste(levels(group))
    colnames(tab) = c("n", "Pop.", "XUcoal", "XUalt", "XUother", "Coal users", "BM users", "BM users other", "BM users Nova")
    sums = colSums(tab, na.rm = TRUE)
    final = rbind(tab, sums)
    rownames(final)[length(final[, 1] - 1)] = "TOTAL, MEAN"
    final["TOTAL, MEAN", "XUcoal"] = weighted.mean(final[1:(length(final[, 1]) - 1), "XUcoal"], final[1:(length(final[, 1]) - 1), "Pop."])
    final["TOTAL, MEAN", "XUalt"] = weighted.mean(final[1:(length(final[, 1]) - 1), "XUalt"], final[1:(length(final[, 1]) - 1), "Coal users"])
    final["TOTAL, MEAN", "XUother"] = weighted.mean(final[1:(length(final[, 1]) - 1), "XUother"], final[1:(length(final[, 1]) - 1), "BM users"])
    if (BM == FALSE)
        final[, -na.omit(match(c("XUalt", "BM\\ users", "BM\\ users other", "BM\\ users Nova"), colnames(tab)))] else final
    if (other == FALSE)
        final = final[, -na.omit(match(c("XUother", "BM\\ users\\ other"), colnames(tab)))] else final
    if (coal == FALSE)
        final = final[, -na.omit(match(c("XUcoal", "Coal\\ users"), colnames(tab)))] else final
    final[is.na(final)] = 0
    final
}

#' Project summary UTA Raw
#'
#' Function to calculate UTA but leaves output unrounded
#'
#' @param df Data frame containing survey data
#' @param Coal The variable associated with whether a household uses coal
#' @param coalvar The value of the coal use variable of importance
#' @param var The variable associated with whether a household uses coal
#' @param pop Data frame containing population data
#' @param group The grouping variable
#' @param othervar Any other variable to be considered
#' @param altvar The alternative variable that could also be present
#' @param altname The value under consideration within the alternate variable
#' @param coal If FALSE the function omits coal users from the table
#' @param BM If If FALSE the function omits BM users from the table
#' @param other If FALSE the function omits other users from the table
#' @export

project.summary.UTA.raw = function(df,
                                   Coal = "hh.coal.use.r",
                                   coalvar = "YES",
                                   var = "hh.coal.use.r",
                                   pop = pop,
                                   group = haq$town,
                                   othervar = "introduction.to.bm.r",
                                   altvar = "hh.BM.use",
                                   altname = "BM",
                                   coal = TRUE,
                                   BM = TRUE,
                                   other = TRUE) {

    group = as.character(group)
    group = as.factor(group)
    n = ldply(by(df, group, function(x) length(x[, 1])))[[2]]
    prop.Coal = ldply(by(df, group, function(x) {
        length(x[which(x[, Coal] == coalvar), 1])/length(x[, 1])
    }))[[2]]
    prop.other = ldply(by(df, group, function(x) {
        length(x[which(x[, altvar] == "BM old" | x[, othervar] == "Radio" | x[, othervar] == "Billboard" | x[, othervar] == "Newspaper/magazine" |
            x[, othervar] == "Other" | x[, othervar] == "other"), 1]) / length(x[which(x[, altvar] == "BM" | x[, altvar] == "BM other" | x[, altvar] == "BM old"), 1])
    }))[[2]]
    prop.alt = ldply(by(df, group, function(x) {
        length(x[which(x[, altvar] == "BM" | x[, altvar] == "BM other" | x[, altvar] == "BM old"), 1])/length(x[which(x[, Coal] == coalvar),
            1])
    }))[[2]]  #/prop.Coal
    idx = match(levels(group), pop[, "Town"])
    popl = pop[idx, 2]
    point.Coal = as.numeric(prop.Coal) * as.integer(popl)
    point.BM = as.numeric(prop.alt) * as.numeric(prop.Coal) * as.integer(popl)
    point.BM.other = as.numeric(prop.other) * point.BM
    tab = cbind(n, as.integer(popl), prop.Coal, prop.alt, prop.other, point.Coal, point.BM, point.BM.other, point.BM - point.BM.other)
    rownames(tab) = paste(levels(group))
    colnames(tab) = c("n", "Pop.", "XUcoal", "XUalt", "XUother", "Coal users", "BM users", "BM users other", "BM users Nova")
    sums = colSums(tab, na.rm = TRUE)
    final = rbind(tab, sums)
    rownames(final)[length(final[, 1] - 1)] = "TOTAL, MEAN"
    final["TOTAL, MEAN", "XUcoal"] = weighted.mean(final[1:(length(final[, 1]) - 1), "XUcoal"], final[1:(length(final[, 1]) - 1), "Pop."])
    final["TOTAL, MEAN", "XUalt"] = weighted.mean(final[1:(length(final[, 1]) - 1), "XUalt"], final[1:(length(final[, 1]) - 1), "Coal users"])
    final["TOTAL, MEAN", "XUother"] = weighted.mean(final[1:(length(final[, 1]) - 1), "XUother"], final[1:(length(final[, 1]) - 1), "BM users"])
    if (BM == FALSE)
        final[, -na.omit(match(c("XUalt", "BM\\ users", "BM\\ users other", "BM\\ users Nova"), colnames(tab)))] else final
    if (other == FALSE)
        final = final[, -na.omit(match(c("XUother", "BM\\ users\\ other"), colnames(tab)))] else final
    if (coal == FALSE)
        final = final[, -na.omit(match(c("XUcoal", "Coal\\ users"), colnames(tab)))] else final
    final[is.na(final)] = 0
    final
}

#' CI Ualt
#'
#' Creates 90 percent confidence interval for Ualt
#'
#' @param df Data frame containing survey data
#' @param Coal The variable associated with whether a household uses coal
#' @param coalvar The value of the coal use variable of importance
#' @param var The variable associated with whether a household uses coal
#' @param pop Data frame containing population data
#' @param group The grouping variable
#' @param othervar Any other variable to be considered
#' @param altvar The alternative variable that could also be present
#' @export

cl.Ualt = function(df, Coal = "hh.coal.use.r", coalvar = "YES",
                   var = "hh.BM.use", pop = pop, group = haq$town, othervar = "introduction.to.bm.r",
                   altvar = "hh.BM.use") {

    base = data.frame(project.summary.UTA.raw(df = df, Coal = Coal, coalvar = as.character(coalvar), var = var, pop = pop, group = group, altvar = altvar,
                                              othervar = othervar))
    base = base[-match("TOTAL, MEAN", rownames(base)), c("n", "Pop.", "XUcoal", "XUalt", "XUother")]
    n = base[, "n"]
    prop.alt = (base[, "XUalt"] -(base[, "XUalt"] * base[, "XUother"])) * base[, "XUcoal"]
    popl = base[, "Pop."]
    point.alt.prop = strat90(popl, n, prop.alt)[2]
    low.alt.prop = strat90(popl, n, prop.alt)[4]
    high.alt.prop = strat90(popl, n, prop.alt)[5]
    point.alt = as.numeric(point.alt.prop) * as.integer(sum(popl))
    low.alt = as.numeric(low.alt.prop) * as.integer(sum(popl))
    high.alt = as.numeric(high.alt.prop) * as.integer(sum(popl))
    tab = cbind(low.alt.prop, low.alt, point.alt.prop, point.alt, high.alt.prop, high.alt)
    colnames(tab) = c("low prop.", "low estimate", "point prop.", "point estimate", "high prop.", "high estimate")
    rownames(tab) = NULL
    tab
}

#' CI UTA
#'
#' Creates 90 percent confidence interval for UTA
#'
#' @param df Data frame containing survey data
#' @param Coal The variable associated with whether a household uses coal
#' @param var The variable associated with whether a household uses coal
#' @param coalvar The value of the coal use variable of importance
#' @param pop Data frame containing population data
#' @param group The grouping variable
#' @param othervar Any other variable to be considered
#' @export

cl.UTA = function(df, Coal = "hh.coal.use.r",
                  var = "hh.coal.use.r",
                  coalvar="Yes",
                  pop = pop,
                  group = haq$town,
                  othervar = "introduction.to.bm") {

    base = data.frame(project.summary.UTA.raw(df = df, Coal = Coal, var = var, pop = pop, group = group, othervar=othervar, coalvar = coalvar))
    base = base[-match("TOTAL, MEAN", rownames(base)), c("n", "Pop.", "XUcoal", "XUalt", "XUother")]
    n = base[, "n"]
    prop.Coal = base[, "XUcoal"]
    popl = base[, "Pop."]
    point.Coal.prop = strat90(popl, n, prop.Coal)[2]
    low.Coal.prop = strat90(popl, n, prop.Coal)[4]
    high.Coal.prop = strat90(popl, n, prop.Coal)[5]
    point.Coal = as.numeric(point.Coal.prop) * as.integer(sum(popl))
    low.Coal = as.numeric(low.Coal.prop) * as.integer(sum(popl))
    high.Coal = as.numeric(high.Coal.prop) * as.integer(sum(popl))
    tab = cbind(low.Coal.prop, low.Coal, point.Coal.prop, point.Coal, high.Coal.prop, high.Coal)
    colnames(tab) = c("low prop.", "low estimate", "point prop.", "point estimate", "high prop.", "high estimate")
    rownames(tab) = NULL
    tab
}
