#' Project Summary
#'
#' Function to summarise household BM use, coal use and Emission results
#'
#' @param df Data frame containing BM data
#' @param BM The BM use variable as character vector
#' @param var The variable corresponding to seasonal savings
#' @param coef The coefficient used to calculate variable CO2 emissions
#' @param pop Data frame containing population data
#' @param group The grouping variable
#' @export

project.summary = function(df, BM = "hh.BM.use", var = "total.winter.save",
                           coef = 2.2099, pop = pop, group = haq$town) {
    group = as.character(group)
    group = as.factor(group)
    n = ldply(by(df, group, function(x) length(x[, 1])))[[2]]
    prop.BM = ldply(by(df, group, function(x) {
        length(x[which(x[, BM] == "BM" | x[, BM] == "Historic BM"), 1])/length(x[, 1])
    }))[[2]]
    idx = match(levels(as.factor(group)), pop[, "Town"])
    popl = pop[idx, 2]
    point.BM = as.numeric(prop.BM) * as.integer(popl)
    mean = ldply(tapply(df[, var], group, mean, na.rm = TRUE))[[2]]
    stddev = ldply(tapply(df[, var], group, sd, na.rm = TRUE))[[2]]
    total.var = mean * point.BM
    total.var.co2 = total.var * coef
    tab = cbind(n, as.integer(popl), round(point.BM), round(mean, 2), round(stddev, 2), round(total.var/1000,
        2), round(total.var.co2/1000, 2))
    rownames(tab) = paste(levels(group))
    colnames(tab) = c("n", "Population", "BM users", "Mean (kg)", "Stddev", "Total coal(t)", "CO2 (t)")
    sums.a = colSums(tab, na.rm = TRUE)[1:3]
    means.a = colMeans(tab, na.rm = TRUE)[4:5]
    sums.b = colSums(tab, na.rm = TRUE)[6:7]
    tot.tab = matrix(c(round(sums.a, 0), round(means.a, 2), round(sums.b, 2)), ncol = 7)
    final = rbind(tab, tot.tab)
    rownames(final)[length(final[, 1] - 1)] = "TOTAL, MEAN"
    final
}

#' Project Summary 09
#'
#' Function to summarise household BM use, coal use and Emission results but
#' does not include hostoric BM use
#'
#' @param df Data frame containing BM data
#' @param BM The BM use variable as character vector
#' @param var The variable corresponding to seasonal savings
#' @param coef The coefficient used to calculate variable CO2 emissions
#' @param pop Data frame containing population data
#' @param group The grouping variable
#' @export

project.summary.09 = function(df, BM = "hh.BM.use", var = "total.winter.save",
                              coef = 2.2099, pop = pop, group = haq$town) {
    group = as.character(group)
    group = as.factor(group)
    n = ldply(by(df, group, function(x) length(x[, 1])))[[2]]
    prop.BM = ldply(by(df, group, function(x) {
        length(x[which(x[, BM] == "BM"), 1])/length(x[, 1])
    }))[[2]]
    idx = match(levels(group), pop[, "Town"])
    popl = pop[idx, 2]
    point.BM = as.numeric(prop.BM) * as.integer(popl)
    mean = ldply(by(df[, var], group, mean, na.rm = TRUE))[[2]]
    stddev = ldply(by(df[, var], group, sd, na.rm = TRUE))[[2]]
    total.var = mean * point.BM
    total.var.co2 = total.var * coef
    tab = cbind(n, as.integer(popl), round(point.BM), round(mean, 2), round(stddev, 2), round(total.var/1000,
        2), round(total.var.co2/1000, 2))
    rownames(tab) = paste(levels(group))
    colnames(tab) = c("n", "Population", "BM users", "Mean (kg)", "Stddev", "Total coal(t)", "CO2 (t)")
    sums.a = colSums(tab, na.rm = TRUE)[1:3]
    means.a = colMeans(tab, na.rm = TRUE)[4:5]
    sums.b = colSums(tab, na.rm = TRUE)[6:7]
    tot.tab = matrix(c(round(sums.a, 0), round(means.a, 2), round(sums.b, 2)), ncol = 7)
    final = rbind(tab, tot.tab)
    rownames(final)[length(final[, 1] - 1)] = "TOTAL, MEAN"
    final
}

#' Attendance Summary
#'
#' Function to summarise attendance
#'
#' @param df Data frame containing BM data
#' @param var The variable corresponding to BM demo attendance
#' @param value The value of concern for the supplied var
#' @param pop Data frame containing population
#' @param group The grouping variable
#' @export

att.summary = function(df, var = "bm.demo.attendance.r",
                       value = "YES", pop = pop, group = haq$town) {
    group = as.character(group)
    group = as.factor(group)
    n = ldply(by(df, group, function(x) length(x[, 1])))[[2]]
    prop.est = ldply(by(df, group, function(x) {
        length(x[which(x[, var] == value), 1])/length(x[, 1])
    }))[[2]]
    idx = match(levels(group), pop[, "Town"])
    popl = pop[idx, 2]
    point.est = prop.est * as.integer(popl)
    tab = cbind(n, as.integer(popl), point.est)
    rownames(tab) = paste(levels(group))
    colnames(tab) = c("n", "Population", "Estimation")
    tab
}

#' Project Summary 09 Projection
#'
#' Function to summarise BM use, coal use and Emission results
#' including projected discounts and percentages CO2
#'
#' @param df Data frame containing BM data
#' @param BM The BM use variable as character vector
#' @param var The variable corresponding to seasonal savings
#' @param coef The coefficient used to calculate variable CO2 emissions
#' @param pop Data frame containing population data
#' @param group The grouping variable
#' @param datevar The variable corresponding to the dates under consideration
#' @export

project.summary.09.proj = function(df, BM = "hh.BM.use", var = "total.winter.save",
                                   coef = 2.2099, pop = pop,
                                   group = haq$town, datevar = "received") {
    group = as.character(group)
    group = as.factor(group)
    maxdate = as.Date(ldply(by(as.Date(df[, datevar]), group, function(x) as.Date(max(x))))[[2]], origin = "1970-01-01")
    diffday = as.integer(as.Date("2010-05-01") - ifelse(maxdate < as.Date("2009-09-01"), as.Date("2009-09-01"),
        maxdate))
    proj.prop = diffday/242
    maxdate = format(as.POSIXlt(maxdate), "%Y-%m-%d")
    n = ldply(by(df, group, function(x) length(x[, 1])))[[2]]
    prop.BM = ldply(by(df, group, function(x) {
        length(x[which(x[, BM] == "BM"), 1])/length(x[, 1])
    }))[[2]]
    idx = match(levels(group), pop[, "Town"])
    popl = pop[idx, 2]
    point.BM = as.numeric(prop.BM) * as.integer(popl)
    mean = ldply(by(df[, var], group, mean, na.rm = TRUE))[[2]]
    stddev = ldply(by(df[, var], group, sd, na.rm = TRUE))[[2]]
    total.var = mean * point.BM
    total.var.co2 = total.var * coef
    proj.co2 = round((total.var.co2 * proj.prop * 0.75)/1000 + (total.var.co2 * (1 - proj.prop))/1000,
        2)
    tab = cbind(n, as.integer(popl), round(point.BM), round(mean, 2), round(stddev, 2), round(total.var/1000,
        2), round(total.var.co2/1000, 2), round(proj.prop * 100, 2), proj.co2)
    rownames(tab) = paste(levels(group))
    colnames(tab) = c("n", "Population", "BM users", "Mean (kg)", "Stddev", "Total coal(t)", "CO2 (t)",
        "Projected %", "Projected discounted CO2")
    sums.a = colSums(tab, na.rm = TRUE)[1:3]
    means.a = colMeans(tab, na.rm = TRUE)[4:5]
    sums.b = colSums(tab, na.rm = TRUE)[6:7]
    means.b = colMeans(tab, na.rm = TRUE)[8]
    sums.c = colSums(tab, na.rm = TRUE)[9]
    tot.tab = matrix(c(round(sums.a, 0), round(means.a, 2), round(sums.b, 2), round(means.b, 2), round(sums.c)),
        ncol = 9)
    final = cbind(matrix(c(maxdate, NA), ncol = 1), rbind(tab, tot.tab))
    rownames(final)[length(final[, 1])] = "TOTAL, MEAN"
    final
}

#' Project Summary CI
#'
#' Creates project summary of BM use, coal use and Emission results
#' with Confidence Interval included
#'
#' @param df Data frame containing BM data
#' @param BM The BM use variable as character vector
#' @param var The variable corresponding to seasonal savings
#' @param coef The coefficient used to calculate variable CO2 emissions
#' @param pop Data frame containing population data
#' @param group The grouping variable
#' @export

project.summary.cl = function(df, BM = "hh.BM.use",
                              var = "total.winter.save", coef = 2.2099, pop = pop,
                              group = haq$town) {
    group = as.character(group)
    group = as.factor(group)
    n = ldply(by(df, group, function(x) length(x[, 1])))[[2]]
    bm = ldply(by(df, group, function(x) {
        length(x[which(x[, BM] == "BM"), 1])
    }))[[2]]
    cl = binconf(bm, n, alpha = 0.05, method = "asymptotic", include.x = FALSE, include.n = FALSE)
    idx = match(levels(group), pop[, "Town"])
    popl = pop[idx, 2]
    point.BM = round(cl * as.integer(popl))[, 1]
    low.BM = round(cl * as.integer(popl))[, 2]
    high.BM = round(cl * as.integer(popl))[, 3]
    mean = ldply(tapply(df[, var], group, mean, na.rm = TRUE))[[2]]
    stddev = ldply(tapply(df[, var], group, sd, na.rm = TRUE))[[2]]
    total.var = mean * low.BM
    total.var.co2 = total.var * coef
    tab = cbind(n, as.integer(popl), round(point.BM), low.BM, high.BM, round(mean, 2), round(stddev, 2),
        round(total.var/1000, 2), round(total.var.co2/1000, 2))
    rownames(tab) = paste(levels(group))
    colnames(tab) = c("n", "Population", "BM users (point)", "BM users (low)", "BM users (high)", "Mean (kg)",
        "Stddev", "Total coal(t)", "CO2 (t)")
    sums.a = colSums(tab, na.rm = TRUE)[1:5]
    means.a = colMeans(tab, na.rm = TRUE)[6:7]
    sums.b = colSums(tab, na.rm = TRUE)[8:9]
    tot.tab = matrix(c(round(sums.a, 0), round(means.a, 2), round(sums.b, 2)), ncol = length(tab[1, ]))
    final = rbind(tab, tot.tab)
    rownames(final)[length(final[, 1] - 1)] = "TOTAL, MEAN"
    final
}

#' Coal Summary CI
#'
#' Creates coal use summary with Confidence Interval included
#'
#' @param df Data frame containing BM data
#' @param BM The BM use variable as character vector
#' @param var The variable corresponding to seasonal savings
#' @param con The value that corresponds to active BM use
#' @param coef The coefficient used to calculate variable CO2 emissions
#' @param pop Data frame containing population data
#' @param group The grouping variable
#' @export

coal.summary.cl = function(df, BM = "hh.BM.use",
                           var = "total.winter.save", con = "BM", coef = 2.2099,
                           pop = pop, group = haq$town) {
    group = as.character(group)
    group = as.factor(group)
    n = ldply(by(df, group, function(x) length(x[, 1])))[[2]]
    bm = ldply(by(df, group, function(x) {
        length(x[which(x[, BM] == con), 1])
    }))[[2]]
    cl = binconf(bm, n, alpha = 0.05, method = "asymptotic", include.x = FALSE, include.n = FALSE)
    idx = match(levels(group), pop[, "Town"])
    popl = pop[idx, 2]
    point.BM = round(cl * as.integer(popl))[, 1]
    low.BM = round(cl * as.integer(popl))[, 2]
    high.BM = round(cl * as.integer(popl))[, 3]
    mean = ldply(tapply(df[, var], group, mean, na.rm = TRUE))[[2]]
    stddev = ldply(tapply(df[, var], group, sd, na.rm = TRUE))[[2]]
    total.var = mean * point.BM
    total.var.co2 = total.var * coef
    tab = cbind(n, as.integer(popl), round(point.BM), low.BM, high.BM, round(mean, 2), round(stddev, 2),
        round(total.var/1000, 2), round(total.var.co2/1000, 2))
    rownames(tab) = paste(levels(group))
    colnames(tab) = c("n", "Population", "Users (point)", "Users (low)", "Users (high)", "Mean (kg)", "Stddev",
        "Total coal(t)", "CO2 (t)")
    sums.a = colSums(tab, na.rm = TRUE)[1:5]
    means.a = colMeans(tab, na.rm = TRUE)[6:7]
    sums.b = colSums(tab, na.rm = TRUE)[8:9]
    tot.tab = matrix(c(round(sums.a, 0), round(means.a, 2), round(sums.b, 2)), ncol = length(tab[1, ]))
    final = rbind(tab, tot.tab)
    rownames(final)[length(final[, 1] - 1)] = "TOTAL, MEAN"
    final
}
