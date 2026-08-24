#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Compare two correlations, either from two independent groups or from two
# dependent (same-sample) correlations that overlap (share a variable) or not.
#
# The comparison statistics are Pearson-based and port the closed-form tests
# from the cocor package (Diedenhofen & Musch, 2015, PLoS ONE 10(4): e0121945):
#   - Independent groups:        Fisher's (1925) z  + Zou's (2007) CI
#   - Dependent, overlapping:    Steiger's (1980) z + Zou's (2007) CI
#   - Dependent, non-overlapping: Steiger's (1980) z + Zou's (2007) CI

#' @import jaspBase
#' @importFrom stats cor complete.cases pnorm qnorm
#' @export
twoCorrelations <- function(jaspResults, dataset, options, ...) {
  ready <- .twoCorrelationsReady(options)

  if (ready)
    .twoCorrelationsCheckErrors(dataset, options)

  .twoCorrelationsTable(jaspResults, dataset, options, ready)
  .twoCorrelationsScatterPlots(jaspResults, dataset, options, ready)

  return()
}

# The variable/layout options that decide which correlations are compared.
.twoCorrelationsVariableDeps <- c("samples", "dependentType",
                                  "independentVariable1", "independentVariable2", "groupingVariable",
                                  "commonVariable", "overlapVariable1", "overlapVariable2",
                                  "nonoverlapVariable1", "nonoverlapVariable2",
                                  "nonoverlapVariable3", "nonoverlapVariable4")

.twoCorrelationsReady <- function(options) {
  if (options[["samples"]] == "independent")
    return(all(c(options[["independentVariable1"]], options[["independentVariable2"]],
                 options[["groupingVariable"]]) != ""))

  if (options[["dependentType"]] == "overlapping")
    return(all(c(options[["commonVariable"]], options[["overlapVariable1"]],
                 options[["overlapVariable2"]]) != ""))

  return(all(c(options[["nonoverlapVariable1"]], options[["nonoverlapVariable2"]],
               options[["nonoverlapVariable3"]], options[["nonoverlapVariable4"]]) != ""))
}

.twoCorrelationsVariables <- function(options) {
  if (options[["samples"]] == "independent")
    return(c(options[["independentVariable1"]], options[["independentVariable2"]]))

  if (options[["dependentType"]] == "overlapping")
    return(c(options[["commonVariable"]], options[["overlapVariable1"]], options[["overlapVariable2"]]))

  return(c(options[["nonoverlapVariable1"]], options[["nonoverlapVariable2"]],
           options[["nonoverlapVariable3"]], options[["nonoverlapVariable4"]]))
}

.twoCorrelationsCheckErrors <- function(dataset, options) {
  .hasErrors(dataset, type = c("infinity", "variance", "observations"),
             all.target = .twoCorrelationsVariables(options), observations.amount = "< 4",
             exitAnalysisIfErrors = TRUE)

  if (options[["samples"]] == "independent") {
    .hasErrors(dataset, type = "factorLevels",
               factorLevels.target = options[["groupingVariable"]], factorLevels.amount = "!= 2",
               exitAnalysisIfErrors = TRUE)
  }
}

.twoCorrelationsTable <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["outputTable"]]))
    return()

  outputTable <- createJaspTable(title = gettext("Comparison of Two Correlations"))
  outputTable$dependOn(c(.twoCorrelationsVariableDeps, "alternative", "ci", "ciLevel"))
  outputTable$position <- 1
  jaspResults[["outputTable"]] <- outputTable

  outputTable$addColumnInfo(name = "term", title = "",             type = "string")
  outputTable$addColumnInfo(name = "n",    title = gettext("n"),   type = "integer")
  outputTable$addColumnInfo(name = "est",  title = gettext("r"),   type = "number")
  outputTable$addColumnInfo(name = "z",    title = gettext("z"),   type = "number")
  outputTable$addColumnInfo(name = "p",    title = gettext("p"),   type = "pvalue")

  if (options[["ci"]]) {
    overtitle <- gettextf("%s%% Confidence Interval", format(100 * options[["ciLevel"]]))
    outputTable$addColumnInfo(name = "lowerCi", title = gettext("Lower"), type = "number", overtitle = overtitle)
    outputTable$addColumnInfo(name = "upperCi", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  outputTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  .twoCorrelationsFillTable(outputTable, dataset, options)

  return()
}

.twoCorrelationsFillTable <- function(outputTable, dataset, options) {
  res <- switch(options[["samples"]],
                "independent" = .twoCorrelationsIndependent(dataset, options),
                "dependent"   = .twoCorrelationsDependent(dataset, options))

  if (!is.null(res$error)) {
    outputTable$setError(res$error)
    return()
  }

  outputTable$setData(res$rows)
  for (note in res$footnotes)
    outputTable$addFootnote(note)

  return()
}

# ---- Scatter plots --------------------------------------------------------

# One plot per compared correlation, except for independent groups: there both correlations
# involve the same pair of variables, so a single plot coloured by group shows the comparison
# on one pair of axes.
.twoCorrelationsScatterPlotSpecs <- function(options) {
  if (options[["samples"]] == "independent")
    return(list(list(x     = options[["independentVariable1"]],
                     y     = options[["independentVariable2"]],
                     group = options[["groupingVariable"]])))

  if (options[["dependentType"]] == "overlapping")
    return(list(list(x = options[["commonVariable"]], y = options[["overlapVariable1"]], group = NULL),
                list(x = options[["commonVariable"]], y = options[["overlapVariable2"]], group = NULL)))

  return(list(list(x = options[["nonoverlapVariable1"]], y = options[["nonoverlapVariable2"]], group = NULL),
              list(x = options[["nonoverlapVariable3"]], y = options[["nonoverlapVariable4"]], group = NULL)))
}

.twoCorrelationsScatterPlots <- function(jaspResults, dataset, options, ready) {
  if (!options[["scatterPlot"]] || !is.null(jaspResults[["scatterPlots"]]))
    return()

  scatterContainer <- createJaspContainer(title = gettext("Scatter Plots"))
  scatterContainer$dependOn(c(.twoCorrelationsVariableDeps, .correlationScatterPlotDeps))
  scatterContainer$position <- 2
  jaspResults[["scatterPlots"]] <- scatterContainer

  if (!ready)
    return()

  specs <- .twoCorrelationsScatterPlotSpecs(options)

  for (i in seq_along(specs)) {
    spec     <- specs[[i]]
    tempPlot <- createJaspPlot(title  = gettextf("%1$s and %2$s",
                                                 jaspBase::decodeColNames(spec$x),
                                                 jaspBase::decodeColNames(spec$y)),
                               width = 500, height = 500)
    tempPlot$position <- i
    scatterContainer[[paste0("scatterPlot", i)]] <- tempPlot

    plotObject <- .correlationScatterPlotObject(dataset, options, spec$x, spec$y, spec$group)
    if (isTryError(plotObject)) {
      tempPlot$setError(.extractErrorMessage(plotObject))
      next
    }

    tempPlot$plotObject <- plotObject
  }

  return()
}

# ---- Independent groups: Fisher (1925) z + Zou (2007) CI ------------------

.twoCorrelationsIndependent <- function(dataset, options) {
  v1     <- options[["independentVariable1"]]
  v2     <- options[["independentVariable2"]]
  factor <- as.factor(dataset[[options[["groupingVariable"]]]])
  levels <- levels(factor)

  groups <- lapply(levels, function(lvl) {
    idx      <- which(factor == lvl)
    complete <- complete.cases(dataset[[v1]][idx], dataset[[v2]][idx])
    x        <- dataset[[v1]][idx][complete]
    y        <- dataset[[v2]][idx][complete]
    list(n = length(x), r = cor(x, y))
  })

  if (any(vapply(groups, function(g) g$n < 4, logical(1))))
    return(list(error = gettext("Each group needs at least 4 complete observations.")))

  r1 <- groups[[1]]$r; n1 <- groups[[1]]$n
  r2 <- groups[[2]]$r; n2 <- groups[[2]]$n

  test <- .twoCorFisherIndep(r1, n1, r2, n2, options[["alternative"]])
  ci   <- .twoCorZouIndep(r1, n1, r2, n2, options[["ciLevel"]])

  corLabel <- gettextf("Correlation (%1$s, %2$s)",
                       jaspBase::decodeColNames(v1), jaspBase::decodeColNames(v2))
  rows <- list(
    .twoCorRow(gettextf("%1$s: Group %2$s", corLabel, levels[1]), n1, r1,
               .twoCorFisherCi(r1, n1, options), options),
    .twoCorRow(gettextf("%1$s: Group %2$s", corLabel, levels[2]), n2, r2,
               .twoCorFisherCi(r2, n2, options), options),
    .twoCorDiffRow(r1 - r2, test$z, test$p, ci, options)
  )

  footnotes <- gettext("Fisher's (1925) z-test and Zou's (2007) confidence interval for the difference between two independent correlations.")

  return(list(rows = do.call(rbind, rows), footnotes = footnotes, error = NULL))
}

.twoCorFisherIndep <- function(r1, n1, r2, n2, alternative) {
  z <- (atanh(r1) - atanh(r2)) / sqrt(1 / (n1 - 3) + 1 / (n2 - 3))
  list(z = z, p = .twoCorPvalue(z, alternative))
}

.twoCorZouIndep <- function(r1, n1, r2, n2, confLevel) {
  zAlpha <- qnorm((1 - confLevel) / 2, lower.tail = FALSE)
  l1 <- tanh(atanh(r1) - zAlpha * sqrt(1 / (n1 - 3)))
  u1 <- tanh(atanh(r1) + zAlpha * sqrt(1 / (n1 - 3)))
  l2 <- tanh(atanh(r2) - zAlpha * sqrt(1 / (n2 - 3)))
  u2 <- tanh(atanh(r2) + zAlpha * sqrt(1 / (n2 - 3)))

  L <- r1 - r2 - sqrt((r1 - l1)^2 + (u2 - r2)^2)
  U <- r1 - r2 + sqrt((u1 - r1)^2 + (r2 - l2)^2)
  c(L, U)
}

# ---- Dependent groups: Steiger (1980) z + Zou (2007) CI -------------------

.twoCorrelationsDependent <- function(dataset, options) {
  vars     <- .twoCorrelationsVariables(options)
  complete <- complete.cases(dataset[vars])
  sub      <- dataset[complete, vars, drop = FALSE]
  n        <- nrow(sub)

  if (n < 4)
    return(list(error = gettext("At least 4 complete observations are required.")))

  corMat <- cor(sub)

  if (options[["dependentType"]] == "overlapping") {
    # j = common, k = overlapVariable1, h = overlapVariable2
    r.jk <- corMat[1, 2]
    r.jh <- corMat[1, 3]
    r.kh <- corMat[2, 3]

    test <- .twoCorSteigerOverlap(r.jk, r.jh, r.kh, n, options[["alternative"]])
    ci   <- .twoCorZouOverlap(r.jk, r.jh, r.kh, n, options[["ciLevel"]])

    label1 <- gettextf("Correlation (%1$s, %2$s)",
                       jaspBase::decodeColNames(vars[1]), jaspBase::decodeColNames(vars[2]))
    label2 <- gettextf("Correlation (%1$s, %2$s)",
                       jaspBase::decodeColNames(vars[1]), jaspBase::decodeColNames(vars[3]))
    r1 <- r.jk; r2 <- r.jh
    footnotes <- gettext("Steiger's (1980) z-test and Zou's (2007) confidence interval for the difference between two overlapping correlations.")
  } else {
    # j = var1, k = var2, h = var3, m = var4; compare r.jk with r.hm
    r.jk <- corMat[1, 2]; r.hm <- corMat[3, 4]
    r.jh <- corMat[1, 3]; r.jm <- corMat[1, 4]
    r.kh <- corMat[2, 3]; r.km <- corMat[2, 4]

    test <- .twoCorSteigerNonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n, options[["alternative"]])
    ci   <- .twoCorZouNonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n, options[["ciLevel"]])

    label1 <- gettextf("Correlation (%1$s, %2$s)",
                       jaspBase::decodeColNames(vars[1]), jaspBase::decodeColNames(vars[2]))
    label2 <- gettextf("Correlation (%1$s, %2$s)",
                       jaspBase::decodeColNames(vars[3]), jaspBase::decodeColNames(vars[4]))
    r1 <- r.jk; r2 <- r.hm
    footnotes <- gettext("Steiger's (1980) z-test and Zou's (2007) confidence interval for the difference between two non-overlapping correlations.")
  }

  rows <- list(
    .twoCorRow(label1, n, r1, .twoCorFisherCi(r1, n, options), options),
    .twoCorRow(label2, n, r2, .twoCorFisherCi(r2, n, options), options),
    .twoCorDiffRow(r1 - r2, test$z, test$p, ci, options)
  )

  return(list(rows = do.call(rbind, rows), footnotes = footnotes, error = NULL))
}

.twoCorSteigerOverlap <- function(r.jk, r.jh, r.kh, n, alternative) {
  r.p <- (r.jk + r.jh) / 2
  covariance <- (r.kh * (1 - 2 * r.p^2) - 0.5 * r.p^2 * (1 - 2 * r.p^2 - r.kh^2)) / (1 - r.p^2)^2
  z <- sqrt(n - 3) * (atanh(r.jk) - atanh(r.jh)) / sqrt(2 - 2 * covariance)
  list(z = z, p = .twoCorPvalue(z, alternative))
}

.twoCorZouOverlap <- function(r.jk, r.jh, r.kh, n, confLevel) {
  x <- qnorm((1 - confLevel) / 2, lower.tail = FALSE) * sqrt(1 / (n - 3))
  cc <- ((r.kh - 0.5 * r.jk * r.jh) * (1 - r.jk^2 - r.jh^2 - r.kh^2) + r.kh^3) / ((1 - r.jk^2) * (1 - r.jh^2))
  l1 <- tanh(atanh(r.jk) - x); u1 <- tanh(atanh(r.jk) + x)
  l2 <- tanh(atanh(r.jh) - x); u2 <- tanh(atanh(r.jh) + x)
  L <- r.jk - r.jh - sqrt((r.jk - l1)^2 + (u2 - r.jh)^2 - 2 * cc * (r.jk - l1) * (u2 - r.jh))
  U <- r.jk - r.jh + sqrt((u1 - r.jk)^2 + (r.jh - l2)^2 - 2 * cc * (u1 - r.jk) * (r.jh - l2))
  c(L, U)
}

.twoCorSteigerNonoverlap <- function(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n, alternative) {
  r.p <- (r.jk + r.hm) / 2
  covariance.enum <- (r.jh - r.p * r.kh) * (r.km - r.kh * r.p) + (r.jm - r.jh * r.p) * (r.kh - r.p * r.jh) +
    (r.jh - r.jm * r.p) * (r.km - r.p * r.jm) + (r.jm - r.p * r.km) * (r.kh - r.km * r.p)
  covariance <- covariance.enum / (2 * (1 - r.p^2)^2)
  z <- sqrt(n - 3) * (atanh(r.jk) - atanh(r.hm)) / sqrt(2 - 2 * covariance)
  list(z = z, p = .twoCorPvalue(z, alternative))
}

.twoCorZouNonoverlap <- function(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n, confLevel) {
  cc <- (0.5 * r.jk * r.hm * (r.jh^2 + r.jm^2 + r.kh^2 + r.km^2) + r.jh * r.km + r.jm * r.kh -
           (r.jk * r.jh * r.jm + r.jk * r.kh * r.km + r.jh * r.kh * r.hm + r.jm * r.km * r.hm)) /
    ((1 - r.jk^2) * (1 - r.hm^2))
  x <- qnorm((1 - confLevel) / 2, lower.tail = FALSE) * sqrt(1 / (n - 3))
  l1 <- tanh(atanh(r.jk) - x); u1 <- tanh(atanh(r.jk) + x)
  l2 <- tanh(atanh(r.hm) - x); u2 <- tanh(atanh(r.hm) + x)
  L <- r.jk - r.hm - sqrt((r.jk - l1)^2 + (u2 - r.hm)^2 - 2 * cc * (r.jk - l1) * (u2 - r.hm))
  U <- r.jk - r.hm + sqrt((u1 - r.jk)^2 + (r.hm - l2)^2 - 2 * cc * (u1 - r.jk) * (r.hm - l2))
  c(L, U)
}

# ---- shared helpers -------------------------------------------------------

.twoCorPvalue <- function(z, alternative) {
  switch(alternative,
         "two.sided" = 2 * pnorm(-abs(z)),
         "greater"   = pnorm(z, lower.tail = FALSE),
         "less"      = pnorm(z))
}

.twoCorFisherCi <- function(r, n, options) {
  if (!options[["ci"]])
    return(c(NA, NA))
  .corrFisherCi(r, n, options[["alternative"]], options[["ciLevel"]])
}

.twoCorRow <- function(term, n, r, ci, options) {
  row <- data.frame(term = term, n = n, est = r, z = NA, p = NA)
  if (options[["ci"]]) {
    row$lowerCi <- ci[1]
    row$upperCi <- ci[2]
  }
  return(row)
}

.twoCorDiffRow <- function(diff, z, p, ci, options) {
  row <- data.frame(term = gettext("Difference"), n = NA, est = diff, z = z, p = p)
  if (options[["ci"]]) {
    row$lowerCi <- ci[1]
    row$upperCi <- ci[2]
  }
  return(row)
}
