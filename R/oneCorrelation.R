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

# Test a single correlation against a hypothesized value.
# Reuses jaspRegression for the Fisher-z confidence interval and the
# Vovk-Sellke maximum p-ratio (jaspBase).

#' @import jaspBase
#' @importFrom stats cor cor.test complete.cases pnorm qnorm quantile
#' @export
oneCorrelation <- function(jaspResults, dataset, options, ...) {
  # makes the bootstrap confidence intervals reproducible when the user sets a seed
  jaspBase::.setSeedJASP(options)

  # the scatter plot only needs the two variables, the table also needs a coefficient
  plotReady  <- options[["firstVariable"]] != "" && options[["secondVariable"]] != ""
  tableReady <- plotReady && any(c(options[["pearson"]], options[["spearman"]], options[["kendall"]]))

  if (plotReady) {
    vars <- c(options[["firstVariable"]], options[["secondVariable"]])
    .hasErrors(dataset, type = c("infinity", "variance", "observations"),
               all.target = vars, observations.amount = "< 3",
               exitAnalysisIfErrors = TRUE)
  }

  .oneCorrelationTable(jaspResults, dataset, options, tableReady)
  .oneCorrelationScatterPlot(jaspResults, dataset, options, plotReady)

  return()
}

.oneCorrelationMethods <- function(options) {
  methods <- character(0)
  if (options[["pearson"]])  methods <- c(methods, "pearson")
  if (options[["spearman"]]) methods <- c(methods, "spearman")
  if (options[["kendall"]])  methods <- c(methods, "kendall")
  return(methods)
}

.oneCorrelationMethodLabel <- function(method) {
  switch(method,
         pearson  = gettext("Pearson's r"),
         spearman = gettext("Spearman's rho"),
         kendall  = gettext("Kendall's tau B"))
}

.oneCorrelationTable <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["outputTable"]]))
    return()

  outputTable <- createJaspTable(title = gettext("One Correlation Test"))
  outputTable$dependOn(c("firstVariable", "secondVariable", "pearson", "spearman", "kendall",
                         "testValue", "alternative", "ci", "ciLevel", "ciBootstrap", "ciBootstrapSamples",
                         "setSeed", "seed", "effectSize", "vovkSellke"))
  outputTable$position <- 1
  jaspResults[["outputTable"]] <- outputTable

  outputTable$addColumnInfo(name = "test", title = gettext("Test"),      type = "string")
  outputTable$addColumnInfo(name = "n",    title = gettext("n"),         type = "integer")
  outputTable$addColumnInfo(name = "r",    title = gettext("Estimate"),  type = "number")
  outputTable$addColumnInfo(name = "p",    title = gettext("p"),         type = "pvalue")

  if (options[["ci"]]) {
    overtitle <- gettextf("%s%% Confidence Interval", format(100 * options[["ciLevel"]]))
    outputTable$addColumnInfo(name = "lowerCi", title = gettext("Lower"), type = "number", overtitle = overtitle)
    outputTable$addColumnInfo(name = "upperCi", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  if (options[["effectSize"]]) {
    outputTable$addColumnInfo(name = "effectSize",   title = gettext("Fisher's z"),   type = "number")
    outputTable$addColumnInfo(name = "seEffectSize", title = gettext("SE"),           type = "number")
  }

  if (options[["vovkSellke"]])
    outputTable$addColumnInfo(name = "vsmpr", title = gettext("VS-MPR"), type = "number")

  outputTable$showSpecifiedColumnsOnly <- TRUE

  .oneCorrelationFootnotes(outputTable, options)

  if (!ready)
    return()

  .oneCorrelationFillTable(outputTable, dataset, options)

  return()
}

.oneCorrelationFootnotes <- function(outputTable, options) {
  testValue <- options[["testValue"]]
  hypLabel  <- switch(options[["alternative"]],
                      "two.sided" = gettextf("The alternative hypothesis is that the population correlation is not equal to %s.", format(testValue)),
                      "greater"   = gettextf("The alternative hypothesis is that the population correlation is greater than %s.", format(testValue)),
                      "less"      = gettextf("The alternative hypothesis is that the population correlation is less than %s.", format(testValue)))
  outputTable$addFootnote(hypLabel)

  if (options[["ci"]]) {
    if (options[["ciBootstrap"]])
      outputTable$addFootnote(gettextf("Confidence intervals are percentile bootstrap intervals based on %s replicates.",
                                       format(options[["ciBootstrapSamples"]], scientific = FALSE)))
    else if (options[["spearman"]] || options[["kendall"]])
      outputTable$addFootnote(gettext("Confidence intervals are only available for Pearson's r."))
  }

  if (options[["vovkSellke"]])
    outputTable$addFootnote(gettext("Vovk-Sellke maximum p-ratio: Based on a two-sided p-value, the maximum possible odds in favor of H₁ over H₀ equals 1/(-e p log(p)) for p ≤ .37 (Sellke, Bayarri, & Berger, 2001)."),
                            symbol = "*")
}

.oneCorrelationFillTable <- function(outputTable, dataset, options) {
  x <- dataset[[options[["firstVariable"]]]]
  y <- dataset[[options[["secondVariable"]]]]

  complete <- complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]
  n <- length(x)

  methods <- .oneCorrelationMethods(options)

  # one set of resamples shared by every coefficient, so the intervals describe the same bootstrap
  bootstrapCis <- if (options[["ci"]] && options[["ciBootstrap"]])
    .oneCorrelationBootstrapCis(x, y, methods, options)

  rows <- lapply(methods, function(method)
    .oneCorrelationComputeRow(x, y, n, method, options, outputTable, bootstrapCis[[method]]))

  outputTable$setData(do.call(rbind, rows))

  return()
}

.oneCorrelationComputeRow <- function(x, y, n, method, options, outputTable, bootstrapCi = NULL) {
  label     <- .oneCorrelationMethodLabel(method)
  testValue <- options[["testValue"]]
  alt       <- options[["alternative"]]

  corTest <- try(cor.test(x, y, method = method, alternative = alt), silent = TRUE)
  if (isTryError(corTest)) {
    outputTable$addFootnote(gettextf("%1$s could not be computed: %2$s", label, .oneCorrelationCleanError(corTest)),
                            symbol = gettext("<b>Warning:</b>"))
    # NA-filled row with the same columns as a successful row, so rbind() does
    # not fail when one method errors while another succeeds.
    return(.oneCorrelationRow(label, n, r = NA, p = NA, ci = c(NA, NA),
                              effectSize = NA, seEffectSize = NA, vsmpr = NA, options))
  }

  r <- unname(corTest$estimate)

  # p-value: use cor.test when testing against 0, otherwise Fisher-z against the test value
  if (testValue == 0) {
    p <- corTest$p.value
  } else {
    z <- (atanh(r) - atanh(testValue)) * sqrt(n - 3)
    p <- switch(alt,
                "two.sided" = 2 * pnorm(-abs(z)),
                "greater"   = pnorm(z, lower.tail = FALSE),
                "less"      = pnorm(z))
  }

  # The analytic (Fisher-z) interval is only defined for Pearson's r; the bootstrap covers
  # every coefficient, so other methods report NA bounds unless it is switched on.
  ci <- if (!is.null(bootstrapCi))
    bootstrapCi
  else if (method == "pearson")
    .corrFisherCi(r, n, alt, options[["ciLevel"]])
  else
    c(NA, NA)

  .oneCorrelationRow(label, n, r, p, ci,
                     effectSize   = atanh(r),
                     seEffectSize = .oneCorrelationEffectSizeSE(r, n, method),
                     vsmpr        = .oneCorrelationVsmpr(p), options)
}

# Assemble a table row, adding the optional CI / effect-size / VS-MPR columns
# only when requested. Used by both the success and error paths so every row
# carries identical columns.
.oneCorrelationRow <- function(label, n, r, p, ci, effectSize, seEffectSize, vsmpr, options) {
  row <- data.frame(test = label, n = n, r = r, p = p)

  if (options[["ci"]]) {
    row$lowerCi <- ci[1]
    row$upperCi <- ci[2]
  }

  if (options[["effectSize"]]) {
    row$effectSize   <- effectSize
    row$seEffectSize <- seEffectSize
  }

  if (options[["vovkSellke"]])
    row$vsmpr <- vsmpr

  return(row)
}

# ---- scatter plot (shared with twoCorrelations) ---------------------------

# Options that change the scatter plot itself, as opposed to the variables it displays.
.correlationScatterPlotDeps <- c("scatterPlot", "scatterPlotDensity", "scatterPlotRegressionLine",
                                 "scatterPlotRegressionLineCi", "scatterPlotRegressionLineCiLevel")

.oneCorrelationScatterPlot <- function(jaspResults, dataset, options, ready) {
  if (!options[["scatterPlot"]] || !is.null(jaspResults[["scatterPlot"]]))
    return()

  scatterPlot <- createJaspPlot(title = gettext("Scatter Plot"), width = 500, height = 500)
  scatterPlot$position <- 2
  # deliberately not depending on the coefficients: the plot does not change with them
  scatterPlot$dependOn(c("firstVariable", "secondVariable", .correlationScatterPlotDeps))
  jaspResults[["scatterPlot"]] <- scatterPlot

  if (!ready)
    return()

  plotObject <- .correlationScatterPlotObject(dataset, options,
                                              options[["firstVariable"]],
                                              options[["secondVariable"]])
  if (isTryError(plotObject)) {
    scatterPlot$setError(.extractErrorMessage(plotObject))
    return()
  }

  scatterPlot$plotObject <- plotObject

  return()
}

# Scatter plot used by oneCorrelation and twoCorrelations. Column names are passed through
# unchanged; Desktop decodes them in the rendered plot (same as .boxplotMV).
# forceLinearSmooth because these analyses test a linear (or rank) association: a loess line
# would contradict the reported coefficient.
.correlationScatterPlotObject <- function(dataset, options, xVar, yVar, groupVar = NULL) {
  vars     <- c(xVar, yVar, groupVar)
  plotDat  <- na.omit(dataset[, vars, drop = FALSE])
  group    <- if (!is.null(groupVar)) as.factor(plotDat[[groupVar]]) else NULL
  marginal <- if (options[["scatterPlotDensity"]]) "density" else "none"

  p <- try(jaspGraphs::JASPScatterPlot(
    x                 = plotDat[[xVar]],
    y                 = plotDat[[yVar]],
    group             = group,
    xName             = xVar,
    yName             = yVar,
    addSmooth         = options[["scatterPlotRegressionLine"]],
    addSmoothCI       = options[["scatterPlotRegressionLineCi"]],
    smoothCIValue     = options[["scatterPlotRegressionLineCiLevel"]],
    forceLinearSmooth = TRUE,
    plotAbove         = marginal,
    plotRight         = marginal,
    showLegend        = !is.null(group),
    legendTitle       = groupVar
  ))

  return(p)
}

# Fisher's z normal-approximation confidence interval for a Pearson correlation.
# Ported from jaspRegression's .corrNormalApproxConfidenceIntervals so the module
# does not need jaspRegression as a hard dependency. Verified numerically
# identical to that implementation (see test-oneCorrelation.R).
.corrFisherCi <- function(obsCor, n, hypothesis = "two.sided", confLevel = 0.95) {
  zCor  <- atanh(obsCor)
  se    <- 1 / sqrt(n - 3)
  alpha <- 1 - confLevel

  if (hypothesis == "two.sided") {
    z     <- qnorm(alpha / 2, lower.tail = FALSE)
    lower <- tanh(zCor - z * se)
    upper <- tanh(zCor + z * se)
  } else if (hypothesis == "less") {
    z     <- qnorm(alpha, lower.tail = FALSE)
    lower <- -1
    upper <- tanh(zCor + z * se)
  } else { # greater
    z     <- qnorm(alpha, lower.tail = FALSE)
    lower <- tanh(zCor - z * se)
    upper <- 1
  }

  return(c(lower, upper))
}

# Percentile bootstrap confidence intervals for every selected coefficient, following
# jaspRegression's .corrCalculateBootstrapCI: pairs are resampled with replacement and each
# coefficient is recomputed on the same resample, so all intervals share one bootstrap
# distribution of the data. Returns a named list of c(lower, upper), one entry per method.
.oneCorrelationBootstrapCis <- function(x, y, methods, options) {
  samples    <- options[["ciBootstrapSamples"]]
  n          <- length(x)
  estimates  <- matrix(NA_real_, nrow = samples, ncol = length(methods), dimnames = list(NULL, methods))

  startProgressbar(expectedTicks = samples, label = gettext("Bootstrapping"))
  for (i in seq_len(samples)) {
    idx <- sample.int(n, replace = TRUE)
    for (method in methods)
      # a resample can be constant in x or y, which makes cor() return NA with a warning
      estimates[i, method] <- suppressWarnings(tryCatch(cor(x[idx], y[idx], method = method),
                                                        error = function(e) NA_real_))
    progressbarTick()
  }

  cis <- lapply(methods, function(method)
    .oneCorrelationPercentileCi(estimates[, method], options[["alternative"]], options[["ciLevel"]]))
  names(cis) <- methods

  return(cis)
}

# Percentile interval from the bootstrap estimates. One-sided alternatives get a one-sided
# interval bounded by the range of the coefficient, matching .corrFisherCi.
.oneCorrelationPercentileCi <- function(estimates, hypothesis = "two.sided", confLevel = 0.95) {
  if (all(is.na(estimates)))
    return(c(NA_real_, NA_real_))

  alpha <- 1 - confLevel
  bound <- function(p) unname(stats::quantile(estimates, probs = p, na.rm = TRUE))

  if (hypothesis == "two.sided")
    return(c(bound(alpha / 2), bound(1 - alpha / 2)))

  if (hypothesis == "less")
    return(c(-1, bound(confLevel)))

  # greater
  return(c(bound(alpha), 1))
}

# Fisher-transformed effect-size standard errors (same formulas as jaspRegression's .corr.test)
.oneCorrelationEffectSizeSE <- function(r, n, method) {
  if (method == "pearson")
    return(sqrt(1 / (n - 3)))

  if (method == "spearman")
    return(sqrt((1 / (n - 2)) + (abs(atanh(r)) / ((6 * n) + (4 * n)^(1 / 2)))))

  # kendall
  s1 <- asin(sin((pi / 2) * r))
  s2 <- asin(sin((pi / 2) * r) / 2)
  return(sqrt((2 / (n * (n - 1))) * (1 - (4 * (s1^2 / pi^2)) + (2 * (n - 2) * ((1 / 9) - (4 * (s2^2 / pi^2)))))))
}

.oneCorrelationVsmpr <- function(p) {
  vsmpr <- VovkSellkeMPR(p)
  if (identical(vsmpr, "∞"))
    return(Inf)
  return(as.numeric(vsmpr))
}

.oneCorrelationCleanError <- function(x) {
  gsub("[\r\n]+", " ", as.character(x))
}
