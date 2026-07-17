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
#' @importFrom stats cor cor.test complete.cases pnorm qnorm
#' @export
oneCorrelation <- function(jaspResults, dataset, options, ...) {
  ready <- (options[["firstVariable"]] != "" && options[["secondVariable"]] != "" &&
              any(c(options[["pearson"]], options[["spearman"]], options[["kendall"]])))

  if (ready) {
    vars <- c(options[["firstVariable"]], options[["secondVariable"]])
    .hasErrors(dataset, type = c("infinity", "variance", "observations"),
               all.target = vars, observations.amount = "< 3",
               exitAnalysisIfErrors = TRUE)
  }

  .oneCorrelationTable(jaspResults, dataset, options, ready)

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
                         "testValue", "alternative", "ci", "ciLevel", "effectSize", "vovkSellke"))
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
                      "two.sided" = gettextf("The alternative hypothesis is that the population correlation differs from %s.", format(testValue)),
                      "greater"   = gettextf("The alternative hypothesis is that the population correlation is greater than %s.", format(testValue)),
                      "less"      = gettextf("The alternative hypothesis is that the population correlation is less than %s.", format(testValue)))
  outputTable$addFootnote(hypLabel)

  if (options[["ci"]] && (options[["spearman"]] || options[["kendall"]]))
    outputTable$addFootnote(gettext("Confidence intervals are only available for Pearson's r."))

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

  rows <- lapply(.oneCorrelationMethods(options), function(method)
    .oneCorrelationComputeRow(x, y, n, method, options, outputTable))

  outputTable$setData(do.call(rbind, rows))

  return()
}

.oneCorrelationComputeRow <- function(x, y, n, method, options, outputTable) {
  label     <- .oneCorrelationMethodLabel(method)
  testValue <- options[["testValue"]]
  alt       <- options[["alternative"]]

  corTest <- try(cor.test(x, y, method = method, alternative = alt), silent = TRUE)
  if (isTryError(corTest)) {
    outputTable$addFootnote(gettextf("%1$s could not be computed: %2$s", label, .oneCorrelationCleanError(corTest)),
                            symbol = gettext("<b>Warning:</b>"))
    return(data.frame(test = label, n = n, r = NA, p = NA))
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

  row <- data.frame(test = label, n = n, r = r, p = p)

  if (options[["ci"]]) {
    if (method == "pearson") {
      ci <- .corrFisherCi(r, n, alt, options[["ciLevel"]])
      row$lowerCi <- ci[1]
      row$upperCi <- ci[2]
    } else {
      row$lowerCi <- NA
      row$upperCi <- NA
    }
  }

  if (options[["effectSize"]]) {
    row$effectSize   <- atanh(r)
    row$seEffectSize <- .oneCorrelationEffectSizeSE(r, n, method)
  }

  if (options[["vovkSellke"]])
    row$vsmpr <- .oneCorrelationVsmpr(p)

  return(row)
}

# Fisher's z normal-approximation confidence interval for a Pearson correlation.
# Ported from jaspRegression's .corrNormalApproxConfidenceIntervals so the module
# does not need jaspRegression as a hard dependency.
# TODO: Test this against jaspRegression's implementation to make sure they are equivalent.
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
  vsmpr <- jaspBase:::VovkSellkeMPR(p)
  if (identical(vsmpr, "∞"))
    return(Inf)
  return(as.numeric(vsmpr))
}

.oneCorrelationCleanError <- function(x) {
  gsub("[\r\n]+", " ", as.character(x))
}
