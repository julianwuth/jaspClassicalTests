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

# Comparison of two independent proportions: difference (with CI) and test,
# plus optional relative risk and odds ratio effect sizes.
# Shared input reshaping, validation and descriptives live in proportionsCommon.R.

#' @import jaspBase
#' @importFrom stats prop.test qnorm
#' @export
twoProportions <- function(jaspResults, dataset, options, ...) {

  data  <- .mpAggregateData(dataset, options)
  ready <- !is.null(data) && nrow(data) == 2

  if (ready) {
    .mpCheckErrors(data, options)
  } else if (!is.null(data) && nrow(data) != 2) {
    .quitAnalysis(gettext("This analysis requires the factor to have exactly two levels."))
  }

  .tpMainTable(jaspResults, data, options, ready)
  .tpEffectSizeTable(jaspResults, data, options, ready)

  if (options[["descriptivesTable"]])
    .mpDescriptivesTable(jaspResults, data, options, ready)

  if (options[["descriptivesPlot"]])
    .mpDescriptivesPlot(jaspResults, data, options, ready)

  return()
}

# Test table: chi-square statistic, df and (directional) p-value.
.tpMainTable <- function(jaspResults, data, options, ready) {
  if (!is.null(jaspResults[["mainTable"]]))
    return()

  mainTable <- createJaspTable(title = gettext("Test of Two Proportions"))
  mainTable$dependOn(c("factor", "successes", "sampleSize", "alternative",
                       "continuityCorrection", "vovkSellke"))
  mainTable$position <- 1
  mainTable$showSpecifiedColumnsOnly <- TRUE

  mainTable$addColumnInfo(name = "chisq", title = "χ²",       type = "number")
  mainTable$addColumnInfo(name = "df",    title = gettext("df"), type = "integer")
  mainTable$addColumnInfo(name = "p",     title = gettext("p"),  type = "pvalue")

  if (options[["vovkSellke"]]) {
    mainTable$addColumnInfo(name = "vovkSellke", title = gettextf("VS-MPR%s", "*"), type = "number")
    mainTable$addFootnote(gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on a two-sided <em>p</em>-value, the maximum possible odds in favor of H%1$s over H%2$s equals 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37 (Sellke, Bayarri, & Berger, 2001).", "₁", "₀", "≤"), symbol = "*")
  }

  jaspResults[["mainTable"]] <- mainTable

  if (!ready)
    return()

  res <- .tpTest(data, options)
  if (isTryError(res)) {
    mainTable$setError(.extractErrorMessage(res))
    return()
  }

  row <- list(chisq = unname(res$statistic), df = unname(res$parameter), p = res$p.value)
  if (options[["vovkSellke"]])
    row$vovkSellke <- VovkSellkeMPR(res$p.value)

  mainTable$addRows(row)
  .tpAddGroupFootnote(mainTable, data)

  return()
}

# Effect sizes: difference in proportions (always), relative risk and odds ratio
# (optional). Estimates with two-sided confidence intervals.
.tpEffectSizeTable <- function(jaspResults, data, options, ready) {
  if (!is.null(jaspResults[["effectSizeTable"]]))
    return()

  esTable <- createJaspTable(title = gettext("Effect Sizes"))
  esTable$dependOn(c("factor", "successes", "sampleSize", "continuityCorrection",
                     "relativeRisk", "oddsRatio", "ci", "ciLevel"))
  esTable$position <- 2
  esTable$showSpecifiedColumnsOnly <- TRUE

  esTable$addColumnInfo(name = "measure",  title = "",                  type = "string")
  esTable$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")

  if (options[["ci"]]) {
    overtitle <- gettextf("%s%% Confidence Interval", 100 * options[["ciLevel"]])
    esTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    esTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  jaspResults[["effectSizeTable"]] <- esTable

  if (!ready)
    return()

  rows <- .tpEffectSizeData(data, options)
  if (isTryError(rows)) {
    esTable$setError(.extractErrorMessage(rows))
    return()
  }

  esTable$setData(rows)
  for (note in attr(rows, "footnotes"))
    esTable$addFootnote(note, symbol = gettext("<b>Warning:</b>"))
  .tpAddGroupFootnote(esTable, data)

  return()
}

# Directional test via prop.test; statistic/p reflect the chosen alternative.
.tpTest <- function(data, options) {
  try(prop.test(data$successes, data$sampleSize,
                alternative = options[["alternative"]],
                correct     = options[["continuityCorrection"]]),
      silent = TRUE)
}

.tpEffectSizeData <- function(data, options) {
  x1 <- data$successes[1]; n1 <- data$sampleSize[1]
  x2 <- data$successes[2]; n2 <- data$sampleSize[2]
  p1 <- x1 / n1;           p2 <- x2 / n2
  withCI <- options[["ci"]]
  z      <- if (withCI) qnorm(1 - (1 - options[["ciLevel"]]) / 2) else NA_real_

  # Difference from a two-sided prop.test so its CI is comparable to RR/OR.
  diffTest <- try(prop.test(data$successes, data$sampleSize,
                            alternative = "two.sided", conf.level = options[["ciLevel"]],
                            correct     = options[["continuityCorrection"]]),
                  silent = TRUE)
  if (isTryError(diffTest))
    return(diffTest)

  rows <- .tpRow(gettext("Difference (p₁ − p₂)"), p1 - p2,
                 if (withCI) diffTest$conf.int[1] else NA_real_,
                 if (withCI) diffTest$conf.int[2] else NA_real_, withCI)
  footnotes <- character(0)

  if (options[["relativeRisk"]]) {
    rr    <- p1 / p2
    # log-SE, and hence the CI, is undefined when either success count is 0.
    valid <- x1 > 0 && x2 > 0
    ci    <- .tpRatioCi(log(rr), sqrt((1 - p1) / x1 + (1 - p2) / x2), z, withCI && valid)
    if (withCI && !valid)
      footnotes <- c(footnotes, gettext("The relative-risk confidence interval is undefined when a success count is zero."))
    rows <- rbind(rows, .tpRow(gettext("Relative risk"), rr, ci[1], ci[2], withCI))
  }

  if (options[["oddsRatio"]]) {
    or    <- (x1 * (n2 - x2)) / (x2 * (n1 - x1))
    # log-SE, and hence the CI, is undefined when any of the four cells is 0.
    valid <- x1 > 0 && x2 > 0 && (n1 - x1) > 0 && (n2 - x2) > 0
    ci    <- .tpRatioCi(log(or), sqrt(1 / x1 + 1 / (n1 - x1) + 1 / x2 + 1 / (n2 - x2)), z, withCI && valid)
    if (withCI && !valid)
      footnotes <- c(footnotes, gettext("The odds-ratio confidence interval is undefined when a cell count is zero."))
    rows <- rbind(rows, .tpRow(gettext("Odds ratio"), or, ci[1], ci[2], withCI))
  }

  attr(rows, "footnotes") <- footnotes
  return(rows)
}

# Log-scale Wald CI for a ratio effect size, back-transformed to the ratio
# scale. Returns NA bounds when the interval is not defined (degenerate cell).
.tpRatioCi <- function(logEstimate, seLog, z, valid) {
  if (!valid || !is.finite(seLog))
    return(c(NA_real_, NA_real_))
  c(exp(logEstimate - z * seLog), exp(logEstimate + z * seLog))
}

.tpRow <- function(measure, estimate, lower, upper, withCI) {
  row <- data.frame(measure = measure, estimate = estimate, stringsAsFactors = FALSE)
  if (withCI) {
    row$lower <- lower
    row$upper <- upper
  }
  return(row)
}

# Note which factor level is treated as group 1 vs group 2.
.tpAddGroupFootnote <- function(table, data) {
  levels <- as.character(data$level)
  table$addFootnote(gettextf("Group 1 = %1$s, Group 2 = %2$s.", levels[1], levels[2]))
}
