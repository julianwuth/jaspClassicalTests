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

#' @import jaspBase
#' @importFrom stats prop.test binom.test
#' @export
multipleProportions <- function(jaspResults, dataset, options, ...) {

  data  <- .mpAggregateData(dataset, options)
  ready <- !is.null(data) && nrow(data) >= 2

  if (ready)
    .mpCheckErrors(data, options)

  .mpMainTable(jaspResults, data, options, ready)

  if (options[["descriptivesTable"]])
    .mpDescriptivesTable(jaspResults, data, options, ready)

  if (options[["descriptivesPlot"]])
    .mpDescriptivesPlot(jaspResults, data, options, ready)

  return()
}

# Reshape the input into a per-group data.frame(level, successes, sampleSize).
# Individual data: one row per unit with a binary success variable and no sample
#   size -> aggregate to counts via table().
# Aggregated data: one row per group with the number of successes and the sample
#   size supplied directly.
# Returns NULL when the input is not (yet) complete enough to compute.
.mpAggregateData <- function(dataset, options) {
  factorName <- options[["factor"]]
  succName   <- options[["successes"]]
  sizeName   <- options[["sampleSize"]]

  if (factorName == "" || succName == "")
    return(NULL)

  factorCol <- dataset[[factorName]]
  if (is.null(factorCol))
    return(NULL)
  factorCol <- as.factor(factorCol)

  isIndividual <- nlevels(factorCol) != length(factorCol)

  if (isIndividual) {
    if (sizeName != "")
      .quitAnalysis(gettext("No sample size should be provided when the individual successes for each factor level are specified."))

    succCol <- dataset[[succName]]
    if (length(unique(stats::na.omit(succCol))) != 2)
      .quitAnalysis(gettext("The successes variable must have exactly two levels when the sample size is not specified."))

    frequencies <- table(factorCol, succCol)
    data <- data.frame(
      level      = factor(rownames(frequencies), levels = levels(factorCol)),
      successes  = as.integer(frequencies[, 2]),
      sampleSize = as.integer(rowSums(frequencies))
    )
  } else {
    if (sizeName == "")
      return(NULL)

    data <- data.frame(
      level      = factor(as.character(factorCol), levels = levels(factorCol)),
      successes  = dataset[[succName]],
      sampleSize = dataset[[sizeName]]
    )
    data <- data[!is.na(data$level), , drop = FALSE]
  }

  return(data)
}

.mpCheckErrors <- function(data, options) {

  customChecks <- list(
    checkInput = function() {
      if (any(data$successes != round(data$successes)))
        return(gettext("Invalid successes: variable must contain only integer values."))
      if (any(data$sampleSize != round(data$sampleSize)))
        return(gettext("Invalid sample size: variable must contain only integer values."))
      if (any(data$successes > data$sampleSize))
        return(gettext("The sample size must be at least as large as the number of successes."))
      if (any(data$sampleSize < 1))
        return(gettext("Invalid sample size: each group must contain at least one observation."))
    }
  )

  .hasErrors(data,
             type                  = c("negativeValues", "infinity"),
             negativeValues.target = c("successes", "sampleSize"),
             infinity.target       = c("successes", "sampleSize"),
             custom                = customChecks,
             exitAnalysisIfErrors  = TRUE)
}

.mpMainTable <- function(jaspResults, data, options, ready) {
  if (!is.null(jaspResults[["mainTable"]]))
    return()

  if (options[["hypothesis"]] == "equal")
    title <- gettext("Test of Equal Proportions")
  else
    title <- gettextf("Test of Proportions Against %s", options[["testValue"]])

  mainTable <- createJaspTable(title = title)
  mainTable$dependOn(c("factor", "successes", "sampleSize", "hypothesis", "testValue",
                       "continuityCorrection", "vovkSellke"))
  mainTable$position <- 1
  mainTable$showSpecifiedColumnsOnly <- TRUE

  mainTable$addColumnInfo(name = "chisq", title = "χ²",    type = "number")
  mainTable$addColumnInfo(name = "df",    title = gettext("df"),     type = "integer")
  mainTable$addColumnInfo(name = "p",     title = gettext("p"),      type = "pvalue")

  if (options[["vovkSellke"]]) {
    mainTable$addColumnInfo(name = "vovkSellke", title = gettextf("VS-MPR%s", "*"), type = "number")
    mainTable$addFootnote(gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on a two-sided <em>p</em>-value, the maximum possible odds in favor of H%1$s over H%2$s equals 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37 (Sellke, Bayarri, & Berger, 2001).", "₁", "₀", "≤"), symbol = "*")
  }

  jaspResults[["mainTable"]] <- mainTable

  if (!ready)
    return()

  .mpFillMainTable(mainTable, data, options)

  return()
}

.mpFillMainTable <- function(mainTable, data, options) {
  correct <- options[["continuityCorrection"]]

  warn <- NULL
  res  <- try(withCallingHandlers(
    if (options[["hypothesis"]] == "equal")
      prop.test(data$successes, data$sampleSize, correct = correct)
    else
      prop.test(data$successes, data$sampleSize, p = rep(options[["testValue"]], nrow(data)), correct = correct),
    warning = function(w) {warn <<- conditionMessage(w); invokeRestart("muffleWarning")}
  ), silent = TRUE)

  if (isTryError(res)) {
    mainTable$setError(.extractErrorMessage(res))
    return()
  }

  row <- list(
    chisq = unname(res$statistic),
    df    = unname(res$parameter),
    p     = res$p.value
  )

  if (options[["vovkSellke"]])
    row$vovkSellke <- VovkSellkeMPR(res$p.value)

  mainTable$addRows(row)

  if (!is.null(warn))
    mainTable$addFootnote(warn, symbol = gettext("<b>Warning:</b>"))

  return()
}

.mpDescriptivesTable <- function(jaspResults, data, options, ready) {
  if (!is.null(jaspResults[["descriptivesTable"]]))
    return()

  descTable <- createJaspTable(title = gettext("Descriptives"))
  descTable$dependOn(c("factor", "successes", "sampleSize", "descriptivesTable",
                       "descriptivesDisplay", "descriptivesTableCi", "descriptivesTableCiLevel"))
  descTable$position <- 2
  descTable$showSpecifiedColumnsOnly <- TRUE

  factorTitle <- if (options[["factor"]] == "") gettext("Factor") else options[["factor"]]
  descTable$addColumnInfo(name = "level", title = factorTitle, type = "string")

  if (options[["descriptivesDisplay"]] == "counts")
    descTable$addColumnInfo(name = "observed", title = gettext("Observed"), type = "integer")
  else
    descTable$addColumnInfo(name = "observed", title = gettext("Observed"), type = "number")

  descTable$addColumnInfo(name = "size", title = gettext("Sample size"), type = "integer")

  if (options[["descriptivesTableCi"]]) {
    overtitle <- gettextf("%s%% Confidence Interval", 100 * options[["descriptivesTableCiLevel"]])
    descTable$addColumnInfo(name = "lowerCI", title = gettext("Lower"), type = "number", overtitle = overtitle)
    descTable$addColumnInfo(name = "upperCI", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  jaspResults[["descriptivesTable"]] <- descTable

  if (!ready)
    return()

  descTable$setData(.mpDescriptivesData(data, options,
                                        ciLevel = options[["descriptivesTableCiLevel"]],
                                        withCI  = options[["descriptivesTableCi"]]))

  if (options[["descriptivesTableCi"]])
    descTable$addFootnote(gettext("Confidence intervals are based on independent binomial distributions (Clopper-Pearson)."))

  return()
}

# Build the per-group descriptives (counts/proportions and optional CIs).
.mpDescriptivesData <- function(data, options, ciLevel, withCI) {
  asCounts <- options[["descriptivesDisplay"]] == "counts"

  observed <- if (asCounts) as.integer(data$successes) else data$successes / data$sampleSize

  out <- data.frame(
    level    = as.character(data$level),
    observed = observed,
    size     = as.integer(data$sampleSize),
    stringsAsFactors = FALSE
  )

  if (withCI) {
    ci <- .mpBinomCI(data$successes, data$sampleSize, ciLevel, asCounts)
    out$lowerCI <- ci$lower
    out$upperCI <- ci$upper
  }

  return(out)
}

# Per-group Clopper-Pearson CI, on the proportion scale or (scaled) count scale.
.mpBinomCI <- function(successes, sampleSize, ciLevel, asCounts) {
  lower <- upper <- numeric(length(successes))
  for (i in seq_along(successes)) {
    res <- try(binom.test(successes[i], sampleSize[i], conf.level = ciLevel)$conf.int, silent = TRUE)
    if (isTryError(res))
      res <- c(NA, NA)
    scale <- if (asCounts) sampleSize[i] else 1
    lower[i] <- res[1] * scale
    upper[i] <- res[2] * scale
  }
  return(data.frame(lower = lower, upper = upper))
}

.mpDescriptivesPlot <- function(jaspResults, data, options, ready) {
  if (!is.null(jaspResults[["descriptivesPlot"]]))
    return()

  descPlot <- createJaspPlot(title = gettext("Descriptives Plot"), width = 480, height = 320)
  descPlot$dependOn(c("factor", "successes", "sampleSize", "descriptivesPlot",
                      "descriptivesDisplay", "descriptivesPlotCiLevel"))
  descPlot$position <- 3
  jaspResults[["descriptivesPlot"]] <- descPlot

  if (!ready)
    return()

  plotData <- .mpDescriptivesData(data, options,
                                  ciLevel = options[["descriptivesPlotCiLevel"]],
                                  withCI  = TRUE)

  descPlot$plotObject <- .mpMakePlot(plotData, options)

  return()
}

.mpMakePlot <- function(plotData, options) {
  asCounts <- options[["descriptivesDisplay"]] == "counts"
  yName    <- if (asCounts) gettext("Observed counts") else gettext("Observed proportions")

  # reverse the levels because of the coord_flip below
  plotData$level <- factor(plotData$level, levels = rev(plotData$level))

  # y-axis margin: prefer the upper CI, fall back to the observed value
  plotData$yMax <- ifelse(is.na(plotData$upperCI), plotData$observed, plotData$upperCI)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotData$yMax))

  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = level, y = observed)) +
    ggplot2::geom_bar(stat = "identity", linewidth = 0.75, colour = "black", fill = "grey") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lowerCI, ymax = upperCI), linewidth = 0.75, width = 0.3) +
    ggplot2::xlab(if (options[["factor"]] == "") gettext("Factor") else options[["factor"]]) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks) +
    ggplot2::coord_flip() +
    jaspGraphs::geom_rangeframe(sides = "b") +
    jaspGraphs::themeJaspRaw(axis.title.cex = jaspGraphs::getGraphOption("axis.title.cex"))

  return(p)
}
