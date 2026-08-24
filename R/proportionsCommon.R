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

# Shared helpers for the proportions analyses (twoProportions, multipleProportions):
# input reshaping, dataset validation, and the descriptives table/plot.

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

  succCol <- dataset[[succName]]
  sizeCol <- if (sizeName != "") dataset[[sizeName]] else NULL

  # Trailing/blank spreadsheet rows arrive as NA in every assigned column. Drop rows with
  # missing values in any assigned column before deciding individual vs aggregated input,
  # otherwise the nlevels/length heuristic below misreads aggregated data as individual data.
  # droplevels() afterwards: a level that only occurred in dropped rows would otherwise
  # survive as an all-zero group.
  cols <- list(factorCol, succCol)
  if (!is.null(sizeCol))
    cols <- c(cols, list(sizeCol))

  keep     <- Reduce(`&`, lapply(cols, function(x) !is.na(x)))
  nRemoved <- sum(!keep)

  factorCol <- droplevels(factorCol[keep])
  succCol   <- succCol[keep]
  if (!is.null(sizeCol))
    sizeCol <- sizeCol[keep]

  isIndividual <- nlevels(factorCol) != length(factorCol)

  if (isIndividual) {
    if (sizeName != "")
      .quitAnalysis(gettext("No sample size should be provided when the individual successes for each factor level are specified."))

    if (length(unique(succCol)) != 2)
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
      successes  = succCol,
      sampleSize = sizeCol
    )
  }

  attr(data, "nRemoved") <- nRemoved

  return(data)
}

# Report rows excluded because one of the assigned columns was empty.
.mpAddMissingFootnote <- function(table, data) {
  nRemoved <- attr(data, "nRemoved")
  if (is.null(nRemoved) || nRemoved == 0)
    return()

  table$addFootnote(gettextf(ngettext(nRemoved,
                                      "%i row with missing values was removed.",
                                      "%i rows with missing values were removed."),
                             nRemoved))
}

# The descriptive confidence intervals are intervals for the proportion, so they are
# only meaningful when the descriptives are displayed on the proportion scale.
.mpShowsProportions <- function(options) {
  return(options[["descriptivesDisplay"]] == "proportions")
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

.mpDescriptivesTable <- function(jaspResults, data, options, ready) {
  if (!is.null(jaspResults[["descriptivesTable"]]))
    return()

  descTable <- createJaspTable(title = gettext("Descriptives"))
  descTable$dependOn(c("factor", "successes", "sampleSize", "descriptivesTable",
                       "descriptivesDisplay", "descriptivesTableCi", "descriptivesTableCiLevel"))
  # Descriptives come first in both proportions analyses, before the test output.
  descTable$position <- 1
  descTable$showSpecifiedColumnsOnly <- TRUE

  # A disabled QML control still ships its stored value to R, so gate the CI here too.
  withCi <- options[["descriptivesTableCi"]] && .mpShowsProportions(options)

  factorTitle <- if (options[["factor"]] == "") gettext("Factor") else options[["factor"]]
  descTable$addColumnInfo(name = "level", title = factorTitle, type = "string")

  if (options[["descriptivesDisplay"]] == "counts")
    descTable$addColumnInfo(name = "observed", title = gettext("Observed"), type = "integer")
  else
    descTable$addColumnInfo(name = "observed", title = gettext("Observed"), type = "number")

  descTable$addColumnInfo(name = "size", title = gettext("Sample size"), type = "integer")

  if (withCi) {
    overtitle <- gettextf("%s%% Confidence Interval", 100 * options[["descriptivesTableCiLevel"]])
    descTable$addColumnInfo(name = "lowerCI", title = gettext("Lower"), type = "number", overtitle = overtitle)
    descTable$addColumnInfo(name = "upperCI", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  jaspResults[["descriptivesTable"]] <- descTable

  if (!ready)
    return()

  descTable$setData(.mpDescriptivesData(data, options,
                                        ciLevel = options[["descriptivesTableCiLevel"]],
                                        withCI  = withCi))

  if (withCi)
    descTable$addFootnote(gettextf("Confidence intervals are for %s (the population proportion), based on independent binomial distributions (Clopper-Pearson).", "θ"))

  .mpAddMissingFootnote(descTable, data)

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
    ci <- .mpBinomCI(data$successes, data$sampleSize, ciLevel)
    out$lowerCI <- ci$lower
    out$upperCI <- ci$upper
  }

  return(out)
}

# Per-group Clopper-Pearson CI for the population proportion θ.
.mpBinomCI <- function(successes, sampleSize, ciLevel) {
  lower <- upper <- numeric(length(successes))
  for (i in seq_along(successes)) {
    res <- try(stats::binom.test(successes[i], sampleSize[i], conf.level = ciLevel)$conf.int, silent = TRUE)
    if (isTryError(res))
      res <- c(NA, NA)
    lower[i] <- res[1]
    upper[i] <- res[2]
  }
  return(data.frame(lower = lower, upper = upper))
}

.mpDescriptivesPlot <- function(jaspResults, data, options, ready) {
  if (!is.null(jaspResults[["descriptivesPlot"]]))
    return()

  descPlot <- createJaspPlot(title = gettext("Descriptives Plot"), width = 480, height = 320)
  descPlot$dependOn(c("factor", "successes", "sampleSize", "descriptivesPlot",
                      "descriptivesDisplay", "descriptivesPlotCi", "descriptivesPlotCiLevel"))
  descPlot$position <- 2
  jaspResults[["descriptivesPlot"]] <- descPlot

  if (!ready)
    return()

  plotData <- .mpDescriptivesData(data, options,
                                  ciLevel = options[["descriptivesPlotCiLevel"]],
                                  withCI  = options[["descriptivesPlotCi"]] && .mpShowsProportions(options))

  descPlot$plotObject <- .mpMakePlot(plotData, options)

  return()
}

.mpMakePlot <- function(plotData, options) {
  asCounts <- options[["descriptivesDisplay"]] == "counts"
  yName    <- if (asCounts) gettext("Observed counts") else gettext("Observed proportions")

  # reverse the levels because of the coord_flip below
  plotData$level <- factor(plotData$level, levels = rev(plotData$level))

  hasCi <- all(c("lowerCI", "upperCI") %in% names(plotData))

  # y-axis margin: prefer the upper CI, fall back to the observed value
  plotData$yMax <- if (hasCi) ifelse(is.na(plotData$upperCI), plotData$observed, plotData$upperCI)
                   else       plotData$observed
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotData$yMax))

  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = level, y = observed)) +
    ggplot2::geom_bar(stat = "identity", linewidth = 0.75, colour = "black", fill = "grey")

  if (hasCi)
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lowerCI, ymax = upperCI), linewidth = 0.75, width = 0.3)

  p <- p +
    ggplot2::xlab(if (options[["factor"]] == "") gettext("Factor") else options[["factor"]]) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks) +
    ggplot2::coord_flip() +
    jaspGraphs::geom_rangeframe(sides = "b") +
    jaspGraphs::themeJaspRaw(axis.title.cex = jaspGraphs::getGraphOption("axis.title.cex"))

  return(p)
}
