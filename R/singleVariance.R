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
#' @export
singleVariance <- function(jaspResults, dataset, options, ...) {
  # makes the bootstrap confidence interval reproducible when the user sets a seed
  jaspBase::.setSeedJASP(options)

  # is ready if test is selected and data was provided
  if (options[["inputType"]] == "rawData") {
    ready <- (ncol(dataset) > 0 && options[["chiSquareTest"]])
    if (ready)
      .hasErrors(dataset, type = c('infinity', 'variance'),
                 all.target = options[["dependent"]], variance.equalTo = 0,
                 exitAnalysisIfErrors = TRUE)
  } else {
    ready <- options[["chiSquareTest"]]
  }

  .pruneDisabledOutputSV(jaspResults, options)

  .createOutputTableSV(jaspResults, dataset, options, ready)

  # assumption checks require raw data
  if (options[["inputType"]] == "rawData")
    .assumptionChecksSV(jaspResults, dataset, options, ready)

  return()
}

# Output that is merely skipped by its builder (assumption checks after a switch to summarized
# input, or with both boxes unticked) is not covered by $dependOn(), so drop it explicitly.
.pruneDisabledOutputSV <- function(jaspResults, options) {
  wantAssumptionChecks <- options[["inputType"]] == "rawData" &&
                          (options[["normalityTest"]] || options[["qqPlot"]])

  if (!wantAssumptionChecks)
    .removeJaspElement(jaspResults, "assumptionChecks")

  return()
}

# Normalize both input types to a list of per-variable summaries: list(name, n, variance)
.getVarianceDataSV <- function(dataset, options) {
  if (options[["inputType"]] == "rawData") {
    return(lapply(colnames(dataset), function(colname) {
      col <- na.omit(dataset[[colname]])
      list(name = colname, n = length(col), variance = if (length(col) > 1) var(col) else NA_real_)
    }))
  }

  return(list(list(name = "", n = options[["sampleSize"]], variance = options[["sampleVariance"]])))
}

.createOutputTableSV <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["outputTable"]]))
    return()

  outputTable <- createJaspTable(title = gettext("Single Variance Test"))
  outputTable$dependOn(c("alternative", "chiSquareTest", "ciMethod", "confLevel", "dependent",
                         "sdEstimate", "sdCi", "testVariance", "varEstimate", "varianceCi",
                         "bootstrapSamples", "setSeed", "seed",
                         "inputType", "sampleVariance", "sampleSize"))
  jaspResults[["outputTable"]] <- outputTable

  outputTable$addColumnInfo(name = "varName",   title = gettext("Variable"),          type = "string")

  if (options[["varEstimate"]])
    outputTable$addColumnInfo(name = "varEst", title = gettext("Variance"), type = "number")

  if (options[["sdEstimate"]])
    outputTable$addColumnInfo(name = "sdEst", title = gettext("Std. deviation"), type = "number")

  outputTable$addColumnInfo(name = "chiSquare", title = "χ²", type = "number")
  outputTable$addColumnInfo(name = "df",        title = gettext("df"),  type = "integer")
  outputTable$addColumnInfo(name = "pValue",    title = gettext("p"),   type = "pvalue")

  if (.showVarianceCiSV(options)) {
    ciOvertitle <- gettextf("%i%% Confidence Interval<br>Variance", options[["confLevel"]] * 100)
    outputTable$addColumnInfo(name = "ciLower", title = gettext("Lower"), type = "number", overtitle = ciOvertitle)
    outputTable$addColumnInfo(name = "ciUpper", title = gettext("Upper"), type = "number", overtitle = ciOvertitle)
  }

  if (.showSdCiSV(options)) {
    sdOvertitle <- gettextf("%i%% Confidence Interval<br>Std. Deviation", options[["confLevel"]] * 100)
    outputTable$addColumnInfo(name = "sdCiLower", title = gettext("Lower"), type = "number", overtitle = sdOvertitle)
    outputTable$addColumnInfo(name = "sdCiUpper", title = gettext("Upper"), type = "number", overtitle = sdOvertitle)
  }

  outputTable$showSpecifiedColumnsOnly <- TRUE

  if(!ready)
    return()

  .fillOutputTableSV(outputTable, dataset, options)

  return()

}

.fillOutputTableSV <- function(outputTable, dataset, options) {

  varData <- .getVarianceDataSV(dataset, options)

  resList <- lapply(varData, .computeSVTest,
                    options = options, outputTable = outputTable, dataset = dataset)

  keep <- !vapply(resList, is.null, logical(1))
  if (!any(keep)) # happens if all entries failed / had too few observations
    return()

  res <- do.call(rbind.data.frame, resList[keep])
  res$varName <- vapply(varData[keep], `[[`, character(1), "name")
  outputTable$setData(res)

  # name the confidence interval method actually used, and flag its small-sample caveat
  if (.showVarianceCiSV(options) || .showSdCiSV(options))
    .varianceCiFootnotesVar(outputTable, options,
                            minN = min(vapply(varData[keep], function(entry) as.numeric(entry[["n"]]), numeric(1))))

  # add footnote describing the hypothesis
  outputTable$addFootnote(
    switch(options[["alternative"]],
           "two.sided" = gettextf("For all tests, the alternative hypothesis is that the variance is not equal to %.2f.", round(options[["testVariance"]], 2)), # explicit rounding because gettextf would round 2.255 to 2.25
           "greater" = gettextf("For all tests, the alternative hypothesis is that the variance is greater than %.2f.", round(options[["testVariance"]], 2)),
           "less" = gettextf("For all tests, the alternative hypothesis is that the variance is less than %.2f.", round(options[["testVariance"]], 2))
    )
  )

  return()
}

.computeSVTest <- function(entry, options, outputTable, dataset) {
  if (options[["inputType"]] == "rawData")
    col <- na.omit(dataset[[entry[["name"]]]])
  else
    col <- .syntheticSampleSV(entry[["n"]], entry[["variance"]])

  if (length(col) < 2) {
    outputTable$addFootnote(gettextf("%s has too few observations after removing missing values.", entry[["name"]]),
                            symbol = gettext("<b>Warning:</b>"))
    return(NULL)
  }

  # Note that the p-value is not the same as 2 * pchisq(test_val, df, lower.tail = FALSE)
  # for the two-sided test
  out <- try(DescTools::VarTest(col, alternative = options[["alternative"]],
                                sigma.squared = options[["testVariance"]],
                                conf.level = options[["confLevel"]]), silent = TRUE)
  if (isTryError(out)) {
    outputTable$setError(.extractErrorMessage(out))
    return(NULL)
  }

  varEst <- out$estimate
  sdEst <- sqrt(varEst)
  chiSquare <- out$statistic
  pValue <- out$p.value
  df <- out$parameter[1]

  # VarTest already returns the chi-square interval honouring `alternative`; Bonett and the
  # bootstrap need the raw values, so .ciMethodVar falls back to chi-square for summarized input.
  # Skip the (potentially expensive) bootstrap entirely when no interval is displayed.
  if (.ciMethodVar(options) == "chiSquare" ||
      !(.showVarianceCiSV(options) || .showSdCiSV(options))) {
    ciRes <- list(lower = out$conf.int[1], upper = out$conf.int[2], error = NULL)
  } else {
    ciRes <- .varianceCi(col, options, sides = .getSidesCi(options))
    if (!is.null(ciRes$error)) {
      outputTable$setError(ciRes$error)
      return(NULL)
    }
  }

  sdCiRes <- .sdCiFromVarianceCi(ciRes)

  # remove row names that are induced by the package
  return(data.frame(varEst, sdEst, chiSquare, df, pValue,
                    ciLower   = ciRes$lower,   ciUpper   = ciRes$upper,
                    sdCiLower = sdCiRes$lower, sdCiUpper = sdCiRes$upper,
                    row.names = NULL))
}

# The confidence interval columns require both the estimate and its interval checkbox, mirroring
# the QML where each interval is nested under its estimate.
.showVarianceCiSV <- function(options) {
  return(options[["varEstimate"]] && options[["varianceCi"]])
}

.showSdCiSV <- function(options) {
  return(options[["sdEstimate"]] && options[["sdCi"]])
}

.getSidesCi <- function(options) {
  sides <- switch(options[["alternative"]],
                  "greater" = "left",
                  "less" = "right",
                  "two.sided")
  return(sides)
}


.assumptionChecksSV <- function(jaspResults, dataset, options, ready) {
  # never emit an empty titled container when neither check is requested
  if (!options[["normalityTest"]] && !options[["qqPlot"]])
    return()

  if(!is.null(jaspResults[["assumptionChecks"]]))
    return()

   assumptionContainer <- createJaspContainer(title = gettext("Assumption Checks"))
   assumptionContainer$dependOn(c("qqPlot", "normalityTest", "dependent"))

   jaspResults[["assumptionChecks"]] <- assumptionContainer

  if (options[["normalityTest"]])
   .createNormalityTestTable(jaspResults, dataset, options, ready)

  if (options[["qqPlot"]])
    .createQQPlot(jaspResults, dataset, ready)

  return()
}

.createQQPlot <- function(jaspResults, dataset, ready) {
  if (!is.null(jaspResults[["assumptionChecks"]][["qqPlots"]]))
    return()

  if (!ready) {
    jaspResults[["assumptionChecks"]][["qqPlots"]] <- createJaspPlot(title = gettext("Q-Q Plots"))
    return()
  }

  qqContainer <- createJaspContainer(title = gettext("Q-Q Plots"))
  qqContainer$dependOn(c("qqPlot", "dependent"))
  jaspResults[["assumptionChecks"]][["qqPlots"]] <- qqContainer

  for (i in seq_len(ncol(dataset))) {
    colName  <- colnames(dataset)[i]
    tempDat  <- dataset[, i]
    tempPlot <- createJaspPlot(title = jaspBase::decodeColNames(colName), height = 400, width = 500)
    # Standardized-residual Q-Q plot, matching jaspTTests (plotQQnorm(scale(resid), ...)).
    tempPlot$plotObject <- jaspGraphs::plotQQnorm(as.vector(scale(tempDat)), # as.vector extracts the standardized values
                                                  ciLevel = 0.95,
                                                  yName = gettext("Standardized Residuals"),
                                                  xName = gettext("Theoretical Quantiles"))
    qqContainer[[colName]] <- tempPlot
  }

  return()
}

.createNormalityTestTable <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["assumptionChecks"]][["normalityTest"]]))
    return()

  normalityTable <- createJaspTable(title = gettext("Test of Normality (Shapiro-Wilk)"))
  normalityTable$dependOn(c("normalityTest", "dependent"))
  jaspResults[["assumptionChecks"]][["normalityTest"]] <- normalityTable

  normalityTable$addColumnInfo(name = "varName", title = gettext("Residuals"), type = "string")
  normalityTable$addColumnInfo(name = "W",       title = gettext("W"),         type = "number")
  normalityTable$addColumnInfo(name = "pValue",  title = gettext("p"),         type = "pvalue")

  normalityTable$addFootnote(gettext("Significant results suggest a deviation from normality."))

  if (ready)
    .fillNormalityTestTable(normalityTable, dataset, options)

  return()
}

.fillNormalityTestTable <- function(normalityTable, dataset, options) {

  resList <- lapply(colnames(dataset), function(name) {

    x <- na.omit(dataset[[name]])
    # Shapiro-Wilk is invariant to location and scale, so centering (or scaling)
    # leaves W and the p-value unchanged; the deviations are shown as "Residuals".
    residuals <- x - mean(x)
    swTest <- try(shapiro.test(residuals), silent = TRUE)

    if (isTryError(swTest)) {
      normalityTable$setError(.extractErrorMessage(swTest))
      return(NULL)
    }

    data.frame(varName = name, W = as.numeric(swTest$statistic),
               pValue = as.numeric(swTest$p.value), stringsAsFactors = FALSE)
  })

  results <- do.call(rbind, resList)

  if (!is.null(results))
    normalityTable$setData(results)

  return()
}


