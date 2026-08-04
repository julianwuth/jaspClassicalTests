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

  .createOutputTableSV(jaspResults, dataset, options, ready)

  # assumption checks require raw data
  if (options[["inputType"]] == "rawData")
    .assumptionChecksSV(jaspResults, dataset, options, ready)

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
                         "sdEstimate", "testVariance", "varEstimate", "varianceCi",
                         "inputType", "sampleVariance", "sampleSize"))
  jaspResults[["outputTable"]] <- outputTable

  outputTable$addColumnInfo(name = "varName",   title = gettext("Variable"),          type = "string")

  if (options$varEstimate)
    outputTable$addColumnInfo(name = "varEst", title = gettext("Variance"), type = "number")

  if (options$sdEstimate)
    outputTable$addColumnInfo(name = "sdEst", title = gettext("Std"), type = "number")

  outputTable$addColumnInfo(name = "chiSquare", title = gettext("&#967<sup>2</sup>"), type = "number")
  outputTable$addColumnInfo(name = "df",        title = gettext("df"),                type = "integer")
  outputTable$addColumnInfo(name = "pValue",    title = gettext("p"),                 type = "pvalue")

  if (options$varianceCi) {
    outputTable$addColumnInfo(name = "ciLower", title = gettext("Lower"), type = "number", overtitle = gettextf("%i%% Confidence Interval<br>Variance", options$confLevel * 100))
    outputTable$addColumnInfo(name = "ciUpper", title = gettext("Upper"), type = "number", overtitle = gettextf("%i%% Confidence Interval<br>Variance", options$confLevel * 100))
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

  # add footnote describing the hypothesis
  outputTable$addFootnote(
    switch(options$alternative,
           "two.sided" = gettextf("Variances tested against value: %.2f.", round(options$testVariance, 2)), # explicit rounding because gettextf would round 2.255 to 2.25
           "greater" = gettextf("For all tests, the alternative hypothesis is that the variance is greater than %.2f.", round(options$testVariance, 2)),
           "less" = gettextf("For all tests, the alternative hypothesis is that the variance is less than %.2f.", round(options$testVariance, 2))
    )
  )

  return()
}

# Build a sample with exactly the requested size and variance so the summarized
# input runs through the same VarTest/VarCI code path as raw data.
.syntheticSampleSV <- function(n, variance) {
  z <- seq_len(n) - (n + 1) / 2 # centered sequence
  z / sd(z) * sqrt(variance)
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
  out <- try(DescTools::VarTest(col, alternative = options$alternative,
                                sigma.squared = options$testVariance,
                                conf.level = options$confLevel), silent = TRUE)
  if (isTryError(out)) {
    outputTable$setError(gettext(as.character(out)))
    return(NULL)
  }

  varEst <- out$estimate
  sdEst <- sqrt(varEst)
  chiSquare <- out$statistic
  pValue <- out$p.value
  df <- out$parameter[1]

  # Bonett CI needs the raw values (kurtosis); force chi-square method for summarized input
  useBonett <- options[["ciMethod"]] == "bonett" && options[["inputType"]] == "rawData"

  if (!useBonett) {
    ciLower <- out$conf.int[1]
    ciUpper <- out$conf.int[2]
  } else { # Bonett method
    ciRes <- try(DescTools::VarCI(col, method = "bonett", conf.level = options$confLevel,
                                  sides = .getSidesCi(options)), silent = TRUE)
    if (isTryError(ciRes)) {
      outputTable$setError(gettext(as.character(ciRes)))
      return(NULL)
    }

    ciLower <- ciRes["lwr.ci"]
    ciUpper <- ciRes["upr.ci"]
  }

  # remove row names that are induced by the package
  return(data.frame(varEst, sdEst, chiSquare,
                    df, pValue, ciLower, ciUpper, row.names = NULL))
}

.getSidesCi <- function(options) {
  sides <- switch(options$alternative,
                  "greater" = "left",
                  "less" = "right",
                  "two.sided")
  return(sides)
}


.assumptionChecksSV <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["assumptionChecks"]]))
    return()

   assumptionContainer <- createJaspContainer(title = gettext("Assumption Checks"))
   assumptionContainer$dependOn(c("qqPlot", "normalityTest", "dependent"))

   jaspResults[["assumptionChecks"]] <- assumptionContainer

  if (options$normalityTest)
   .createNormalityTestTable(jaspResults, dataset, options, ready)

  if (options$qqPlot)
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

  for (i in 1:ncol(dataset)) {
    tempDat <- dataset[, i]
    tempPlot <- createJaspPlot(title = gettext(colnames(dataset)[i]), height = 400, width = 500)
    tempPlot$plotObject <- jaspGraphs::plotQQnorm(as.vector(scale(tempDat)), # as.vector extracts the standardized values
                                                  ciLevel = 0.95,
                                                  yName = "Standardized Residuals",
                                                  xName = "Theoretical Quantiles") # TODO this does not match the results from other modules
    qqContainer[[colnames(dataset)[i]]] <- tempPlot
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
    residuals <- x - mean(x) # TODO should this also be scaled?
    swTest <- try(shapiro.test(residuals), silent = TRUE)

    if (isTryError(swTest)) {
      normalityTable$setError(gettext(as.character(swTest)))
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


