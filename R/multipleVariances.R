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
#' @importFrom stats bartlett.test var.test shapiro.test na.omit pchisq pf pnorm qchisq sd uniroot var ave
#' @importFrom car leveneTest
#' @export
multipleVariances <- function(jaspResults, dataset, options, ...) {
  isRaw <- options[["inputType"]] == "rawData"

  # is ready if test is selected and data was provided
  if (isRaw) {
    ready <- (length(options[["dependent"]]) > 0 && options[["factor"]] != "")
    if (ready)
      # TODO check if this should also check for exactly 2 levels of the factor
      .hasErrors(dataset, type = c('infinity', 'variance', 'factorLevels'),
                 infinity.target = options[["dependent"]],
                 variance.target = options[["dependent"]],
                 variance.equalTo = 0,
                 factorLevels.target = options[["factor"]],
                 factorLevels.amount = '< 2',
                 exitAnalysisIfErrors = TRUE)
  } else {
    ready <- (length(options[["summarizedGroups"]]) >= 2 &&
              any(c(options[["fTest"]], options[["bartlettTest"]])))
  }

  .createOutputTableMV(jaspResults, dataset, options, ready)

  if (options[["descriptives"]])
    .createDescriptivesTableMV(jaspResults, dataset, options, ready)

  if (options[["varianceRatioCi"]])
    .createVarianceRatioTableMV(jaspResults, dataset, options, ready)

  # variance-ratio and variance-estimate plots work from summaries; box and raincloud need raw data
  wantPlots <- options[["varRatioPlot"]] || options[["varEstimatePlot"]] ||
               (isRaw && (options[["boxPlot"]] || options[["rainCloudPlot"]]))
  if (wantPlots)
    .createSummaryPlotContainerMV(jaspResults, dataset, options, ready)

  # assumption checks require raw data
  if (isRaw)
    .assumptionChecksMV(jaspResults, dataset, options, ready)

  return()
}

# The "variables" to loop over: the assigned columns for raw data, or a single
# synthetic variable (blank label) for summarized input.
.getDepNamesMV <- function(options) {
  if (options[["inputType"]] == "rawData")
    return(options[["dependent"]])
  return("")
}

# Returns an na.omit'd data.frame(y, group) for one "variable".
# Raw data:   the actual column paired with the grouping factor.
# Summarized: synthetic per-group samples with the exact entered variance and size, so the
#             same var.test / bartlett.test code path yields results identical to real data
#             (both tests depend only on the per-group variance and sample size).
.getGroupDataMV <- function(dataset, options, depName) {
  if (options[["inputType"]] == "rawData") {
    factor <- as.factor(dataset[[options[["factor"]]]])
    return(na.omit(data.frame(y = dataset[[depName]], group = factor)))
  }

  groups <- options[["summarizedGroups"]]
  ys     <- vector("list", length(groups))
  labs   <- character(0)
  for (i in seq_along(groups)) {
    g   <- groups[[i]]
    lab <- if (is.null(g[["groupName"]]) || g[["groupName"]] == "") as.character(i) else g[["groupName"]]
    ys[[i]] <- .syntheticSampleSV(g[["n"]], g[["variance"]])
    labs    <- c(labs, rep(lab, g[["n"]]))
  }
  data.frame(y = unlist(ys), group = factor(labs, levels = unique(labs)))
}

# Bonett's method needs the raw values (kurtosis); fall back to the sufficient-statistic
# methods (chi-square variance CI, F-test ratio CI) for summarized input.
.ciMethodMV <- function(options) {
  if (options[["inputType"]] == "rawData") options[["ciMethod"]] else "chiSquare"
}

.ratioCiMethodMV <- function(options) {
  if (options[["inputType"]] == "rawData") options[["ratioCiMethod"]] else "fTest"
}

.createOutputTableMV <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["outputTable"]]))
    return()

  outputTable <- createJaspTable(title = gettext("Test for Equality of Variances"))
  outputTable$dependOn(c("dependent", "factor", "fTest", "leveneTest", "bonettTest", "bartlettTest",
                         "inputType", "summarizedGroups"))
  outputTable$position <- 1
  jaspResults[["outputTable"]] <- outputTable

  outputTable$addColumnInfo(name = "var",   title = gettext("Variable"),  type = "string")
  outputTable$addColumnInfo(name = "test",  title = gettext("Test"),      type = "string")
  outputTable$addColumnInfo(name = "stat",  title = gettext("Statistic"), type = "number", format = "dp:3")
  outputTable$addColumnInfo(name = "df1",   title = gettext("df1"),       type = "integer")
  outputTable$addColumnInfo(name = "df2",   title = gettext("df2"),       type = "integer")
  outputTable$addColumnInfo(name = "p",     title = gettext("p"),         type = "pvalue")

  outputTable$showSpecifiedColumnsOnly <- TRUE

  if(!ready)
    return()

  .fillOutputTableMV(outputTable, dataset, options)

  return()
}

.fillOutputTableMV <- function(outputTable, dataset, options) {
  isRaw <- options[["inputType"]] == "rawData"

  rows <- list()
  fTestFootnoteAdded <- FALSE

  for (depName in .getDepNamesMV(options)) {
    subData <- .getGroupDataMV(dataset, options, depName)

    if (nrow(subData) == 0) {
      outputTable$addFootnote(gettextf("%s has no observations after removing missing values.", depName),
                              symbol = gettext("<b>Warning:</b>"))
      next
    }

    y       <- subData$y
    group   <- droplevels(subData$group)
    nLevels <- nlevels(group)

    # F-Test (only if 2 levels)
    if (options[["fTest"]] && nLevels == 2) {
      res <- try(var.test(y ~ group), silent = TRUE)
      if (isTryError(res)) {
        outputTable$setError(gettext(as.character(res)))
        return()
      }
      rows[[length(rows) + 1]] <- list(var = depName, test = gettext("F"),
                                       stat = res$statistic, df1 = res$parameter[1], df2 = res$parameter[2], p = res$p.value)
    } else if (options[["fTest"]] && !fTestFootnoteAdded) {
      outputTable$addFootnote(gettext("F-test is only available for 2 groups."))
      fTestFootnoteAdded <- TRUE
    }

    # Levene's Test (requires raw data)
    if (isRaw && options[["leveneTest"]]) {
      # TODO using the median technically means that this is the Brown-Forsythe test
      res <- try(car::leveneTest(y ~ group, center = median), silent = TRUE)
      if (isTryError(res)) {
        outputTable$setError(gettext(as.character(res)))
        return()
      }
      rows[[length(rows) + 1]] <- list(var = depName, test = gettext("Levene's"),
                                       stat = res$`F value`[1], df1 = res$Df[1], df2 = res$Df[2], p = res$`Pr(>F)`[1])
    }

    # Bartlett's Test
    if (options[["bartlettTest"]]) {
      res <- try(bartlett.test(y ~ group), silent = TRUE)
      if (isTryError(res)) {
        outputTable$setError(gettext(as.character(res)))
        return()
      }
      rows[[length(rows) + 1]] <- list(var = depName, test = gettext("Bartlett's"),
                                       stat = res$statistic, df1 = res$parameter[1], df2 = NA, p = res$p.value)
    }

    # Bonett's Test (requires raw data)
    if (isRaw && options[["bonettTest"]]) {
      res <- try(.computeBonettTest(y, group, depName), silent = TRUE)
      if (isTryError(res)) {
        outputTable$setError(gettext(as.character(res)))
        return()
      }
      if (!is.null(res$error)) {
        outputTable$addFootnote(res$error)
      } else {
        rows[[length(rows) + 1]] <- list(var = depName, test = gettext("Bonett's"),
                                         stat = res$statistic, df1 = res$df, df2 = NA, p = res$p.value)
      }
    }
  }

  outputTable$addRows(rows)

  return()
}

.computeBonettTest <- function(y, group, varName) {
  # Bonett (2006) test for equality of variances.
  # For k = 2 groups: ELTR (Banga & Fox 2013) with pooled kurtosis and CI inversion.
  # For k > 2 groups: weighted chi-square test on log-variances.
  group <- droplevels(group)
  groups <- split(y, group)
  k <- length(groups)

  ns   <- vapply(groups, length, integer(1))
  sds  <- vapply(groups, sd, numeric(1))
  vars <- sds^2

  # Compute Winsorized kurtosis components for each group
  kurtComponents <- lapply(groups, .computeWinsorizedComponents)
  gammas <- vapply(kurtComponents, `[[`, numeric(1), "gamma")

  # Check for non-finite kurtosis (e.g., zero-variance groups or constant data)
  if (any(!is.finite(gammas)) || any(vars == 0)) {
    return(list(error = gettextf("Bonett's test could not be computed for variable %s.", varName)))
  }

  if (k == 2) {
    # --- Two-group ELTR test (Banga & Fox, 2013) ---
    return(.computeBonettTestTwoGroups(ns, sds, kurtComponents, varName))
  }

  # --- k > 2 groups: weighted chi-square test on log-variances ---
  logVars  <- log(vars)
  asymVars <- (gammas + 2) / (ns - 1)

  if (any(asymVars <= 0)) {
    return(list(error = gettextf("Bonett's test could not be computed for variable %s.", varName)))
  }

  ws        <- 1 / asymVars
  logVarBar <- sum(ws * logVars) / sum(ws)
  statistic <- sum(ws * (logVars - logVarBar)^2)
  df        <- k - 1
  p.value   <- pchisq(statistic, df = df, lower.tail = FALSE)

  if (!is.finite(statistic) || !is.finite(p.value)) {
    return(list(error = gettextf("Bonett's test could not be computed for variable %s.", varName)))
  }

  list(statistic = statistic, df = df, p.value = p.value, error = NULL)
}

.computeWinsorizedComponents <- function(x) {
  # Winsorized kurtosis components following Bonett (2006).
  # Returns gamma (excess kurtosis), sum_d4, and var for pooled kurtosis computation.
  # Uses Winsorized mean with trim proportion g/n, where g = floor(0.2 * n).
  n <- length(x)
  if (n < 4) return(list(gamma = NaN, sum_d4 = NaN, var = NaN))

  g  <- floor(0.2 * n)
  xs <- sort(x)
  xw <- xs
  if (g > 0) {
    xw[seq_len(g)]    <- xs[g + 1]
    xw[(n - g + 1):n] <- xs[n - g]
  }
  m <- mean(xw) # Winsorized mean

  d2 <- sum((x - m)^2)
  d4 <- sum((x - m)^4)

  if (d2 == 0) return(list(gamma = NaN, sum_d4 = NaN, var = NaN))

  gamma <- n * d4 / d2^2 - 3

  list(gamma = gamma, sum_d4 = d4, var = var(x))
}

.computeBonettTestTwoGroups <- function(ns, sds, kurtComponents, varName) {
  # Two-group Bonett test using ELTR (Banga & Fox, 2013).
  # Tests H0: sigma1^2 = sigma2^2 using pooled kurtosis and CI inversion.
  # Reference: Banga & Fox (2013), "Bonett's Method: Confidence Interval for
  # a Ratio of Standard Deviations" (Minitab whitepaper).
  n1 <- ns[1]; n2 <- ns[2]
  s1 <- sds[1]; s2 <- sds[2]
  var1 <- s1^2; var2 <- s2^2

  # Correction factors r_i = (n_i - 3) / n_i
  r1 <- (n1 - 3) / n1
  r2 <- (n2 - 3) / n2

  # Pooled kurtosis under H0: sigma1^2 = sigma2^2
  sumD4_1 <- kurtComponents[[1]][["sum_d4"]]
  sumD4_2 <- kurtComponents[[2]][["sum_d4"]]
  poolDenom <- ((n1 - 1) * var1 + (n2 - 1) * var2)^2
  omegaP <- (n1 + n2) * (sumD4_1 + sumD4_2) / poolDenom

  # Standard error of ln(sigma1^2 / sigma2^2)
  se2 <- (omegaP - r1) / (n1 - 1) + (omegaP - r2) / (n2 - 1)

  if (!is.finite(se2) || se2 <= 0) {
    return(list(error = gettextf("Bonett's test could not be computed for variable %s.", varName)))
  }

  se <- sqrt(se2)

  # Test statistic: Z^2 = [ln(S1^2/S2^2)]^2 / se^2 ~ chi^2(1)
  logVarRatio <- log(var1 / var2)
  Z <- logVarRatio / se
  statistic <- Z^2

  # P-value via CI inversion using equalizer constant (Banga & Fox, 2013).
  # L(z) = ln(c(z) * S1^2 / S2^2) - z * se, where c(z) = n1*(n2-z) / ((n1-z)*n2).
  # Find z_L such that L(z_L) = 0 and z_U such that U(z_U) = 0 (swap groups).
  Lfunc <- function(z, nn1, nn2, ss1, ss2) {
    cz <- nn1 * (nn2 - z) / ((nn1 - z) * nn2)
    if (cz <= 0) return(NaN)
    log(cz * ss1^2 / ss2^2) - z * se
  }

  zL <- tryCatch(
    uniroot(function(z) Lfunc(z, n1, n2, s1, s2), interval = c(-20, 20), tol = 1e-10)$root,
    error = function(e) NA
  )
  zU <- tryCatch(
    uniroot(function(z) Lfunc(z, n2, n1, s2, s1), interval = c(-20, 20), tol = 1e-10)$root,
    error = function(e) NA
  )

  alphaL <- if (!is.na(zL)) pnorm(zL, lower.tail = FALSE) else 0
  alphaU <- if (!is.na(zU)) pnorm(zU, lower.tail = FALSE) else 0
  p.value <- min(2 * min(alphaL, alphaU), 1)

  if (!is.finite(statistic) || !is.finite(p.value)) {
    return(list(error = gettextf("Bonett's test could not be computed for variable %s.", varName)))
  }

  list(statistic = statistic, df = 1, p.value = p.value, error = NULL)
}

.computeBonettRatioCiMV <- function(y1, y2, confLevel) {
  # Bonett (2006) CI for sigma1^2 / sigma2^2 using the Banga & Fox (2013) / Minitab
  # equalizer constant c(z) = n1*(n2-z) / ((n1-z)*n2).
  n1 <- length(y1); n2 <- length(y2)
  if (n1 < 4 || n2 < 4)
    return(list(lower = NA, upper = NA,
                error = gettext("Bonett CI requires at least 4 observations per group.")))

  k1 <- .computeWinsorizedComponents(y1)
  k2 <- .computeWinsorizedComponents(y2)
  var1 <- k1$var; var2 <- k2$var
  if (!is.finite(var1) || !is.finite(var2) || var1 <= 0 || var2 <= 0)
    return(list(lower = NA, upper = NA, error = gettext("Bonett CI could not be computed.")))

  r1 <- (n1 - 3) / n1
  r2 <- (n2 - 3) / n2

  # Pooled kurtosis (matches Banga & Fox / Minitab for both test and CI)
  poolDenom <- ((n1 - 1) * var1 + (n2 - 1) * var2)^2
  omegaP    <- (n1 + n2) * (k1$sum_d4 + k2$sum_d4) / poolDenom
  se2       <- (omegaP - r1) / (n1 - 1) + (omegaP - r2) / (n2 - 1)

  if (!is.finite(se2) || se2 <= 0)
    return(list(lower = NA, upper = NA, error = gettext("Bonett CI could not be computed.")))

  se <- sqrt(se2)
  z  <- qnorm(1 - (1 - confLevel) / 2)

  if (n1 <= z || n2 <= z)
    return(list(lower = NA, upper = NA, error = gettext("Bonett CI could not be computed.")))

  # c(z) for lower bound (original order) and upper bound (groups swapped)
  cLow <- n1 * (n2 - z) / ((n1 - z) * n2)
  cUpp <- n2 * (n1 - z) / ((n2 - z) * n1)

  if (cLow <= 0 || cUpp <= 0)
    return(list(lower = NA, upper = NA, error = gettext("Bonett CI could not be computed.")))

  ratio <- var1 / var2
  lower <- cLow * ratio * exp(-z * se)
  upper <- ratio / cUpp * exp( z * se)

  list(lower = lower, upper = upper, error = NULL)
}

.createDescriptivesTableMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["descriptivesTable"]]))
    return()

  descTable <- createJaspTable(title = gettext("Descriptive Statistics"))
  descTable$dependOn(c("dependent", "factor", "descriptives", "varianceCi", "confLevel", "ciMethod",
                       "inputType", "summarizedGroups"))
  descTable$position <- 2
  jaspResults[["descriptivesTable"]] <- descTable

  descTable$addColumnInfo(name = "var",    title = gettext("Variable"),  type = "string")
  descTable$addColumnInfo(name = "group",  title = gettext("Group"),     type = "string")
  descTable$addColumnInfo(name = "n",      title = gettext("N"),         type = "integer")

  # the mean is not available from summarized input
  if (options[["inputType"]] == "rawData")
    descTable$addColumnInfo(name = "mean", title = gettext("Mean"),      type = "number")

  descTable$addColumnInfo(name = "sd",     title = gettext("SD"),        type = "number")
  descTable$addColumnInfo(name = "varEst", title = gettext("Variance"),  type = "number")

  if (options[["varianceCi"]]) {
    overtitle <- gettextf("%i%% Confidence Interval<br>Variance", options[["confLevel"]] * 100)
    descTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    descTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  descTable$showSpecifiedColumnsOnly <- TRUE

  if(!ready)
    return()

  .fillDescriptivesTableMV(descTable, dataset, options)

  return()
}

.fillDescriptivesTableMV <- function(descTable, dataset, options) {
  rows <- list()

  for (depName in .getDepNamesMV(options)) {
    groupData <- .getGroupDataMV(dataset, options, depName)
    levels    <- levels(droplevels(groupData$group))

    for (lvl in levels) {
      subY <- groupData$y[groupData$group == lvl]

      n <- length(subY)

      if (n < 2) {
        meanEst <- NA
        sdEst   <- NA
        varEst  <- NA
        ci      <- c(NA, NA)
      } else {
        meanEst <- mean(subY)
        varEst  <- var(subY)
        sdEst   <- sqrt(varEst)
      }

      row <- list(var = depName, group = lvl, n = n, mean = meanEst, sd = sdEst, varEst = varEst)

      if (options[["varianceCi"]]) {
        ci <- .computeGroupVarianceCi(subY, varEst, options)
        row$lower <- ci[1]
        row$upper <- ci[2]
      }

      rows[[length(rows) + 1]] <- row
    }
  }

  descTable$addRows(rows)

  return()
}

.computeGroupVarianceCi <- function(subY, varEst, options) {
  if (.ciMethodMV(options) == "bonett") {
    ciRes <- try(DescTools::VarCI(subY, method = "bonett", conf.level = options[["confLevel"]]), silent = TRUE)

    # TODO perhaps return an error message here
    if (isTryError(ciRes))
      return(c(NA, NA))

    return(c(ciRes["lwr.ci"], ciRes["upr.ci"]))
  }

  # TODO check that these computations are correct
  # Default: chi-square method
  df    <- length(subY) - 1
  alpha <- 1 - options[["confLevel"]]
  lower <- df * varEst / qchisq(1 - alpha/2, df)
  upper <- df * varEst / qchisq(alpha/2, df)
  return(c(lower, upper))
}

.createVarianceRatioTableMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["varianceRatioTable"]]))
    return()

  ratioTable <- createJaspTable(title = gettext("Variance Ratio"))
  ratioTable$dependOn(c("dependent", "factor", "varianceRatioCi", "ratioCiMethod", "confLevel",
                        "inputType", "summarizedGroups"))
  ratioTable$position <- 3
  jaspResults[["varianceRatioTable"]] <- ratioTable

  ratioTable$addColumnInfo(name = "var",   title = gettext("Variable"),       type = "string")
  ratioTable$addColumnInfo(name = "ratio", title = gettext("Variance Ratio"), type = "number")

  overtitle <- gettextf("%i%% Confidence Interval", options[["confLevel"]] * 100)
  ratioTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  ratioTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

  ratioTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  .fillVarianceRatioTableMV(ratioTable, dataset, options)

  return()
}

.fillVarianceRatioTableMV <- function(ratioTable, dataset, options) {
  rows       <- list()
  useBonett  <- identical(.ratioCiMethodMV(options), "bonett")
  levels     <- NULL

  for (depName in .getDepNamesMV(options)) {
    subData <- .getGroupDataMV(dataset, options, depName)

    if (nrow(subData) == 0) {
      ratioTable$addFootnote(gettextf("%s has no observations after removing missing values.", depName),
                             symbol = gettext("<b>Warning:</b>"))
      next
    }

    subData$group <- droplevels(subData$group)
    levels        <- levels(subData$group)

    if (length(levels) != 2) {
      ratioTable$addFootnote(gettext("Variance ratio confidence interval is only available for 2 groups."))
      return()
    }

    y1 <- subData$y[subData$group == levels[1]]
    y2 <- subData$y[subData$group == levels[2]]

    if (length(y1) < 2 || length(y2) < 2 || var(y1) <= 0 || var(y2) <= 0) {
      ratioTable$addFootnote(gettextf("%s: insufficient data to compute the variance ratio.", depName),
                             symbol = gettext("<b>Warning:</b>"))
      next
    }

    ratioEst <- var(y1) / var(y2)

    if (useBonett) {
      ciRes <- .computeBonettRatioCiMV(y1, y2, options[["confLevel"]])
      if (!is.null(ciRes$error)) {
        ratioTable$addFootnote(gettextf("%1$s: %2$s", depName, ciRes$error),
                               symbol = gettext("<b>Warning:</b>"))
        next
      }
      rows[[length(rows) + 1]] <- list(var = depName, ratio = ratioEst,
                                       lower = ciRes$lower, upper = ciRes$upper)
    } else {
      res <- try(var.test(subData$y ~ subData$group, conf.level = options[["confLevel"]]), silent = TRUE)
      if (isTryError(res)) {
        ratioTable$setError(as.character(res))
        return()
      }
      rows[[length(rows) + 1]] <- list(var = depName, ratio = unname(res$estimate),
                                       lower = res$conf.int[1], upper = res$conf.int[2])
    }
  }

  ratioTable$addRows(rows)

  if (is.null(levels))
    return()

  ratioTable$addFootnote(gettextf("Variance ratio: Group %1$s / Group %2$s.", levels[1], levels[2]))
  ratioTable$addFootnote(
    if (useBonett)
      gettext("Confidence interval based on Bonett's method (Banga & Fox, 2013).")
    else
      gettext("Confidence interval based on the F-test.")
  )

  return()
}

# This could potentially become a common function across analyses
.assumptionChecksMV <- function(jaspResults, dataset, options, ready) {
  if (is.null(jaspResults[["assumptionChecks"]])) {
    assumptionContainer <- createJaspContainer(title = gettext("Assumption Checks"))
    assumptionContainer$dependOn(c("dependent", "factor"))
    assumptionContainer$position <- 5
    jaspResults[["assumptionChecks"]] <- assumptionContainer
  }

  if (options[["normalityTest"]])
    .createNormalityTestTableMV(jaspResults, dataset, options, ready)

  if (options[["qqPlot"]])
    .createQQPlotMV(jaspResults, dataset, options, ready)

  return()
}

.createNormalityTestTableMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["assumptionChecks"]][["normalityTest"]]))
    return()

  normalityTable <- createJaspTable(title = gettext("Test of Normality (Shapiro-Wilk)"))
  normalityTable$dependOn("normalityTest")
  jaspResults[["assumptionChecks"]][["normalityTest"]] <- normalityTable

  normalityTable$addColumnInfo(name = "varName", title = gettext("Variable"), type = "string", combine = TRUE)
  normalityTable$addColumnInfo(name = "group",   title = gettext("Group"),    type = "string")
  normalityTable$addColumnInfo(name = "W",       title = gettext("W"),        type = "number")
  normalityTable$addColumnInfo(name = "pValue",  title = gettext("p"),        type = "pvalue")

  normalityTable$showSpecifiedColumnsOnly <- TRUE

  normalityTable$addFootnote(gettext("Normality is tested within each group. Significant results suggest a deviation from normality."))

  if (ready)
    .fillNormalityTestTableMV(normalityTable, dataset, options)

  return()
}

.fillNormalityTestTableMV <- function(normalityTable, dataset, options) {

  factor <- as.factor(dataset[[options[["factor"]]]])
  levels <- levels(factor)

  rows          <- list()
  smallGroupHit <- FALSE

  for (depName in options[["dependent"]]) {
    y <- dataset[[depName]]

    for (lvl in levels) {
      subY <- na.omit(y[factor == lvl & !is.na(factor)])
      n    <- length(subY)

      # Shapiro-Wilk requires between 3 and 5000 observations
      if (n < 3) {
        rows[[length(rows) + 1]] <- list(varName = depName, group = lvl, W = NA, pValue = NA)
        smallGroupHit <- TRUE
        next
      }

      swTest <- try(shapiro.test(subY), silent = TRUE)
      if (isTryError(swTest)) {
        normalityTable$setError(as.character(swTest))
        return()
      }

      rows[[length(rows) + 1]] <- list(varName = depName, group = lvl,
                                       W = as.numeric(swTest$statistic), pValue = as.numeric(swTest$p.value))
    }
  }

  normalityTable$addRows(rows)

  if (smallGroupHit)
    normalityTable$addFootnote(gettext("Groups with fewer than 3 observations are omitted from the test."))

  return()
}

.createQQPlotMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["assumptionChecks"]][["qqPlots"]]))
    return()

  if (!ready) {
    jaspResults[["assumptionChecks"]][["qqPlots"]] <- createJaspPlot(title = gettext("Q-Q Plots"))
    return()
  }

  qqContainer <- createJaspContainer(title = gettext("Q-Q Plots"))
  qqContainer$dependOn("qqPlot")
  jaspResults[["assumptionChecks"]][["qqPlots"]] <- qqContainer

  factor <- as.factor(dataset[[options[["factor"]]]])
  levels <- levels(factor)

  for (depName in options[["dependent"]]) {
    varContainer <- createJaspContainer(title = gettext(depName))
    qqContainer[[depName]] <- varContainer

    y <- dataset[[depName]]

    for (lvl in levels) {
      subY <- na.omit(y[factor == lvl & !is.na(factor)])

      tempPlot <- createJaspPlot(title = lvl, height = 400, width = 500)
      varContainer[[lvl]] <- tempPlot

      # Standardize within the group so normality is assessed per group
      if (length(subY) < 3 || sd(subY) == 0) {
        tempPlot$setError(gettextf("Group %s has insufficient observations for a Q-Q plot.", lvl))
        next
      }

      stdY <- as.vector(scale(subY))
      tempPlot$plotObject <- jaspGraphs::plotQQnorm(stdY,
                                                    ciLevel = 0.95,
                                                    yName = gettext("Standardized Observations"),
                                                    xName = gettext("Theoretical Quantiles"))
    }
  }

  return()
}

.createSummaryPlotContainerMV <- function(jaspResults, dataset, options, ready) {
  if (is.null(jaspResults[["summaryPlots"]])) {
    summaryPlots <- createJaspContainer(gettext("Summary Plots"))
    summaryPlots$dependOn(c("dependent", "factor"))
    summaryPlots$position <- 4
    jaspResults[["summaryPlots"]] <- summaryPlots
  }

  isRaw <- options[["inputType"]] == "rawData"

  # box and raincloud plots require raw data
  if (isRaw && options[["boxPlot"]])
    .boxplotMV(jaspResults, dataset, options, ready)

  if (options[["varRatioPlot"]])
    .varRatioPlotMV(jaspResults, dataset, options, ready)

  if (options[["varEstimatePlot"]])
    .varEstimatePlotMV(jaspResults, dataset, options, ready)

  if (isRaw && options[["rainCloudPlot"]])
    .rainCloudPlotMV(jaspResults, dataset, options, ready)

  return()
}

.rainCloudPlotMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["summaryPlots"]][["rainCloudPlot"]]))
    return()

  rainContainer <- createJaspContainer(title = gettext("Raincloud Plot (Demeaned)"))
  rainContainer$dependOn(c("rainCloudPlot", "rainCloudPlotHorizontal"))
  jaspResults[["summaryPlots"]][["rainCloudPlot"]] <- rainContainer

  if (!ready)
    return()

  factorName <- options[["factor"]]
  factor     <- as.factor(dataset[[factorName]])
  horiz      <- options[["rainCloudPlotHorizontal"]]

  for (depName in options[["dependent"]]) {
    tempPlot <- createJaspPlot(title = gettext(depName), height = 320, width = 480)
    rainContainer[[depName]] <- tempPlot

    plotDat <- na.omit(data.frame(y = dataset[[depName]], group = factor))
    if (nrow(plotDat) == 0) {
      tempPlot$setError(gettextf("%s has no observations after removing missing values.", depName))
      next
    }

    # Demean within each group: preserve variance, set every group mean to 0
    plotDat$y <- ave(plotDat$y, plotDat$group, FUN = function(z) z - mean(z))

    # Reuse the shared raincloud implementation from jaspTTests
    names(plotDat) <- c(depName, factorName)
    p <- try(jaspTTests:::.descriptivesPlotsRainCloudFill(plotDat, depName, factorName,
                                                          yLabel = depName, xLabel = factorName,
                                                          addLines = FALSE, horiz = horiz, testValue = NULL))
    if (isTryError(p)) {
      tempPlot$setError(as.character(p))
      next
    }

    tempPlot$plotObject <- p
  }

  return()
}

.boxplotMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["summaryPlots"]][["boxPlot"]]))
    return()

  boxContainer <- createJaspContainer(title = gettext("Box Plot"))
  boxContainer$dependOn("boxPlot")
  jaspResults[["summaryPlots"]][["boxPlot"]] <- boxContainer

  if (!ready)
    return()

  factorName <- options[["factor"]]
  factor     <- as.factor(dataset[[factorName]])

  for (depName in options[["dependent"]]) {
    # TODO check if it is actually advised to wrap variable name titles into gettext
    tempPlot <- createJaspPlot(title = gettext(depName), height = 350, width = 500)
    boxContainer[[depName]] <- tempPlot

    plotDat <- na.omit(data.frame(y = dataset[[depName]], group = factor))
    if (nrow(plotDat) == 0) {
      tempPlot$setError(gettextf("%s has no observations after removing missing values.", depName))
      next
    }

    yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotDat$y)
    yLims   <- range(yBreaks)

    p <- try(
      ggplot2::ggplot(plotDat, ggplot2::aes(x = group, y = y)) +
        ggplot2::geom_boxplot(outlier.shape = 4) +
        ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLims) +
        ggplot2::labs(x = factorName, y = depName) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw()
    )
    if (isTryError(p)) {
      tempPlot$setError(as.character(p))
      next
    }

    tempPlot$plotObject <- p
  }

  return()
}

.varRatioPlotMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["summaryPlots"]][["varRatioPlot"]]))
    return()

  ratioContainer <- createJaspContainer(title = gettext("Variance Ratio Plot"))
  ratioContainer$dependOn(c("varRatioPlot", "confLevel", "ratioCiMethod", "inputType", "summarizedGroups"))
  jaspResults[["summaryPlots"]][["varRatioPlot"]] <- ratioContainer

  if (!ready)
    return()

  levels <- levels(droplevels(.getGroupDataMV(dataset, options, .getDepNamesMV(options)[1])$group))

  if (length(levels) != 2) {
    placeholder <- createJaspPlot(title = gettext("Variance Ratio"))
    placeholder$setError(gettext("Variance ratio plot is only available for 2 groups."))
    ratioContainer[["notApplicable"]] <- placeholder
    return()
  }

  xLabel    <- gettextf("%i%% CI for \u03C3\u00B2(%s) / \u03C3\u00B2(%s)",
                        round(options[["confLevel"]] * 100), levels[1], levels[2])
  useBonett <- identical(.ratioCiMethodMV(options), "bonett")
  methodLab <- if (useBonett) gettext("Bonett") else gettext("F-test")

  for (depName in .getDepNamesMV(options)) {
    plotTitle    <- if (depName == "") gettext("Variance Ratio") else gettext(depName)
    containerKey <- if (depName == "") "summarized" else depName
    tempPlot     <- createJaspPlot(title = plotTitle, height = 250, width = 500)
    ratioContainer[[containerKey]] <- tempPlot

    plotDat <- .getGroupDataMV(dataset, options, depName)
    if (nrow(plotDat) == 0) {
      tempPlot$setError(gettextf("%s has no observations after removing missing values.", depName))
      next
    }
    plotDat$group <- droplevels(plotDat$group)

    y1 <- plotDat$y[plotDat$group == levels[1]]
    y2 <- plotDat$y[plotDat$group == levels[2]]

    if (length(y1) < 2 || length(y2) < 2 || var(y1) <= 0 || var(y2) <= 0) {
      tempPlot$setError(gettextf("%s: insufficient data to compute the variance ratio.", depName))
      next
    }

    ratioEst <- var(y1) / var(y2)

    if (useBonett) {
      ciRes <- .computeBonettRatioCiMV(y1, y2, options[["confLevel"]])
      if (!is.null(ciRes$error)) {
        tempPlot$setError(gettextf("%1$s: %2$s", depName, ciRes$error))
        next
      }
      lower <- ciRes$lower; upper <- ciRes$upper
    } else {
      res <- try(var.test(plotDat$y ~ plotDat$group, conf.level = options[["confLevel"]]), silent = TRUE)
      if (isTryError(res)) {
        tempPlot$setError(as.character(res))
        next
      }
      lower <- res$conf.int[1]; upper <- res$conf.int[2]
    }

    df <- data.frame(
      method   = methodLab,
      estimate = ratioEst,
      lower    = lower,
      upper    = upper
    )

    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(1, df$lower, df$upper))
    xLims   <- range(xBreaks)

    p <- ggplot2::ggplot(df, ggplot2::aes(y = method, x = estimate, xmin = lower, xmax = upper)) +
      ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "red") +
      ggplot2::geom_errorbarh(height = 0.2) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLims) +
      ggplot2::labs(x = xLabel, y = "") +
      jaspGraphs::geom_rangeframe(sides = "bl") +
      jaspGraphs::themeJaspRaw()

    tempPlot$plotObject <- p
  }

  return()
}

.varEstimatePlotMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["summaryPlots"]][["varEstimatePlot"]]))
    return()

  estContainer <- createJaspContainer(title = gettext("Variance Estimate Plot"))
  estContainer$dependOn(c("varEstimatePlot", "confLevel", "ciMethod", "inputType", "summarizedGroups"))
  jaspResults[["summaryPlots"]][["varEstimatePlot"]] <- estContainer

  if (!ready)
    return()

  # Bonett CI needs raw values; use the chi-square ("classic") CI for summarized input
  ciMethod   <- if (identical(.ciMethodMV(options), "bonett")) "bonett" else "classic"
  xLabel     <- gettextf("%i%% CI for \u03C3\u00B2", round(options[["confLevel"]] * 100))
  yLabel     <- if (options[["inputType"]] == "rawData") options[["factor"]] else gettext("Group")

  for (depName in .getDepNamesMV(options)) {
    plotTitle    <- if (depName == "") gettext("Variance Estimate") else gettext(depName)
    containerKey <- if (depName == "") "summarized" else depName
    tempPlot     <- createJaspPlot(title = plotTitle, height = 350, width = 500)
    estContainer[[containerKey]] <- tempPlot

    plotDat <- .getGroupDataMV(dataset, options, depName)
    if (nrow(plotDat) == 0) {
      tempPlot$setError(gettextf("%s has no observations after removing missing values.", depName))
      next
    }
    plotDat$group <- droplevels(plotDat$group)
    factorLvls    <- levels(plotDat$group)

    rowsList <- list()
    ciErr <- NULL
    for (lvl in factorLvls) {
      yg <- plotDat$y[plotDat$group == lvl]
      if (length(yg) < 2) next

      ci <- try(DescTools::VarCI(yg, method = ciMethod, conf.level = options[["confLevel"]]), silent = TRUE)
      if (isTryError(ci)) {
        ciErr <- as.character(ci)
        break
      }
      rowsList[[lvl]] <- data.frame(
        group    = lvl,
        estimate = var(yg),
        lower    = unname(ci["lwr.ci"]),
        upper    = unname(ci["upr.ci"])
      )
    }

    if (!is.null(ciErr)) {
      tempPlot$setError(ciErr)
      next
    }
    if (length(rowsList) == 0) {
      tempPlot$setError(gettextf("%s has no groups with sufficient observations.", depName))
      next
    }

    df <- do.call(rbind, rowsList)
    df$group <- factor(df$group, levels = factorLvls)

    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(df$lower, df$upper))
    xLims   <- range(xBreaks)

    p <- ggplot2::ggplot(df, ggplot2::aes(y = group, x = estimate, xmin = lower, xmax = upper)) +
      ggplot2::geom_errorbarh(height = 0.2) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLims) +
      ggplot2::labs(x = xLabel, y = yLabel) +
      jaspGraphs::geom_rangeframe(sides = "bl") +
      jaspGraphs::themeJaspRaw()

    tempPlot$plotObject <- p
  }

  return()
}
