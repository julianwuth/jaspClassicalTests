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
twoSamplePoissonRate <- function(jaspResults, dataset, options) {

  inputType  <- options[["inputType"]]
  hasAnyTest <- options[["exactTest"]] || options[["normalApprox"]]

  if (inputType == "rawData") {
    ready <- options[["count"]] != "" && options[["group"]] != "" && hasAnyTest
    if (ready)
      .hasErrors(dataset, type = c("infinity", "negativeValues", "factorLevels"),
                 infinity.target = options[["count"]],
                 negativeValues.target = c(options[["count"]], options[["time"]]),
                 factorLevels.target = options[["group"]],
                 factorLevels.amount = '!= 2',
                 exitAnalysisIfErrors = TRUE)
  } else {
    ready <- hasAnyTest
  }

  if (options[["descriptives"]])
    .createDescriptivesTableTR(jaspResults, dataset, options, ready)

  .createMainTableTR(jaspResults, dataset, options, ready)

  return()
}

# --- Data extraction ---------------------------------------------------------

.getGroupDataTR <- function(dataset, options) {
  if (options[["inputType"]] == "rawData") {
    countVar <- options[["count"]]
    groupVar <- options[["group"]]

    countCol <- dataset[[countVar]]
    groupCol <- dataset[[groupVar]]
    lvls     <- levels(factor(groupCol))

    groups <- vector("list", 2)
    for (i in 1:2) {
      mask   <- !is.na(groupCol) & groupCol == lvls[i]
      counts <- stats::na.omit(countCol[mask])
      # poisson.test requires an integer event count; round the summed raw counts
      # (consistent with the one-sample rate analysis).
      events <- as.integer(round(sum(counts)))

      if (options[["time"]] != "") {
        timeCol <- dataset[[options[["time"]]]]
        time    <- sum(stats::na.omit(timeCol[mask]))
      } else {
        time <- length(counts)
      }

      groups[[i]] <- list(name = as.character(lvls[i]), events = events, time = time)
    }
  } else {
    n1 <- options[["groupOneName"]]
    n2 <- options[["groupTwoName"]]
    groups <- list(
      list(name   = if (nchar(n1) > 0) n1 else gettext("Group 1"),
           events = options[["groupOneOccurrences"]],
           time   = options[["groupOneInterval"]]),
      list(name   = if (nchar(n2) > 0) n2 else gettext("Group 2"),
           events = options[["groupTwoOccurrences"]],
           time   = options[["groupTwoInterval"]])
    )
  }
  groups[[1]]$rate <- groups[[1]]$events / groups[[1]]$time
  groups[[2]]$rate <- groups[[2]]$events / groups[[2]]$time
  return(groups)
}

# --- Descriptives table ------------------------------------------------------

.createDescriptivesTableTR <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["descriptivesTable"]]))
    return()

  descTable <- createJaspTable(title = gettext("Descriptive Statistics"))
  descTable$dependOn(c("inputType", "count", "group", "time",
                       "groupOneName", "groupOneOccurrences", "groupOneInterval",
                       "groupTwoName", "groupTwoOccurrences", "groupTwoInterval",
                       "descriptives", "descriptiveCi", "descriptiveConfLevel"))
  descTable$position <- 1
  jaspResults[["descriptivesTable"]] <- descTable

  descTable$addColumnInfo(name = "groupName", title = gettext("Group"),       type = "string")
  descTable$addColumnInfo(name = "events",    title = gettext("Occurrences"), type = "integer")
  descTable$addColumnInfo(name = "time",      title = gettext("Interval"),    type = "number")
  descTable$addColumnInfo(name = "rate",      title = gettext("Rate"),        type = "number")

  if (options[["descriptiveCi"]]) {
    ciTitle <- gettextf("%i%% Confidence Interval", as.integer(options[["descriptiveConfLevel"]] * 100))
    descTable$addColumnInfo(name = "ciLower", title = gettext("Lower"), type = "number",
                            overtitle = ciTitle)
    descTable$addColumnInfo(name = "ciUpper", title = gettext("Upper"), type = "number",
                            overtitle = ciTitle)
  }

  descTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  groupData <- try(.getGroupDataTR(dataset, options), silent = TRUE)
  if (isTryError(groupData)) {
    descTable$setError(.extractErrorMessage(groupData))
    return()
  }

  rows <- lapply(groupData, function(g) {
    row <- data.frame(
      groupName = g$name,
      events    = as.integer(g$events),
      time      = g$time,
      rate      = g$rate,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    if (options[["descriptiveCi"]]) {
      ciOut <- try(
        stats::poisson.test(x = g$events, T = g$time, conf.level = options[["descriptiveConfLevel"]]),
        silent = TRUE
      )
      if (!isTryError(ciOut)) {
        row$ciLower <- ciOut$conf.int[1]
        row$ciUpper <- ciOut$conf.int[2]
      } else {
        row$ciLower <- NA
        row$ciUpper <- NA
      }
    }
    return(row)
  })

  descTable$setData(do.call(rbind, rows))

  if (options[["descriptiveCi"]])
    descTable$addFootnote(gettextf("Confidence interval for %s based on the exact Poisson distribution.", "λ"))

  return()
}

# --- Main test table ---------------------------------------------------------

.createMainTableTR <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["outputTable"]]))
    return()

  outputTable <- createJaspTable(title = gettext("Two-Sample Poisson Rate Test"))
  outputTable$dependOn(c("inputType", "count", "group", "time",
                         "groupOneName", "groupOneOccurrences", "groupOneInterval",
                         "groupTwoName", "groupTwoOccurrences", "groupTwoInterval",
                         "testTarget", "exactTest", "normalApprox", "pooledSe",
                         "testRatio", "testDifference",
                         "alternative", "confLevel", "ratioCi", "ciMethod"))
  outputTable$position <- 2
  jaspResults[["outputTable"]] <- outputTable

  isDiff       <- options[["testTarget"]] == "difference"
  effectTitle  <- if (isDiff) gettext("Difference") else gettext("Ratio")
  ciEffectName <- if (isDiff) gettext("Difference") else gettext("Ratio")

  outputTable$addColumnInfo(name = "method", title = gettext("Method"),   type = "string")
  outputTable$addColumnInfo(name = "rate1",  title = gettext("Rate\u2081"), type = "number")
  outputTable$addColumnInfo(name = "rate2",  title = gettext("Rate\u2082"), type = "number")
  outputTable$addColumnInfo(name = "effect", title = effectTitle,          type = "number")

  if (options[["normalApprox"]])
    outputTable$addColumnInfo(name = "statistic", title = gettext("z"), type = "number")

  outputTable$addColumnInfo(name = "pValue", title = gettext("p"), type = "pvalue")

  if (options[["ratioCi"]]) {
    ciTitle <- gettextf("%i%% CI on %s", as.integer(options[["confLevel"]] * 100), ciEffectName)
    outputTable$addColumnInfo(name = "ciLower", title = gettext("Lower"), type = "number",
                              overtitle = ciTitle)
    outputTable$addColumnInfo(name = "ciUpper", title = gettext("Upper"), type = "number",
                              overtitle = ciTitle)
  }

  outputTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  .fillMainTableTR(outputTable, dataset, options)

  return()
}

.fillMainTableTR <- function(outputTable, dataset, options) {
  groupData <- try(.getGroupDataTR(dataset, options), silent = TRUE)
  if (isTryError(groupData)) {
    outputTable$setError(.extractErrorMessage(groupData))
    return()
  }

  g1 <- groupData[[1]]
  g2 <- groupData[[2]]

  isDiff <- options[["testTarget"]] == "difference"
  rows   <- list()

  if (isDiff) {
    if (options[["exactTest"]]) {
      row <- .computeExactDiffTR(g1, g2, options, outputTable)
      if (!is.null(row))
        rows[["exact"]] <- row
    }
    if (options[["normalApprox"]]) {
      row <- .computeNormalApproxDiffTR(g1, g2, options, outputTable)
      if (!is.null(row))
        rows[["normal"]] <- row
    }
  } else {
    if (options[["exactTest"]]) {
      row <- .computeExactTestTR(g1, g2, options, outputTable)
      if (!is.null(row))
        rows[["exact"]] <- row
    }
    if (options[["normalApprox"]]) {
      row <- .computeNormalApproxTR(g1, g2, options, outputTable)
      if (!is.null(row))
        rows[["normal"]] <- row
    }
  }

  if (length(rows) == 0)
    return()

  outputTable$setData(do.call(rbind.data.frame, rows))

  if (isDiff) {
    d0 <- options[["testDifference"]]
    outputTable$addFootnote(
      switch(options[["alternative"]],
        "two.sided" = gettextf("H\u2081: Rate\u2081 \u2212 Rate\u2082 \u2260 %.4g.", d0),
        "greater"   = gettextf("H\u2081: Rate\u2081 \u2212 Rate\u2082 > %.4g.", d0),
        "less"      = gettextf("H\u2081: Rate\u2081 \u2212 Rate\u2082 < %.4g.", d0)
      )
    )
    if (options[["ratioCi"]]) {
      if (options[["ciMethod"]] == "exact")
        outputTable$addFootnote(gettext("Confidence interval for the difference based on the MOVER method combining exact single-rate Poisson intervals (Zou & Donner, 2008)."))
      else
        outputTable$addFootnote(gettext("Confidence interval for the difference based on the unpooled standard-error normal approximation."))
    }
  } else {
    r0 <- options[["testRatio"]]
    outputTable$addFootnote(
      switch(options[["alternative"]],
        "two.sided" = gettextf("H\u2081: Rate\u2081/Rate\u2082 \u2260 %.4g.", r0),
        "greater"   = gettextf("H\u2081: Rate\u2081/Rate\u2082 > %.4g.", r0),
        "less"      = gettextf("H\u2081: Rate\u2081/Rate\u2082 < %.4g.", r0)
      )
    )
    if (options[["ratioCi"]]) {
      if (options[["ciMethod"]] == "exact")
        outputTable$addFootnote(gettext("Confidence interval for the ratio based on the exact conditional Poisson method (matching the exact test)."))
      else
        outputTable$addFootnote(gettext("Confidence interval for the ratio based on the Wald approximation for the log rate-ratio; the accompanying normal-approximation test uses a conditional binomial score statistic."))
    }
  }

  outputTable$addFootnote(
    gettextf("Group 1 = %1$s; Group 2 = %2$s.", g1$name, g2$name)
  )

  return()
}

.computeExactTestTR <- function(g1, g2, options, outputTable) {
  out <- try(
    stats::poisson.test(x           = c(g1$events, g2$events),
                        T           = c(g1$time,   g2$time),
                        r           = options[["testRatio"]],
                        alternative = options[["alternative"]],
                        conf.level  = options[["confLevel"]]),
    silent = TRUE
  )

  if (isTryError(out)) {
    outputTable$setError(.extractErrorMessage(out))
    return(NULL)
  }

  row <- data.frame(
    method    = gettext("Exact"),
    rate1     = g1$rate,
    rate2     = g2$rate,
    effect    = g1$rate / g2$rate,
    statistic = NA,
    pValue    = out$p.value,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (options[["ratioCi"]])
    row <- .addRatioCiTR(row, g1, g2, options, outputTable)

  return(row)
}

.computeNormalApproxTR <- function(g1, g2, options, outputTable) {
  x1 <- g1$events
  x2 <- g2$events
  T1 <- g1$time
  T2 <- g2$time
  r0 <- options[["testRatio"]]

  # Conditional binomial score test: X1 | X1+X2 ~ Bin(n, p0)
  # Add as doubles so the total cannot overflow R's 32-bit integer type.
  n  <- as.numeric(x1) + as.numeric(x2)
  p0 <- r0 * T1 / (r0 * T1 + T2)

  if (n == 0) {
    outputTable$setError(gettext("Normal approximation requires at least one observed event."))
    return(NULL)
  }

  pHat      <- x1 / n
  statistic <- (pHat - p0) / sqrt(p0 * (1 - p0) / n)

  pValue <- switch(options[["alternative"]],
    "two.sided" = 2 * stats::pnorm(-abs(statistic)),
    "greater"   = stats::pnorm(statistic, lower.tail = FALSE),
    "less"      = stats::pnorm(statistic)
  )

  row <- data.frame(
    method    = gettext("Normal approximation"),
    rate1     = g1$rate,
    rate2     = g2$rate,
    effect    = g1$rate / g2$rate,
    statistic = statistic,
    pValue    = pValue,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (options[["ratioCi"]])
    row <- .addRatioCiTR(row, g1, g2, options, outputTable)

  return(row)
}

.computeExactDiffTR <- function(g1, g2, options, outputTable) {
  d0 <- options[["testDifference"]]

  if (d0 != 0) {
    outputTable$addFootnote(
      gettext("Exact test for the difference is only available when the hypothesized difference is 0."))
    return(NULL)
  }

  out <- try(
    stats::poisson.test(x           = c(g1$events, g2$events),
                        T           = c(g1$time,   g2$time),
                        r           = 1,
                        alternative = options[["alternative"]]),
    silent = TRUE
  )

  if (isTryError(out)) {
    outputTable$setError(.extractErrorMessage(out))
    return(NULL)
  }

  row <- data.frame(
    method    = gettext("Exact"),
    rate1     = g1$rate,
    rate2     = g2$rate,
    effect    = g1$rate - g2$rate,
    statistic = NA,
    pValue    = out$p.value,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (options[["ratioCi"]])
    row <- .addDiffCiTR(row, g1, g2, options)

  return(row)
}

.computeNormalApproxDiffTR <- function(g1, g2, options, outputTable) {
  x1 <- g1$events; x2 <- g2$events
  T1 <- g1$time;   T2 <- g2$time
  d0 <- options[["testDifference"]]

  if (T1 <= 0 || T2 <= 0) {
    outputTable$setError(gettext("Normal approximation requires a positive interval in both groups."))
    return(NULL)
  }

  diff       <- g1$rate - g2$rate
  pooledRate <- (x1 + x2) / (T1 + T2)
  sePooled   <- sqrt(pooledRate * (1 / T1 + 1 / T2))
  seUnpooled <- sqrt(g1$rate / T1 + g2$rate / T2)
  se         <- if (options[["pooledSe"]]) sePooled else seUnpooled

  # The pooled rate is the null-restricted estimate only when the hypothesized
  # difference is 0; otherwise the pooled standard error does not match H0.
  if (options[["pooledSe"]] && d0 != 0)
    outputTable$addFootnote(
      gettextf("The pooled standard error assumes a hypothesized difference of 0, but the hypothesized difference is %.4g. Select the unpooled standard error instead.", d0),
      symbol = gettext("<b>Warning:</b>"))

  if (!is.finite(se) || se == 0) {
    outputTable$setError(gettext("Normal approximation for the difference could not be computed (zero standard error)."))
    return(NULL)
  }

  statistic <- (diff - d0) / se

  pValue <- switch(options[["alternative"]],
    "two.sided" = 2 * stats::pnorm(-abs(statistic)),
    "greater"   = stats::pnorm(statistic, lower.tail = FALSE),
    "less"      = stats::pnorm(statistic)
  )

  row <- data.frame(
    method    = gettext("Normal approximation"),
    rate1     = g1$rate,
    rate2     = g2$rate,
    effect    = diff,
    statistic = statistic,
    pValue    = pValue,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (options[["ratioCi"]])
    row <- .addDiffCiTR(row, g1, g2, options)

  return(row)
}

# Dispatch the difference CI on the selected method:
#   exact  -> MOVER interval from exact single-rate Poisson intervals
#   normal -> unpooled Wald interval
.addDiffCiTR <- function(row, g1, g2, options) {
  if (options[["ciMethod"]] == "exact")
    return(.moverDiffCiTR(row, g1, g2, options))
  return(.waldDiffCiTR(row, g1, g2, options))
}

.waldDiffCiTR <- function(row, g1, g2, options) {
  seUnpooled <- sqrt(g1$rate / g1$time + g2$rate / g2$time)

  if (!is.finite(seUnpooled)) {
    row$ciLower <- NA
    row$ciUpper <- NA
    return(row)
  }

  diff  <- g1$rate - g2$rate
  alpha <- 1 - options[["confLevel"]]
  z     <- stats::qnorm(1 - alpha / ifelse(options[["alternative"]] == "two.sided", 2, 1))

  if (options[["alternative"]] == "two.sided") {
    row$ciLower <- diff - z * seUnpooled
    row$ciUpper <- diff + z * seUnpooled
  } else if (options[["alternative"]] == "greater") {
    row$ciLower <- diff - z * seUnpooled
    row$ciUpper <- Inf
  } else {
    row$ciLower <- -Inf
    row$ciUpper <- diff + z * seUnpooled
  }
  return(row)
}

# MOVER (Method of Variance Estimates Recovery) CI for the rate difference.
# Combines the exact single-rate Poisson intervals (l_i, u_i) into an interval
# for Rate1 - Rate2. Zou, G. Y., & Donner, A. (2008). Construction of confidence
# limits about effect measures: A general approach. Statistics in Medicine,
# 27(10), 1693-1702. Formulas:
#   Lower = d - sqrt((r1 - l1)^2 + (u2 - r2)^2)
#   Upper = d + sqrt((u1 - r1)^2 + (r2 - l2)^2)
# One-sided alternatives use one-sided single-rate limits at the same level.
.moverDiffCiTR <- function(row, g1, g2, options) {
  r1  <- g1$rate
  r2  <- g2$rate
  d   <- r1 - r2
  cl  <- options[["confLevel"]]
  alt <- options[["alternative"]]

  # exact one-sided Poisson limits for a single rate (events / time)
  lowerLimit <- function(events, time)
    stats::poisson.test(events, time, conf.level = cl, alternative = "greater")$conf.int[1]
  upperLimit <- function(events, time)
    stats::poisson.test(events, time, conf.level = cl, alternative = "less")$conf.int[2]

  limits <- try(
    if (alt == "two.sided") {
      ci1 <- stats::poisson.test(g1$events, g1$time, conf.level = cl)$conf.int
      ci2 <- stats::poisson.test(g2$events, g2$time, conf.level = cl)$conf.int
      l1  <- ci1[1]; u1 <- ci1[2]
      l2  <- ci2[1]; u2 <- ci2[2]
      c(d - sqrt((r1 - l1)^2 + (u2 - r2)^2),
        d + sqrt((u1 - r1)^2 + (r2 - l2)^2))
    } else if (alt == "greater") {
      l1 <- lowerLimit(g1$events, g1$time)
      u2 <- upperLimit(g2$events, g2$time)
      c(d - sqrt((r1 - l1)^2 + (u2 - r2)^2), Inf)
    } else {
      u1 <- upperLimit(g1$events, g1$time)
      l2 <- lowerLimit(g2$events, g2$time)
      c(-Inf, d + sqrt((u1 - r1)^2 + (r2 - l2)^2))
    },
    silent = TRUE
  )

  if (isTryError(limits)) {
    row$ciLower <- NA
    row$ciUpper <- NA
  } else {
    row$ciLower <- limits[1]
    row$ciUpper <- limits[2]
  }
  return(row)
}

.addRatioCiTR <- function(row, g1, g2, options, outputTable) {
  if (options[["ciMethod"]] == "exact") {
    ciOut <- try(
      stats::poisson.test(x           = c(g1$events, g2$events),
                          T           = c(g1$time,   g2$time),
                          r           = options[["testRatio"]],
                          alternative = options[["alternative"]],
                          conf.level  = options[["confLevel"]]),
      silent = TRUE
    )
    if (isTryError(ciOut)) {
      outputTable$addFootnote(gettext("Exact CI could not be computed."), symbol = gettext("<b>Warning:</b>"))
      row$ciLower <- NA
      row$ciUpper <- NA
    } else {
      row$ciLower <- ciOut$conf.int[1]
      row$ciUpper <- ciOut$conf.int[2]
    }
  } else { # Wald CI for the log rate-ratio
    # Standard Wald interval for a Poisson rate ratio: Var(log(rate1/rate2)) = 1/x1 + 1/x2
    # (delta method), so CI = (rate1/rate2) * exp(+/- z * sqrt(1/x1 + 1/x2)). Verified to
    # match a Poisson-GLM Wald interval (Rothman, Greenland & Lash, 2008). Undefined if a
    # count is 0. Note the accompanying normal-approx test is a conditional binomial score
    # test, so the ratio test statistic and this CI use different approximations.
    x1 <- g1$events
    x2 <- g2$events
    if (x1 == 0 || x2 == 0) {
      outputTable$addFootnote(
        gettext("Normal approximation CI for ratio requires both event counts > 0."),
        symbol = gettext("<b>Warning:</b>")
      )
      row$ciLower <- NA
      row$ciUpper <- NA
    } else {
      alpha   <- 1 - options[["confLevel"]]
      z       <- stats::qnorm(1 - alpha / ifelse(options[["alternative"]] == "two.sided", 2, 1))
      logRat  <- log(g1$rate / g2$rate)
      seLR    <- sqrt(1 / x1 + 1 / x2)

      if (options[["alternative"]] == "two.sided") {
        row$ciLower <- exp(logRat - z * seLR)
        row$ciUpper <- exp(logRat + z * seLR)
      } else if (options[["alternative"]] == "greater") {
        row$ciLower <- exp(logRat - z * seLR)
        row$ciUpper <- Inf
      } else {
        row$ciLower <- 0
        row$ciUpper <- exp(logRat + z * seLR)
      }
    }
  }
  return(row)
}
