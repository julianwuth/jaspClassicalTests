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

# Chi-square test of equal proportions across more than two groups.
# Shared input reshaping, validation and descriptives live in proportionsCommon.R.

#' @import jaspBase
#' @importFrom stats prop.test
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

.mpMainTable <- function(jaspResults, data, options, ready) {
  if (!is.null(jaspResults[["mainTable"]]))
    return()

  mainTable <- createJaspTable(title = gettext("Test of Equal Proportions"))
  mainTable$dependOn(c("factor", "successes", "sampleSize",
                       "continuityCorrection", "vovkSellke"))
  mainTable$position <- 3
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
    prop.test(data$successes, data$sampleSize, correct = correct),
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

  .mpAddMissingFootnote(mainTable, data)

  return()
}
