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

# Shared helpers for the variance analyses (singleVariance, multipleVariances).

# Build a sample with exactly the requested size and variance so summarized input
# runs through the same VarTest / VarCI / var.test code path as raw data.
.syntheticSampleSV <- function(n, variance) {
  z <- seq_len(n) - (n + 1) / 2 # centered sequence
  z / sd(z) * sqrt(variance)
}

# Bonett needs the raw values (kurtosis) and the bootstrap needs a real sample; the synthetic
# sample built by .syntheticSampleSV has the requested variance but is a deterministic, equally
# spaced sequence, so resampling it is meaningless. Fall back to the chi-square interval, which
# depends on nothing but the variance and the sample size.
.ciMethodVar <- function(options) {
  if (options[["inputType"]] == "rawData")
    return(options[["ciMethod"]])

  return("chiSquare")
}

# Map the GUI method name onto DescTools::VarCI's method argument.
.varCiMethodArg <- function(method) {
  switch(method,
         "chiSquare" = "classic",
         "bonett"    = "bonett",
         "bootstrap" = "bca")
}

# One entry point for every variance CI in the module. Returns list(lower, upper, error).
# The SD interval is the square root of this interval (monotone reparameterisation preserves
# coverage exactly), so callers never bootstrap twice.
.varianceCi <- function(x, options, sides = "two.sided") {
  method <- .varCiMethodArg(.ciMethodVar(options))

  args <- list(x = x, method = method, conf.level = options[["confLevel"]], sides = sides)
  if (method == "bca")
    args$R <- options[["bootstrapSamples"]]

  ciRes <- try(do.call(DescTools::VarCI, args), silent = TRUE)
  if (isTryError(ciRes))
    return(list(lower = NA_real_, upper = NA_real_, error = .extractErrorMessage(ciRes)))

  return(list(lower = unname(ciRes["lwr.ci"]), upper = unname(ciRes["upr.ci"]), error = NULL))
}

# A CI for the variance maps to a CI for the standard deviation by sqrt(): coverage is preserved
# exactly because the transformation is monotone. Guards against a numerically negative lower
# bound (bootstrap endpoints are always >= 0, but Bonett's are computed on the log scale).
.sdCiFromVarianceCi <- function(ci) {
  return(list(lower = sqrt(max(ci$lower, 0)), upper = sqrt(max(ci$upper, 0)), error = ci$error))
}

# Footnote naming the CI method actually used, so the user can tell a BCa interval from a
# chi-square one (and sees why the choice was overridden for summarized input).
.varianceCiFootnotesVar <- function(table, options, minN = Inf) {
  method <- .ciMethodVar(options)

  table$addFootnote(switch(method,
    "chiSquare" = gettext("Confidence intervals are \u03C7\u00B2 intervals."),
    "bonett"    = gettext("Confidence intervals are based on Bonett's method."),
    "bootstrap" = gettextf("Confidence intervals are BCa bootstrap intervals based on %i replicates.",
                           options[["bootstrapSamples"]])
  ))

  if (options[["inputType"]] != "rawData" && options[["ciMethod"]] != "chiSquare")
    table$addFootnote(gettext("Bonett's method and the bootstrap require the raw observations, so the \u03C7\u00B2 interval is used for summarized data."))

  # Schenker (1985): the resampled EDF cannot see tail behaviour beyond the observed values,
  # so every nonparametric bootstrap interval for a variance under-covers in small samples.
  if (method == "bootstrap" && minN < 20)
    table$addFootnote(gettext("Bootstrap intervals for a variance can under-cover in small samples; Bonett's interval is preferable here."))

  return()
}

# JASP removes an output element only when one of its dependencies changes. Elements that a
# builder simply stops creating (raw-data-only plots after a switch to summarized input, an
# assumption-check container whose boxes are all unticked) would otherwise linger, so remove
# them explicitly on every invocation.
.removeJaspElement <- function(parent, key) {
  if (!is.null(parent) && !is.null(parent[[key]]))
    parent[[key]] <- NULL

  return()
}
