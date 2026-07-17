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

# Correlation matrix with pairwise tests. Delegates entirely to the
# Correlation analysis implemented in jaspRegression; the QML options
# (inst/qml/multipleCorrelations.qml) mirror jaspRegression's Correlation.qml.
#
# jaspRegression is an optional (Suggests) dependency used only at runtime, so
# that a missing jaspRegression cannot prevent this module from loading. In JASP
# the Regression module is available, so the delegate resolves normally.

#' @import jaspBase
#' @export
multipleCorrelations <- function(jaspResults, dataset, options, ...) {
  if (!requireNamespace("jaspRegression", quietly = TRUE)) {
    errorTable <- createJaspTable(title = gettext("Correlation Matrix"))
    errorTable$setError(gettext("This analysis requires the Regression (jaspRegression) module, which is not available."))
    jaspResults[["error"]] <- errorTable
    return()
  }

  jaspRegression::CorrelationInternal(jaspResults, dataset, options)
}
