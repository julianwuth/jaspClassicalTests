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

#' @import jaspFrequencies
#' @export
singleProportion <- function(jaspResults, dataset, options, ...) {
  dataset <- .spDropMissingCounts(dataset, options)
  return(jaspFrequencies::BinomialTestInternal(jaspResults, dataset, options, ...))
}

# Blank cells below the last data row arrive as NA and make the count expansion inside
# jaspFrequencies fail with "invalid 'times' argument"; drop those rows first.
.spDropMissingCounts <- function(dataset, options) {
  countsName <- options[["counts"]]
  if (is.null(dataset) || countsName == "" || is.null(dataset[[countsName]]))
    return(dataset)

  keep <- !is.na(dataset[[countsName]])
  if (all(keep))
    return(dataset)

  if (!any(keep))
    .quitAnalysis(gettext("The counts variable contains no observed values."))

  return(dataset[keep, , drop = FALSE])
}
