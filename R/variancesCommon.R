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
