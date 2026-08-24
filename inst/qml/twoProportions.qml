//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Form
{
	info: qsTr("Compares the proportion of successes between two groups, reporting their difference (with confidence interval) and, optionally, the relative risk and odds ratio.\n" + "## " + "Assumptions\n" + "- Independent observations within and between groups.\n- A binary outcome (success/failure) recorded for each group.")

	VariablesForm
	{
		preferredHeight:				190 * preferencesModel.uiScale
		marginBetweenVariablesLists:	15
		info:							qsTr("**Input**. The data can be supplied either as individual observations (one row per unit, with a binary success variable) or as aggregated counts (one row per group, with the number of successes and the sample size). The factor must have exactly two levels.")

		AvailableVariablesList
		{
			name:	"allVariablesList"
		}

		AssignedVariablesList
		{
			id:				factor
			name:			"factor"
			title:			qsTr("Factor")
			singleVariable:	true
			allowedColumns:	["nominal"]
			info:			qsTr("The grouping variable that defines the two groups whose proportions are compared.")
		}

		AssignedVariablesList
		{
			name:			"successes"
			title:			qsTr("Successes")
			singleVariable:	true
			allowedColumns:	["scale"]
			info:			qsTr("For aggregated data, the number of successes per group. For individual data, the binary variable indicating a success or failure. Rows with an empty cell in any assigned variable are excluded from the analysis.")
		}

		AssignedVariablesList
		{
			name:			"sampleSize"
			title:			qsTr("Sample Size")
			singleVariable:	true
			allowedColumns:	["scale"]
			info:			qsTr("For aggregated data, the total number of observations per group. Leave empty for individual data. Rows with an empty cell in any assigned variable are excluded from the analysis.")
		}
	}

	RadioButtonGroup
	{
		name:				"alternative"
		title:				qsTr("Alternative Hypothesis")
		Layout.columnSpan:	2

		RadioButton
		{
			value:		"two.sided"
			label:		qsTr("Group 1 ≠ Group 2")
			checked:	true
			info:		qsTr("Two-sided alternative hypothesis that the two groups' proportions are not equal.")
		}

		RadioButton
		{
			value:	"greater"
			label:	qsTr("Group 1 > Group 2")
			info:	qsTr("One-sided alternative hypothesis that the first group's proportion is greater than the second's.")
		}

		RadioButton
		{
			value:	"less"
			label:	qsTr("Group 1 < Group 2")
			info:	qsTr("One-sided alternative hypothesis that the first group's proportion is less than the second's.")
		}
	}

	Group
	{
		title:	qsTr("Tests")

		CheckBox
		{
			name:		"chiSquaredTest"
			label:		qsTr("χ² test")
			checked:	true
			info:		qsTr("Pearson's χ² test comparing the two proportions (with optional continuity correction).")
		}

		CheckBox
		{
			name:	"fisherTest"
			label:	qsTr("Fisher's exact test")
			info:	qsTr("Fisher's exact test for the 2×2 table. Reports an exact p-value (respecting the chosen alternative) and, in the Effect Sizes table, the conditional maximum-likelihood odds ratio with an exact confidence interval. Recommended for small samples where the χ² approximation is unreliable.")
		}
	}

	Group
	{
		title:	qsTr("Additional Statistics")

		CheckBox
		{
			name:				"ci"
			label:				qsTr("Confidence interval")
			checked:			true
			childrenOnSameRow:	true
			info:				qsTr("Two-sided confidence intervals for the effect sizes.")

			CIField
			{
				name:	"ciLevel"
			}
		}

		CheckBox
		{
			name:	"relativeRisk"
			label:	qsTr("Relative risk")
			info:	qsTr("The ratio of the two proportions (p₁ / p₂).")
		}

		CheckBox
		{
			name:	"oddsRatio"
			label:	qsTr("Odds ratio")
			info:	qsTr("The ratio of the odds of success in the two groups.")
		}

		CheckBox
		{
			name:	"continuityCorrection"
			label:	qsTr("Continuity correction")
			info:	qsTr("Applies Yates' continuity correction to the chi-square statistic and the difference interval.")
		}

		CheckBox
		{
			name:	"vovkSellke"
			label:	qsTr("Vovk-Sellke maximum p-ratio")
			info:	qsTr("An upper bound on how much more likely the data are under the alternative hypothesis than under the null.")
		}
	}

	ColumnLayout
	{
		RadioButtonGroup
		{
			name:	"descriptivesDisplay"
			title:	qsTr("Display")

			RadioButton
			{
				value:		"counts"
				label:		qsTr("Counts")
				checked:	true
				info:		qsTr("Displays the descriptives as counts.")
			}

			RadioButton
			{
				id:		displayProportions
				value:	"proportions"
				label:	qsTr("Proportions")
				info:	qsTr("Displays the descriptives as proportions.")
			}
		}

		Group
		{
			title:	qsTr("Descriptives")

			CheckBox
			{
				name:	"descriptivesTable"
				label:	qsTr("Table")
				info:	qsTr("Displays a table of observed counts or proportions and the sample size per group. Rows with an empty cell in any of the assigned variables are excluded, and a footnote reports how many were removed.")

				CheckBox
				{
					name:				"descriptivesTableCi"
					label:				qsTr("Confidence interval")
					enabled:			displayProportions.checked
					childrenOnSameRow:	true
					info:				qsTr("Per-group Clopper-Pearson confidence intervals for the proportion θ. Only available when the descriptives are displayed as proportions.")

					CIField
					{
						name:	"descriptivesTableCiLevel"
					}
				}
			}

			CheckBox
			{
				name:	"descriptivesPlot"
				label:	qsTr("Plot")
				info:	qsTr("Displays a plot of observed counts or proportions.")

				CheckBox
				{
					name:				"descriptivesPlotCi"
					label:				qsTr("Confidence interval")
					checked:			true
					enabled:			displayProportions.checked
					childrenOnSameRow:	true
					info:				qsTr("Adds Clopper-Pearson confidence intervals for θ to the plot. Only available when the descriptives are displayed as proportions.")

					CIField
					{
						name:	"descriptivesPlotCiLevel"
					}
				}
			}
		}
	}
}
