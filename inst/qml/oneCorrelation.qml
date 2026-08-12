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
import JASP.Widgets
import JASP

Form
{
	VariablesForm
	{
		infoLabel: qsTr("Input")
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "firstVariable";  title: qsTr("First Variable");  allowedColumns: ["scale"]; singleVariable: true; info: qsTr("The first variable of the correlation.") }
		AssignedVariablesList { name: "secondVariable"; title: qsTr("Second Variable"); allowedColumns: ["scale"]; singleVariable: true; info: qsTr("The second variable of the correlation.") }
	}

	Group
	{
		title: qsTr("Sample Correlation Coefficient")
		CheckBox { name: "pearson";  label: qsTr("Pearson's r");     checked: true; info: qsTr("Pearson's product moment correlation coefficient.") }
		CheckBox { name: "spearman"; label: qsTr("Spearman's rho");  info: qsTr("Spearman's rank-order correlation coefficient.") }
		CheckBox { name: "kendall";  label: qsTr("Kendall's tau-b"); info: qsTr("Kendall's tau-b rank-order correlation coefficient.") }
	}

	Group
	{
		title: qsTr("Test")
		DoubleField
		{
			name:         "testValue"
			label:        qsTr("Test value:")
			defaultValue: 0
			min:          -1
			max:          1
			decimals:     3
			info:         qsTr("The hypothesized value of the population correlation. The p-value is computed against this value (Fisher's z when non-zero).")
		}
	}

	RadioButtonGroup
	{
		name:  "alternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton { value: "two.sided"; label: qsTr("≠ Test value"); checked: true; info: qsTr("Two-sided alternative hypothesis that the population correlation differs from the test value.") }
		RadioButton { value: "greater";   label: qsTr("> Test value"); info: qsTr("One-sided alternative hypothesis that the population correlation is greater than the test value.") }
		RadioButton { value: "less";      label: qsTr("< Test value"); info: qsTr("One-sided alternative hypothesis that the population correlation is less than the test value.") }
	}

	Group
	{
		title: qsTr("Additional Statistics")
		CheckBox
		{
			name:  "ci"
			label: qsTr("Confidence interval")
			info:  qsTr("Confidence interval for the population correlation (available only for Pearson's r).")
			childrenOnSameRow: true
			CIField { name: "ciLevel" }
		}
		CheckBox { name: "effectSize"; label: qsTr("Effect size (Fisher's z)"); info: qsTr("The Fisher transformed effect size with its standard error.") }
		CheckBox { name: "vovkSellke"; label: qsTr("Vovk-Sellke maximum p-ratio"); info: qsTr("The maximum ratio of the likelihood of the observed p-value under H1 vs H0.") }
	}

	Group
	{
		title: qsTr("Plots")

		CheckBox
		{
			name:    "scatterPlot"
			label:   qsTr("Scatter plot")
			checked: true
			info:    qsTr("Scatter plot of the correlated variables.")

			CheckBox { name: "scatterPlotDensity"; label: qsTr("Densities for variables"); checked: true; info: qsTr("Adds marginal densities above and to the right of the scatter plot.") }

			CheckBox
			{
				name:    "scatterPlotRegressionLine"
				label:   qsTr("Regression line")
				checked: true
				info:    qsTr("Adds a linear regression line.")

				CheckBox
				{
					name:              "scatterPlotRegressionLineCi"
					label:             qsTr("Confidence interval")
					childrenOnSameRow: true
					CIField { name: "scatterPlotRegressionLineCiLevel" }
				}
			}
		}
	}
}
