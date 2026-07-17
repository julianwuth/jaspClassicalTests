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
	RadioButtonGroup
	{
		id:    samples
		name:  "samples"
		title: qsTr("Samples")
		RadioButton { value: "independent"; label: qsTr("Independent groups"); checked: true; info: qsTr("Two correlations of the same pair of variables measured in two independent groups.") }
		RadioButton { value: "dependent";   label: qsTr("Dependent (same sample)"); info: qsTr("Two correlations measured in the same sample.") }
	}

	RadioButtonGroup
	{
		id:      dependentType
		name:    "dependentType"
		title:   qsTr("Dependent Correlations")
		visible: samples.value === "dependent"
		RadioButton { value: "overlapping";    label: qsTr("Overlapping (share a variable)"); checked: true; info: qsTr("The two correlations have one variable in common, e.g. cor(X, Z) vs cor(Y, Z).") }
		RadioButton { value: "nonoverlapping"; label: qsTr("Non-overlapping"); info: qsTr("The two correlations share no variable, e.g. cor(X, Y) vs cor(V, W).") }
	}

	VariablesForm
	{
		visible: samples.value === "independent"
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesListIndep" }
		AssignedVariablesList { name: "independentVariable1"; title: qsTr("First Variable");  allowedColumns: ["scale"];  singleVariable: true; info: qsTr("First variable of the correlation.") }
		AssignedVariablesList { name: "independentVariable2"; title: qsTr("Second Variable"); allowedColumns: ["scale"];  singleVariable: true; info: qsTr("Second variable of the correlation.") }
		AssignedVariablesList { name: "groupingVariable";     title: qsTr("Grouping Variable"); allowedColumns: ["nominal", "nominalText", "ordinal"]; singleVariable: true; info: qsTr("Factor with exactly two levels defining the two independent groups.") }
	}

	VariablesForm
	{
		visible: samples.value === "dependent" && dependentType.value === "overlapping"
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesListOverlap" }
		AssignedVariablesList { name: "commonVariable";   title: qsTr("Common Variable"); allowedColumns: ["scale"]; singleVariable: true; info: qsTr("The variable shared by both correlations.") }
		AssignedVariablesList { name: "overlapVariable1"; title: qsTr("First Variable");  allowedColumns: ["scale"]; singleVariable: true; info: qsTr("Correlated with the common variable in the first correlation.") }
		AssignedVariablesList { name: "overlapVariable2"; title: qsTr("Second Variable"); allowedColumns: ["scale"]; singleVariable: true; info: qsTr("Correlated with the common variable in the second correlation.") }
	}

	VariablesForm
	{
		visible: samples.value === "dependent" && dependentType.value === "nonoverlapping"
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesListNonoverlap" }
		AssignedVariablesList { name: "nonoverlapVariable1"; title: qsTr("Correlation 1: First Variable");  allowedColumns: ["scale"]; singleVariable: true }
		AssignedVariablesList { name: "nonoverlapVariable2"; title: qsTr("Correlation 1: Second Variable"); allowedColumns: ["scale"]; singleVariable: true }
		AssignedVariablesList { name: "nonoverlapVariable3"; title: qsTr("Correlation 2: First Variable");  allowedColumns: ["scale"]; singleVariable: true }
		AssignedVariablesList { name: "nonoverlapVariable4"; title: qsTr("Correlation 2: Second Variable"); allowedColumns: ["scale"]; singleVariable: true }
	}

	RadioButtonGroup
	{
		name:  "alternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton { value: "two.sided"; label: qsTr("Correlations differ"); checked: true; info: qsTr("Two-sided alternative hypothesis that the two population correlations differ.") }
		RadioButton { value: "greater";   label: qsTr("First > Second"); info: qsTr("One-sided alternative hypothesis that the first population correlation is greater than the second.") }
		RadioButton { value: "less";      label: qsTr("First < Second"); info: qsTr("One-sided alternative hypothesis that the first population correlation is less than the second.") }
	}

	Group
	{
		title: qsTr("Additional Statistics")
		CheckBox
		{
			name:  "ci"
			label: qsTr("Confidence interval")
			info:  qsTr("Confidence intervals for the individual Pearson correlations (Fisher's z) and for their difference (Zou, 2007).")
			childrenOnSameRow: true
			CIField { name: "ciLevel" }
		}
	}
}
