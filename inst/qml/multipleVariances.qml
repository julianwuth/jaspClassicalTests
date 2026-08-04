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
        name:    "inputType"
        title:   qsTr("Input")
        columns: 2

        RadioButton
        {
            value:   "rawData"
            label:   qsTr("Raw data")
            checked: true
            id:      inputRawData
        }

        RadioButton
        {
            value: "summarized"
            label: qsTr("Summarized data")
            id:    inputSummarized
        }
    }

    VariablesForm
    {
        infoLabel: qsTr("Input")
        preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
        enabled: inputRawData.checked
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList { name: "dependent"; title: qsTr("Dependent Variable"); allowedColumns: ["scale"]; info: qsTr("Scale variable(s) whose variances are compared across groups.") }
        AssignedVariablesList { name: "factor"; title: qsTr("Grouping Variable"); allowedColumns: ["nominal"]; singleVariable: true; info: qsTr("Nominal variable defining the groups.") }
    }

    ComponentsList
    {
        name:            "summarizedGroups"
        title:           qsTr("Summarized data")
        enabled:         inputSummarized.checked
        optionKey:       "groupName"
        addItemManually: true
        minimumItems:    2
        defaultValues:   [ { "groupName": "1" }, { "groupName": "2" } ]
        headerLabels:    [ qsTr("Group"), qsTr("Variance"), qsTr("N") ]
        info:            qsTr("Per-group summary statistics. Add one row per group with its sample variance and sample size.")

        rowComponent: RowLayout
        {
            TextField
            {
                name:            "groupName"
                label:           ""
                placeholderText: qsTr("Group")
                fieldWidth:      70
            }

            DoubleField
            {
                name:         "variance"
                label:        ""
                defaultValue: 1
                min:          0
                inclusive:    JASP.MaxOnly
                decimals:     3
                fieldWidth:   70
            }

            IntegerField
            {
                name:         "n"
                label:        ""
                defaultValue: 2
                min:          2
                fieldWidth:   60
            }
        }
    }

    Group
    {
        title: qsTr("Tests")
        CheckBox { name: "fTest"; label: qsTr("F-test (2 groups)"); info: qsTr("F-test for equality of two variances.") }
        CheckBox { name: "leveneTest"; label: qsTr("Levene's test"); checked: true; enabled: inputRawData.checked; info: qsTr("Levene's test for equality of variances based on the median. Requires raw data.") }
        CheckBox { name: "bonettTest"; label: qsTr("Bonett's test"); enabled: inputRawData.checked; info: qsTr("Bonett's test for equality of variances using Winsorized kurtosis. Requires raw data.") }
        CheckBox { name: "bartlettTest"; label: qsTr("Bartlett's test"); info: qsTr("Bartlett's test for equality of variances; assumes normality.") }
    }

    Group
    {
        title: qsTr("Assumption Checks")
        enabled: inputRawData.checked
        CheckBox { name: "normalityTest"; label: qsTr("Normality"); info: qsTr("Shapiro-Wilk test of normality.") }
        CheckBox { name: "qqPlot"; label: qsTr("Q-Q plot residuals"); info: qsTr("Q-Q plot of the residuals.") }
    }

    Group
    {
        title: qsTr("Additional Statistics")

        CheckBox 
        { 
            name: "descriptives" 
            label: qsTr("Descriptives")
            id: descriptives 
            
            CheckBox
            {
                name: "varianceCi"
                label: qsTr("Confidence interval")
                id: varianceCi
                childrenOnSameRow: true
                CIField { name: "confLevel" }
            }

            RadioButtonGroup
            {
                name: "ciMethod"
                title: qsTr("Method")
                enabled: varianceCi.checked && inputRawData.checked
                radioButtonsOnSameRow: true
                info: qsTr("Method for the per-group variance confidence interval. Bonett's method needs the raw observations (it relies on the sample kurtosis), so with summarized data only the χ² interval is available and the method cannot be changed.")
                RadioButton { value: "chiSquare"; label: qsTr("χ²"); checked: true }
                RadioButton { value: "bonett"; label: qsTr("Bonett") }
            }
        }

        CheckBox
        {
            name: "varianceRatioCi"
            label: qsTr("Variance ratio (2 groups)")
            id: varianceRatioCi
            info: qsTr("Confidence interval for the variance ratio (2 groups only).")

            RadioButtonGroup
            {
                name: "ratioCiMethod"
                title: qsTr("Method")
                enabled: varianceRatioCi.checked && inputRawData.checked
                radioButtonsOnSameRow: true
                info: qsTr("Method for the variance ratio confidence interval. Bonett's method needs the raw observations (it relies on the sample kurtosis), so with summarized data only the F-test interval is available and the method cannot be changed.")
                RadioButton { value: "fTest"; label: qsTr("F-test"); checked: true }
                RadioButton { value: "bonett"; label: qsTr("Bonett") }
            }
        }
    }

    Group
    {
        title: qsTr("Summary Plots")

        CheckBox { name: "boxPlot"; label: qsTr("Box plot"); enabled: inputRawData.checked; info: qsTr("Box plot of the dependent variable across groups. Requires raw data.") }
        CheckBox { name: "varRatioPlot"; label: qsTr("Variance ratio plot (2 groups)"); info: qsTr("Plot of the variance ratio with confidence interval (F-test based, 2 groups only).") }
        CheckBox { name: "varEstimatePlot"; label: qsTr("Variance estimate plot"); info: qsTr("Plot of the variance estimates with confidence intervals.") }

        CheckBox
        {
            name: "rainCloudPlot"
            label: qsTr("Raincloud plot (demeaned)")
            enabled: inputRawData.checked
            info: qsTr("Raincloud plot of the group-demeaned values: each group is centered at mean 0 while its variance is preserved, so the plot compares the spread of the groups. Requires raw data.")
            CheckBox { name: "rainCloudPlotHorizontal"; label: qsTr("Horizontal display"); info: qsTr("Changes the orientation of the raincloud plot.") }
        }
    }

}