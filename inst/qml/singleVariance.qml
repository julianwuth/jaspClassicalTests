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
    // TODO add info here for help file

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
		AssignedVariablesList { name: "dependent"; title: qsTr("Variables"); info: qsTr("In this box the dependent variable is selected.") ; allowedColumns: ["scale"]; minNumericLevels: 2 }
	}

    Group
    {
        title:   qsTr("Summarized data")
        enabled: inputSummarized.checked
        columns: 2

        DoubleField
        {
            name:         "sampleVariance"
            label:        qsTr("Sample variance")
            defaultValue: 1
            min:          0
            inclusive:    JASP.MaxOnly
            decimals:     3
            info:         qsTr("The observed sample variance.")
        }

        IntegerField
        {
            name:         "sampleSize"
            label:        qsTr("Sample size")
            defaultValue: 2
            min:          2
            info:         qsTr("The number of observations the sample variance is based on.")
        }
    }

    Group 
    {
        title: qsTr("Tests")

        CheckBox 
        {
            label: qsTr("χ² test")
            name: "chiSquareTest"
            checked: true
        }
        
        DoubleField
        {
            label: qsTr("Test value:")
            name: "testVariance"
            defaultValue: 1
            decimals: 3
            inclusive: JASP.MaxOnly
        }
    }

    RadioButtonGroup
	{
		name: "alternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton 
        { 
            value: "two.sided" // to fit the input pattern of the underlying R package		
            label: qsTr("≠ Test value"); 
            info: qsTr("Two sided alternative hypothesis that the sample variance is not equal to the test value. Selected by default."); checked: true	
        }
		RadioButton 
        { 
            value: "greater"	
            label: qsTr("> Test value")	; 
            info: qsTr("One sided alternative hypothesis that the sample variance is greater than the test value.")				
        }
		RadioButton 
        { 
            value: "less"		
            label: qsTr("< Test value")	; 
            info: qsTr("One sided alternative hypothesis that the sample variance is less than the test value.")				
        }
	}

    Group
	{
		title: qsTr("Additional Statistics")

        CheckBox
        {
            label: qsTr("Sample variance")
            name: "varEstimate"

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
                info: qsTr("Method for the confidence interval. Bonett's method needs the raw observations (it relies on the sample kurtosis), so with summarized data only the χ² interval is available and the method cannot be changed.")
                RadioButton { value: "chiSquare"; label: qsTr("χ²") ; checked: true }
                RadioButton { value: "bonett"; label: qsTr("Bonett") }
            }
        }

        CheckBox
        {
            label: qsTr("Standard deviation")
            name: "sdEstimate"
        }
        
    }

    Group
	{
		title: qsTr("Assumption checks")
		enabled: inputRawData.checked
		CheckBox { name: "normalityTest"; label: qsTr("Normality"); info: qsTr("Shapiro-Wilk test of normality.") }
		CheckBox { name: "qqPlot";		 	label: qsTr("Q-Q plot residuals"); info: qsTr("Q-Q plot of the standardized residuals.") }
	}

}