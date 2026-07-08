import QtQuick
import JASP.Module

Description
{
	name		: "jaspHypothesisTests"
	title		: qsTr("Hypothesis Tests")
	description	: qsTr("Module that bundles hypothesis tests.")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon        : "exampleIcon.png" // Located in /inst/icons/
	preloadData: true
	requiresData: true

	GroupTitle
	{
		title: qsTr("Means")
	}

	Analysis
	{
		title: qsTr("One Mean") // Title for window
		menu: qsTr("One Mean")  // Title for ribbon
		func: "oneSampleTests"           // Function to be called
		qml: "oneSampleTests.qml"               // Design input window
		requiresData: true                
	}
	

	Analysis
	{
		title: qsTr("Two Independent Means") // Title for window
		menu: qsTr("Two Independent Means")  // Title for ribbon
		func: "independentSamplesTests"           // Function to be called
		qml: "independentSamplesTests.qml"               // Design input window
		requiresData: true                
	}

	Analysis
	{
		title: qsTr("Two Dependent Means") // Title for window
		menu: qsTr("Two Dependent Means")  // Title for ribbon
		func: "pairedSamplesTests"           // Function to be called
		qml: "pairedSamplesTests.qml"               // Design input window
		requiresData: true                
	}

	Separator{}

	GroupTitle
	{
		title: qsTr("Proportions")
	}

	Analysis
	{
		title: qsTr("One Proportion") // Title for window
		menu: qsTr("One Proportion")  // Title for ribbon
		func: "singleProportion"           // Function to be called
		qml: "singleProportion.qml"               // Design input window
		requiresData: true                
	}

	Analysis
	{
		title: qsTr("Multiple Proportions") // Title for window
		menu: qsTr("Multiple Proportions")  // Title for ribbon
		func: "multipleProportions"           // Function to be called
		qml: "multipleProportions.qml"               // Design input window
		requiresData: true                
	}

	Separator{}

	GroupTitle
	{
		title: qsTr("Rates")
	}

	Analysis
	{
		title:        qsTr("One Rate")
		menu:         qsTr("One Rate")
		func:         "oneSamplePoissonRate"
		qml:          "oneSamplePoissonRate.qml"
		requiresData: false
	}

	Analysis
	{
		title:        qsTr("Two Rates")
		menu:         qsTr("Two Rates")
		func:         "twoSamplePoissonRate"
		qml:          "twoSamplePoissonRate.qml"
		requiresData: false
	}

	Separator{}

	GroupTitle
	{
		title: qsTr("Variances")
	}

	Analysis
	{
		title: qsTr("One Variance") // Title for window
		menu: qsTr("One Variance")  // Title for ribbon
		func: "singleVariance"           // Function to be called
		qml: "singleVariance.qml"               // Design input window
		requiresData: true                
	}

	Analysis
	{
		title: qsTr("Multiple Variances") // Title for window
		menu: qsTr("Multiple Variances")  // Title for ribbon
		func: "multipleVariances"           // Function to be called
		qml: "multipleVariances.qml"               // Design input window
		requiresData: true                
	}
}
