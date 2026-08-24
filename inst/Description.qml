import QtQuick
import JASP.Module

Description
{
	name		: "jaspClassicalTests"
	title		: qsTr("Classical Tests")
	description	: qsTr("Module that bundles hypothesis tests.")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon        : "ribbon-classical-tests.svg" // Located in /inst/icons/
	preloadData: true
	requiresData: true

	GroupTitle
	{
		title: qsTr("Means")
        icon: "param-mu-grey.svg"
	}

	Analysis
	{
		title: qsTr("One Mean") 
		menu: qsTr("One Mean")  
		func: "oneSampleTests"
		qml: "oneSampleTests.qml"
		requiresData: true                
	}
	

	Analysis
	{
		title: qsTr("Two Independent Means") 
		menu: qsTr("Two Independent Means")  
		func: "independentSamplesTests"
		qml: "independentSamplesTests.qml"
		requiresData: true                
	}

	Analysis
	{
		title: qsTr("Two Dependent Means") 
		menu: qsTr("Two Dependent Means")  
		func: "pairedSamplesTests"
		qml: "pairedSamplesTests.qml"
		requiresData: true                
	}

	Separator{}

	GroupTitle
	{
		title: qsTr("Proportions")
        icon: "param-pi-grey.svg"
	}

	Analysis
	{
		title: qsTr("One Proportion") 
		menu: qsTr("One Proportion")  
		func: "singleProportion"
		qml: "singleProportion.qml"
		requiresData: true                
	}

	Analysis
	{
		title: qsTr("Two Proportions") 
		menu: qsTr("Two Proportions")  
		func: "twoProportions"
		qml: "twoProportions.qml"
		requiresData: true
	}

	Analysis
	{
		title: qsTr("> 2 Proportions") 
		menu: qsTr("> 2 Proportions")  
		func: "multipleProportions"
		qml: "multipleProportions.qml"
		requiresData: true
	}

	Separator{}

	GroupTitle
	{
		title: qsTr("Rates")
        icon: "param-lambda-grey.svg"
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
        icon: "param-sigma2-grey.svg"
	}

	Analysis
	{
		title: qsTr("One Variance")
		menu: qsTr("One Variance")
		func: "singleVariance"
		qml: "singleVariance.qml"
		requiresData: false
	}

	Analysis
	{
		title: qsTr("Multiple Variances")
		menu: qsTr("Multiple Variances")
		func: "multipleVariances"
		qml: "multipleVariances.qml"
		requiresData: false
	}

	Separator{}

	GroupTitle
	{
		title: qsTr("Correlations")
        icon: "param-rho-grey.svg"
	}

	Analysis
	{
		title: qsTr("One Correlation") 
		menu: qsTr("One Correlation")  
		func: "oneCorrelation"
		qml: "oneCorrelation.qml"
		requiresData: true
	}

	Analysis
	{
		title: qsTr("Two Correlations") 
		menu: qsTr("Two Correlations")  
		func: "twoCorrelations"
		qml: "twoCorrelations.qml"
		requiresData: true
	}

	Analysis
	{
		title: qsTr("> 2 Correlations") 
		menu: qsTr("> 2 Correlations")  
		func: "multipleCorrelations"
		qml: "multipleCorrelations.qml"
		requiresData: true
	}
}
