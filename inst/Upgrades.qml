import QtQuick
import JASP.Module

Upgrades
{
	Upgrade
	{
		functionName:	"singleVariance"
		fromVersion:	"0.1.0"
		toVersion:		"0.1.1"

		// chi-square test table is now always shown, so the checkbox is removed
		ChangeRemove {	name: "chiSquareTest"	}
	}
}
