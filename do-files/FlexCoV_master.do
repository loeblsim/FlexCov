*************************************
* master file           		    *
* 								    *
*									*
*									*
*************************************

	set more off

	clear all

	if "`c(username)'" == "Simon" {
		cd "C:\Users\Simon\Nextcloud\Shared\COVID-19 Shared Folder\analysis\FlexCov"
	}
	if "`c(username)'" == "heisig" {
		cd "C:\Users\heisig\Nextcloud\Shared\COVID-19 Shared Folder\analysis\FlexCov"
	}
	
	* "cd YOURPATH"


****************
**** Data   ****
****************
	*** pulling variables relevant for analysis 
	do "do-files/FlexCov_pull.do"
	
	*** sample restrictions & recoding
	do "do-files/FlexCov_recode.do"
	
	*** analyses
	do "do-files/FlexCov_analyses.do"

	*** bootstrapping to test for mediation by mask wearing and fear
	do "do-files/FlexCov_bstrap.do"

	*** leave-1-out analysis for influential cases 
	do "do-files/FlexCov_outliers.do"

