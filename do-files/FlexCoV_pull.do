*************************************
* pull file		           		    *
* 								    *
*									*
*									*
*************************************
	
	version 15
	
	clear all
	
	set more off
	
	
	***************************
	*** Dependent variables ***
	***************************
	
	* Daily cumulative number of confirmed cases
	* Daily cumulative number of confirmed cases (per million people)
	* Daily confirmed cases
	* Daily confirmed cases (per million people)
	
	* Daily cumulative number of confirmed deaths
	* Daily cumulative number of confirmed deaths (per million people)
	* Daily confirmed deaths
	* Daily confirmed deaths (per million people) */
	
	clear all
	
	use "data_out/owid_covid.dta", clear
	
	keep date iso_year_week iso_code total_cases new_cases total_deaths ///
		new_deaths total_cases_per_million new_cases_per_million ///
		total_deaths_per_million new_deaths_per_million
		  
	rename iso_code country_iso3
	
	rename total_cases daily_cum_cases 
	rename total_cases_per_million daily_cum_cases_million 
	rename new_cases daily_cases
	rename new_cases_per_million daily_cases_million
	
	rename total_deaths daily_cum_deaths
	rename total_deaths_per_million daily_cum_deaths_million
	rename new_deaths daily_deaths
	rename new_deaths_per_million daily_deaths_million
	 
	lab var daily_cum_cases "Daily cumulative number of confirmed cases"
	lab var daily_cum_cases_million "Daily cumulative number of confirmed cases (per million people)"
	lab var daily_cases "Daily confirmed cases"
	lab var daily_cases_million "Daily confirmed cases (per million people)"

	lab var daily_cum_deaths "Daily cumulative number of confirmed deaths"
	lab var daily_cum_deaths_million "Daily cumulative number of confirmed deaths (per million people)"
	lab var daily_deaths "Daily confirmed deaths"
	lab var daily_deaths_million "Daily confirmed deaths (per million people)"
	
	rename iso_year_week week
	destring week, ignore("_") replace
	destring date, ignore("_") replace
	
	tempfile FlexCoV_DepVars
	save `FlexCoV_DepVars', replace

	
	*************************************
	***  National cultural indicators ***
	*************************************
	
	*** Individualism (vs. Collectivism)
	*** Flexibility (vs. Monumentalism)
	*net install usespss, replace from(http://radyakin.org/transfer/usespss/beta)
	
	clear all

	usespss "data/IDVCOLLMONFLX.sav"
	
	drop if IDVCOLL == . & FLXMON == .
	
	//create country identifier
	gen country_iso3 = "."
	replace country_iso3 = "ARG" if countryname == "Argentina"
	replace country_iso3 = "AUS" if countryname == "Australia"
	replace country_iso3 = "AUT" if countryname == "Austria"
	replace country_iso3 = "BEL" if countryname == "Belgium"
	replace country_iso3 = "BRA" if countryname == "Brazil"
	replace country_iso3 = "CAN" if countryname == "Canada"
	replace country_iso3 = "CHL" if countryname == "Chile"
	replace country_iso3 = "CHN" if countryname == "China"
	replace country_iso3 = "COL" if countryname == "Colombia"
	replace country_iso3 = "CZE" if countryname == "CzechR"
	replace country_iso3 = "DNK" if countryname == "Denmark"
	replace country_iso3 = "DOM" if countryname == "DominicanR"
	replace country_iso3 = "EGY" if countryname == "Egypt"
	replace country_iso3 = "FIN" if countryname == "Finland"
	replace country_iso3 = "FRA" if countryname == "France"
	replace country_iso3 = "DEU" if countryname == "Germany"
	replace country_iso3 = "GRC" if countryname == "Greece"
	replace country_iso3 = "HKG" if countryname == "HongKong"
	replace country_iso3 = "HUN" if countryname == "Hungary"
	replace country_iso3 = "IND" if countryname == "India"
	replace country_iso3 = "IDN" if countryname == "Indonesia"
	replace country_iso3 = "IRL" if countryname == "Ireland"
	replace country_iso3 = "ISR" if countryname == "Israel"
	replace country_iso3 = "ITA" if countryname == "Italy"
	replace country_iso3 = "JPN" if countryname == "Japan"
	replace country_iso3 = "KAZ" if countryname == "Kazakhstan"
	replace country_iso3 = "KEN" if countryname == "Kenya"
	replace country_iso3 = "KOR" if countryname == "Korea"
	replace country_iso3 = "MYS" if countryname == "Malaysia"
	replace country_iso3 = "MEX" if countryname == "Mexico"
	replace country_iso3 = "MMR" if countryname == "Myanmar"
	replace country_iso3 = "NLD" if countryname == "Netherlands"
	replace country_iso3 = "NZL" if countryname == "NZealand"
	replace country_iso3 = "NGA" if countryname == "Nigeria"
	replace country_iso3 = "NOR" if countryname == "Norway"
	replace country_iso3 = "PER" if countryname == "Peru"
	replace country_iso3 = "PHL" if countryname == "Philippines"
	replace country_iso3 = "POL" if countryname == "Poland"
	replace country_iso3 = "PRT" if countryname == "Portugal"
	replace country_iso3 = "PRI" if countryname == "PuertoRico"
	replace country_iso3 = "ROU" if countryname == "Romania"
	replace country_iso3 = "RUS" if countryname == "Russia"
	replace country_iso3 = "SRB" if countryname == "Serbia"
	replace country_iso3 = "SGP" if countryname == "Singapore"
	replace country_iso3 = "ZAF" if countryname == "SouthAfrica"
	replace country_iso3 = "ESP" if countryname == "Spain"
	replace country_iso3 = "SWE" if countryname == "Sweden"
	replace country_iso3 = "CHE" if countryname == "Switzerland"
	replace country_iso3 = "TWN" if countryname == "Taiwan"
	replace country_iso3 = "THA" if countryname == "Thailand"
	replace country_iso3 = "TUR" if countryname == "Turkey"
	replace country_iso3 = "GBR" if countryname == "UK"
	replace country_iso3 = "UKR" if countryname == "Ukraine"
	replace country_iso3 = "USA" if countryname == "US"
	replace country_iso3 = "VEN" if countryname == "Venezuela"
	replace country_iso3 = "VNM" if countryname == "Vietnam"
	
	lab var IDVCOLL "Individualism (vs. Collectivism)"
	lab var FLXMON "Flexibility (vs. Monumentalism)"
	
	drop countryname countabbrev
	
	tempfile IDVCOLL_MONFLX
	save `IDVCOLL_MONFLX', replace
	
	** Gelfand et al. 2021: Tightness (vs. Looseness)
	
	clear all

	import excel using "data/Tightness_Scores.xlsx", sheet("Tightness_Scores") firstrow
	
	keep Country Tightness
	
	//create country identifier
	gen country_iso3 = "."
	replace country_iso3 = "DZA" if Country == "Algeria"
	replace country_iso3 = "ARG" if Country == "Argentina"
	replace country_iso3 = "ARM" if Country == "Armenia"
	replace country_iso3 = "AUS" if Country == "Australia"
	replace country_iso3 = "AUT" if Country == "Austria"
	replace country_iso3 = "BIH" if Country == "Bosnia and Herzegovina"
	replace country_iso3 = "BWA" if Country == "Botswana"
	replace country_iso3 = "BRA" if Country == "Brazil"
	replace country_iso3 = "CAN" if Country == "Canada"
	replace country_iso3 = "CHL" if Country == "Chile"
	replace country_iso3 = "CHN" if Country == "China"
	replace country_iso3 = "COL" if Country == "Colombia"
	replace country_iso3 = "CZE" if Country == "Czech Republic"
	replace country_iso3 = "ECU" if Country == "Ecuador"
	replace country_iso3 = "EST" if Country == "Estonia"
	replace country_iso3 = "FIN" if Country == "Finland"
	replace country_iso3 = "DEU" if Country == "Germany"
	replace country_iso3 = "GHA" if Country == "Ghana"
	replace country_iso3 = "GRC" if Country == "Greece"
	replace country_iso3 = "HUN" if Country == "Hungary"
	replace country_iso3 = "ISL" if Country == "Iceland"
	replace country_iso3 = "IND" if Country == "India"
	replace country_iso3 = "IDN" if Country == "Indonesia"
	replace country_iso3 = "IRN" if Country == "Iran"
	replace country_iso3 = "IRL" if Country == "Ireland"
	replace country_iso3 = "ISR" if Country == "Israel"
	replace country_iso3 = "ITA" if Country == "Italy"
	replace country_iso3 = "CIV" if Country == "Ivory Coast"
	replace country_iso3 = "JPN" if Country == "Japan"
	replace country_iso3 = "KAZ" if Country == "Kazakhstan"
	replace country_iso3 = "KEN" if Country == "Kenya"
	replace country_iso3 = "LVA" if Country == "Latvia"
	replace country_iso3 = "MYS" if Country == "Malaysia"
	replace country_iso3 = "MEX" if Country == "Mexico"
	replace country_iso3 = "MOZ" if Country == "Mozambique"
	replace country_iso3 = "NLD" if Country == "Netherlands"
	replace country_iso3 = "NGA" if Country == "Nigeria"
	replace country_iso3 = "PER" if Country == "Peru"
	replace country_iso3 = "POL" if Country == "Poland"
	replace country_iso3 = "PRT" if Country == "Portugal"
	replace country_iso3 = "QAT" if Country == "Qatar"
	replace country_iso3 = "RUS" if Country == "Russia"
	replace country_iso3 = "SAU" if Country == "Saudi Arabia"
	replace country_iso3 = "SGP" if Country == "Singapore"
	replace country_iso3 = "SVK" if Country == "Slovakia"
	replace country_iso3 = "KOR" if Country == "South Korea"
	replace country_iso3 = "ESP" if Country == "Spain"
	replace country_iso3 = "LKA" if Country == "Sri Lanka"
	replace country_iso3 = "SWE" if Country == "Sweden"
	replace country_iso3 = "THA" if Country == "Thailand"
	replace country_iso3 = "TTO" if Country == "Trinidad and Tobago"
	replace country_iso3 = "TUR" if Country == "Turkey"
	replace country_iso3 = "ARE" if Country == "UAE"
	replace country_iso3 = "UKR" if Country == "Ukraine"
	replace country_iso3 = "GBR" if Country == "United Kingdom"
	replace country_iso3 = "USA" if Country == "United States"
	replace country_iso3 = "VNM" if Country == "Vietnam"
	
	lab var Tightness "Tightness (vs. Looseness)"
	
	drop Country
	
	tempfile tightness
	save `tightness', replace

	
	*****************************
	*** Political Instiutions ***
	*****************************
	
	** Liberal Democracy Index, 2020
	clear all
	
	use "data/V-Dem-CY-Core-v11.1.dta", clear 

	keep country_text_id year v2x_libdem 
		
	keep if year == 2020
	drop year
	
	rename country_text_id country_iso3
	
	lab var v2x_libdem "Liberal Democracy Index"

	tempfile VDem
	save `VDem', replace
	
	
	*************************************
	***  		Mediators		 	 ***
	*************************************
		
	** Mask wearing prevalence
	** Weekly average mask wearing prevalence
	//note: most weekly averages consist of only one time of measurement
	
	clear all

	use "data_out/YouGov_WeeklyMaskWearing"
	
	keep DateTime country percent iso_year_week mean
	
	rename mean WeeklyMaskWearing
	rename percent MaskWearing
	rename DateTime date
	rename iso_year_week week
	
	lab var WeeklyMaskWearing "Weekly average mask wearing prevalence"
	lab var MaskWearing "Mask wearing prevalence"
	
	destring week, ignore("_") replace
	destring date, ignore("_") replace

	//create country identifier
	gen country_iso3 = "."
	replace country_iso3 = "AUS" if country == 1
	replace country_iso3 = "CAN" if country == 2
	replace country_iso3 = "CHN" if country == 3
	replace country_iso3 = "DNK" if country == 4
	replace country_iso3 = "FIN" if country == 5
	replace country_iso3 = "FRA" if country == 6
	replace country_iso3 = "DEU" if country == 7
	replace country_iso3 = "HKG" if country == 8
	replace country_iso3 = "IND" if country == 9
	replace country_iso3 = "IDN" if country == 10
	replace country_iso3 = "ITA" if country == 11
	replace country_iso3 = "JPN" if country == 12
	replace country_iso3 = "MYS" if country == 13
	replace country_iso3 = "MEX" if country == 14
	replace country_iso3 = "NOR" if country == 15
	replace country_iso3 = "PHL" if country == 16
	replace country_iso3 = "SAU" if country == 17
	replace country_iso3 = "SGP" if country == 18
	replace country_iso3 = "ESP" if country == 19
	replace country_iso3 = "SWE" if country == 20
	replace country_iso3 = "TWN" if country == 21
	replace country_iso3 = "THA" if country == 22
	replace country_iso3 = "UAE" if country == 23
	replace country_iso3 = "GBR" if country == 24
	replace country_iso3 = "USA" if country == 25
	replace country_iso3 = "VNM" if country == 26
	
	drop country
	
	tempfile WeeklyMaskWearing
	save `WeeklyMaskWearing', replace
	
	** Fear of catching COVID-19
	** Weekly average fear of catching COVID-19 
	// note: most weekly averages consist of only one time of measurement 
	
	clear all

	use "data_out/YouGov_WeeklyFear"
	
	keep DateTime country percent iso_year_week mean
	
	rename mean WeeklyFear
	rename percent Fear
	rename DateTime date
	rename iso_year_week week
	
	lab var WeeklyFear "Weekly average fear of catching COVID-19"
	lab var Fear "Fear of catching COVID-19"
	
	destring week, ignore("_") replace
	destring date, ignore("_") replace
	
	//create country identifier
	gen country_iso3 = "."
	replace country_iso3 = "AUS" if country == 1
	replace country_iso3 = "BRA" if country == 2
	replace country_iso3 = "CAN" if country == 3
	replace country_iso3 = "CHN" if country == 4
	replace country_iso3 = "DNK" if country == 5
	replace country_iso3 = "FIN" if country == 6
	replace country_iso3 = "FRA" if country == 7
	replace country_iso3 = "DEU" if country == 8
	replace country_iso3 = "HKG" if country == 9
	replace country_iso3 = "IND" if country == 10
	replace country_iso3 = "IDN" if country == 11
	replace country_iso3 = "ITA" if country == 12
	replace country_iso3 = "JPN" if country == 13
	replace country_iso3 = "MYS" if country == 14
	replace country_iso3 = "MEX" if country == 15
	replace country_iso3 = "NLD" if country == 16
	replace country_iso3 = "NOR" if country == 17
	replace country_iso3 = "PHL" if country == 18
	replace country_iso3 = "SAU" if country == 19
	replace country_iso3 = "SGP" if country == 20
	replace country_iso3 = "KOR" if country == 21
	replace country_iso3 = "ESP" if country == 22
	replace country_iso3 = "SWE" if country == 23
	replace country_iso3 = "TWN" if country == 24
	replace country_iso3 = "THA" if country == 25
	replace country_iso3 = "UAE" if country == 26
	replace country_iso3 = "GBR" if country == 27
	replace country_iso3 = "USA" if country == 28
	replace country_iso3 = "VNM" if country == 29
	
	drop country
	
	tempfile WeeklyFear
	save `WeeklyFear', replace
	
	************************
	***	   Covariates	 ***
	************************
	
	*** GDP per capita per capita (current US$) in 2019***
	clear all
	
	import excel using "data/GDP.xls", sheet("Data") cellrange(A4:BM268) firstrow
	
	keep CountryCode BL 
	
	rename CountryCode country_iso3
	rename BL GDP
	
	lab var GDP "GDP per capita"
	
	/*//Worldbank does not provide any data for Taiwan. For this reason 
	  this data has to be put in manually (data for Taiwan retrieved from:
	  https://eng.dgbas.gov.tw/public/data/dgbas03/bs2/yearbook_eng/Yearbook2020.pdf, p.77)*/
	input
	"TWN" 25941
	end
	
	tempfile GDP
	save `GDP', replace
	
	** People per sq. km of land area
	** Median age
	** Hospital beds per 1,000 people
	clear all 
	
	import excel using "data/OwidCovData.xlsx", sheet("Sheet1") firstrow
	
	keep iso_code date population_density median_age hospital_beds_per_thousand 
	
	//keep only one row per country since value doesn't change over time
	egen group = group(iso_code)
	sort group iso_code date
	by group: gen n = _n
	keep if n == 1
	
	drop n date group
	
	rename iso_code country_iso3
	
	lab var population_density "People per sq. km of land area" 
	lab var median_age "Median age"
	lab var hospital_beds_per_thousand "Hospital beds per 1,000 people"
	
	tempfile OWID
	save `OWID', replace
		
*************************************
****** merge all information ********
*************************************

	***************************
	*** Dependent variables ***
	***************************
	
	use `FlexCoV_DepVars', clear
	
	************************************
	*** National cultural indicators ***
	************************************

	// Individualism (vs. Collectivism)
	// Flexibility (vs. Monumentalism)
	merge m:1 country_iso3 using `IDVCOLL_MONFLX', nogenerate keep (1 2 3)
	
	// Gelfand et al. 2021: Tightness (vs. Looseness)
	merge m:1 country_iso3 using `tightness', nogenerate keep (1 2 3)
	
	******************************
	*** Political institutions ***
	******************************
	
	// Liberal Democracy Index
	merge m:1 country_iso3 using `VDem',  nogenerate keep (1 2 3)
	
	******************************
	*** 	  Mediators	       ***
	******************************
	
	// Weekly average mask wearing prevalence, YouGov
	merge 1:1 country_iso3 date using `WeeklyMaskWearing', nogenerate keep (1 2 3)
	
	// Weekly average fear of catching the disease, YouGov
	merge 1:1 country_iso3 date using `WeeklyFear', nogenerate keep (1 2 3)
	
	************************
	*** Covariates       ***
	************************

	// People per sq. km of land area
	// Median age
	// Hospital beds per 1,000 people
	merge m:1 country_iso3 using `OWID', nogenerate keep (1 2 3)
	
	// GDP per capita in 2019
	merge m:1 country_iso3 using `GDP', nogenerate keep (1 2 3)

	   
*********************************************************************************************	
	
	sort country_iso3 date week
	
	compress
	
	save "data_out/FlexCov_pull", replace
