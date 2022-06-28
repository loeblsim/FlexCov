*************************************
*  sample restrictions				*
*		  and						*
*	  recode file		            *
* 								    *
*									*
*									*
*************************************
	
	version 15
	
	clear all
	
	set more off
	
	use "data_out/FlexCov_pull"

*************************************
****** Sample restrictions	 ********
*************************************	
	
	*** I. Keep countries only ***
	
	// - drop regions that are not considered as a country
	// - Kosovo (XRX, XKX) is missing from the list (status unclear anyway)
	// - status unclear for the Palestenian Territories (PSE, PSG) and Kosovo (dropped)
	foreach no_country in AIA /// Anguilla
						  ALA /// Åland
						  ANT /// Netherlands Antilles
						  ARB /// Arab World
						  ASM /// American Samoa
						  ATF /// French Southern Territories
						  BES /// Bonaire Sint Eustatius and Saba
						  BLM /// Saint-​Barthélemy
						  BMU /// Bermuda
						  CCK /// Cocos (Keeling) Islands
						  CEB /// Central Europe and the Baltics
						  CHI /// Channel Islands
						  CIS /// Community of Independent States
						  COK /// Cook Islands
						  CSS /// Caribbean small states
						  CUW /// Curacao
						  CXR /// Christmas Island
						  CYM /// Cayman Islands
						  EAP /// East Asia & Pacific (excluding high income)
						  EAR /// Early-demographic dividend
						  EAS /// East Asia & Pacific
						  ECA /// Europe & Central Asia (excluding high income)
						  ECS /// Europe & Central Asia
						  EMU /// Euro area
						  ESH /// Western Sahara
						  EUU /// European Union
						  FCS /// Fragile and conflict affected situations
						  FLK /// Falkland Islands
						  FRO /// Faroe Islands
						  GGY /// Guernsey
						  GIB /// Gibraltar
						  GLP /// Guadeloupe
						  GRL /// Greenland
						  GUF /// French Guiana
						  GUM /// Guam
						  HIC /// High income
						  HKG /// Honk Kong
						  HMD /// Heard Island and McDonald Islands
						  HPC /// Heavily indebted poor countries (HIPC)
						  IBD /// IBRD only
						  IBT /// IDA & IBRD total
						  IDA /// IDA total
						  IDB /// IDA blend
						  IDX /// IDA only
						  IMN /// Isle of Man
						  INX /// Not classified
						  IOT /// British Indian Ocean Territory
						  JEY /// Jersey
						  LAC /// Latin America & Caribbean (excluding high income)
						  LCN /// Latin America & Caribbean
						  LDC /// Least developed countries: UN classification
						  LIC /// Low income
						  LMC /// Lower middle income
						  LMY /// Low & middle income
						  LTE /// Late-demographic dividend
						  MAC /// Macao SAR, China
						  MAF /// St. Martin (French part)
						  MEA /// Middle East & North Africa
						  MIC /// Middle income
						  MNA /// Middle East & North Africa (excluding high income)
						  MNP /// Northern Mariana Islands
						  MSR /// Montserrat
						  MTQ /// Martinique
						  MYT /// Mayotte
						  NAC /// North America
						  NCL /// New Caledonia
						  NFK /// Norfolk Island
						  NIU /// Niue
						  OED /// OECD members
						  OSS /// Other small states
						  PCN /// Pitcairn
						  PRE /// Pre-demographic dividend
						  PSE /// Palastine/West Bank
						  PSG /// Palastine/Gaza
						  PSS /// Pacific island small states
						  PST /// Post-demographic dividend
						  PYF /// French Polynesia
						  REU /// Réunion
						  SAS /// South Asia
						  SGS /// South Georgia and the South Sandwich Islands
						  SHN /// Saint Helena
						  SJM /// Svalbard and Jan Mayen
						  SML /// Somaliland
						  SPM /// Saint Pierre and Miquelon
						  SSA /// Sub-Saharan Africa (excluding high income)
						  SSF /// Sub-Saharan Africa
						  SST /// Small states
						  SXM /// Sint Maarten (Dutch part)
						  TCA /// Turks and Caicos Islands
						  TEA /// East Asia & Pacific (IDA & IBRD countries)
						  TEC /// Europe & Central Asia (IDA & IBRD countries)
						  TKL /// Tokelau 
						  TLA /// Latin America & the Caribbean (IDA & IBRD countries)
						  TMN /// Middle East & North Africa (IDA & IBRD countries)
						  TSA /// South Asia (IDA & IBRD)
						  TSS /// Sub-Saharan Africa (IDA & IBRD countries)
						  UMC /// Upper middle income
						  UMI /// United States Minor Outlying Islands
						  VGB /// British Virgin Islands
						  VIR /// Virgin Islands (U.S.)
						  WLF /// Wallis and Futuna
						  XKX /// Kosovo
						  OECD36 ///
						  OWID_WRL /// World
						  OWID_KOS /// Kosovo
						  OWID_AFR /// Africa
						  OWID_ASI /// Asia
						  OWID_CYN /// Northern Cyprus
						  OWID_EUN /// European Union
						  OWID_EUR /// Europe
						  OWID_INT /// International
						  OWID_NAM /// North America
						  OWID_OCE /// Oceania
						  OWID_SAM /// South America
						  OWID_LMC /// Lower middle income countries
						  OWID_HIC /// High income countries 
						  OWID_LIC /// Low income countries 
						  OWID_UMC /// Upper middle income countries
						  CARINFONET /// The Central Asian Republics Information Network
						  EU_AFTER_MAY2004 ///
						  EU_BEFORE_MAY2004 ///
						  EU_MEMBERS ///
						  NORDIC ///
						  SEEHN /// South-eastern Europe Health Network
						  SMALL ///
						  UMC ///
						  WHO_EURO ///
						  WLD /// World
						  ZZB /// Zanzibar
						  {
		
		drop if country_iso3 == "`no_country'"
	}
	
	*** II. drop countries due to poor vital statistics
	drop if inlist(country_iso3,"NGA", "KEN") // Nigeria, Kenya
		
***********************************
******		 Recoding	   ********
***********************************

	** Log. of dependent variables
	local dv "daily_cum_cases daily_cases daily_cum_deaths daily_deaths daily_cum_cases_million daily_cases_million daily_cum_deaths_million daily_deaths_million"
	
	foreach var in `dv' {	
	        gen ln_`var' = ln(`var')
			}
			
	foreach var in `dv'{
		local lbl: variable label `var'
		label var ln_`var' `"Log. `lbl'"'
}	
	
	*** Average mask wearing for selected time spans
	
	//create averages:
	
	//Average mask wearing prevalencefor for the whole time span (24.02. - 31.10.)
	//Average mask wearing prevalence in March
	//Average mask wearing prevalence in April
	//Average fear of catching the disease for the whole time span (24.02. - 31.10.)
	//Average fear of catching the disease in March
	//Average fear of catching the disease in April
	
	foreach var in Fear MaskWearing {
	    egen `var'_whole = mean(cond(inrange(date,20200224,20201031)),`var', .), by(country_iso3) 
		egen `var'_mar = mean(cond(inrange(date,20200301,20200331)),`var', .), by(country_iso3)
		egen `var'_apr = mean(cond(inrange(date,20200401,20200430)),`var', .), by(country_iso3)
	}
	
	lab var Fear_whole "Average fear of catching COVID-19 from 24.02. - 31.10."
	lab var Fear_mar "Average fear of catching COVID-19 in March"
	lab var Fear_apr "Average fear of catching COVID-19 in April"
	lab var MaskWearing_whole "Average mask wearing prevalence from 24.02. - 31.10."
	lab var MaskWearing_mar "Average mask wearing prevalence in March"
	lab var MaskWearing_apr "Average mask wearing prevalence in April"
	
	*** Log. pop density
	gen ln_PopDensity = ln(population_density)

	lab var ln_PopDensity "Log. Population density (people per sq. km of land area)"
	
	*** Log. GDP per capita in 2019
	gen ln_gdp = ln(GDP)
	
	lab var ln_gdp "Log. GDP per capita"

***********************************
******   Define main analysis sample and
******	standardize vars based on this one
****** We do NOT re-standardize for analyses of oiher samples
****** to ensure coefs can be compared across models
***********************************

	qui reg ln_daily_cum_deaths_million FLXMON Tightness IDVCOLL ///
			v2x_libdem ln_gdp ln_PopDensity hospital_beds_per_thousand ///
			median_age if date == 20201031, vce(hc3)
	gen tag=e(sample)

	foreach var in IDVCOLL FLXMON Tightness v2x_libdem { 
		sum `var' if tag == 1
		generate `var'_std = (`var' - r(mean)) / r(sd)
	}
	
	lab var IDVCOLL_std "Individualism (vs. Collectivism), standardized"
	lab var FLXMON_std "Flexibility (vs. Monumentalism), standardized"
	lab var Tightness_std "Tightness (vs. Looseness), standardized"
	lab var v2x_libdem_std "Liberal Democracy Index, standardized"


*******************************************************************************************
	compress
	
	save "data_out/FlexCov", replace     
