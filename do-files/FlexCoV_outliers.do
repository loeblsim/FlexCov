*************************************
* Delete-1 analysis (inflential cases)
*************************************

version 15
clear all
set more off


use "./data_out/FlexCov.dta", clear

// Restrict to analysis sample
reg ln_daily_cum_deaths_million FLXMON_std Tightness_std IDVCOLL_std ///
		v2x_libdem_std ln_gdp ln_PopDensity hospital_beds_per_thousand ///
		median_age if date == 20201031
keep if e(sample)

// We run if for all specifications in Table 1
// Model 4 is the one of primary interest

global m1 FLXMON_std

global m2 FLXMON_std IDVCOLL_std

global m3 FLXMON_std Tightness_std

global m4 FLXMON_std IDVCOLL_std v2x_libdem_std ///
		Tightness_std  ln_gdp

global m5 FLXMON_std IDVCOLL_std Tightness_std ///
		v2x_libdem_std ln_gdp ln_PopDensity hospital_beds_per_thousand ///
		median_age 

/////////////////////////////////////////////////////////////////////////////
// LEAVE-1-OUT
// We could use standard commands (dfbeta, predict ..., cook) for delete-1
// but let's write it ourselves and avoid error prone renaming of dfbetas
/////////////////////////////////////////////////////////////////////////////

// Postfile

tempfile results
tempname memhold
postfile `memhold' ///
	str3(iso) int(model) double(b_FLXMON_std se_FLXMON_std ///
		b_Tightness_std se_Tightness_std b_IDVCOLL_std se_IDVCOLL_std ///
		b_v2x_libdem_std se_v2x_libdem_std b_ln_gdp se_ln_gdp ///
		b_ln_PopDensity se_ln_PopDensity b_hospital_beds_per_thousand se_hospital_beds_per_thousand ///
		b_median_age se_median_age cooksd) int(N) ///
	using "./data_out/MainRegs-Delete-1.dta", replace

levelsof country_iso3, local(cntries)

//set trace on
qui forvalues spec = 1/5 {

	// Full-sample results
	qui reg ln_daily_cum_deaths_million ${m`spec'}, hc3

	predict res_full, resid
	scalar resvar = e(rmse)^2

	foreach var of varlist $m5 {
		cap scalar b_`var' = _b[`var']
		if _rc scalar b_`var' = .
		cap scalar se_`var' = _se[`var']
		if _rc scalar se_`var' = .
	}

		// Post full-sample results
	post `memhold' ///
		("FS") (`spec') ///
		(b_FLXMON_std) (se_FLXMON_std) ///
		(b_Tightness_std) (se_Tightness_std) (b_IDVCOLL_std) (se_IDVCOLL_std) ///
		(b_v2x_libdem_std) (se_v2x_libdem_std) (b_ln_gdp) (se_ln_gdp) ///
		(b_ln_PopDensity) (se_ln_PopDensity) (b_hospital_beds_per_thousand) (se_hospital_beds_per_thousand) ///
		(b_median_age) (se_median_age) ///
		(.) ///
		(e(N))


	foreach cntry of local cntries {
		
		qui reg ln_daily_cum_deaths_million ${m`spec'} if country_iso3 != "`cntry'", hc3
		estimates store reg_reduced

		// Compute Cook's D
		predict res_reduced, resid
		gen sqdev = (res_full - res_reduced)^2
		qui total sqdev

		scalar cooksd = _b[sqdev]/(2*resvar)
		
		estimates restore reg_reduced

		foreach var of varlist $m5 {
			cap scalar b_`var' = _b[`var']
			if _rc scalar b_`var' = .
			cap scalar se_`var' = _se[`var']
			if _rc scalar se_`var' = .
		}

		// Post full-sample results
		post `memhold' ///
			("`cntry'") (`spec') ///
			(b_FLXMON_std) (se_FLXMON_std) ///
			(b_Tightness_std) (se_Tightness_std) (b_IDVCOLL_std) (se_IDVCOLL_std) ///
			(b_v2x_libdem_std) (se_v2x_libdem_std) (b_ln_gdp) (se_ln_gdp) ///
			(b_ln_PopDensity) (se_ln_PopDensity) (b_hospital_beds_per_thousand) (se_hospital_beds_per_thousand) ///
			(b_median_age) (se_median_age) ///
			(cooksd) ///
			(e(N))


		drop res_reduced sqdev
		}

	drop res_full

}

postclose `memhold'


*************************************
* Plot results of delete-1 and delete-2 analysis
*************************************

version 15
clear
set more off



// Plot delete-1 results
// Appendix: Figure S6
//----------------

// Coefficient estimates

use "./data_out/MainRegs-Delete-1.dta", clear

// Generate DFBETA
// Tag full-sample estimate
gen tag = iso == "FS"

sort m tag
foreach pred in FLXMON_std Tightness_std IDVCOLL_std ///
		v2x_libdem_std  ln_gdp ///
		ln_PopDensity  hospital_beds_per_thousand ///
		median_age {

	by model (tag): gen dfb_`pred' = (b_`pred'[_N] - b_`pred')/se_`pred' if !tag
}

// Generate baseline for rspike 
gen baseline = 0

// Main graph for paper/appendix
// --> DFBETAs from Model 4

gen iso_bold = "{bf:" + iso + "}"

keep if model == 4

foreach pred in FLXMON_std IDVCOLL_std Tightness_std ///
		v2x_libdem_std ln_gdp {

		if "`pred'" == "FLXMON_std" local title A - Flexibility
		if "`pred'" == "IDVCOLL_std" local title B - Individualism
		if "`pred'" == "Tightness_std" local title C - Tightness
		if "`pred'" == "v2x_libdem_std" local title D - Liberal democracy index
		if "`pred'" == "ln_gdp"  local title E - Log GDP per capita

		egen axis = axis(dfb_`pred')
	
		// Calculate cutoff
	 	distinct iso
	 	local N = `r(ndistinct)' - 1 // subtract 1 because we have iso = "FS" for full sample
		local cutoff = 2/sqrt(`N')
		display `N'
		display `cutoff'

			tw ///
			  || rspike dfb_`pred' baseline axis, lcol(gs3) ///
			  || scatter dfb_`pred' axis ///
			 		, msym(O) mcol(pll2) msize(*1.25) /// 
			  || scatter dfb_`pred' axis if dfb_`pred' > .2 ///
			  		, msym(none) mlab(iso_bold) mlabpos(1) ///
			  		mlabangle(90) mlabsize(*.9) mlabgap(*1.25) ///
			  || scatter dfb_`pred' axis if dfb_`pred' < -.2 ///
			  		, msym(none) mlab(iso_bold) mlabpos(8) ///
			  		mlabangle(90) mlabsize(*.9) mlabgap(*1.5) ///
			  || if iso != "FS" ///
			  	, ylabel(-1 "{bf:-1}" -.75 "{bf:-.75}" -.5 "{bf:-.5}" -.25 "{bf:-.25}" 0 "{bf:0}" ///
			  			.25 "{bf:.25}" .5 "{bf:.5}" .75 "{bf:.75}" 1 "{bf:1}", labsize(*.95)) ///
			  	ylabel(-1.05 " " 1.05 " ", notick custom add) ///
			  	yline(-1 (.25) 1, lstyle(grid)) ///
			  	yline(0, lpat(solid) lcol(gs3%50) lw(*1.25)) ///
			  	yline(-1 -`cutoff' `cutoff' 1, lpat(solid) lcol(plr2%50) lw(*1.25)) ///
			  	ytitle("") ///
			  	xtitle("") ///
			  	xlabel(0 " " 38 " ") ///
			  	yscale(range(-1.05 1.05)) ///
			  	xline(0 (5) 30, lstyle(grid)) ///
			  	subtitle("{bf:`title'}") ///
			  	legend(off) ///
			  	scheme(plotplainblind) ///
			  	name(`pred', replace)

			drop axis

		}

graph combine FLXMON_std IDVCOLL_std Tightness_std ///
		v2x_libdem_std ln_gdp, rows(3)
graph display, xsize(9.6) ysize(6.2) 
gr export ./graphs/Appendix-Figure6.eps, replace

// Check regressions mentioned in text

use "data_out/FlexCov.dta", clear

estimates clear

reg ln_daily_cum_deaths_million FLXMON_std IDVCOLL_std Tightness_std  ///
		 v2x_libdem_std ln_gdp if date == 20201031, vce(hc3)
estimates store full

reg ln_daily_cum_deaths_million FLXMON_std IDVCOLL_std Tightness_std ///
		v2x_libdem_std  ln_gdp if country_iso3 != "VNM" & date == 20201031, vce(hc3)
estimates store woVNM

reg ln_daily_cum_deaths_million FLXMON_std IDVCOLL_std Tightness_std ///
		v2x_libdem_std  ln_gdp if country_iso3 != "IND" & date == 20201031, vce(hc3)
estimates store woIND

reg ln_daily_cum_deaths_million FLXMON_std IDVCOLL_std Tightness_std ///
		v2x_libdem_std  ln_gdp if !inlist(country_iso3, "VNM", "IND") & date == 20201031, vce(hc3)
estimates store woVNMIND

esttab *, b se mtitles(full woVNM woIND woVNMIND)


exit




exit