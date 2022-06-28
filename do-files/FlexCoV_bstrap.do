*******************************************
*  preliminary analysis                   *
*          						          *      
*                                         *
*******************************************
	
	version 15
	
	clear all
	
	set more off

	set seed 99932312
	
	use "./data_out/FlexCov.dta", clear
	
*******************************
****	Sample definitions	***
*******************************

	// Restrict to Table 2 analysis sample
	qui reg ln_daily_cum_deaths_million FLXMON_std ln_gdp MaskWearing_mar Fear_mar ///
		if date == 20201031
	keep if e(sample)
	

	// Mediation in terms of indirect effect
	capture program drop mediation_masks
	program mediation_masks, rclass
	regress MaskWearing_mar FLXMON_std ln_gdp
	scalar b1 = _b[FLXMON_std]
	regress ln_daily_cum_deaths_million MaskWearing_mar FLXMON_std ln_gdp
	return scalar indeff = b1 * _b[MaskWearing_mar]
	end

	// Full sample
	mediation_masks
	return list
	regress ln_daily_cum_deaths_million FLXMON_std ln_gdp
	regress ln_daily_cum_deaths_million MaskWearing_mar FLXMON_std ln_gdp


	// Bootstrapping w/ 999 replications
	bootstrap r(indeff), ///
		reps(999) dots(100) saving(./data_out/bootstrap_masks.dta, replace): ///
	 	mediation_masks
	estat bootstrap, percentile bc
	preserve
	use ./data_out/bootstrap_masks.dta, clear
	// Sort and identify limits of 95/90%-CIs using percentile method
	sort _bs_1
	list in 25
	list in 975
	list in 50
	list in 950
	restore

	
	// Repeat for mediation by fear
	capture program drop mediation_fear
	program mediation_fear, rclass
	regress Fear_mar FLXMON_std ln_gdp
	scalar b1 = _b[FLXMON_std]
	regress ln_daily_cum_deaths_million Fear_mar FLXMON_std ln_gdp
	return scalar indeff = b1 * _b[Fear_mar]
	end 

	// Full sample
	mediation_fear
	return list
	regress ln_daily_cum_deaths_million FLXMON_std ln_gdp
	regress ln_daily_cum_deaths_million Fear_mar FLXMON_std ln_gdp


	bootstrap r(indeff), ///
	 reps(999) dots(100) saving(./data_out/bootstrap_fear.dta, replace): ///
	 mediation_fear
	estat bootstrap, percentile bc
	preserve
	use ./data_out/bootstrap_fear.dta, clear
	sort _bs_1
	list in 25
	list in 975
	list in 50
	list in 950
	restore


	// Repeat for simultaneous mediation
	capture program drop mediation_both
	program mediation_both, rclass
	
	regress Fear_mar FLXMON_std ln_gdp
	scalar b_fear = _b[FLXMON_std]
	
	regress MaskWearing_mar FLXMON_std ln_gdp
	scalar b_masks = _b[FLXMON_std]

	regress ln_daily_cum_deaths_million Fear_mar MaskWearing_mar FLXMON_std ln_gdp
	return scalar indeff_fear = b_fear * _b[Fear_mar]
	return scalar indeff_masks = b_masks * _b[MaskWearing_mar]
	return scalar indeff_tot =  b_fear * _b[Fear_mar] + b_masks * _b[MaskWearing_mar]
	scalar flex_full = _b[FLXMON_std]

	regress ln_daily_cum_deaths_million FLXMON_std ln_gdp
	scalar flex_red = _b[FLXMON_std]
	return scalar reduction = flex_red - flex_full

	end 

	// Full sample
	mediation_both
	return list
	regress ln_daily_cum_deaths_million FLXMON_std ln_gdp
	regress ln_daily_cum_deaths_million MaskWearing_mar Fear_mar FLXMON_std ln_gdp

	bootstrap r(indeff_fear) r(indeff_masks) r(indeff_tot) r(reduction), ///
	 reps(999) dots(100) saving(./data_out/bootstrap_both.dta, replace): ///
	 mediation_both
	estat bootstrap, percentile bc
	preserve
	use ./data_out/bootstrap_both.dta, clear
	forv i = 1/4 {
		if `i' == 1 display in white "--> Ind. eff. fear"
		if `i' == 2 display in white "--> Ind. eff. masks"
		if `i' == 3 display in white "--> Ind. eff. total"
		if `i' == 4 display in white "--> Reduction"

		sort _bs_`i'
		list  _bs_`i' in 25
		list  _bs_`i' in 975
		list  _bs_`i' in 50
		list  _bs_`i' in 950
	}
	restore



exit