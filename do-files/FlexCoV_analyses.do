*******************************************
*  analyses                  			  *
*          						          *      
*                                         *
*******************************************
	
	version 15
	
	clear all
	
	set more off
	
	use "data_out/FlexCov", clear
	
*******************************
****	Sample definitions	***
*******************************
	
	//Sample #1, Main Model
	qui reg ln_daily_cum_deaths_million FLXMON Tightness IDVCOLL ///
		v2x_libdem ln_gdp ln_PopDensity hospital_beds_per_thousand ///
		median_age if date == 20201031, vce(hc3)
	cap drop sample1
	gen sample1=e(sample)

	//Sample #2, Mediator Model I
	qui reg ln_daily_cum_deaths_million FLXMON ln_gdp MaskWearing_whole ///
		Fear_whole if date == 20201031, vce(hc3)
	cap drop sample2
	gen sample2=e(sample)
	
	//Sample #3, Mediator Model II
	qui reg ln_daily_cum_deaths_million FLXMON ln_gdp v2x_libdem median_age ///
		Fear_mar MaskWearing_mar if date == 20201031, vce(hc3)
	cap drop sample3
	gen sample3=e(sample)
	
	//Sample #4, Mediator Model III
	qui reg ln_daily_cum_deaths_million FLXMON ln_gdp v2x_libdem median_age ///
		Fear_apr MaskWearing_apr if date == 20201031, vce(hc3)
	cap drop sample4
	gen sample4=e(sample)
	
	corr sample3 sample4
	
	drop sample4
	
	// Inspect logged and unlogged version of mortality
	tw 	kdensity daily_cum_deaths_million if sample1  ///
		, lcol(plr1) lw(*1.5) subtitle(Unlogged) name(unlogged, replace)

	tw kdensity ln_daily_cum_deaths_million if sample1 ///
		, lcol(plr1) lw(*1.5) subtitle(Logged) name(logged, replace)

	gr combine unlogged logged, rows(1)

	// Normality?
	swilk daily_cum_deaths_million ln_daily_cum_deaths_million if sample1
	// Both non-normal (but logged slightly less so)

	summarize daily_cum_deaths_million ln_daily_cum_deaths_million if sample1, detail
	// Skewness lower (in abs. terms) for logged version

	// Predictions for Table 3, Model 4, for effect size discussion
	reg ln_daily_cum_deaths_million FLXMON_std IDVCOLL_std v2x_libdem_std ///
		Tightness_std ln_gdp if date == 20201031 & sample1 == 1

	// To get predictions in original (unlogged) metric, we have to factor
	// in the error variance - assuming normality (which is of course 
	// somewhat inconsistent with using robust SEs of course, but we can 
	// tolerate some imprecision) the predicted value is given by
	// exp(xb + (\sigma^2)/2) or exp(xb) * exp((\sigma^2)/2) where 
	// with the model rmse used as an estimate of \sigma 
	// See, e.g., https://www.stata.com/stata-news/news34-2/spotlight/
	// Or 

	sum FLXMON_std IDVCOLL_std v2x_libdem_std Tightness_std ln_gdp if e(sample)
	// Calculate predicted values 
	margins, expression(exp(predict(xb))*exp((`e(rmse)'^2)/2)) ///
		at(FLXMON_std = (-1 0 1) (means) _all)

	// Table 3, Model 4 w/o Vietnam for outlier discussion

	// Full sample
	reg ln_daily_cum_deaths_million FLXMON_std IDVCOLL_std Tightness_std ///
		v2x_libdem_std ln_gdp ///
		if date == 20201031 & sample1 == 1, vce(hc3)
	scalar b_gdp_full = _b[ln_gdp]

	// W/o VNM
	reg ln_daily_cum_deaths_million FLXMON_std IDVCOLL_std Tightness_std ///
		  v2x_libdem_std ln_gdp ///
		  if date == 20201031 & sample1 == 1 & country_iso3 != "VNM", vce(hc3)

	// DFBETA
	display (b_gdp_full - _b[ln_gdp])/_se[ln_gdp] 

******************************
**** Correlation Matrices ****
******************************
	*ssc install estout
	
	// Appendix: Table S1a. Correlations COIVD mortality, national cultural indicators, and co-variates.  
	local varlist1 "ln_daily_cum_cases_million FLXMON_std IDVCOLL_std Tightness_std v2x_libdem_std ln_gdp ln_PopDensity hospital_beds_per_thousand median_age"
	
	estpost correlate `varlist1' if sample1==1 & date == 20201031, matrix 
	
	esttab using tables/corr_matrix1.rtf, not unstack compress noobs ///
		cells(b(star fmt(%9.2f))) ///
		stats(N, fmt(%9.0g) labels(N)) replace

	
	// Appendix: Table S1b. Correlations among COVID mortality, national cultural indicators, and co-variates.
	local varlist2 "ln_daily_cum_cases_million FLXMON_std IDVCOLL_std ln_gdp Fear_mar MaskWearing_mar Fear_whole MaskWearing_whole"
	
	estpost correlate `varlist2' if sample3==1 & date == 20201031 , matrix 
	
	esttab using tables/corr_matrix2.rtf, not unstack compress noobs ///
		cells(b(star fmt(%9.2f))) ///
		stats(N, fmt(%9.0g) labels(N)) replace


*****************************
**** 	Main Model		 ****
*****************************
	
	// Table 1. National cultural traits and cumulative COVID-19 deaths (in log) as of October 31, 2020
	
	// Model 1
	qui reg ln_daily_cum_deaths_million FLXMON_std if date == 20201031 & sample1 == 1, ///
		pformat (%5.2f) vce(hc3)
	estadd vif	
	est sto m1, title(Model 1)
	
	// Model 2
	qui reg ln_daily_cum_deaths_million FLXMON_std IDVCOLL_std ///
		if date == 20201031 & sample1 == 1, pformat (%5.2f) vce(hc3)
	estadd vif	
	est sto m2, title(Model 2)

	// Model 3
	qui reg ln_daily_cum_deaths_million FLXMON_std Tightness_std ///
		if date == 20201031 & sample1 == 1, pformat (%5.2f) vce(hc3)
	estadd vif	
	est sto m3, title(Model 3)
	
	// Model 4
	qui reg ln_daily_cum_deaths_million FLXMON_std IDVCOLL_std v2x_libdem_std ///
		Tightness_std  ln_gdp if date == 20201031 & sample1 == 1, pformat ///
		(%5.2f) vce(hc3)
	estadd vif	
	est sto m4, title(Model 4)

	// Model 5
	qui reg ln_daily_cum_deaths_million FLXMON_std IDVCOLL_std Tightness_std ///
		v2x_libdem_std ln_gdp ln_PopDensity hospital_beds_per_thousand ///
		median_age if date == 20201031 & sample1 == 1, pformat (%5.2f) vce(hc3)
	estadd vif	
	est sto m5, title(Model 5)
	
	estout m1 m2 m3 m4 m5 using "tables/table1.xls", ///
		cells("b(star fmt(%9.2f)) se(par(( ))) vif(fmt(2))") ///
		stats(r2_a N, fmt(%9.2f %9.0g) labels(Adjusted-R² Observations)) ///
		varlabels(_cons Constant) legend label collabels(none) dmarker(,) ///
		title("National cultural traits and cumulative COVID-19 deaths (in log) as of October 31 2020") ///
		note(Standard errors in parentheses) replace

*****************************
****  Testing mediators	 ****
*****************************
	
/*	Appendix: Table S2. Flexibility, fear of catching COVID-19, mask use, and 
	cumulative COVID-19 deaths (in log) as of October 31 2020 
	   
	Tables: Table 2. Flexibility, mediators (March) and cumulative COVID-19 deaths 
	(in log) as of October 31 2020
	   
	Appendix: Table S3. Flexibility, mediators (April), further controls and cumulative 
	COVID-19 deaths (in log) as of October 31 2020								*/
	
	
	local mediator "whole mar apr"
	local sample "3 3 2"
	local title "Feb.-Oct. March April"       
	local table_no "2 3a 3b"
	local n: word count `mediator'
	
		forvalues i = 1/`n' {
		    local a: word `i' of `mediator'
			local b: word `i' of `sample'
			local c: word `i' of `title'
			local d: word `i' of `table_no'
			
			// Model 1
			 reg ln_daily_cum_deaths_million FLXMON_std ln_gdp if week == 202044 ///
				& sample`b' == 1, pformat (%5.2f) vce(hc3)
			estadd vif	
			est sto m1, title(Model 1)
			
		    // Model 2
			 reg ln_daily_cum_deaths_million FLXMON_std ln_gdp Fear_`a' ///
				if date == 20201031 & sample`b' == 1, pformat (%5.2f) vce(hc3)
			estadd vif	
			est sto m2, title(Model 2)
			
			// Model 3
			 reg ln_daily_cum_deaths_million FLXMON_std ln_gdp MaskWearing_`a' ///
				if date == 20201031 & sample`b' == 1, pformat (%5.2f) vce(hc3)
			estadd vif	
			est sto m3, title(Model 3)
			
			// Model 4
			 reg ln_daily_cum_deaths_million FLXMON_std ln_gdp Fear_`a' MaskWearing_`a' ///
				if date == 20201031 & sample`b' == 1, pformat (%5.2f) vce(hc3)
			estadd vif	
			est sto m4, title(Model 4)
			
			estout m1 m2 m3 m4 using "tables/table`d'.xls", ///
				cells("b(star fmt(%9.2f)) se(par(( ))) vif(fmt(2))") ///
				stats(r2_a N, fmt(%9.2f %9.0g) labels(Adjusted-R² Observations)) ///
				varlabels(_cons Constant) legend label collabels(none) dmarker(,) ///
				title("Flexibility, mediators (`c') and cumulative COVID-19 deaths (in log) as of October 31 2020") ///
				note(Standard errors in parentheses) replace	
		}
		
*****************************
****     Figures		 ****
*****************************

	// create region identifier for countries in sample of analysis
	cap drop region
	gen region =.
	replace region = 1 if ///
		inlist(country_iso3, "CHN", "JPN", "KOR", "SGP", "THA")
	replace region = 2 if ///
		inlist(country_iso3, "IND", "MYS", "VNM")
	replace region = 3 if  ///
		inlist(country_iso3, "ARG","BRA","CHL","COL","MEX","PER")
	replace region = 4 if ///
		inlist(country_iso3, "AUT", "CZE", "DEU",  "ESP", "FIN", "GBR", "GRC")
	replace region = 4 if ///
		inlist(country_iso3, "HUN", "IRL", "ITA", "NLD", "POL", "PRT", "SWE", "UKR")
	replace region = 5 if ///
		inlist(country_iso3, "AUS", "CAN", "IDN", "ISR", "KAZ", "RUS", "TUR", "USA")
	
	label define region_lab 1 "East Asia" 2 "Southeast and South Asia" 3 "Central and South America" 4 "Europe" 5 "Other"
	label value region region_lab
		
		
	separate daily_cum_deaths_million,  by(region) veryshortlabel
	
	separate ln_daily_cum_deaths_million,  by(region) veryshortlabel
	
	separate FLXMON, by(region) veryshortlabel
	
	separate FLXMON_std, by(region) veryshortlabel
	
	//set overall look of graphs
	*ssc install grstyle
	*ssc install palettes

	grstyle clear
	grstyle init
	grstyle set graphsize 9cm 9cm
	grstyle set plain, noextend grid horizontal 
	grstyle set color cblind         
	grstyle set linewidth 1pt: p#
	grstyle set lpattern solid: p#
	grstyle set legend 6, klength(medsmall)
	grstyle set size 9pt: heading
	grstyle set size 6pt: subheading axis_title key_label
	grstyle set size 11pt: tick_label body small_body
	grstyle set size 3pt: legend_key_gap
	grstyle set size 10pt: legend_key_xsize
	grstyle set size 0pt: legend_row_gap
	grstyle set symbolsize 3 pt
	grstyle set linewidth .4pt: pmark legend axisline tick major_grid
	grstyle set margin zero
	grstyle set margin "0 3 3 0": axis_title
	grstyle set ci, opacity(30)
	graph set svg fontface "Times New Roman"
	
	
	//Tables: Figure 1.  Flexibility (vs. Monumentalism) and COVID mortality rate as of 31 October 2020.
	cap drop ln_deaths flexibility_std
	clonevar ln_deaths = ln_daily_cum_deaths_million
	clonevar flexibility_std = FLXMON_std
	lab var ln_deaths "		Log. Cum. number of deaths per million pop. as of 31 October 2020"
	lab var flexibility_std "	Monumentalism <<<   	  				>>> Flexibility"
	
	multidot flexibility_std ln_deaths ///
		if sample1==1 & date ==20201031, over(country_iso3) ///
		xlabel(#7, labsize(tiny)) ytitle("") ylabel(, labsize(vsmall)) ///
		subtitle(, position(6) size(*.65) nobox xoffset(-1) margin(0 10 10 0)) ///
		sepby(region) msymbol(o) msize(*1.5) ///
		legend(on lab(1 "East Asia") lab(2 "Southeast and South Asia") ///
		lab(3 "Central and South America") lab(4 "Europe") lab(5 "Other") ///
		position(0) ring(0) yoffset(-13) xoffset(-52) size(tiny) row(3)) ///
		mcolor(purple black edkblue cyan dkorange)
		 
	graph export "graphs/figure1.eps", replace
		
	drop ln_deaths flexibility_std
	
*****************************
****     Appendix		 ****
*****************************

	//Appendix: Figure S1: Log. number of deaths per million up to October 31, 2020
	twoway ///
		(scatter ln_daily_cum_deaths_million v2x_libdem if date == 20201031 & sample3 == 0, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(magenta%40)) ///
		(scatter ln_daily_cum_deaths_million v2x_libdem if date == 20201031 & sample3 == 1, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(ebblue%60)) ///
		(lfit ln_daily_cum_deaths_million v2x_libdem if date == 20201031), ///
		legend(col(1) label (1 "Not included in the present analysis due to missing on flexibility and covariates") label (2 "included in the present analysis") label (3 "Fitted values for 160 countries with available data on death rate and" "liberal democracy index") size(vsmall) region(col(white))) ///
		ylab(-3(1)8, labsize(*0.5)) xlab(0(0.2)1, labsize(*0.5)) ///
		ytitle("Log. number of deaths per million up to October 31, 2020") ///
		note("Source of death numbers: Ritchie et al. 2020" "Retrieval date: 20.02.2022", size(*.3))
	   
	graph export "graphs/appendix1.eps", replace
	
	//Appendix: Figure S2: Log. number of infections per million up to October 31, 2020
	twoway ///
		(scatter ln_daily_cum_cases_million v2x_libdem if date == 20201031 & sample3 == 0, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(magenta%40)) ///
		(scatter ln_daily_cum_cases_million v2x_libdem if date == 20201031 & sample3 == 1, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(ebblue%60)) ///
		(lfit ln_daily_cum_cases_million v2x_libdem if date == 20201031), ///
		legend(col(1) label (1 "Not included in the present analysis due to missing on flexibility and covariates") label (2 "included in the present analysis") label (3 "Fitted values for 168 countries with available data on infection rate and" "liberal democracy index") size(vsmall) region(col(white))) ///
		ylab(1(1)11, labsize(*0.5)) xlab(0(0.2)1, labsize(*0.5)) ///
		ytitle("Log. number of infections per million up to October 31, 2020") ///
		note("Source of infection numbers: Ritchie et al. 2020" "Retrieval date: 20.02.2022", size(*.3))
	   
	graph export "graphs/appendix2.eps", replace
	
	//Appendix: Figure S3: Log. number of deaths per million up to October 31, 2020
	twoway ///
		(scatter ln_daily_cum_deaths_million ln_gdp if date == 20201031 & sample3 == 0, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(magenta%40)) ///
		(scatter ln_daily_cum_deaths_million ln_gdp if date == 20201031 & sample3 == 1, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(ebblue%60)) ///
		(lfit ln_daily_cum_deaths_million ln_gdp if date == 20201031), ///
		legend(col(1) label (1 "Not included in the present analysis due to missing on flexibility and covariates") label (2 "included in the present analysis") label (3 "Fitted values for 159 countries with available data on death rate and" "GDP per capita") size(vsmall) region(col(white))) ///
		ylab(-3(1)8, labsize(*0.5)) xlab(5(1)12, labsize(*0.5)) ///
		ytitle("Log. number of deaths per million up to October 31, 2020") ///
		note("Source of death numbers: Ritchie et al. 2020" "Retrieval date: 20.02.2022", size(*.3))
	   
	graph export "graphs/appendix3.eps", replace
	   
	//Appendix: Figure S4: Log. number of infections per million up to October 31, 2020
	twoway ///
		(scatter ln_daily_cum_cases_million ln_gdp if date == 20201031 & sample3 == 0, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(magenta%40)) ///
		(scatter ln_daily_cum_cases_million ln_gdp if date == 20201031 & sample3 == 1, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(ebblue%60)) ///
		(lfit ln_daily_cum_cases_million ln_gdp if date == 20201031), ///
		legend(col(1) label (1 "Not included in the present analysis due to missing on flexibility and covariates") label (2 "included in the present analysis") label (3 "Fitted values for 171 countries with available data on infection rate and" "GDP per capita") size(vsmall) region(col(white))) ///
		ylab(1(1)11, labsize(*0.5)) xlab(5(1)12, labsize(*0.5)) ///
		ytitle("Log. number of infections per million up to October 31, 2020") ///
		note("Source of infection numbers: Ritchie et al. 2020" "Retrieval date: 20.02.2022", size(*.3))
	
	graph export "graphs/appendix4.eps", replace
	
	//Appendix: Figure S5: Robustness check for main models
	twoway ///
		(scatter ln_daily_cum_deaths_million FLXMON_std if date == 20201031 & sample1 == 0, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(magenta%40)) ///
		(scatter ln_daily_cum_deaths_million FLXMON_std if date == 20201031 & sample1 == 1, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(ebblue%60)) ///
		(lfitci ln_daily_cum_deaths_million FLXMON_std if date == 20201031 & sample1 == 1) ///
		(lfitci ln_daily_cum_deaths_million FLXMON_std if date == 20201031), ///
		legend(col(1) label (1 "Not included in the present analysis due to missing on flexibility and covariates") label (2 "included in the present analysis") label(3 "95 % CI's") label (4 "Fitted values for 37 countries with valid information on all covariates") label (5 "Fitted values for 50 countries with available data on flexibility-monumentalism") size(vsmall) region(col(white))) ///
		ylab(-2(1)8, labsize(*0.5)) xlab(-3(1)3, labsize(*0.5)) ///
		ytitle("Log. number of infections per million up to October 31, 2020") ///
		xtitle("Monumentalism <<<   >>> Flexibility", xoffset(-2))
		
	graph export "graphs/appendix5.pdf", replace	
	
	//Appendix: Figure 7: Robustness check for mediator model II
	twoway ///
		(scatter ln_daily_cum_deaths_million FLXMON_std if date == 20201031 & sample3 == 0, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(magenta%40)) ///
		(scatter ln_daily_cum_deaths_million FLXMON_std if date == 20201031 & sample3 == 1, ///
		mlabel(country_iso3) mlabsize(tiny) msize(small) mcolor(ebblue%60)) ///
		(lfitci ln_daily_cum_deaths_million FLXMON_std if date == 20201031 & sample3 == 1) ///
		(lfitci ln_daily_cum_deaths_million FLXMON_std if date == 20201031), ///
		legend(col(1) label (1 "Not included in the present analysis due to missing on flexibility and covariates") label (2 "included in the present analysis") label(3 "95 % CI's") label (4 "Fitted values for 23 countries with valid information on all covariates") label (5 "Fitted values for 50 countries with available data on flexibility-monumentalism") size(vsmall) region(col(white))) ///
		ylab(-2(1)8, labsize(*0.5)) xlab(-3(1)3, labsize(*0.5)) ///
		ytitle("Log. number of infections per million up to October 31, 2020") ///
		xtitle("Monumentalism <<<   >>> Flexibility", xoffset(-2))
		
	graph export "graphs/appendix7.pdf", replace	
		