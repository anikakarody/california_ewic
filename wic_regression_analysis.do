//ANIKA KARODY
//REGRESSION ANALYSIS (with and without ACS data)
********************************************************************************
//SETTING THINGS UP
cap log close
cap clear all
cd "/Users/anikakarody/Desktop/eWIC Research "
use "data/wic_acs_merged.dta", clear

keep county_code county yyyymm_raw period_type year month mdate_num ///
    families_redeemed vouchers_redeemed voucher_dollars avg_cost_per_family ///
    infant_formula_rebate total_cost_adjusted avg_cost_adjusted ///
    ebt_transactions ebt_dollars yyyymm_s county_id ///
    total_txn total_dol ebt_share_txn ebt_share_dol ///
    ln_families_redeemed county_clean county_fips ///
    unemp_rate hispanic_share median_hh_income children_under5 ///
    female_under5 no_acs_coverage outside_acs_years ///
    children_under5_200fpl fpl200_share

drop if county == "Statewide"
cap drop first_ebt treat_date treat_time event_time

log using ///
    "/Users/anikakarody/Desktop/eWIC Research /wic_regression_analysis.log", replace
********************************************************************************
//INSTALLING PACKAGES
ssc install drdid, all replace
ssc install csdid, all replace
ssc install estout, replace
ssc install ftools, replace
ftools, compile
ssc install reghdfe, replace
ssc install coefplot, replace
********************************************************************************
//SETTING PANEL STRUCTURE
xtset county_id mdate_num
********************************************************************************
//GENERATE TREATMENT VARIABLE FOR CS-DID
bysort county_id (mdate_num): gen first_ebt = mdate_num ///
    if ebt_transactions > 0 & !missing(ebt_transactions)
bysort county_id: egen treat_date = min(first_ebt)
gen treat_time = treat_date
replace treat_time = 0 if missing(treat_date)
drop first_ebt

gen event_time = mdate_num - treat_date 
	* EBT = 1 if event_time ≥ 0, less than for missing 
	*EBT = 0 if event_time is < 0
	* THIS IS RHS OF EQUATION!

// BINARY TREATMENT INDICATOR: IS EBT ACTIVE IN MY COUNTY NOW?
gen ebt_active = (mdate_num >= treat_date) & !missing(treat_date)
********************************************************************************
//CREATING WAVE VARIABLES
	* Define timings based on county (EBT variable should be set by county)
	* Identify counties by map
	*Also make this change to event-time
	
tab treat_date

bysort county: egen first_treat = min(treat_date)

gen wave = .
replace wave = 0 if treat_date == tm(2019m6)   // Pilot
replace wave = 1 if treat_date == tm(2019m7)   // Wave 1
replace wave = 2 if treat_date == tm(2019m8)   // Wave 2
replace wave = 3 if treat_date == tm(2019m9)   // Wave 3
replace wave = 4 if treat_date == tm(2019m10)  // Wave 4
replace wave = 5 if treat_date == tm(2025m8)   // Wave 5 (Mariposa - late outlier)

label define wavelbl 0 "Pilot" 1 "Wave 1" 2 "Wave 2" ///
    3 "Wave 3" 4 "Wave 4" 5 "Wave 5"
label values wave wavelbl

list county wave treat_date if !missing(wave) & mdate_num == first_treat, ///
    clean noobs
********************************************************************************
//GENERATING PARTICIPATION PROXY
cap drop participation_proxy ln_participation_proxy

// Professor Bitler's specification:
// families redeemed / share of children under 5 below 200% FPL
// normalizes by county-level income eligibility share
gen participation_proxy = families_redeemed / fpl200_share
gen ln_participation_proxy = log(participation_proxy)
********************************************************************************
//SUMMARY STATISTICS
cap mkdir "tables"
eststo clear

estpost summarize families_redeemed ebt_share_dol ebt_share_txn ///
    unemp_rate hispanic_share median_hh_income children_under5 ///
    children_under5_200fpl fpl200_share participation_proxy ///
    if mdate_num < treat_date | treat_time == 0, detail
esttab using "tables/summary_stats_pre.tex", ///
    replace ///
    cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") ///
    title("Summary Statistics: Pre-eWIC") ///
    collabels("Mean" "Std. Dev." "Min" "Max") ///
    nomtitle nonumber

estpost summarize families_redeemed ebt_share_dol ebt_share_txn ///
    unemp_rate hispanic_share median_hh_income children_under5 ///
    children_under5_200fpl fpl200_share participation_proxy ///
    if mdate_num >= treat_date & !missing(treat_date), detail
esttab using "tables/summary_stats_post.tex", ///
    replace ///
    cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") ///
    title("Summary Statistics: Post-eWIC") ///
    collabels("Mean" "Std. Dev." "Min" "Max") ///
    nomtitle nonumber
********************************************************************************
//SANITY CHECKS
xtdescribe
tab year
tab year if missing(fpl200_share)
********************************************************************************
//OLS REGRESSIONS
eststo clear

// Families redeemed
eststo ols1: reg ln_families_redeemed ebt_active i.year i.month, ///
    vce(cluster county_id)
	
	*MAKE SURE THAT EBT_ACTIVE MATCHES DATES

// Families redeemed with ACS controls
eststo ols2: reg ln_families_redeemed ebt_active i.year i.month ///
    unemp_rate hispanic_share median_hh_income children_under5, ///
    vce(cluster county_id)

// Participation proxy
eststo ols3: reg ln_participation_proxy ebt_active i.year i.month, ///
    vce(cluster county_id)

// Participation proxy with ACS controls
eststo ols4: reg ln_participation_proxy ebt_active i.year i.month ///
    unemp_rate hispanic_share median_hh_income children_under5, ///
    vce(cluster county_id)

esttab ols1 ols2 using "tables/ols_families.tex", ///
    replace label b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("OLS Regressions: eWIC Adoption and WIC Participation") ///
    mtitles("Families Redeemed" "With ACS Controls") ///
    stats(N r2, labels("Observations" "R-squared"))

esttab ols3 ols4 using "tables/ols_participation.tex", ///
    replace label b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("OLS Regressions: eWIC Adoption and WIC Participation Proxy") ///
    mtitles("Participation Proxy" "With ACS Controls") ///
    stats(N r2, labels("Observations" "R-squared"))
********************************************************************************
//FIXED-EFFECT REGRESSIONS: FAMILIES REDEEMED
eststo clear

// Without ACS controls
eststo m1: xtreg ln_families_redeemed ebt_share_txn, fe vce(cluster county_id)
eststo m2: xtreg ln_families_redeemed ebt_share_dol, fe vce(cluster county_id)

// Binary treatment with year and month FE
eststo m3: xtreg ln_families_redeemed ebt_active i.year i.month, ///
    fe vce(cluster county_id)

// Continuous EBT share with year and month FE
eststo m4: xtreg ln_families_redeemed ebt_share_dol i.year i.month, ///
    fe vce(cluster county_id)

// With ACS controls
eststo m5: xtreg ln_families_redeemed ebt_share_dol i.year i.month ///
    unemp_rate hispanic_share median_hh_income children_under5, ///
    fe vce(cluster county_id)

// Binary treatment with ACS controls
eststo m6: xtreg ln_families_redeemed ebt_active i.year i.month ///
    unemp_rate hispanic_share median_hh_income children_under5, ///
    fe vce(cluster county_id)

esttab m1 m2 m3 using "tables/fe_regressions_noacs.tex", ///
    replace label b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Fixed Effects Regressions: eWIC Adoption and WIC Participation") ///
    mtitles("EBT Share (Txn)" "EBT Share (Dol)" "EBT Active") ///
    stats(N r2_w, labels("Observations" "Within R-squared"))

esttab m4 m5 m6 using "tables/fe_regressions_acs.tex", ///
    replace label b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Fixed Effects Regressions with ACS Controls") ///
    mtitles("Year/Month FE" "EBT Share + ACS" "EBT Active + ACS") ///
    stats(N r2_w, labels("Observations" "Within R-squared"))
********************************************************************************
//FIXED-EFFECT REGRESSIONS: PARTICIPATION PROXY
eststo clear

eststo t1: xtreg ln_participation_proxy ebt_share_txn, fe vce(cluster county_id)
eststo t2: xtreg ln_participation_proxy ebt_share_dol, fe vce(cluster county_id)
eststo t3: xtreg ln_participation_proxy ebt_active i.year i.month, ///
    fe vce(cluster county_id)
eststo t4: xtreg ln_participation_proxy ebt_share_dol i.year i.month, ///
    fe vce(cluster county_id)
eststo t5: xtreg ln_participation_proxy ebt_share_dol i.year i.month ///
    unemp_rate hispanic_share median_hh_income children_under5, ///
    fe vce(cluster county_id)
eststo t6: xtreg ln_participation_proxy ebt_active i.year i.month ///
    unemp_rate hispanic_share median_hh_income children_under5, ///
    fe vce(cluster county_id)

esttab t1 t2 t3 using "tables/fe_participation_noacs.tex", ///
    replace label b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Fixed Effects: eWIC Adoption and WIC Participation Proxy") ///
    mtitles("EBT Share (Txn)" "EBT Share (Dol)" "EBT Active") ///
    stats(N r2_w, labels("Observations" "Within R-squared"))

esttab t4 t5 t6 using "tables/fe_participation_acs.tex", ///
    replace label b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Fixed Effects: Participation Proxy with ACS Controls") ///
    mtitles("Year/Month FE" "EBT Share + ACS" "EBT Active + ACS") ///
    stats(N r2_w, labels("Observations" "Within R-squared"))
********************************************************************************
//CS-DID REGRESSION: FAMILIES REDEEMED
// Without ACS controls
csdid ln_families_redeemed, ivar(county_id) time(mdate_num) ///
    gvar(treat_time) agg(event)
csdid_plot
graph export "graphs/csdid_event_study_noacs.png", replace

// With ACS controls
csdid ln_families_redeemed unemp_rate hispanic_share ///
    median_hh_income children_under5, ///
    ivar(county_id) time(mdate_num) gvar(treat_time) agg(event)
csdid_plot
graph export "graphs/csdid_event_study_acs.png", replace
********************************************************************************
//CS-DID REGRESSION: PARTICIPATION PROXY
// Without ACS controls
csdid ln_participation_proxy, ivar(county_id) time(mdate_num) ///
    gvar(treat_time) agg(event)
csdid_plot
graph export "graphs/csdid_participation_noacs.png", replace

// With ACS controls
csdid ln_participation_proxy unemp_rate hispanic_share ///
    median_hh_income children_under5, ///
    ivar(county_id) time(mdate_num) gvar(treat_time) agg(event)
csdid_plot
graph export "graphs/csdid_participation_acs.png", replace
********************************************************************************
//ROBUSTNESS CHECK: EXCLUDING SMALL RURAL COUNTIES WITH NO ACS COVERAGE
// Families redeemed
csdid ln_families_redeemed if no_acs_coverage == 0, ///
    ivar(county_id) time(mdate_num) gvar(treat_time) agg(event)
csdid_plot
graph export "graphs/csdid_event_study_robust.png", replace

// Participation proxy
csdid ln_participation_proxy if no_acs_coverage == 0, ///
    ivar(county_id) time(mdate_num) gvar(treat_time) agg(event)
csdid_plot
graph export "graphs/csdid_participation_robust.png", replace
********************************************************************************
//EVENT STUDY REGRESSIONS (ALREADY DOES PROF'S BINNING SUGGESTION!)


	// EVENT STUDY: EVENT-TIME DUMMIES
	//
	// event_time_binned: months relative to adoption, capped at +/- 24
	// event_time_shifted: adds 25 so all values are positive (Stata requirement)
	//     t=-1 --> 24 (omitted reference), t=0 --> 25 (first treatment period)
	//
	// ib24.event_time_shifted creates one dummy per relative time period.
	// Pre-period (t=-24 to t=-2): test parallel trends, should be near zero.
	// Post-period (t=0 to t=+24): dynamic effect of eWIC adoption.
********************************************************************************
cap drop event_time_binned event_time_shifted
gen event_time_binned = event_time
replace event_time_binned = -24 if event_time < -24
replace event_time_binned = 24  if event_time > 24 & !missing(event_time)

// Shift so all values positive, t=-1 becomes 24 (omitted base category)
gen event_time_shifted = event_time_binned + 25

// Families redeemed
eststo es1: reghdfe ln_families_redeemed ib24.event_time_shifted ///
    i.month if !missing(event_time_binned), ///
    absorb(county_id year) vce(cluster county_id)

// Participation proxy
eststo es2: reghdfe ln_participation_proxy ib24.event_time_shifted ///
    i.month if !missing(event_time_binned), ///
    absorb(county_id year) vce(cluster county_id)

// Plot event studies
coefplot es1, keep(*.event_time_shifted) omitted ///
    vertical recast(connected) ///
    yline(0, lcolor(black) lpattern(dash)) ///
    xline(25, lcolor(red) lpattern(dash)) ///
    title("Event Study: eWIC Adoption and Families Redeemed") ///
    xtitle("Months Relative to eWIC Adoption") ///
    ytitle("Coefficient") ///
    name(es_families, replace) nodraw
graph export "graphs/event_study_families.png", replace

coefplot es2, keep(*.event_time_shifted) omitted ///
    vertical recast(connected) ///
    yline(0, lcolor(black) lpattern(dash)) ///
    xline(25, lcolor(red) lpattern(dash)) ///
    title("Event Study: eWIC Adoption and Participation Proxy") ///
    xtitle("Months Relative to eWIC Adoption") ///
    ytitle("Coefficient") ///
    name(es_participation, replace) nodraw
graph export "graphs/event_study_participation.png", replace
********************************************************************************
//COUNTY-BASED DATA VISUALIZATIONS
cap mkdir "graphs"

levelsof county, local(counties)
foreach c of local counties {
    levelsof county_clean if county == "`c'", local(cclean) clean

    quietly summarize treat_date if county == "`c'"
    local tdate = r(min)

    if !missing(`tdate') {
        local xline_opt xline(`tdate', lcolor(red) lpattern(dash))
    }
    else {
        local xline_opt
    }

    cap twoway ///
        (line families_redeemed mdate_num if county == "`c'", ///
            yaxis(1) lcolor(navy)) ///
        (line ebt_share_dol mdate_num if county == "`c'", ///
            yaxis(2) lcolor(orange)), ///
        title("`c': WIC Participation and eWIC Adoption") ///
        xtitle("Date") ///
        xlabel(600(12)800, format(%tmCCYY) angle(45)) ///
        ytitle("Families Redeemed", axis(1)) ///
        ytitle("EBT Dollar Share", axis(2)) ///
        legend(order(1 "Families Redeemed" 2 "EBT Share (dollars)")) ///
        `xline_opt' ///
        name(g_`cclean', replace) nodraw

    cap graph export "graphs/combined_`cclean'.png", replace
    cap graph close g_`cclean'
}
********************************************************************************
//CLOSING LOG FILE
log close
