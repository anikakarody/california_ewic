/*******************************************************************************
ANIKA KARODY
Undergraduate, Department of Economics @ UC Davis

RESEARCH QUESTION: How does the varied structure and speed of county-level
eWIC roll-out affect take-up rates in California?

-------------------------------------------------------------------------------
This do-file conducts several types of regression analysis to assess the
impacts of eWIC on take-up and participation, given California's staggered
adoption strategy. Below are the regressions used:

    * OLS Regressions
    * Standard Two-Way Fixed Effects (TWFE) Regressions
    * Callaway-Sant'Anna (CS-DID) Regressions
    * Event-Study Regressions

In case my other .dta files are not accessible, I will comment some of the
key regression outputs. If you need additional information or have questions
about my methodology, email me at anika.karody@gmail.com :)

-------------------------------------------------------------------------------
AI DISCLOSURE STATEMENT

I used Claude (Anthropic) to help debug errors in this Stata code and to write
and refine inline comments throughout. I also worked with Claude to construct
the Callaway-Sant'Anna (CS-DID) implementation in this file (Sections 14-18), as
this was a largely new framework that I hadn't learned in undergraduate
econometrics. I have since learned and worked through the underlying methodology
in detail. The research question, data construction (Sections 0-9), OLS and 
TWFE specifications (Sections 10-13), and all interpretations of my results are
my own original work.

*******************************************************************************/


********************************************************************************
// 0. SETUP
********************************************************************************
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
    "/Users/anikakarody/Desktop/eWIC Research /wic_regression_analysis.log", ///
    replace


********************************************************************************
// 1. INSTALLING PACKAGES
********************************************************************************
ssc install drdid, all replace
ssc install csdid, all replace
ssc install estout, replace
ssc install ftools, replace
ftools, compile
ssc install reghdfe, replace
ssc install coefplot, replace


********************************************************************************
// 2. SETTING PANEL STRUCTURE
********************************************************************************
set graphics off
xtset county_id mdate_num


********************************************************************************
// 3. GENERATING TREATMENT VARIABLE FOR CS-DID
********************************************************************************
bysort county_id (mdate_num): gen first_ebt = mdate_num ///
    if ebt_transactions > 0 & !missing(ebt_transactions)
bysort county_id: egen treat_date = min(first_ebt)
gen treat_time = treat_date
replace treat_time = 0 if missing(treat_date)
drop first_ebt

//Event-time specification, binary treatment indicator
	*EBT = 1 if event_time >= 0 (i.e. mdate_num >= treat_date), else 0
	*This is the RHS treatment indicator used throughout.

gen event_time = mdate_num - treat_date

// Binary treatment indicator (Is eWIC active in my county now?)
gen ebt_active = (mdate_num >= treat_date) & !missing(treat_date)


********************************************************************************
// 4. CREATING WAVE VARIABLES
********************************************************************************
// Defining timings based on county (EBT variable should be set by county);
// identifying counties by map.

tab treat_date

bysort county: egen first_treat = min(treat_date)

gen wave = .

	* Pilot Wave
	replace wave = 0 if treat_date == tm(2019m6)

	* Wave 1
	replace wave = 1 if treat_date == tm(2019m7)

	* Wave 2
	replace wave = 2 if treat_date == tm(2019m8)

	* Wave 3
	replace wave = 3 if treat_date == tm(2019m9)

	* Wave 4
	replace wave = 4 if treat_date == tm(2019m10)

	* Wave 5
	replace wave = 5 if treat_date == tm(2025m8)

cap label drop wavelbl
label define wavelbl 0 "Pilot" 1 "Wave 1" 2 "Wave 2" ///
    3 "Wave 3" 4 "Wave 4" 5 "Wave 5"
label values wave wavelbl

list county wave treat_date if !missing(wave) & mdate_num == first_treat, ///
    clean noobs

// Crosswalk for map construction
// (Used to build an updated wave-timing map in Google Colab)
preserve
    keep if !missing(wave) & mdate_num == first_treat
    keep county wave treat_date
    duplicates drop

    gen treat_date_str = string(treat_date, "%tm")

    export delimited "county_wave_crosswalk.csv", replace
restore


********************************************************************************
// 5. CREATING AGGREGATED RURAL COUNTY
********************************************************************************
// Pooling small rural counties with missing ACS data into a single synthetic
// "rural county" unit rather than dropping them.

// Rural counties with no ACS coverage are identified via no_acs_coverage == 1
// (there are 15 such counties). The aggregate is assigned a new county_id
// that doesn't conflict with existing ones (99).

cap drop rural_flag county_id_agg
gen rural_flag = (no_acs_coverage == 1)

// Creating aggregated dataset
    keep if rural_flag == 1

    *Collapsing to monthly aggregates across all rural counties
    collapse (sum) families_redeemed vouchers_redeemed voucher_dollars ///
                   ebt_transactions ebt_dollars total_txn total_dol ///
                   children_under5 female_under5 children_under5_200fpl ///
             (mean) unemp_rate hispanic_share median_hh_income fpl200_share ///
                    ebt_share_txn ebt_share_dol ///
             (min) treat_date treat_time wave ///
             , by(mdate_num year month)

    *Assigning synthetic county identifiers
    gen county_id = 99
    gen county = "Rural Aggregate"
    gen county_clean = "Rural_Aggregate"
    gen county_fips = "999"
    gen county_code = "999"
    gen yyyymm_s = ""
    gen no_acs_coverage = 0   // now has ACS coverage via aggregation
    gen outside_acs_years = 0

    *Recalculating log outcomes
    gen ln_families_redeemed = log(families_redeemed)
    gen ln_voucher_dollars = log(voucher_dollars)
    gen ln_total_dol = log(total_dol)

    *Recalculating participation proxy
    gen participation_proxy = families_redeemed / fpl200_share
    gen ln_participation_proxy = log(participation_proxy)

    *Recalculating event time
    gen event_time = mdate_num - treat_date
    gen ebt_active = (mdate_num >= treat_date) & !missing(treat_date)

    tempfile rural_agg
    save `rural_agg'
restore

// Appending aggregated rural county to main dataset (drop individual rural
// counties first)
drop if rural_flag == 1
append using `rural_agg', force

// Resetting panel structure
xtset county_id mdate_num


********************************************************************************
// 6. GENERATING PARTICIPATION PROXY
********************************************************************************
cap drop participation_proxy ln_participation_proxy

// Professor Bitler's specification:
	*families redeemed / share of children under 5 below 200% FPL
	*normalizes by county-level income eligibility share
gen participation_proxy = families_redeemed / fpl200_share
gen ln_participation_proxy = log(participation_proxy)


********************************************************************************
// 7. GENERATING ADDITIONAL OUTCOME VARIABLES
********************************************************************************
cap drop ln_voucher_dollars ln_total_dol
gen ln_voucher_dollars = log(voucher_dollars)
gen ln_total_dol = log(total_dol)


********************************************************************************
// 8. SUMMARY STATISTICS
********************************************************************************
cap mkdir "tables"
cap mkdir "graphs"
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
// 9. SANITY CHECKS
********************************************************************************
xtdescribe
tab year
tab year if missing(fpl200_share)


********************************************************************************
// 10. OLS REGRESSIONS
********************************************************************************
eststo clear

// Families redeemed
eststo ols1: reg ln_families_redeemed ebt_active i.year i.month, ///
    vce(cluster county_id)

* MAKE SURE THAT EBT_ACTIVE MATCHES DATES

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

// Exporting tables
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
// 11. FIXED-EFFECT REGRESSIONS: FAMILIES REDEEMED
********************************************************************************
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

// Exporting tables
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
// 12. FIXED-EFFECT REGRESSIONS: PARTICIPATION PROXY
********************************************************************************
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

// Exporting tables
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
// 13. FIXED-EFFECT REGRESSIONS: VOUCHER DOLLARS
********************************************************************************
eststo clear

eststo v1: xtreg ln_voucher_dollars ebt_share_txn, fe vce(cluster county_id)
eststo v2: xtreg ln_voucher_dollars ebt_share_dol, fe vce(cluster county_id)
eststo v3: xtreg ln_voucher_dollars ebt_active i.year i.month, ///
    fe vce(cluster county_id)
eststo v4: xtreg ln_voucher_dollars ebt_share_dol i.year i.month, ///
    fe vce(cluster county_id)
eststo v5: xtreg ln_voucher_dollars ebt_share_dol i.year i.month ///
    unemp_rate hispanic_share median_hh_income children_under5, ///
    fe vce(cluster county_id)
eststo v6: xtreg ln_voucher_dollars ebt_active i.year i.month ///
    unemp_rate hispanic_share median_hh_income children_under5, ///
    fe vce(cluster county_id)

// --- Export tables ------------------------------------------------------------
esttab v1 v2 v3 using "tables/fe_voucher_noacs.tex", ///
    replace label b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Fixed Effects: eWIC Adoption and Voucher Dollars Redeemed") ///
    mtitles("EBT Share (Txn)" "EBT Share (Dol)" "EBT Active") ///
    stats(N r2_w, labels("Observations" "Within R-squared"))

esttab v4 v5 v6 using "tables/fe_voucher_acs.tex", ///
    replace label b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    title("Fixed Effects: Voucher Dollars with ACS Controls") ///
    mtitles("Year/Month FE" "EBT Share + ACS" "EBT Active + ACS") ///
    stats(N r2_w, labels("Observations" "Within R-squared"))


********************************************************************************
// 14. PROGRAM: CALENDAR-DATE CS-DID EVENT PLOT
********************************************************************************
// Replaces csdid_plot's default "Periods to Treatment" axis with
	*calendar-date labels, anchored to the modal adoption date (Sept 2019).
	*Call this immediately after a `csdid ..., agg(event)` estimation, 
	*extracts event-time coefficients (Tm##/Tp##) directly from e(b)/e(V).

// NOTE: post-treatment coverage is short (typically only Tp0-Tp2) since
	*the not-yet-treated comparison pool shrinks quickly after 2019.
	*Default window is -24 to 2; widen the pre-period side as needed.

// Usage: cal_csdid_plot using "graphs/myplot.pdf"
	*title("My Title") ytitle("ATT") window(-24 2)

cap program drop cal_csdid_plot
program define cal_csdid_plot
    syntax using/, [title(string) ytitle(string) window(numlist min=2 max=2)]

    if "`window'" == "" local window "-24 2"
    tokenize `window'
    local wlo `1'
    local whi `2'

    tempname b V
    matrix `b' = e(b)
    matrix `V' = e(V)
    local cn : colfullnames `b'
    local ncols : word count `cn'

    preserve
        clear
        set obs `ncols'
        gen coef = .
        gen se = .
        gen parmname = ""

        forvalues i = 1/`ncols' {
            replace coef = `b'[1,`i'] in `i'
            replace se = sqrt(`V'[`i',`i']) in `i'
            local this : word `i' of `cn'
            // strip any equation prefix (e.g. "eq1:Tm5" -> "Tm5")
            local this = substr("`this'", strpos("`this'",":")+1, .)
            replace parmname = "`this'" in `i'
        }

        // Keep only event-time parameters (Tm# or Tp#); drop Pre_avg/Post_avg/etc.
        gen prefix = substr(parmname, 1, 2)
        keep if inlist(prefix, "Tm", "Tp")

        if _N == 0 {
            di as error "cal_csdid_plot: no Tm##/Tp## parameters found in e(b)."
            di as error "Column names returned: `cn'"
            di as error "Check the naming convention and adjust the prefix parsing."
            restore
            exit 198
        }

        gen sign = -1 if prefix == "Tm"
        replace sign = 1 if prefix == "Tp"
        gen numpart = real(substr(parmname, 3, .))
        gen event_time = sign * numpart

        keep if event_time >= `wlo' & event_time <= `whi'

        if _N == 0 {
            di as error "cal_csdid_plot: no event times fall within window(`wlo' `whi')."
            di as error "Available event times range from -116 to +2 (varies by spec)."
            restore
            exit 198
        }

        gen ci_low = coef - 1.96*se
        gen ci_high = coef + 1.96*se

        sort event_time

        // Build calendar-year x-axis labels anchored to Sept 2019
        local xlabopt ""
        forvalues e = `wlo'(4)`whi' {
            local cm = tm(2019m9) + `e'
            local lbl : display %tmCCYY `cm'
            local xlabopt `xlabopt' `e' "`lbl'"
        }

        twoway ///
            (rcap ci_low ci_high event_time, lcolor(gs10)) ///
            (scatter coef event_time, mcolor(navy) msize(small)), ///
            xline(0, lcolor(red) lpattern(dash)) ///
            yline(0, lcolor(black) lpattern(dash)) ///
            xlabel(`xlabopt', angle(45)) ///
            title("`title'") ///
            subtitle("Calendar year shown relative to modal adoption date (Sept. 2019)") ///
            xtitle("Year") ///
            ytitle("`ytitle'") ///
            legend(off) ///
            name(cal_event_plot, replace)

        cap noisily graph export "`using'", name(cal_event_plot) replace
        if _rc {
            di as error "cal_csdid_plot: graph export failed."
            restore
            exit 198
        }
        cap graph close cal_event_plot
    restore
end


********************************************************************************
// 15. CS-DID REGRESSION: FAMILIES REDEEMED
********************************************************************************
// Without ACS controls
csdid ln_families_redeemed, ivar(county_id) time(mdate_num) ///
    gvar(treat_time) agg(event)
cal_csdid_plot using "graphs/csdid_event_study_noacs.pdf", ///
    title("CS-DID Event Study: Families Redeemed (No ACS Controls)") ///
    ytitle("ATT") window(-24 2)

// With ACS controls
csdid ln_families_redeemed unemp_rate hispanic_share ///
    median_hh_income children_under5, ///
    ivar(county_id) time(mdate_num) gvar(treat_time) agg(event)
cal_csdid_plot using "graphs/csdid_event_study_acs.pdf", ///
    title("CS-DID Event Study: Families Redeemed (With ACS Controls)") ///
    ytitle("ATT") window(-24 2)


********************************************************************************
// 16. CS-DID REGRESSION: PARTICIPATION PROXY
********************************************************************************
// Without ACS controls
csdid ln_participation_proxy, ivar(county_id) time(mdate_num) ///
    gvar(treat_time) agg(event)
cal_csdid_plot using "graphs/csdid_participation_noacs.pdf", ///
    title("CS-DID Event Study: Participation Proxy (No ACS Controls)") ///
    ytitle("ATT") window(-24 2)

// With ACS controls
csdid ln_participation_proxy unemp_rate hispanic_share ///
    median_hh_income children_under5, ///
    ivar(county_id) time(mdate_num) gvar(treat_time) agg(event)
cal_csdid_plot using "graphs/csdid_participation_acs.pdf", ///
    title("CS-DID Event Study: Participation Proxy (With ACS Controls)") ///
    ytitle("ATT") window(-24 2)


********************************************************************************
// 17. CS-DID REGRESSION: VOUCHER DOLLARS
********************************************************************************
// Without ACS controls
csdid ln_voucher_dollars, ivar(county_id) time(mdate_num) ///
    gvar(treat_time) agg(event)
cal_csdid_plot using "graphs/csdid_voucher_noacs.pdf", ///
    title("CS-DID Event Study: Voucher Dollars (No ACS Controls)") ///
    ytitle("ATT") window(-24 2)

// With ACS controls
csdid ln_voucher_dollars unemp_rate hispanic_share ///
    median_hh_income children_under5, ///
    ivar(county_id) time(mdate_num) gvar(treat_time) agg(event)
cal_csdid_plot using "graphs/csdid_voucher_acs.pdf", ///
    title("CS-DID Event Study: Voucher Dollars (With ACS Controls)") ///
    ytitle("ATT") window(-24 2)

// Robustness: excluding rural counties (no ACS controls only)
csdid ln_voucher_dollars if no_acs_coverage == 0, ///
    ivar(county_id) time(mdate_num) gvar(treat_time) agg(event)
cal_csdid_plot using "graphs/csdid_voucher_robust.pdf", ///
    title("CS-DID Event Study: Voucher Dollars (Excluding Rural Counties)") ///
    ytitle("ATT") window(-24 2)


********************************************************************************
// 18. ROBUSTNESS CHECK: EXCLUDING SMALL RURAL COUNTIES WITH NO ACS COVERAGE
********************************************************************************
// Families redeemed
csdid ln_families_redeemed if no_acs_coverage == 0, ///
    ivar(county_id) time(mdate_num) gvar(treat_time) agg(event)
cal_csdid_plot using "graphs/csdid_event_study_robust.pdf", ///
    title("CS-DID Event Study: Families Redeemed (Excluding Rural Counties)") ///
    ytitle("ATT") window(-24 2)

// Participation proxy
csdid ln_participation_proxy if no_acs_coverage == 0, ///
    ivar(county_id) time(mdate_num) gvar(treat_time) agg(event)
cal_csdid_plot using "graphs/csdid_participation_robust.pdf", ///
    title("CS-DID Event Study: Participation Proxy (Excluding Rural Counties)") ///
    ytitle("ATT") window(-24 2)


********************************************************************************
// 19. EVENT STUDY REGRESSIONS
********************************************************************************
// EVENT STUDY: EVENT-TIME DUMMIES
//
	*event_time_binned:  months relative to adoption, capped at +/- 24
	*event_time_shifted: adds 25 so all values are positive (Stata requirement)
//                        t=-1 --> 24 (omitted reference), t=0 --> 25 (first
//                        treatment period)


	*ib24.event_time_shifted creates one dummy per relative time period.
	*Pre-period  (t=-24 to t=-2): test parallel trends, should be near zero.
	*Post-period (t=0  to t=+24): dynamic effect of eWIC adoption.

cap drop event_time_binned event_time_shifted
gen event_time_binned = event_time
replace event_time_binned = -24 if event_time < -24
replace event_time_binned = 24  if event_time > 24 & !missing(event_time)

// Shift so all values positive, t=-1 becomes 24 (omitted base category)
gen event_time_shifted = event_time_binned + 25

// --- Estimate event-study models ---------------------------------------------
// Families redeemed
eststo es1: reghdfe ln_families_redeemed ib24.event_time_shifted ///
    i.month if !missing(event_time_binned), ///
    absorb(county_id year) vce(cluster county_id)

// Participation proxy
eststo es2: reghdfe ln_participation_proxy ib24.event_time_shifted ///
    i.month if !missing(event_time_binned), ///
    absorb(county_id year) vce(cluster county_id)

// Voucher dollars
eststo es3: reghdfe ln_voucher_dollars ib24.event_time_shifted ///
    i.month if !missing(event_time_binned), ///
    absorb(county_id year) vce(cluster county_id)

// --- Build calendar-year x-axis labels ----------------------------------------
// Position k corresponds to event time (k - 25); calendar month =
// tm(2019m9) + (k - 25), using the modal adoption date (September 2019,
// Wave 3) as the reference for event time 0.
local reftm = tm(2019m9)
local xlabopt ""
forvalues k = 1(4)49 {
    local actual = `k' - 25
    local calmonth = `reftm' + `actual'
    local lbl : display %tmCCYY `calmonth'
    local xlabopt `xlabopt' `k' "`lbl'"
}

// --- Plot event studies --------------------------------------------------------
coefplot es1, keep(*.event_time_shifted) omitted ///
    vertical recast(connected) ///
    yline(0, lcolor(black) lpattern(dash)) ///
    xline(25, lcolor(red) lpattern(dash)) ///
    xlabel(`xlabopt', angle(45)) ///
    title("Event Study: eWIC Adoption and Families Redeemed") ///
    subtitle("Calendar year shown relative to modal adoption date (Sept. 2019)") ///
    xtitle("Year") ///
    ytitle("Coefficient") ///
    name(es_families, replace)
graph export "graphs/event_study_families.pdf", replace

coefplot es2, keep(*.event_time_shifted) omitted ///
    vertical recast(connected) ///
    yline(0, lcolor(black) lpattern(dash)) ///
    xline(25, lcolor(red) lpattern(dash)) ///
    xlabel(`xlabopt', angle(45)) ///
    title("Event Study: eWIC Adoption and Participation Proxy") ///
    subtitle("Calendar year shown relative to modal adoption date (Sept. 2019)") ///
    xtitle("Year") ///
    ytitle("Coefficient") ///
    name(es_participation, replace)
graph export "graphs/event_study_participation.pdf", replace

coefplot es3, keep(*.event_time_shifted) omitted ///
    vertical recast(connected) ///
    yline(0, lcolor(black) lpattern(dash)) ///
    xline(25, lcolor(red) lpattern(dash)) ///
    xlabel(`xlabopt', angle(45)) ///
    title("Event Study: eWIC Adoption and Voucher Dollars Redeemed") ///
    subtitle("Calendar year shown relative to modal adoption date (Sept. 2019)") ///
    xtitle("Year") ///
    ytitle("Coefficient") ///
    name(es_voucher, replace)
graph export "graphs/event_study_voucher.pdf", replace


********************************************************************************
// 20. COUNTY-BASED DATA VISUALIZATIONS
********************************************************************************
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

    // x-axis labeled by calendar year (Jan of every other year, 2010-2026),
    // built using tm() so it isn't tied to hardcoded mdate_num integers
    cap twoway ///
        (line families_redeemed mdate_num if county == "`c'", ///
            yaxis(1) lcolor(navy)) ///
        (line ebt_share_dol mdate_num if county == "`c'", ///
            yaxis(2) lcolor(orange)), ///
        title("`c': WIC Participation and eWIC Adoption") ///
        xtitle("Year") ///
        xlabel(`=tm(2010m1)'(24)`=tm(2026m1)', format(%tmCCYY) angle(45)) ///
        ytitle("Families Redeemed", axis(1)) ///
        ytitle("EBT Dollar Share", axis(2)) ///
        legend(order(1 "Families Redeemed" 2 "EBT Share (dollars)")) ///
        `xline_opt' ///
        name(g_`cclean', replace)

    cap graph export "graphs/combined_`cclean'.pdf", replace
    cap graph close g_`cclean'
}


********************************************************************************
// 21. CLOSING LOG FILE
********************************************************************************
log close
