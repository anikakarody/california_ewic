# California eWIC Roll-out and WIC Participation

## Overview
This repository contains the data processing and regression analysis for a
study examining how California's staggered county-level roll-out of eWIC
(electronic WIC benefits) affected WIC redemption and participation, using
county-month administrative data from 2010–2025.

The empirical strategy combines:
- **OLS regressions** with year/month fixed effects
- **Two-way fixed effects (TWFE) panel regressions**
- **Event-study specifications** (TWFE with leads/lags)
- **Callaway and Sant'Anna (2021) staggered difference-in-differences**
  (via the `csdid` package), to address known biases in TWFE estimators
  under staggered treatment timing

The headline finding is a 4–8% decline in WIC redemption following eWIC
adoption, robust across specifications and to excluding small rural
counties without ACS coverage.

## Repository Structure
- wic_regression_analysis.do
- data/ # County-month panel: WIC admin data + ACS
- tables/ #LaTeX regression output tables
- graphs/ # Event-study and CS-DID figures (PDF)
- county_wave_crosswalk.csv # County-to-adoption-wave mapping


## Data Sources
- WIC redemption and EBT transaction data: California Department of
  Public Health (CDPH) 
- Demographic and economic controls: American Community Survey (ACS)
  county-level estimates (unemployment rate, Hispanic population share,
  median household income, children under 5)

## Reproducing the Analysis
1. Open `wic_regression_analysis.do` in Stata (18.5 used for development)
2. Update the `cd` path at the top of the file to your local directory
3. Run the do-file; required packages (`drdid`, `csdid`, `estout`,
   `ftools`, `reghdfe`, `coefplot`) are installed automatically via `ssc`
4. Outputs are written to `tables/` (LaTeX regression tables) and
   `graphs/` (PDF figures)

## Methodology Notes
- **Treatment timing** is defined county-by-county as the first month
  with positive EBT transactions, grouped into 6 adoption waves
  (Pilot through Wave 5, June 2019–August 2025; Mariposa County received
  an extension to 2025).
- **Rural counties** with no ACS coverage (15 counties) are aggregated
  into a single synthetic "Rural Aggregate" unit rather than dropped, to
  preserve sample size while allowing ACS-controlled specifications.
- **CS-DID event-study figures** use calendar-year x-axis labels anchored
  to the modal adoption date (September 2019, Wave 3) for interpretability.

## Contact
Anika Karody, UC Davis Department of Economics
anika.karody@gmail.com
