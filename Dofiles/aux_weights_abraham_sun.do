*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*Weights  
*****************************************************

clear all
set more off  
set varabbrev off 

*Install packages 
*ssc install texdoc, replace
*ssc install texify, replace
*ssc install estout, replace 

*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Dofiles"

**********************************************
*Table: EFFECT OF REFORM ON HOMICIDES, WITH LAGGED DV & COHORT WEIGHTS
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*code the cohort categorical variable based on when the state first implemented the reform
gen reform_year=year if reform==1
bysort inegi: egen first_reform = min(reform_year)
drop reform_year

*Code the relative time categorical variable.
gen ry = year - first_reform
order year adopt_year rel_year ry

/*Suppose we will use a specification with lead=0, 1, 2 and 3 and lag=0,1,2,...,7 
to estimate the dynamic effect of reform status on violence.  We first generate 
these relative time indicators. We drop lag=1,8 due to collinearity.
*/
gen g_7 = ry == -7
gen g_6 = ry == -6
gen g_5 = ry == -5
gen g_4 = ry == -4
gen g_3 = ry == -3
gen g_2 = ry == -2
gen g0 = ry == 0
gen g1 = ry == 1
gen g2 = ry == 2
gen g3 = ry == 3

/*For the coefficient associate with each of the above relative time indicators 
in a two-way fixed effects regression, we can estimate the weights and export 
to a spreadsheet "weights.xlsx".*/

eventstudyweights g_7 g_6 g_5 g_4 g_3 g_2 g0 g1 g2 g3, controls(i.inegi i.year) cohort(first_reform) rel_time(ry) saveweights("weights")

*With controls
global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 	
eventstudyweights g_7 g_6 g_5 g_4 g_3 g_2 g0 g1 g2 g3, controls(i.inegi i.year $controls) cohort(first_reform) rel_time(ry) saveweights("weights_wcovariates")

