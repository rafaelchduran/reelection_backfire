*Regressions: Robustness
*Paper: Reelection Backfire
*Author: Rafael Ch (rafael.ch@nyu.edu)
*========================================================================
/*NOTES

*/


*========================================================================
*Environment
clear all
set more off  
set varabbrev off 
set maxvar 30000
set matsize 11000 
*========================================================================
*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles"

*========================================================================
*LOAD DATA
use "../../Data/ConstructionDatabase/data_final.dta", clear

*========================================================================
*SET PANEL
xtset inegi year 

*========================================================================
*SET GLOBALS
*1) controls
	global controlsall *_y_*
	global controls logdefuncionespc_mean_y_1 logdefuncionespc_mean_y_2 logdefuncionespc_mean_y_3 logdefuncionespc_mean_y_4 logdefuncionespc_mean_y_5 logdefuncionespc_mean_y_6 logdefuncionespc_mean_y_7 logdefuncionespc_mean_y_8 ///
	align_gov_y_1 align_gov_y_2 align_gov_y_3 align_gov_y_4 align_gov_y_5 align_gov_y_6 align_gov_y_7 align_gov_y_8 ///
	margin_gov_y_1 margin_gov_y_2 margin_gov_y_3 margin_gov_y_4 margin_gov_y_5 margin_gov_y_6 margin_gov_y_7 margin_gov_y_8 ///
	hayCarteles_y_1 hayCarteles_y_2 hayCarteles_y_3 hayCarteles_y_4 hayCarteles_y_5 hayCarteles_y_6 hayCarteles_y_7 hayCarteles_y_8 ///
	pri_mayor2_y_1 pri_mayor2_y_2 pri_mayor2_y_3 pri_mayor2_y_4 pri_mayor2_y_5 pri_mayor2_y_6 pri_mayor2_y_7 pri_mayor2_y_8 ///
	pan_mayor2_y_1 pan_mayor2_y_2 pan_mayor2_y_3 pan_mayor2_y_4 pan_mayor2_y_5 pan_mayor2_y_6 pan_mayor2_y_7 pan_mayor2_y_8 ///
	winning_margin_mean_y_1 winning_margin_mean_y_2 winning_margin_mean_y_3 winning_margin_mean_y_4 winning_margin_mean_y_5 winning_margin_mean_y_6 winning_margin_mean_y_7 winning_margin_mean_y_8  ///
	logpop_mean_y_1 logpop_mean_y_2 logpop_mean_y_3 logpop_mean_y_4 logpop_mean_y_5 logpop_mean_y_6 logpop_mean_y_7 logpop_mean_y_8

	*DOESN'T WORK WITH THIS [Nickel bias]: acuerdo3_mean_y_1 acuerdo3_mean_y_2 acuerdo3_mean_y_3 acuerdo3_mean_y_4 acuerdo3_mean_y_5 acuerdo3_mean_y_6 acuerdo3_mean_y_7 acuerdo3_mean_y_8
	*WORKS WITH THIS BUT POTENTIAL NICKEL BIAS: acuerdo_mean_y_1 acuerdo_mean_y_2 acuerdo_mean_y_3 acuerdo_mean_y_4 acuerdo_mean_y_5 acuerdo_mean_y_6 acuerdo_mean_y_7 acuerdo_mean_y_8
	*DOESN'T WORK WITH THIS [its the mechanism]: ap4_2_3_mean_y_1 ap4_2_3_mean_y_2 ap4_2_3_mean_y_3 ap4_2_3_mean_y_4 ap4_2_3_mean_y_5 ap4_2_3_mean_y_6 ap4_2_3_mean_y_7 ap4_2_3_mean_y_8 

	
*WITH NO y_6 onwards
	global controls logdefuncionespc_mean_y_1 logdefuncionespc_mean_y_2 logdefuncionespc_mean_y_3 logdefuncionespc_mean_y_4 logdefuncionespc_mean_y_5  ///
	align_gov_y_1 align_gov_y_2 align_gov_y_3 align_gov_y_4 align_gov_y_5 ///
	margin_gov_y_1 margin_gov_y_2 margin_gov_y_3 margin_gov_y_4 margin_gov_y_5  ///
	hayCarteles_y_1 hayCarteles_y_2 hayCarteles_y_3 hayCarteles_y_4 hayCarteles_y_5  ///
	pri_mayor2_y_1 pri_mayor2_y_2 pri_mayor2_y_3 pri_mayor2_y_4 pri_mayor2_y_5 ///
	pan_mayor2_y_1 pan_mayor2_y_2 pan_mayor2_y_3 pan_mayor2_y_4 pan_mayor2_y_5  ///
	winning_margin_mean_y_1 winning_margin_mean_y_2 winning_margin_mean_y_3 winning_margin_mean_y_4 winning_margin_mean_y_5 ///
    logpop_mean_y_1 logpop_mean_y_2 logpop_mean_y_3 logpop_mean_y_4 logpop_mean_y_5 

	*DOESN'T WORK WITH THIS [Nickel bias]: acuerdo3_mean_y_1 acuerdo3_mean_y_2 acuerdo3_mean_y_3 acuerdo3_mean_y_4 acuerdo3_mean_y_5 acuerdo3_mean_y_6 acuerdo3_mean_y_7 acuerdo3_mean_y_8
	*WORKS WITH THIS BUT POTENTIAL NICKEL BIAS: acuerdo_mean_y_1 acuerdo_mean_y_2 acuerdo_mean_y_3 acuerdo_mean_y_4 acuerdo_mean_y_5 acuerdo_mean_y_6 acuerdo_mean_y_7 acuerdo_mean_y_8
	*DOESN'T WORK WITH THIS [its the mechanism]: ap4_2_3_mean_y_1 ap4_2_3_mean_y_2 ap4_2_3_mean_y_3 ap4_2_3_mean_y_4 ap4_2_3_mean_y_5 ap4_2_3_mean_y_6 ap4_2_3_mean_y_7 ap4_2_3_mean_y_8 
	
	
*Pretreatment means
	global controls_mean logdefuncionespc_mean winning_margin_governor_mean  ///
	alignment_governor_strong_mean winning_margin_mean logpop_mean ///
	pan_mayor2_mean pri_mayor2_mean hayCarteles_mean acuerdo5_mean
	* ///
	*alignment_executive_strong_mean
	*ap4_2_3_mean ap4_2_11_mean ap4_12b_mean ap5_4_2_b_mean ap5_4_8_b_mean
	
*temporal globals
	global narco ap4_2_3_lag_8 ap4_2_3_lag_7 ap4_2_3_lag_6 ap4_2_3_lag_5 ap4_2_3_lag_4 ap4_2_3_lag_3 ap4_2_3_lag_2
	global punishment ap4_2_11_lag_8 ap4_2_11_lag_7 ap4_2_11_lag_6 ap4_2_11_lag_5 ap4_2_11_lag_4 ap4_2_11_lag_3 ap4_2_11_lag_2
	global money ap4_12b_lag_8 ap4_12b_lag_7 ap4_12b_lag_6 ap4_12b_lag_5 ap4_12b_lag_4 ap4_12b_lag_3 ap4_12b_lag_2
	global police ap5_4_2_b_lag_8 ap5_4_2_b_lag_7 ap5_4_2_b_lag_6 ap5_4_2_b_lag_5 ap5_4_2_b_lag_4 ap5_4_2_b_lag_3 ap5_4_2_b_lag_2
	global army ap5_4_8_b_lag_8 ap5_4_8_b_lag_7 ap5_4_8_b_lag_6 ap5_4_8_b_lag_5 ap5_4_8_b_lag_4 ap5_4_8_b_lag_3 ap5_4_8_b_lag_2
	global citizens $narco $punishment $money $police $army
	global homicides logdefuncionespc_lag_2 logdefuncionespc_lag_3 logdefuncionespc_lag_4 logdefuncionespc_lag_5 logdefuncionespc_lag_6 logdefuncionespc_lag_7 logdefuncionespc_lag_8
	global executive alignment_executive_strong_lag_2 alignment_executive_strong_lag_3 alignment_executive_strong_lag_4 alignment_executive_strong_lag_5 alignment_executive_strong_lag_6 alignment_executive_strong_lag_7 alignment_executive_strong_lag_8
	global governor alignment_governor_strong_lag_2 alignment_governor_strong_lag_3 alignment_governor_strong_lag_4 alignment_governor_strong_lag_5 alignment_governor_strong_lag_6 alignment_governor_strong_lag_7 alignment_governor_strong_lag_8
	global margin winning_margin_lag_2 winning_margin_lag_3 winning_margin_lag_4 winning_margin_lag_5 winning_margin_lag_6 winning_margin_lag_7 winning_margin_lag_8
	global margin_governor winning_margin_governor_lag_2 winning_margin_governor_lag_3 winning_margin_governor_lag_4 winning_margin_governor_lag_5 winning_margin_governor_lag_6 winning_margin_governor_lag_7 winning_margin_governor_lag_8
	global acuerdo acuerdo_lag_2 acuerdo_lag_3 acuerdo_lag_4 acuerdo_lag_5 acuerdo_lag_6 acuerdo_lag_7 acuerdo_lag_8
	global acuerdo2 acuerdo2_lag_2 acuerdo2_lag_3 acuerdo2_lag_4 acuerdo2_lag_5 acuerdo2_lag_6 acuerdo2_lag_7 acuerdo2_lag_8
	
	global controls_time_acuerdo $punishment $homicides $executive $governor $margin $margin_governor 
	global controls_time_acuerdo2 $punishment $homicides $executive $governor $margin $margin_governor 

	global controls $controls_time_acuerdo
	 
	global controls2 logdefuncionespc winning_margin_governor  ///
	alignment_executive_strong alignment_governor_strong winning_margin ///
	pan_mayor2 pri_mayor2 hayCarteles 
	
*2) treatment
	global lagsleads  lag_7 lag_6 lag_5 lag_4 lag_3 lag_2  date_0 lead_1 lead_2 lead_3

	global lagsleads_short  lag_5 lag_4 lag_3 lag_2  date_0 lead_1 lead_2 lead_3
	global lagsleads_short2  lag_4 lag_3 lag_2  date_0 lead_1 lead_2 lead_3

*3) outcomes
	global acuerdos acuerdo_gobestatal  acuerdo_gobfederal  acuerdo_gobestatal_federal acuerdo_estcom

*============
* 1. I regress the treatment (whether the municipality held reelection) on all the available covariates.
cap drop yhat
reghdfe reform $controls, noa
predict yhat
* 2. then take the fitted value from the regression and use it to predict each outcome, this time including unit and year fixed effects

est clear
foreach i in $acuerdos{
eststo: qui reghdfe `i' yhat, a(inegi year) vce(cluster estado)
*eststo: qui reghdfe `i' yhat i.year, a(inegi ) vce(cluster estado)

}

esttab est*, keep(yhat) t(%9.3f)  star(* 0.1 ** 0.05 *** 0.01)

* 3. This test suggests that – under the assumption that observables are representative of unobservables – selection on unobservables is not driving the results.
