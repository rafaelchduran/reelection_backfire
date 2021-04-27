*Regressions: Reform & Sec. Cooperation Agreements
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
*temporal globals with date_0
	global homicides logdefuncionespc_lag_2 logdefuncionespc_lag_3 logdefuncionespc_lag_4 logdefuncionespc_lag_5 logdefuncionespc_lag_6 logdefuncionespc_lag_7 logdefuncionespc_lag_8
	global executive alignment_executive_strong_lag_2 alignment_executive_strong_lag_3 alignment_executive_strong_lag_4 alignment_executive_strong_lag_5 alignment_executive_strong_lag_6 alignment_executive_strong_lag_7 
	global governor alignment_governor_strong_lag_2 alignment_governor_strong_lag_3 alignment_governor_strong_lag_4 alignment_governor_strong_lag_5 alignment_governor_strong_lag_6 alignment_governor_strong_lag_7 
	global margin winning_margin_lag_2 winning_margin_lag_3 winning_margin_lag_4 winning_margin_lag_5 winning_margin_lag_6 winning_margin_lag_7 
	global margin_governor winning_margin_governor_lag_2 winning_margin_governor_lag_3 winning_margin_governor_lag_4 winning_margin_governor_lag_5 winning_margin_governor_lag_6 winning_margin_governor_lag_7 
	global carteles hayCarteles_lag_2 hayCarteles_lag_3 hayCarteles_lag_4 hayCarteles_lag_5 hayCarteles_lag_6 hayCarteles_lag_7 
	global logpop logpop_lag_2 logpop_lag_3 logpop_lag_4 logpop_lag_5 logpop_lag_6 logpop_lag_7 logpop_lag_8

	global controls_time_acuerdo  $executive $governor $margin  $carteles  
	global controls_time_acuerdo2 $homicides $executive $governor $margin $margin_governor $carteles $logpop


*2) treatment
	global lagsleads   lag_7 lag_6 lag_5 lag_4 lag_3 lag_2  date_0 lead_1 lead_2 lead_3
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	
*3) outcomes
	global violence logdefuncionespc ihs_defuncionespc loghomicidepc loghomicide_oldpc
	global outcome logdefuncionespc
   	global outcome2 ihs_defuncionespc

	
*========================================================================
*Run .ado 
do "wild_areg.do"
do "macros_tables.do"
*========================================================================
*SUBSET DATA TO THAT OF MAIN REGRESSION:
xi: reghdfe  acuerdo_estcom  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
keep if e(sample)==1


*========================================================================
*1) Naive Event study design: split sample
cap drop agreement_pre
gen agreement_pre=0
replace agreement_pre=1 if acuerdo_estcom==1 & year<=2015

est clear
preserve
keep if agreement_pre==0
foreach i in $outcome{
eststo: quietly  xi: areg `i' $lagsleads  $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
macros_tables3
}
restore
preserve
keep if agreement_pre==1
foreach i in $outcome{
eststo: quietly  xi: areg `i' $lagsleads  $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
macros_tables3
}
restore

esttab est*, keep($lagsleads) t(%9.3f)  star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/event_study_homicides_splitagreements.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N r2 controls munfe yearfe clustermun , fmt(%11.2gc 3) ///
 label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "State Cluster S.E.")) ///
keep($lagsleads) ///
mgroups("w/o Agreements" "w/ Agreements", ///
pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(lag_7 "t-7" lag_6 "t-6" lag_5 "t-5" lag_4 "t-4" lag_3 "t-3" lag_2 "t-2"  date_0 "Reform t=0" ///
 lead_1 "t+1" lead_2 "t+2" lead_3 "t+3") ///
collabels(none) nonotes booktabs nomtitles  nolines



*========================================================================
*2) 2SLS: effect of decentralization on homicides, instrumented by reform

*1) First stagre
est clear
xi: reghdfe   acuerdo_estcom $saturated $controls_time_acuerdo2 i.year, a(inegi) vce(cluster estado)
	* Constant 
	cap drop cons_beta
	gen cons_beta=_b[_cons]

	*Estimate yhat
	sum perc if lag_7_2018==1, meanonly
	local a = r(mean)
	sum perc if lag_6_2017==1, meanonly
	local b = r(mean)
	sum perc if lag_6_2018==1, meanonly
	local c = r(mean)
	sum perc if lag_5_2016==1, meanonly
	local d = r(mean)
	sum perc if lag_5_2017==1, meanonly
	local e = r(mean)
	sum perc if lag_5_2018==1, meanonly
	local f = r(mean)
	sum perc if lag_4_2015==1, meanonly
	local g = r(mean)
	sum perc if lag_4_2016==1, meanonly
	local h = r(mean)
	sum perc if lag_4_2017==1, meanonly
	local i = r(mean)
	sum perc if lag_4_2018==1, meanonly
	local j = r(mean)
	sum perc if lag_3_2015==1, meanonly
	local k = r(mean)
	sum perc if lag_3_2016==1, meanonly
	local l = r(mean)
	sum perc if lag_3_2017==1, meanonly
	local m = r(mean)
	sum perc if lag_3_2018==1, meanonly
	local n = r(mean)
	sum perc if lag_2_2015==1, meanonly
	local o = r(mean)
	sum perc if lag_2_2016==1, meanonly
	local p = r(mean)
	sum perc if lag_2_2017==1, meanonly
	local q = r(mean)
	sum perc if lag_2_2018==1, meanonly
	local r = r(mean)
	sum perc if date_0_2015==1, meanonly
	local s = r(mean)
	sum perc if date_0_2016==1, meanonly
	local t = r(mean)
	sum perc if date_0_2017==1, meanonly
	local u = r(mean)
	sum perc if date_0_2018==1, meanonly
	local v = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local w = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local x = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local y = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local z = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local aa = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local bb = r(mean)

*Only consider the exogenous regressors to estimate the predicte value:
cap drop lpred
gen lpred= (_b[lag_7_2018]*`a'*lag_7_2018) ///
		+ (_b[lag_6_2017]*`b'*lag_6_2017) + (_b[lag_6_2018]*`c'*lag_6_2018) ///
		+ (_b[lag_5_2016]*`d'*lag_5_2016)+(_b[lag_5_2017]*`e'*lag_5_2017) + (_b[lag_5_2018]*`f'*lag_5_2018) ///
		+ (_b[lag_4_2015]*`g'*lag_4_2015)+(_b[lag_4_2016]*`h'*lag_4_2016)+(_b[lag_4_2017]*`i'*lag_4_2017) + (_b[lag_4_2018]*`j'*lag_4_2018) ///
		+ (_b[lag_3_2015]*`k'*lag_3_2015)+(_b[lag_3_2016]*`l'*lag_3_2016)+(_b[lag_3_2017]*`m'*lag_3_2017) + (_b[lag_3_2018]*`n'*lag_3_2018) ///
		+ (_b[lag_2_2015]*`o'*lag_2_2015)+(_b[lag_2_2016]*`p'*lag_2_2016)+(_b[lag_2_2017]*`q'*lag_2_2017) + (_b[lag_2_2018]*`r'*lag_2_2018) ///
		+ (_b[date_0_2015]*`s'*date_0_2015)+(_b[date_0_2016]*`t'*date_0_2016)+(_b[date_0_2017]*`u'*date_0_2017) + (_b[date_0_2018]*`v'*date_0_2018) ///
		+ (_b[lead_1_2015]*`w'*lead_1_2015)+(_b[lead_1_2016]*`x'*lead_1_2016)+(_b[lead_1_2017]*`y'*lead_1_2017) ///
		+ (_b[lead_2_2015]*`z'*lead_2_2015)+(_b[lead_2_2016]*`aa'*lead_2_2016) ///
		+ (_b[lead_3_2015]*`bb'*lead_3_2015) 

		cap drop weight		
bys inegi: gen weight=_N
replace weight=_N/weight

*A.1) Weak instrument test
*First stage diagnostics
** see: https://www.ssc.wisc.edu/~gwallace/Papers/April%2018,%202016.pdf 
** I should check the paper with multiple instruments
testparm $saturated
estadd scalar F_joint=r(F)

/*A.2) Overidentification test
ivreghdfe logdefuncionespc lpred $controls_time_acuerdo, a(inegi year) resid
cap drop ehat
cap predict ehat, resid

xi: reghdfe ehat $saturated $controls_time_acuerdo i.year, a(inegi) 
testparm $saturated
cap drop j
gen j=2*r(F) // r(F) is the stored value of the F-statistics 
display chi2tail(1,j) // the right hand tail of a chi-square 
*/
**there is overidentification, so we bootstrap the errors.

*B. Second stage:
est clear
*eststo: ivreghdfe logdefuncionespc (acuerdo_estcom=$saturated) $controls_time_acuerdo, a(inegi year) 
eststo: xi: reghdfe   logdefuncionespc lpred $controls_time_acuerdo i.year, a(inegi) 
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun 
	estadd local wildci 
	estadd local F_joint 1739

*eststo: xi: reghdfe   logdefuncionespc lpred $controls_time_acuerdo i.year [aw=weight], a(inegi) 
eststo: wildcorrection_2sls logdefuncionespc
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	estadd local wildci \checkmark
	estadd local F_joint  1739
	
	
esttab est*, keep(lpred) se star(* 0.1 ** 0.05 *** 0.01)


esttab using "../Tables/2sls_agreement_violence.tex", replace  b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N r2 controls munfe yearfe clustermun wildci F_joint, fmt(%11.2gc 3) ///
 label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "State Cluster S.E." "Wild CI" "F-stat")) ///
keep(lpred) ///
coeflabel(lpred "Predicted Agreement w/ Governor") ///
collabels(none) nonotes booktabs nomtitles  nolines



/*========================================================================
***1) Interactions
est clear
************
***A: log(homicides)
************
cap drop agreement_pre
gen agreement_pre=0
replace agreement_pre=1 if acuerdo_estcom==1   & year<=2014
cap drop inter_*
global split_variable agreement_pre
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

xi: reghdfe  $outcome $saturated $split_variable inter_* $controls_time_acuerdo   i.year, a(inegi) vce(cluster estado)

		glo r2_logdet:  di %5.4f e(r2)
		glo N_logdet: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f  (_b[`i'_2018]*`b')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	 (_b[`i'_2018]*`b')
	glo se_`i'_log: di %5.4f r(se)
}
	
foreach i in lag_6{

	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_5 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d'))
	glo se_`i'_log: di %5.4f r(se)
}



foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c'))
	glo se_`i'_log: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b'))
	glo se_`i'_log: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_log: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_logdet: di %5.4f r(estimate)
	estadd local aggregate_logdet $aggregate_logdet
	glo se_aggregate_logdet: di %5.4f r(se)
	estadd local se_aggregate_logdet $se_aggregate_logdet
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_logdet: di %5.4f r(p)
	estadd local p_aggregate_logdet $p_aggregate_logdet
					glo est_aggregate_logdet= "" 
			if (${p_aggregate_logdet}<=0.11) global est_aggregate_logdet = "*"
			if (${p_aggregate_logdet}<=0.05) global est_aggregate_logdet = "**"
			if (${p_aggregate_logdet}<=0.01) global est_aggregate_logdet = "***"	
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/ 4	
xi: reghdfe  $outcome $saturated $split_variable inter_* $controls_time_acuerdo   i.year, a(inegi) vce(cluster estado)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest [(_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')]/ 4	
xi: reghdfe  $outcome $saturated $split_variable inter_* $controls_time_acuerdo   i.year, a(inegi) vce(cluster estado)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
		
************
***B: ihs(homicides)
************
xi: reghdfe  $outcome2 $saturated $split_variable inter_* $controls_time_acuerdo   i.year, a(inegi) vce(cluster estado)
		glo r2_logdet:  di %5.4f e(r2)
		glo N_logdet: di %11.2gc e(N)
		
*total interaction effects: leads and lags


foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f  (_b[`i'_2018]*`b')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	 (_b[`i'_2018]*`b')
	glo se_`i'_log: di %5.4f r(se)
}
	
foreach i in lag_6{

	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_5 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d'))
	glo se_`i'_log: di %5.4f r(se)
}



foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c'))
	glo se_`i'_log: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b'))
	glo se_`i'_log: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_log: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_logdet: di %5.4f r(estimate)
	estadd local aggregate_logdet $aggregate_logdet
	glo se_aggregate_logdet: di %5.4f r(se)
	estadd local se_aggregate_logdet $se_aggregate_logdet
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_logdet: di %5.4f r(p)
	estadd local p_aggregate_logdet $p_aggregate_logdet
					glo est_aggregate_logdet= "" 
			if (${p_aggregate_logdet}<=0.11) global est_aggregate_logdet = "*"
			if (${p_aggregate_logdet}<=0.05) global est_aggregate_logdet = "**"
			if (${p_aggregate_logdet}<=0.01) global est_aggregate_logdet = "***"	
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/ 4	
qui xi: reghdfe  $outcome2 $saturated $split_variable inter_* $controls_time_acuerdo   i.year, a(inegi) vce(cluster estado)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest [(_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')]/ 4	
qui xi: reghdfe  $outcome2 $saturated $split_variable inter_* $controls_time_acuerdo   i.year, a(inegi) vce(cluster estado)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
		
*Figure
coefplot (est1, rename((1) = "Reform & No Agreement") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est2, rename((1) = "Reform & Agreement")  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est3, rename((1) = "Tot. Interaction") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est4, rename((1) = "Reform & No Agreement") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est5, rename((1) = "Reform & Agreement")  msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est6, rename((1) = "Tot. Interaction") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle("Average Effect from t to t+3") ///
subtitle(" ") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI"  ///
4 "log(homicides pc)" 20 "ihs(homicides pc)") size(small) rows(2) region(col(white))) 
graph export "../Figures/agreement_violence.png", as(png) replace
graph export "../Figures/agreement_violence.pdf", as(pdf) replace
graph export "../Figures/agreement_violence.tif", as(tif) replace
graph save "../Figures/agreement_violence.gph", replace



**********************************************
*5) ABRAHAM AND SUN (2021) WITH AND WITHOUT AGREEMENTS
cap drop agreement_pre
gen agreement_pre=0
replace agreement_pre=1 if acuerdo_estcom==1 

global acuerdo agreement_pre

************
***A: w/o covariates
************	

est clear
xi: reghdfe  $outcome  $saturated $controls_time_acuerdo  i.year if $acuerdo==0 , a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)

***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f  (_b[`i'_2018]*`b')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	 (_b[`i'_2018]*`b')
	glo se_`i'_log: di %5.4f r(se)
}
	
foreach i in lag_6{

	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log: di %5.4f r(se)
}

cap foreach i in lag_5 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d'))
	glo se_`i'_log: di %5.4f r(se)
}



foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c'))
	glo se_`i'_log: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b'))
	glo se_`i'_log: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_log: di %5.4f r(se)
}

**estimate aggregate effect:
	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	lincom 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
	glo aggregate: di %5.4f r(estimate)
	estadd local aggregate $aggregate
	glo se_aggregate: di %5.4f r(se)
	estadd local se_aggregate $se_aggregate
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate: di %5.4f r(p)
	estadd local p_aggregate $p_aggregate
	glo est_aggregate= "" 
			if (${p_aggregate}<=0.1) global est_aggregate = "*"
			if (${p_aggregate}<=0.05) global est_aggregate = "**"
			if (${p_aggregate}<=0.01) global est_aggregate = "***"



************
***B: with covariates
************	
xi: reghdfe  $outcome  $saturated $controls_time_acuerdo  i.year if $acuerdo==1 , a(inegi) vce(cluster estado)

*xi: reghdfe  $outcome2  $saturated $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
*qui eststo: wildcorrection_as_long $outcome
*qui eststo: wildcorrection_as_homicides $outcome
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f  (_b[`i'_2018]*`b')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	 (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{

	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

cap foreach i in lag_5 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d'))
	glo se_`i'_ihs: di %5.4f r(se)
}



foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c'))
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b'))
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}

**estimate aggregate effect:
	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	lincom 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')] / 4
	glo aggregate2: di %5.4f r(estimate)
	estadd local aggregate2 $aggregate2
	glo se_aggregate2: di %5.4f r(se)
	estadd local se_aggregate2 $se_aggregate2
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate2: di %5.4f r(p)
	estadd local p_aggregate2 $p_aggregate2
	glo est_aggregate2= "" 
			if (${p_aggregate2}<=0.1) global est_aggregate2 = "*"
			if (${p_aggregate2}<=0.05) global est_aggregate2 = "**"
			if (${p_aggregate2}<=0.01) global est_aggregate2 = "***"

*Table
	
texdoc init  "../Tables/abraham_sun_homicides_w/oagreement.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence}
tex \label{tab:abraham_sun_lagdv}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicide per capita)} & \multicolumn{1}{c}{log(homicide per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 7 years &      $ ${beta_lag_7_log}^{${est_lag_7_log}} $ &  $ ${beta_lag_7_ihs}^{${est_lag_7_ihs}} $   \\
tex  & ($ ${se_lag_7_log}$) & ($ ${se_lag_7_ihs} $) \\
tex Lag 6 years &          $ ${beta_lag_6_log}^{${est_lag_6_log}} $ &   $ ${beta_lag_6_ihs}^{${est_lag_6_ihs}} $  \\
tex  & ($ ${se_lag_6_log}$) & ($ ${se_lag_6_ihs} $) \\
tex Lag 5 years &        $ ${beta_lag_5_log}^{${est_lag_5_log}} $ &   $ ${beta_lag_5_ihs}^{${est_lag_5_ihs}} $ \\
tex  & ($ ${se_lag_5_log}$) & ($ ${se_lag_5_ihs} $) \\
tex Lag 4 years &         $ ${beta_lag_4_log}^{${est_lag_4_log}} $ &      $ ${beta_lag_4_ihs}^{${est_lag_4_ihs}} $  \\
tex  & ($ ${se_lag_4_log}$) & ($ ${se_lag_4_ihs} $) \\
tex Lag 3 years &        $ ${beta_lag_3_log}^{${est_lag_3_log}} $ &     $ ${beta_lag_3_ihs}^{${est_lag_3_ihs}} $ \\
tex  & ($ ${se_lag_3_log}$) & ($ ${se_lag_3_ihs} $) \\
tex Lag 2 years &        $ ${beta_lag_2_log}^{${est_lag_2_log}} $ &    $ ${beta_lag_2_ihs}^{${est_lag_2_ihs}} $  \\
tex  & ($ ${se_lag_2_log}$) & ($ ${se_lag_3_ihs} $) \\
tex Reform, time 0 &        $ ${beta_date_0_log}^{${est_date_0_log}} $ &     $ ${beta_date_0_ihs}^{${est_date_0_ihs}} $ \\
tex  & ($ ${se_date_0_log}$) & ($ ${se_date_0_ihs} $) \\
tex Lead 1 year &         $ ${beta_lead_1_log}^{${est_lead_1_log}} $ &       $ ${beta_lead_1_ihs}^{${est_lead_1_ihs}} $ \\
tex  & ($ ${se_lead_1_log}$) & ($ ${se_lead_1_ihs} $) \\
tex Lead 2 years &         $ ${beta_lead_2_log}^{${est_lead_2_log}} $ &      $ ${beta_lead_2_ihs}^{${est_lead_2_ihs}} $  \\
tex  & ($ ${se_lead_2_log}$) & ($ ${se_lead_2_ihs} $) \\
tex Lead 3 years &        $ ${beta_lead_3_log}^{${est_lead_3_log}} $ &     $ ${beta_lead_3_ihs}^{${est_lead_3_ihs}} $ \\
tex  & ($ ${se_lead_3_log}$) & ($ ${se_lead_3_ihs} $) \\

tex \addlinespace
tex Observations       &            ${N_log}        &     ${N_ihs}  \\
tex R-squared        &              ${r2_log}        &           ${r2_ihs}   \\
tex Mun. FEs       &     \checkmark         &  \checkmark    \\
tex Year. FEs       &     \checkmark         &  \checkmark   \\
tex Controls$^b$   &      \checkmark       &      \checkmark    \\
tex Cohort weighted   &   \checkmark       &   \checkmark    \\
tex WILD CI   &          &   \checkmark    \\
tex Aggregate effect        &           $   ${aggregate}^{${est_aggregate}} $        &           $${aggregate2}^{${est_aggregate2}} $    \\
tex SE (aggregate eff.)        &              ${se_aggregate}        &           ${se_aggregate2}   \\
*tex p-value(aggregate eff.)       &              ${p_aggregate}        &           ${p_aggregate2}   \\


tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


****Do this but with the parallel trends!!! 


*========================================================================
*1) MEDIATIONA ANALYSIS
preserve
gen M= acuerdo_estcom
gen Y= logdefuncionespc

global controls_mediation $controls_time_acuerdo year_* inegi_*


keep M $saturated Y $controls_mediation

*Mediation analysis:
medeff (regress M $saturated $controls_mediation) (regress Y M $saturated $controls_mediation), treat($saturated) mediate(M) sims(100) vce(cluster estado)
restore


medeff (regress M T1-T2 x1 x2 `controls') (regress Y T1-T2 M x1 x2 `controls'), treat(T1-T2) mediate(M) sims(500) vce(cluster cod_depto)

*Sensitivity analysis: 
local controls lpob1988_1996 regalias1988_1996_pc transferencias1988_1996_pc areakm2 alt_mun discapital left1994 trad1994 third1994 peaceful1988_1996 rec_total1993_1996 exp_total1993_1996 desplazados1993_1996
sum `controls'
medsens (regress M T1 T2 x1 x2 `controls') (regress Y T1 T2 M x1 x2 `controls'), treat(T1) mediate(M) sims(500) 

restore


*========================================================================
*========================================================================
*========================================================================
*===============================OLD=========================================
*========================================================================
*========================================================================
*========================================================================
*========================================================================
*========================================================================
*
