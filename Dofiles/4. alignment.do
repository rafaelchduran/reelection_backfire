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
	*global executive alignment_executive_strong_lag_2 alignment_executive_strong_lag_3 alignment_executive_strong_lag_4 alignment_executive_strong_lag_5 alignment_executive_strong_lag_6 alignment_executive_strong_lag_7 alignment_executive_strong_lag_8
	*global governor alignment_governor_strong_lag_2 alignment_governor_strong_lag_3 alignment_governor_strong_lag_4 alignment_governor_strong_lag_5 alignment_governor_strong_lag_6 alignment_governor_strong_lag_7 alignment_governor_strong_lag_8
	global margin winning_margin_lag_2 winning_margin_lag_3 winning_margin_lag_4 winning_margin_lag_5 winning_margin_lag_6 winning_margin_lag_7 winning_margin_lag_8
	global margin_governor winning_margin_governor_lag_2 winning_margin_governor_lag_3 winning_margin_governor_lag_4 winning_margin_governor_lag_5 winning_margin_governor_lag_6 winning_margin_governor_lag_7 winning_margin_governor_lag_8
	global acuerdo acuerdo_lag_2 acuerdo_lag_3 acuerdo_lag_4 acuerdo_lag_5 acuerdo_lag_6 acuerdo_lag_7 acuerdo_lag_8
	global acuerdo2 acuerdo2_lag_2 acuerdo2_lag_3 acuerdo2_lag_4 acuerdo2_lag_5 acuerdo2_lag_6 acuerdo2_lag_7 acuerdo2_lag_8
	global logpop logpop_lag_2 logpop_lag_3 logpop_lag_4 logpop_lag_5 logpop_lag_6 logpop_lag_7 logpop_lag_8
	global carteles hayCarteles_lag_2 hayCarteles_lag_3 hayCarteles_lag_4 hayCarteles_lag_5 hayCarteles_lag_6 hayCarteles_lag_7 hayCarteles_lag_8

	global controls_time_acuerdo  $homicides $margin $margin_governor $logpop $carteles

	
*2) treatment
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global alignment alignment_executive_strong alignment_governor_strong governor_aligned_notpri governor_aligned_pri governor_notaligned
	global alignment_var alignment_executive_strong
	global alignment_var2 alignment_governor_strong
	global alignment_var3 governor_aligned_pri


*3) outcomes
	global outcome acuerdo_estcom
   	global outcome2 acuerdo_estcom2

	
*========================================================================
*Run .ado 
do "wild_areg.do"
do "macros_tables.do"

*========================================================================
*2) Saturated Event study design (Abraham and Sun, 2021). ALIGNMENT

************
***A: State cluster SEs 
************	
est clear 
preserve
keep if $alignment_var2==0
*eststo: qui wildcorrection_as_long $outcome
xi: reghdfe  $outcome  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols 
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)


***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
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
restore
************
***B: Wild CI
************	
preserve
keep if $alignment_var2==1
*eststo: qui wildcorrection_as_long $outcome
xi: reghdfe  $outcome  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
cap foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
cap foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

cap foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


cap foreach i in lead_1{
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


cap foreach i in lead_2{
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


cap foreach i in lead_3{
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
restore 

*Table
texdoc init  "../Tables/abraham_sun_estimates_alignment.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements, \citet{chaisemarting_etal_2019} correction}
tex \label{tab:alignment}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{Security Cooperation Agreement w/ Governor$^{a}$} \\
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
tex Aggregate effect        &              ${aggregate}        &           ${aggregate2}   \\
tex SE (aggregate eff.)        &              ${se_aggregate}        &           ${se_aggregate2}   \\
tex p-value(aggregate eff.)       &              ${p_aggregate}        &           ${p_aggregate2}   \\

tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to security cooperation agreements signed with the governor. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

*CONCLUSION ALIGNMENT WITH PRESIDENT: NO DIFFERENCE BETWEEN ALIGNED AND NOT ALIGNED AS EXPECTED
*CONCLUSION ALIGNMENT WITH GOVERNOR: A BIT LARGER WHEN ALIGNED WITH GOVERNOR AS EXPECTED
*CONCLUSION ALIGNMENT WITH GOVERNOR PRI: LARGER EFFECT FOR THOSE ALIGNED WITH PRI

*========================================================================
*Figure
est clear

*1) Alignment with President==0
preserve
keep if $alignment_var==0
*eststo: qui wildcorrection_as_long $outcome
xi: reghdfe  $outcome  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
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
	
	eststo: lincomest 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
restore 

*2) Alignment with President==1
preserve
keep if $alignment_var==1
*eststo: qui wildcorrection_as_long $outcome
xi: reghdfe  $outcome  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
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
	
	eststo: lincomest 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
restore

*3) Alignment with Governor==0
preserve
keep if $alignment_var2==0
*eststo: qui wildcorrection_as_long $outcome
xi: reghdfe  $outcome  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
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
	
	eststo: lincomest 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
restore 

*4) Alignment with Governor==1
preserve
keep if $alignment_var2==1
*qui wildcorrection_as_long $outcome
xi: reghdfe  $outcome  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
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
	
	eststo: lincomest 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
restore


*3) Alignment with Governor PRI==0
preserve
keep if $alignment_var3==0
*eststo: qui wildcorrection_as_long $outcome
xi: reghdfe  $outcome  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
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
	
	eststo: lincomest 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
restore 

*4) Alignment with Governor PRI==1
preserve
keep if $alignment_var3==1
*qui wildcorrection_as_long $outcome
xi: reghdfe  $outcome  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
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
	
	eststo: lincomest 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
restore



*create figure
preserve
label variable reform " "

coefplot (est1, rename((1) = "Alignment President = No") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Alignment President = Yes") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Alignment Governor = No") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est4, rename((1) = "Alignment Governor = Yes") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "Alignment Governor PRI = No") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Alignment Governor PRI = Yes") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0) xlabel(-0.7(0.1)0.1) xscale(range(-0.7(0.1)0.1))  ///
ytitle(" ")  xtitle("Term Limit Reform Average Effect" "from t to t+3") ///
subtitle(" ") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures/average_effects_alignment_splitsample.png", as(png) replace
graph export "../Figures/average_effects_alignment_splitsample.pdf", as(pdf) replace
graph export "../Figures/average_effects_alignment_splitsample.tif", as(tif) replace
graph save "../Figures/average_effects_alignment_splitsample.gph", replace
restore


*========================================================================
*2) Interaction effects:
preserve
rename alignment_executive_strong alig_pres
rename alignment_governor_strong alig_gov
rename governor_aligned_pri alig_gov_pri

	global alignment_var alig_pres
	global alignment_var2 alig_gov
	global alignment_var3 alig_gov_pri
	
************
***A: Alignment President
************
cap drop inter_*
global split_variable $alignment_var
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_logdet:  di %5.4f e(r2)
		glo N_logdet: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_logdet: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo se_`i'_logdet: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_logdet: di %5.4f r(se)
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
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
************
***B: Alignment Governor
************
cap drop inter_*
global split_variable $alignment_var2
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_ihsdet:  di %5.4f e(r2)
		glo N_ihsdet: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_ihsdet: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo se_`i'_ihsdet: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_ihsdet: di %5.4f r(se)
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
	glo aggregate_ihsdet: di %5.4f r(estimate)
	estadd local aggregate_ihsdet $aggregate_ihsdet
	glo se_aggregate_ihsdet: di %5.4f r(se)
	estadd local se_aggregate_ihsdet $se_aggregate_ihsdet
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
	glo p_aggregate_ihsdet: di %5.4f r(p)
	estadd local p_aggregate_ihsdet $p_aggregate_ihsdet
					glo est_aggregate_ihsdet= "" 
			if (${p_aggregate_ihsdet}<=0.11) global est_aggregate_ihsdet = "*"
			if (${p_aggregate_ihsdet}<=0.05) global est_aggregate_ihsdet = "**"
			if (${p_aggregate_ihsdet}<=0.01) global est_aggregate_ihsdet = "***"	
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
************
***C: Alignment Governor from PRI
************
cap drop inter_*
global split_variable $alignment_var3
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_her:  di %5.4f e(r2)
		glo N_her: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_her: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo se_`i'_her: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_her: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_her: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_her: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_her: di %5.4f r(se)
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
	glo aggregate_her: di %5.4f r(estimate)
	estadd local aggregate_her $aggregate_her
	glo se_aggregate_her: di %5.4f r(se)
	estadd local se_aggregate_her $se_aggregate_her
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
	glo p_aggregate_her: di %5.4f r(p)
	estadd local p_aggregate_her $p_aggregate_her
					glo est_aggregate_her= "" 
			if (${p_aggregate_her}<=0.11) global est_aggregate_her = "*"
			if (${p_aggregate_her}<=0.05) global est_aggregate_her = "**"
			if (${p_aggregate_her}<=0.01) global est_aggregate_her = "***"	
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4

restore 
*Table			
texdoc init  "../Tables/interaction_alignment.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Services Delegated to the Governor}
tex \label{tab:interaction_alignment}
tex \scalebox{0.70}{    
tex \begin{tabular}{lccc}  
tex \hline \hline       
tex \\ \multicolumn{4}{l}{Dependent variable: Signing Security Cooperation Agreement w/ Governor}\\
tex Alignment: & w/ President  & w/ Governor  & w/ Governor from PRI \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} \\

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \\
tex \addlinespace

tex t-7 &     $ ${beta_lag_7_logdet}^{${est_lag_7_logdet}} $ &     $ ${beta_lag_7_ihsdet}^{${est_lag_7_ihsdet}} $ &  $ ${beta_lag_7_her}^{${est_lag_7_her}} $ \\
tex &     ($${se_lag_7_logdet}$) &     ($${se_lag_7_ihsdet}$) & ($${se_lag_7_her}$) \\
tex t-6 &     $ ${beta_lag_6_logdet}^{${est_lag_6_logdet}} $ &     $ ${beta_lag_6_ihsdet}^{${est_lag_6_ihsdet}} $ &  $ ${beta_lag_6_her}^{${est_lag_6_her}} $  \\ 
tex &     ($${se_lag_6_logdet}$) &     ($${se_lag_6_ihsdet}$) & ($${se_lag_6_her}$) \\
tex t-5 &     $ ${beta_lag_5_logdet}^{${est_lag_5_logdet}} $ &     $ ${beta_lag_5_ihsdet}^{${est_lag_5_ihsdet}} $ &  $ ${beta_lag_5_her}^{${est_lag_5_her}} $  \\
tex &     ($${se_lag_5_logdet}$) &     ($${se_lag_5_ihsdet}$) & ($${se_lag_5_her}$) \\
tex t-4 &     $ ${beta_lag_4_logdet}^{${est_lag_4_logdet}} $ &     $ ${beta_lag_4_ihsdet}^{${est_lag_4_ihsdet}} $ &  $ ${beta_lag_4_her}^{${est_lag_4_her}} $  \\
tex &     ($${se_lag_4_logdet}$) &     ($${se_lag_4_ihsdet}$) & ($${se_lag_4_her}$) \\
tex t-3 &     $ ${beta_lag_3_logdet}^{${est_lag_3_logdet}} $ &     $ ${beta_lag_3_ihsdet}^{${est_lag_3_ihsdet}} $ &  $ ${beta_lag_3_her}^{${est_lag_3_her}} $  \\
tex &     ($${se_lag_3_logdet}$) &     ($${se_lag_3_ihsdet}$) & ($${se_lag_3_her}$) \\
tex t-2 &     $ ${beta_lag_2_logdet}^{${est_lag_2_logdet}} $ &     $ ${beta_lag_2_ihsdet}^{${est_lag_2_ihsdet}} $ &  $ ${beta_lag_2_her}^{${est_lag_2_her}} $  \\
tex &     ($${se_lag_2_logdet}$) &     ($${se_lag_2_ihsdet}$) & ($${se_lag_2_her}$) \\
tex Reform (t=0) &     $ ${beta_date_0_logdet}^{${est_date_0_logdet}} $ &     $ ${beta_date_0_ihsdet}^{${est_date_0_ihsdet}} $ &   $ ${beta_date_0_her}^{${est_date_0_her}} $   \\
tex &     ($${se_date_0_logdet}$) &     ($${se_date_0_ihsdet}$) & ($${se_date_0_her}$) \\
tex t+1 &     $ ${beta_lead_1_logdet}^{${est_lead_1_logdet}} $ &     $ ${beta_lead_1_ihsdet}^{${est_lead_1_ihsdet}} $ &    $ ${beta_lead_1_her}^{${est_lead_1_her}} $ \\
tex &     ($${se_lead_1_logdet}$) &     ($${se_lead_1_ihsdet}$) & ($${se_lead_1_her}$) \\
tex t+2 &     $ ${beta_lead_2_logdet}^{${est_lead_2_logdet}} $ &     $ ${beta_lead_2_ihsdet}^{${est_lead_2_ihsdet}} $  &     $ ${beta_lead_2_her}^{${est_lead_2_her}} $ \\
tex &     ($${se_lead_2_logdet}$) &     ($${se_lead_2_ihsdet}$)  & ($${se_lead_2_her}$) \\
tex t+3 &     $ ${beta_lead_3_logdet}^{${est_lead_3_logdet}} $ &     $ ${beta_lead_3_ihsdet}^{${est_lead_3_ihsdet}} $  &     $ ${beta_lead_3_her}^{${est_lead_3_her}} $ \\
tex &     ($${se_lead_3_logdet}$) &     ($${se_lead_3_ihsdet}$)  & ($${se_lead_3_her}$) \\
tex \\
tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}    &     ${N_her}    \\
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet}    &    ${r2_her}    \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark    \\   
tex Controls$^b$  &    \checkmark     &       \checkmark  &    \checkmark   \\  
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark    \\  
tex Reform average effect         & $${aggregate_logdet}^{${est_aggregate_logdet}} $$      & $${aggregate_ihsdet}^{${est_aggregate_ihsdet}} $$     & $${aggregate_her}^{${est_aggregate_her}} $$     \\
tex SE (average effect)      & (${se_aggregate_logdet})  & (${se_aggregate_ihsdet}) & (${se_aggregate_her}) \\
*tex p-value  & [${p_aggregate_logdet}]   & [${p_aggregate_ihsdet}]  & [${p_aggregate_her}] \\

tex \hline \hline      
tex \multicolumn{4}{p{1.5\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Even columns with outcomes with missing values where replaced by zeros assuming no activity was registered. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close

*Table average effect		
texdoc init  "../Tables/interaction_alignment_average.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Services Delegated to the Governor}
tex \label{tab:interaction_alignment_average}
tex \scalebox{0.70}{    
tex \begin{tabular}{lccc}  
tex \hline \hline       
tex \\ \multicolumn{4}{l}{Dependent variable: Signing Security Cooperation Agreement w/ Governor}\\
tex Alignment: & w/ President  & w/ Governor  & w/ Governor from PRI \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} \\

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \\
tex \addlinespace

tex Reform average effect         & $${aggregate_logdet}^{${est_aggregate_logdet}} $$      & $${aggregate_ihsdet}^{${est_aggregate_ihsdet}} $$     & $${aggregate_her}^{${est_aggregate_her}} $$     \\
tex      & (${se_aggregate_logdet})  & (${se_aggregate_ihsdet}) & (${se_aggregate_her}) \\
*tex p-value  & [${p_aggregate_logdet}]   & [${p_aggregate_ihsdet}]  & [${p_aggregate_her}] \\

tex \\
tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}    &     ${N_her}    \\
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet}    &    ${r2_her}    \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark    \\   
tex Controls$^b$  &    \checkmark     &       \checkmark  &    \checkmark   \\  
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark    \\  

tex \hline \hline      
tex \multicolumn{4}{p{1.5\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Even columns with outcomes with missing values where replaced by zeros assuming no activity was registered. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close

*========================================================================
*Figure
preserve
label variable reform " "
coefplot (est1, rename((1) = "w/ President") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "w/ Governor") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "w/ PRI Governor") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0) xlabel(-0.7(0.1)0.1) xscale(range(-0.7(0.1)0.1))   ///
ytitle("Alignment:")  xtitle("Term Limit Reform Average Effect" "from t to t+3") ///
subtitle(" ") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" 4 "President" ///
 8 "Governor" 12 "PRI Governor") rows(2)) 
graph export "../Figures/interaction_alignment.png", as(png) replace
graph export "../Figures/interaction_alignment.pdf", as(pdf) replace
graph export "../Figures/interaction_alignment.tif", as(tif) replace
graph save "../Figures/interaction_alignment.gph", replace
restore

*========================================================================
*===============================OLD======================================
*========================================================================
*========================================================================
*========================================================================
/*1) aligned vs not aligned
************
*A) alignment_executive_strong==0 | alignment_governor_strong==0:
***********
preserve
foreach outcome in acuerdo {
keep if alignment_executive_strong==0 | alignment_governor_strong==0
did_multiplegt `outcome' group year reform, breps(100) controls($controls) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta_t_`i': di %5.3f e(effect_`i')
	glo se_t_`i': di %5.3f e(se_effect_`i')
	glo t_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval_t_`i': di 2*ttail(${grados_t_`i'},abs(${t_t_`i'})) 
	glo est_t_`i'= "" 
			if (${pval_t_`i'}<=0.1) global est_t_`i' = "*"
			if (${pval_t_`i'}<=0.05) global est_t_`i' = "**"
			if (${pval_t_`i'}<=0.01) global est_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta_t_`i': di %5.3f e(placebo_`i')
	glo pse_t_`i': di %5.3f e(se_placebo_`i')
	glo pt_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval_t_`i': di 2*ttail(${pgrados_t_`i'},abs(${pt_t_`i'})) 
	glo pest_t_`i'= "" 
			if (${ppval_t_`i'}<=0.1) global pest_t_`i' = "*"
			if (${ppval_t_`i'}<=0.05) global pest_t_`i' = "**"
			if (${ppval_t_`i'}<=0.01) global pest_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	


restore


************
*B) alignment_executive_strong==1 | alignment_governor_strong==1:
************
preserve
foreach outcome in acuerdo {
keep if alignment_executive_strong==1 | alignment_governor_strong==1
did_multiplegt `outcome' group year reform, breps(100) controls($controls) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados2_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta2_t_`i': di %5.3f e(effect_`i')
	glo se2_t_`i': di %5.3f e(se_effect_`i')
	glo t2_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval2_t_`i': di 2*ttail(${grados2_t_`i'},abs(${t2_t_`i'})) 
	glo est2_t_`i'= "" 
			if (${pval2_t_`i'}<=0.1) global est2_t_`i' = "*"
			if (${pval2_t_`i'}<=0.05) global est2_t_`i' = "**"
			if (${pval2_t_`i'}<=0.01) global est2_t_`i' = "***"
			*if (${pval2_t_`i'}=0) global est2_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados2_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta2_t_`i': di %5.3f e(placebo_`i')
	glo pse2_t_`i': di %5.3f e(se_placebo_`i')
	glo pt2_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval2_t_`i': di 2*ttail(${pgrados2_t_`i'},abs(${pt2_t_`i'})) 
	glo pest2_t_`i'= "" 
			if (${ppval2_t_`i'}<=0.1) global pest2_t_`i' = "*"
			if (${ppval2_t_`i'}<=0.05) global pest2_t_`i' = "**"
			if (${ppval2_t_`i'}<=0.01) global pest2_t_`i' = "***"
			*if (${ppval2_t_`i'}=0) global pest2_t_`i' = "***"
}


restore
*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVmandounico_alignedvsnotaligned.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of Reelection Incentives on Centralization, by alignment}
tex \label{tab:chaisemartin_alignmentpri}
tex \scalebox{0.55}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable: Signing Security Cooperation Agreements}\\
tex & \multicolumn{1}{c}{Not aligned} & \multicolumn{1}{c}{Aligned (President or Governor's party)} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 5 years &        $ ${pbeta_t_4}^{${pest_t_4}} $ &     $ ${pbeta2_t_4}^{${pest2_t_4}} $ \\
tex  & ($ ${pse_t_4}$) & ($ ${pse2_t_4} $) \\
tex Lag 4 years &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
tex Lag 3 years &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) \\
tex Lag 2 years &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) \\
tex Reform, time 0 &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) \\
tex Lead 1 year &         $ ${beta_t_1}^{${est_t_1}} $ &       $ ${beta2_t_1}^{${est2_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) \\
tex Lead 2 years &         $ ${beta_t_2}^{${est_t_2}} $ &      $ ${beta2_t_2}^{${est2_t_2}} $  \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) \\

tex \addlinespace
tex Controls   &    \checkmark      &   \checkmark    \\
tex 95\% CI overlap in t+1  &    \checkmark      &   \checkmark    \\
tex 95\% CI overlap in t+2  &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{1\textwidth}}{\footnotesize{Notes: State clustered standard errors in parentheses with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

*MESSAGE: ALIGNMENT SEEMS TO BE GREATER THAN NO ALIGNMENT BUT DON'T HAVE ENOUGH POWER TO DO THIS. CI OVERLAP. 

/*
*not aligned:

             |  Estimate         SE      LB CI      UB CI          N  Switchers 
-------------+------------------------------------------------------------------
    Effect_0 | -.0470055   .1209793  -.2841249   .1901139       3311       1150 
    Effect_1 | -.3413971   .0373569  -.4146166  -.2681777       2173        852 
    Effect_2 | -.4538177   .0655598  -.5823148  -.3253206       1530        814 
    Effect_3 | -.4043993          0  -.4043993  -.4043993        727        519 
   Placebo_1 |  .0893315   .1983341  -.2994033   .4780663       3311       1150 
   Placebo_2 |  .0689138   .1521783  -.2293558   .3671833       3311       1150 
   Placebo_3 | -.0245927   .0495318  -.1216751   .0724896       3311       1150 
   Placebo_4 | -.0039759   .0030101  -.0098756   .0019239       2019        631 


*aligned:
             |  Estimate         SE      LB CI      UB CI          N  Switchers 
-------------+------------------------------------------------------------------
    Effect_0 | -.0595035   .1354818  -.3250478   .2060408       3019       1192 
    Effect_1 | -.4173088   .0718435  -.5581221  -.2764954       2107        965 
    Effect_2 | -.4988201   .0697286  -.6354881  -.3621521       1654        927 
    Effect_3 | -.3396412          0  -.3396412  -.3396412        968        694 
   Placebo_1 |  .0723216   .1297454  -.1819794   .3266227       3019       1192 
   Placebo_2 |  .0803293   .0818921  -.0801793   .2408379       3019       1192 
   Placebo_3 |  .0310934    .037863  -.0431182   .1053049       3019       1192 
   Placebo_4 | -.0632085   .0487603  -.1587787   .0323617       1547        498 


*/
*========================================================================
***1) INTERACTION WITH FEDERAL AND/OR STATE ALIGNMENT 
est clear
global interaction alignment_executive_strong alignment_governor_strong double_alignment
global interaction governor_aligned_notpri governor_aligned_pri governor_notaligned // governor alignment, pri vs no pri and no alignment
foreach outcome in acuerdo acuerdo2{
foreach interaction in $interaction{
foreach treatment in L.reform{
eststo: quietly xi: areg `outcome' c.`treatment'##c.`interaction' i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.`interaction'] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.`interaction']
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.`interaction']
global se_`i': di %5.4f r(se)
estadd local se $se_`i'
}
}
}
*esttab est*, keep(reform c.reform#c.alignment_executive_strong c.reform#c.alignment_governor_strong  c.reform#c.double_alignment ) t star(* 0.1 ** 0.05 *** 0.01)
*esttab est*, keep(L.reform cL.reform#c.alignment_executive_strong cL.reform#c.alignment_governor_strong  cL.reform#c.double_alignment ) t star(* 0.1 ** 0.05 *** 0.01)
*esttab est*, keep(L2.reform cL2.reform#c.alignment_executive_strong cL2.reform#c.alignment_governor_strong  cL2.reform#c.double_alignment ) t star(* 0.1 ** 0.05 *** 0.01)
esttab est*, keep(L.reform cL.reform#c.governor_aligned_notpri cL.reform#c.governor_aligned_pri  cL.reform#c.governor_notaligned ) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/alignment.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)")) ///
keep(L.reform cL.reform#c.alignment_executive_strong cL.reform#c.alignment_governor_strong  cL.reform#c.double_alignment ) ///
mgroups("DV: Agreement A" "DV: Agreement B", ///
pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L.reform "Term Limit Reform"  c "Term Limit Reform (t-1)*Alignment President" ///
cL.reform#c.alignment_governor_strong "Term Limit Reform (t-1)*Alignment Governor" ///
cL.reform#c.double_alignment "Term Limit Reform (t-1)*Alignment President \& Governor") ///
collabels(none) nonotes booktabs nomtitles  nolines

esttab using "../Tables/alignment_privariation.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)")) ///
keep(L.reform cL.reform#c.governor_aligned_notpri cL.reform#c.governor_aligned_pri  cL.reform#c.governor_notaligned ) ///
mgroups("DV: Agreement A" "DV: Agreement B", ///
pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L.reform "Term Limit Reform"  ///
cL.reform#c.governor_aligned_notpri "Term Limit Reform (t-1)*Aligned (not PRI)" ///
cL.reform#c.governor_aligned_pri "Term Limit Reform (t-1)*Aligned (PRI)" cL.reform#c.governor_notaligned "Term Limit Reform (t-1)*not aligned") ///
collabels(none) nonotes booktabs nomtitles  nolines



*MESSAGE: NEGATIVE INTERACTION EFFECTS WITH ALIGNMENT: SO BLAME AND TRANSFERS IS ACTING
*MESSAGE2: EFFECTS LARGER WITH GOVERNOR ALIGNMENT THAN PRESIDENT, AND LARGER WITH BOTH.
*MESSAGE3: NEED TO DIFFERENTIATE THE EFFECT OF BLAME AND TRANSFSERS. 

**for Chaisemartin I can only do this by subsetting the data

************
*A) alignment_executive_strong==0:
***********
preserve
foreach outcome in acuerdo {
keep if governor_aligned_pri==0
did_multiplegt `outcome' group year reform, breps(100) controls($controls) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta_t_`i': di %5.3f e(effect_`i')
	glo se_t_`i': di %5.3f e(se_effect_`i')
	glo t_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval_t_`i': di 2*ttail(${grados_t_`i'},abs(${t_t_`i'})) 
	glo est_t_`i'= "" 
			if (${pval_t_`i'}<=0.1) global est_t_`i' = "*"
			if (${pval_t_`i'}<=0.05) global est_t_`i' = "**"
			if (${pval_t_`i'}<=0.01) global est_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta_t_`i': di %5.3f e(placebo_`i')
	glo pse_t_`i': di %5.3f e(se_placebo_`i')
	glo pt_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval_t_`i': di 2*ttail(${pgrados_t_`i'},abs(${pt_t_`i'})) 
	glo pest_t_`i'= "" 
			if (${ppval_t_`i'}<=0.1) global pest_t_`i' = "*"
			if (${ppval_t_`i'}<=0.05) global pest_t_`i' = "**"
			if (${ppval_t_`i'}<=0.01) global pest_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	


restore


************
*B) alignment_executive_strong==1:
************
preserve
foreach outcome in acuerdo {
keep if governor_aligned_pri==1
did_multiplegt `outcome' group year reform, breps(100) controls($controls) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados2_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta2_t_`i': di %5.3f e(effect_`i')
	glo se2_t_`i': di %5.3f e(se_effect_`i')
	glo t2_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval2_t_`i': di 2*ttail(${grados2_t_`i'},abs(${t2_t_`i'})) 
	glo est2_t_`i'= "" 
			if (${pval2_t_`i'}<=0.1) global est2_t_`i' = "*"
			if (${pval2_t_`i'}<=0.05) global est2_t_`i' = "**"
			if (${pval2_t_`i'}<=0.01) global est2_t_`i' = "***"
			*if (${pval2_t_`i'}=0) global est2_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados2_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta2_t_`i': di %5.3f e(placebo_`i')
	glo pse2_t_`i': di %5.3f e(se_placebo_`i')
	glo pt2_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval2_t_`i': di 2*ttail(${pgrados2_t_`i'},abs(${pt2_t_`i'})) 
	glo pest2_t_`i'= "" 
			if (${ppval2_t_`i'}<=0.1) global pest2_t_`i' = "*"
			if (${ppval2_t_`i'}<=0.05) global pest2_t_`i' = "**"
			if (${ppval2_t_`i'}<=0.01) global pest2_t_`i' = "***"
			*if (${ppval2_t_`i'}=0) global pest2_t_`i' = "***"
}


restore
*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVmandounico_byalignment_pri.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin_alignmentpri}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{Alignment but not PRI} & \multicolumn{1}{c}{Alignment with PRI$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 5 years &        $ ${pbeta_t_4}^{${pest_t_4}} $ &     $ ${pbeta2_t_4}^{${pest2_t_4}} $ \\
tex  & ($ ${pse_t_4}$) & ($ ${pse2_t_4} $) \\
tex Lag 4 years &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
tex Lag 3 years &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) \\
tex Lag 2 years &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) \\
tex Reform, time 0 &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) \\
tex Lead 1 year &         $ ${beta_t_1}^{${est_t_1}} $ &       $ ${beta2_t_1}^{${est2_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) \\
tex Lead 2 years &         $ ${beta_t_2}^{${est_t_2}} $ &      $ ${beta2_t_2}^{${est2_t_2}} $  \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) \\

tex \addlinespace
tex State Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.6\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Secondary version of security cooperation agreements. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

*MESSAGE: ALIGNMENT SEEMS TO BE GREATER THAN NO ALIGNMENT BUT DON'T HAVE ENOUGH POWER TO DO THIS. CI OVERLAP. 

************
*Robustness: alignment with governor but not from the PRI
**for Chaisemartin I can only do this by subsetting the data

************
*A) alignment_executive_strong==0:
***********
preserve
foreach outcome in acuerdo {
keep if governor_aligned_notpri==0
did_multiplegt `outcome' group year reform, breps(100) controls($controls) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta_t_`i': di %5.3f e(effect_`i')
	glo se_t_`i': di %5.3f e(se_effect_`i')
	glo t_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval_t_`i': di 2*ttail(${grados_t_`i'},abs(${t_t_`i'})) 
	glo est_t_`i'= "" 
			if (${pval_t_`i'}<=0.1) global est_t_`i' = "*"
			if (${pval_t_`i'}<=0.05) global est_t_`i' = "**"
			if (${pval_t_`i'}<=0.01) global est_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta_t_`i': di %5.3f e(placebo_`i')
	glo pse_t_`i': di %5.3f e(se_placebo_`i')
	glo pt_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval_t_`i': di 2*ttail(${pgrados_t_`i'},abs(${pt_t_`i'})) 
	glo pest_t_`i'= "" 
			if (${ppval_t_`i'}<=0.1) global pest_t_`i' = "*"
			if (${ppval_t_`i'}<=0.05) global pest_t_`i' = "**"
			if (${ppval_t_`i'}<=0.01) global pest_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	


restore


************
*B) alignment_executive_strong==1:
************
preserve
foreach outcome in acuerdo {
keep if governor_aligned_notpri==1
did_multiplegt `outcome' group year reform, breps(100) controls($controls) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados2_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta2_t_`i': di %5.3f e(effect_`i')
	glo se2_t_`i': di %5.3f e(se_effect_`i')
	glo t2_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval2_t_`i': di 2*ttail(${grados2_t_`i'},abs(${t2_t_`i'})) 
	glo est2_t_`i'= "" 
			if (${pval2_t_`i'}<=0.1) global est2_t_`i' = "*"
			if (${pval2_t_`i'}<=0.05) global est2_t_`i' = "**"
			if (${pval2_t_`i'}<=0.01) global est2_t_`i' = "***"
			*if (${pval2_t_`i'}=0) global est2_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados2_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta2_t_`i': di %5.3f e(placebo_`i')
	glo pse2_t_`i': di %5.3f e(se_placebo_`i')
	glo pt2_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval2_t_`i': di 2*ttail(${pgrados2_t_`i'},abs(${pt2_t_`i'})) 
	glo pest2_t_`i'= "" 
			if (${ppval2_t_`i'}<=0.1) global pest2_t_`i' = "*"
			if (${ppval2_t_`i'}<=0.05) global pest2_t_`i' = "**"
			if (${ppval2_t_`i'}<=0.01) global pest2_t_`i' = "***"
			*if (${ppval2_t_`i'}=0) global pest2_t_`i' = "***"
}


restore
*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVmandounico_byalignment_notpri.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin_alignmentpri}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{Alignment} & \multicolumn{1}{c}{Alignment with no PRI party$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 5 years &        $ ${pbeta_t_4}^{${pest_t_4}} $ &     $ ${pbeta2_t_4}^{${pest2_t_4}} $ \\
tex  & ($ ${pse_t_4}$) & ($ ${pse2_t_4} $) \\
tex Lag 4 years &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
tex Lag 3 years &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) \\
tex Lag 2 years &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) \\
tex Reform, time 0 &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) \\
tex Lead 1 year &         $ ${beta_t_1}^{${est_t_1}} $ &       $ ${beta2_t_1}^{${est2_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) \\
tex Lead 2 years &         $ ${beta_t_2}^{${est_t_2}} $ &      $ ${beta2_t_2}^{${est2_t_2}} $  \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) \\

tex \addlinespace
tex State Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.6\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Secondary version of security cooperation agreements. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

*MESSAGE: NO DIFFERENCE








*========================================================================
***2) INTERACTION WITH SUBSET OF ALIGNMENT OF PRI MAYORS WITH PRI GOVERNOR
sort inegi year

*1) PRI GOVERNOR ALIGNMENT
preserve
*drop the ones that are not aligned: it's like a triple interaction 
drop if governor_alignment2==2

est clear
global interaction governor_alignment2
foreach outcome in acuerdo{
foreach treatment in L.reform{
eststo: quietly xi: areg `outcome' c.`treatment'##c.$interaction i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'

}
}
restore


**2) MODEL WITH CONTROL THOSE THAT ARE NOT PRI AND ARE NOT ALIGNED:
preserve
replace governor_alignment2=0 if governor_alignment2==2
global interaction governor_alignment2
foreach outcome in acuerdo{
foreach treatment in L.reform{
eststo: quietly xi: areg `outcome' c.`treatment'##c.$interaction i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'
}
}

restore


*3) PRI PRESIDENT ALIGNMENT
global interaction president_alignment
foreach outcome in acuerdo{
foreach treatment in L.reform{
eststo: quietly xi: areg `outcome' c.`treatment'##c.$interaction i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'

}
}

esttab est*, keep(L.reform cL.reform#c.$interaction ) t star(* 0.1 ** 0.05 *** 0.01)
 

esttab using "../Tables/twfe_interaction_pri_governor_president_alignment.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)")) ///
keep(L.reform cL.reform#c.governor_alignment2 cL.reform#c.president_alignment ) ///
mgroups("DV: Security Cooperation Agreement A" "DV: Security Cooperation Agreement B", ///
pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L.reform "Term Limit Reform (t-1)"  cL.reform#c.governor_alignment2 "Term Limit Reform (t-1)*Alignment PRI Governor" ///
cL.reform#c.president_alignment "Term Limit Reform (t-1)*Alignment PRI President") ///
collabels(none) nonotes booktabs nomtitles  nolines

/*MESSAGE: 
1. for governor_alignment2 there is a PRI effect in aligment: muns with alignment with PRI governor have a decrease in likelihood of signing vs. aligned but no PRI. 
2. this is true even if the experiment at hand is comparing PRI governor alignment to all none PRI alignment (including also misalignment)
3. this does not hold for PRI presidential alignment in the simple interaction but it does for the total interaction. 

So its a story of PRI governor not PRI presidents! 

*/

*========================================================================
***3) INTERACTION WITH GOVERNOR AND PRESIDENT PRI

*A) INTERACTION WITH GOVERNOR PRI
est clear
global interaction governor_pri
foreach outcome in acuerdo{
foreach treatment in reform{
eststo: quietly xi: areg `outcome' c.`treatment'##c.$interaction i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'

}
}

*B) INTERACTION WITH PRESIDENT PRI
global interaction president_pri
foreach outcome in acuerdo {
foreach treatment in reform{
eststo:  xi: areg `outcome' c.`treatment'##c.$interaction i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'

}
}


esttab est*, keep(reform c.reform#c.governor_pri c.reform#c.president_pri ) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/twfe_interaction_governor_from_pri.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)")) ///
keep(reform c.reform#c.governor_pri c.reform#c.president_pri ) ///
mgroups("DV: Security Cooperation Agreement A" "DV: Security Cooperation Agreement B", ///
pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(reform "Term Limit Reform"  c.reform#c.governor_pri "Term Limit Reform*Governor PRI" ///
c.reform#c.president_pri "Term Limit Reform*President PRI" ) ///
collabels(none) nonotes booktabs nomtitles  nolines

*MESSAGE: PRI governor decreases the likelihood of signing agreement for mayors facing reelection. 
*PRI gets erased with the time fixed effects. 



*========================================================================
***3) Saturated Model, Alignment Heterogeneous Effects-Effect of Reform on Sec. Coop Agreement
****NOTE: to test credit claiming: when citizens can blame mayors, mayors move from signing. 


*Set globals: 
	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2  logdefuncionespc_lag_8 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3	 logdefuncionespc_lag_2
	global outcome acuerdo
	global outcome2 acuerdo2
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

************
***A: acuerdo w/o covariates // GOVERNOR ALIGNMENT
************
quietly	areg  $outcome  $saturated  i.year, a(inegi) vce(cluster inegi)
keep if e(sample)==1

est clear
foreach i in governor_alignment2{

areg   $outcome c.${saturated}##c.`i'  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'1:  di %5.4f e(r2)
		glo N_`i'1: di %11.2gc e(N)	
		

lincom _b[1.lead_3_2015] + _b[1.lead_3_2015#c.`i'] 
	glo beta_`i'1: di %5.4f r(estimate)
	glo se_`i'1: di %5.4f r(se)
	glo g_`i'1: di r(df)
	glo t_`i'1: di  %5.4f ${beta_`i'1}/${se_`i'1}
	glo p_`i'1: di 2*ttail(${g_`i'1},abs(${t_`i'1})) 
	glo est_`i'1= "" 
			if (${p_`i'1}<=0.1) global est_`i'1  = "*"
			if (${p_`i'1}<=0.05) global est_`i'1 = "**"
			if (${p_`i'1}<=0.01) global est_`i'1 = "***"	
}
	

************
***B: acuerdo2 w/o covariates // GOVERNOR ALIGNMENT
************


est clear
foreach i in governor_alignment2{

areg   $outcome2 c.${saturated}##c.`i'  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'2:  di %5.4f e(r2)
		glo N_`i'2: di %11.2gc e(N)	

lincom _b[1.lead_3_2015] + _b[1.lead_3_2015#c.`i'] 
	glo beta_`i'2: di %5.4f r(estimate)
	glo se_`i'2: di %5.4f r(se)
	glo g_`i'2: di r(df)
	glo t_`i'2: di  %5.4f ${beta_`i'2}/${se_`i'2}
	glo p_`i'2: di 2*ttail(${g_`i'2},abs(${t_`i'2})) 
	glo est_`i'2= "" 
			if (${p_`i'2}<=0.1) global est_`i'2  = "*"
			if (${p_`i'2}<=0.05) global est_`i'2 = "**"
			if (${p_`i'2}<=0.01) global est_`i'2 = "***"	
}


************
***C: acuerdo w/o covariates // FEDERAL ALIGNMENT
************
est clear
foreach i in president_alignment{

areg   $outcome c.${saturated}##c.`i'  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'3:  di %5.4f e(r2)
		glo N_`i'3: di %11.2gc e(N)	

lincom _b[1.lead_3_2015] + _b[1.lead_3_2015#c.`i'] 
	glo beta_`i'3: di %5.4f r(estimate)
	glo se_`i'3: di %5.4f r(se)
	glo g_`i'3: di r(df)
	glo t_`i'3: di  %5.4f ${beta_`i'3}/${se_`i'3}
	glo p_`i'3: di 2*ttail(${g_`i'3},abs(${t_`i'3})) 
	glo est_`i'3= "" 
			if (${p_`i'3}<=0.1) global est_`i'3  = "*"
			if (${p_`i'3}<=0.05) global est_`i'3 = "**"
			if (${p_`i'3}<=0.01) global est_`i'3 = "***"	
}


************
***D: acuerdo2 w/o covariates // FEDERAL ALIGNMENT
************
est clear
foreach i in president_alignment{

areg   $outcome2 c.${saturated}##c.`i' $controls  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'4:  di %5.4f e(r2)
		glo N_`i'4: di %11.2gc e(N)	

lincom _b[1.lead_3_2015] + _b[1.lead_3_2015#c.`i'] 
	glo beta_`i'4: di %5.4f r(estimate)
	glo se_`i'4: di %5.4f r(se)
	glo g_`i'4: di r(df)
	glo t_`i'4: di  %5.4f ${beta_`i'4}/${se_`i'4}
	glo p_`i'4: di 2*ttail(${g_`i'4},abs(${t_`i'4})) 
	glo est_`i'4= "" 
			if (${p_`i'4}<=0.1) global est_`i'4  = "*"
			if (${p_`i'4}<=0.05) global est_`i'4 = "**"
			if (${p_`i'4}<=0.01) global est_`i'4 = "***"	
}




texdoc init  "../Tables/abraham_sun_estimates_heteffects_alignment.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect$^a$: the role of Alignment with the Party of the Governor and President}
tex \label{tab:abraham_sun_heteffects}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{acuerdo} & \multicolumn{2}{c}{acuerdo2$^{b}$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace

tex Reform (t+3)*Alignment Governor &     $ ${beta_governor_alignment1}^{${est_governor_alignment1}} $ &  &  $ ${beta_governor_alignment2}^{${est_governor_alignment2}} $  & \\
tex  &     ($ ${se_governor_alignment1} $) &  & ($ ${se_governor_alignment2} $)  & \\

tex Reform (t+3)*Alignment President  &    &   $ ${beta_president_alignment3}^{${est_president_alignment3}} $ &  &  $ ${beta_president_alignment4}^{${est_president_alignment4}} $ \\
tex  &    &     ($ ${se_president_alignment3} $) & & ($ ${se_president_alignment4} $)  \\

tex \\
tex \addlinespace
tex Observations       &        ${N_governor_alignment1}    &        ${N_governor_alignment2}    &     ${N_president_alignment3}      &     ${N_president_alignment4}  \\
tex R-squared       &        ${r2_governor_alignment1}    &        ${r2_governor_alignment2}    &     ${r2_president_alignment3}      &     ${r2_president_alignment4}  \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^c$  &  \checkmark       &       \checkmark  &  \checkmark        &   \checkmark    \\
tex Cohort weighted$^d$  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{5}{p{1\textwidth}}{\footnotesize{Notes:$^a$ Total interaction effect tests the linear hypothesis of the estimated coefficient of alignment with Federal Government indicator (winning margin) + estimated coefficient of the interaction of alignment (winning margin)*lead t=3. This is a post-estimation test using the same specification as that of Table \ref{tab:abraham_sun_lagdv} column (1). Other leads and the indicator at time t=0 when reform came to effect are omitted due to collinearity. Standard errors of linear hypothesis test in parentheses with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided test with the null hypothesis equal to 0. Main regression with standard errors clustered at the state-level. $^b$ Refers to the inverse hyperbolic sine transformation. $^c$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. I also include the lag of the outcome, i.e. logged homicides per capita as control. $^d$ Estimates weighted by each cohort's relative share of the sample following \citet{abraham_sun_2020}.}} \\
tex \end{tabular}
tex } 
tex \end{table} 
texdoc close



*========================================================================
*=========================WORKED TILL HERE============================
*========================================================================

*========================================================================
*SIMPLE HET. EFFECTS BY PARTY 
gen pri_mayor2=.
replace  pri_mayor2=1 if firstword=="pri"
replace pri_mayor2=0 if firstword!="pri" & firstword!=""

*fill:
sort inegi year
foreach i in  pri_mayor2{
fillmissing `i', with(previous)
}

gen morena_mayor2=.
replace  morena_mayor2=1 if firstword=="morena"
replace morena_mayor2=0 if firstword!="morena" & firstword!=""

gen pan_mayor2=.
replace  pan_mayor2=1 if firstword=="pan"
replace pan_mayor2=0 if firstword!="pan" & firstword!=""

*fill:
sort inegi year
foreach i in  morena_mayor2 pan_mayor2{
fillmissing `i', with(previous)
}

order pri_mayor2 morena_mayor2 pan_mayor2

*A) INTERACTION WITH PRI MAYOR
est clear
global interaction pri_mayor2
foreach outcome in acuerdo{
foreach treatment in L.reform{
eststo: quietly xi: areg `outcome' c.`treatment'##c.$interaction i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'

}
}

*B) INTERACTION WITH MORENA MAYOR
global interaction morena_mayor2
foreach outcome in acuerdo {
foreach treatment in L.reform{
eststo:  xi: areg `outcome' c.`treatment'##c.$interaction i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'

}
}

*C) INTERACTION WITH PAN MAYOR
global interaction pan_mayor2
foreach outcome in acuerdo {
foreach treatment in L.reform{
eststo:  xi: areg `outcome' c.`treatment'##c.$interaction i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'

}
}


esttab est*, keep(L.reform cL.reform#c.pri_mayor2 cL.reform#c.morena_mayor2 cL.reform#c.pan_mayor2   ) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/twfe_interaction_governor_from_pri.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)")) ///
keep(L.reform cL.reform#c.pri_mayor2 cL.reform#c.morena_mayor2 cL.reform#c.pan_mayor2 ) ///
mgroups("DV: Security Cooperation Agreement A" "DV: Security Cooperation Agreement B", ///
pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L.reform "Term Limit Reform"  cL.reform#c.pri_mayor2 "Term Limit Reform*PRI mayor" ///
cL.reform#c.pan_mayor2 "Term Limit Reform*PRI mayor" ///
cL.reform#c.pri_mayor2 "Term Limit Reform*PRI mayor" ) ///
collabels(none) nonotes booktabs nomtitles  nolines

*MESSAGE: PARTY DOES MATTER, BUT GET SAME RESULTS FOR ALL PARTIES. SO NO BIGGY. 

*========================================================================
*DIFFERENCE WITH PRESIDENT ALIGNMENT. THIS WAS POSITIVE





*========================================================================
*INCLUDE BOTH MODELS: OVERALL ALIGNMENT AND PRI ALIGNMENT TO COMPARE THE COEFFICIENTS
sort inegi year

est clear
*1) PRI GOVERNOR ALIGNMENT
preserve
*drop the ones that are not aligned: it's like a triple interaction 
drop if governor_alignment2==2

global interaction governor_alignment2
global interaction2 governor_pri
foreach outcome in acuerdo{
foreach treatment in L.reform{
eststo: quietly xi: areg `outcome' c.`treatment'##c.$interaction c.`treatment'##c.$interaction2 i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect for first interaction:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'

***this is the total interaction effect for second interaction:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction2] = 0
global total_interaction2: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction2]
estadd local tot_int2 $total_interaction2
global pvalue2: di %5.4f r(p)
estadd local pvalue2 $pvalue2
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction2]
global se2_`i': di %5.4f r(se)
estadd local se2 $se2_`i'

}
}
restore


**2) MODEL WITH CONTROL THOSE THAT ARE NOT PRI AND ARE NOT ALIGNED:
preserve
replace governor_alignment2=0 if governor_alignment2==2
global interaction governor_alignment2
global interaction2 governor_pri
foreach outcome in acuerdo{
foreach treatment in L.reform{
eststo: quietly xi: areg `outcome' c.`treatment'##c.$interaction c.`treatment'##c.$interaction2 i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect for first interaction:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'

***this is the total interaction effect for second interaction:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction2] = 0
global total_interaction2: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction2]
estadd local tot_int2 $total_interaction2
global pvalue2: di %5.4f r(p)
estadd local pvalue2 $pvalue2
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction2]
global se2_`i': di %5.4f r(se)
estadd local se2 $se2_`i'
}
}

restore


*3) PRI PRESIDENT ALIGNMENT
global interaction president_alignment
global interaction2 president_pri
foreach outcome in acuerdo{
foreach treatment in L.reform{
eststo: quietly xi: areg `outcome' c.`treatment'##c.$interaction c.`treatment'##c.$interaction2 i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect for first interaction:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'

***this is the total interaction effect for second interaction:
test  _b[`treatment']+_b[c.`treatment'#c.$interaction2] = 0
global total_interaction2: di %5.4f _b[`treatment']+_b[c.`treatment'#c.$interaction2]
estadd local tot_int2 $total_interaction2
global pvalue2: di %5.4f r(p)
estadd local pvalue2 $pvalue2
lincom _b[`treatment']+_b[c.`treatment'#c.$interaction2]
global se2_`i': di %5.4f r(se)
estadd local se2 $se2_`i'

}
}


esttab est*, keep(L.reform cL.reform#c.governor_alignment2 cL.reform#c.governor_pri ///
cL.reform#c.president_alignment cL.reform#c.president_pri) t star(* 0.1 ** 0.05 *** 0.01)
 

esttab using "../Tables/test.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue tot_int2 se2 pvalue2, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)"  "Tot.Int.2" "S.E.2(Tot. Int.2)" "p-value2(Tot.Int.2)")) ///
keep(L.reform cL.reform#c.governor_alignment2 cL.reform#c.governor_pri ///
president_alignment president_pri)

 ///
mgroups("DV: Security Cooperation Agreement A" "DV: Security Cooperation Agreement B", ///
pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L.reform "Term Limit Reform (t-1)"  cL.reform#c.governor_alignment2 "Term Limit Reform (t-1)*Alignment PRI Governor"  cL.reform#c.governor_alignment "Term Limit Reform (t-1)*PRI Governor"///
cL.reform#c.president_alignment "Term Limit Reform (t-1)*Alignment PRI President") ///
collabels(none) nonotes booktabs nomtitles  nolines


