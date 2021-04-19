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
	global lagsleads2   lag_4 lag_3 lag_2  date_0 lead_1  lead_3


*3) outcomes
	global acuerdos acuerdo_gobfederal acuerdo_gobfederal2 acuerdo_gobfederal3 acuerdo_gobfederal4 acuerdo_gobestatal_federal
	global acuerdos2 acuerdo_estcom acuerdo_fednoest
	global outcome acuerdo_estcom
   	global outcome2 acuerdo_fednoest

	
*========================================================================
*Run .ado 
do "wild_areg.do"
do "macros_tables.do"
*========================================================================
*Subset data for coefficient comparison across models:
qui reghdfe acuerdo_fednoest $lagsleads2 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
keep if e(sample)==1


*========================================================================
*1) AGREEMENTS WITH STATE

************
***A: State cluster SEs 
************	
est clear 
preserve
keep if $alignment_var2==0
eststo: qui wildcorrection_as_trim2 $outcome
*xi: reghdfe  $outcome  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols 
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)


***estimate linear combination by lead/lag:
foreach i in lag_4 {
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di  (_b[`i'_2018]*`d')
	test (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	 (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}
foreach i in lag_3 {
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2017]*`c') 
	test (_b[`i'_2017]*`c') =0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2017]*`c') 
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2017]*`c') 
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_2 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2016]*`b') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}
foreach i in  date_0{

	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2016]*`b')
	test (_b[`i'_2016]*`b')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`b')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2016]*`b')
	glo se_`i'_log: di %5.4f r(se)
}



foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
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
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	lincom 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
	glo aggregate: di %5.4f r(estimate)
	estadd local aggregate $aggregate
	glo se_aggregate: di %5.4f r(se)
	estadd local se_aggregate $se_aggregate
	test 	[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3	=0
	glo p_aggregate: di %5.4f r(p)
	estadd local p_aggregate $p_aggregate
				glo est_aggregate= "" 
			if (${p_aggregate}<=0.11) global est_aggregate = "*"
			if (${p_aggregate}<=0.05) global est_aggregate = "**"
			if (${p_aggregate}<=0.01) global est_aggregate = "***"	

restore
************
***B: Wild CI
************	
preserve
keep if $alignment_var2==1
eststo: qui wildcorrection_as_trim2 $outcome
*xi: reghdfe  $outcome  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols 
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)


***estimate linear combination by lead/lag:

foreach i in lag_4 {
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di  (_b[`i'_2018]*`d')
	test (_b[`i'_2018]*`d')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	 (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}
foreach i in lag_3 {
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2017]*`c') 
	test (_b[`i'_2017]*`c') =0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2017]*`c') 
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2017]*`c') 
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_2 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2016]*`b') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}
foreach i in  date_0{

	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2016]*`b')
	test (_b[`i'_2016]*`b')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2016]*`b')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}



foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
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

	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	lincom 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
	glo aggregate2: di %5.4f r(estimate)
	estadd local aggregate2 $aggregate2
	glo se_aggregate2: di %5.4f r(se)
	estadd local se_aggregate2 $se_aggregate2
	test 	[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3	=0
	glo p_aggregate2: di %5.4f r(p)
	estadd local p_aggregate2 $p_aggregate2
				glo est_aggregate2= "" 
			if (${p_aggregate2}<=0.11) global est_aggregate2 = "*"
			if (${p_aggregate2}<=0.05) global est_aggregate2 = "**"
			if (${p_aggregate2}<=0.01) global est_aggregate2 = "***"	

restore 

*Table
texdoc init  "../Tables/comparisonfedyestatal_alignment.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements, \citet{chaisemarting_etal_2019} correction}
tex \label{tab:comparisonfedyestatal_alignment}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{Security Cooperation Agreement w/ Governor$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 4 years &         $ ${beta_lag_4_log}^{${est_lag_4_log}} $ &      $ ${beta_lag_4_ihs}^{${est_lag_4_ihs}} $  \\
tex  & ($ ${se_lag_4_log}$) & ($ ${se_lag_4_ihs} $) \\
tex Lag 3 years &        $ ${beta_lag_3_log}^{${est_lag_3_log}} $ &     $ ${beta_lag_3_ihs}^{${est_lag_3_ihs}} $ \\
tex  & ($ ${se_lag_3_log}$) & ($ ${se_lag_3_ihs} $) \\
tex Lag 2 years &        $ ${beta_lag_2_log}^{${est_lag_2_log}} $ &    $ ${beta_lag_2_ihs}^{${est_lag_2_ihs}} $  \\
tex  & ($ ${se_lag_2_log}$) & ($ ${se_lag_2_ihs} $) \\
tex Reform, time 0 &        $ ${beta_date_0_log}^{${est_date_0_log}} $ &     $ ${beta_date_0_ihs}^{${est_date_0_ihs}} $ \\
tex  & ($ ${se_date_0_log}$) & ($ ${se_date_0_ihs} $) \\
tex Lead 1 year &         $ ${beta_lead_1_log}^{${est_lead_1_log}} $ &       $ ${beta_lead_1_ihs}^{${est_lead_1_ihs}} $ \\
tex  & ($ ${se_lead_1_log}$) & ($ ${se_lead_1_ihs} $) \\
tex Lead 3 years &        $ ${beta_lead_3_log}^{${est_lead_3_log}} $ &     $ ${beta_lead_3_ihs}^{${est_lead_3_ihs}} $ \\
tex  & ($ ${se_lead_3_log}$) & ($ ${se_lead_3_ihs} $) \\

tex \addlinespace
tex Observations       &            ${N_log}        &     ${N_ihs}  \\
tex R-squared        &              ${r2_log}        &           ${r2_ihs}   \\
tex Mun. FEs       &     \checkmark         &  \checkmark    \\
tex Year. FEs       &     \checkmark         &  \checkmark   \\
tex Controls$^b$   &      \checkmark       &      \checkmark    \\
tex Cohort weighted   &   \checkmark       &   \checkmark    \\
tex WILD CI   &          &       \\
tex Aggregate effect        &              $${aggregate}^{${est_aggregate}} $$         &            $${aggregate2}^{${est_aggregate2}} $$   \\
tex SE (aggregate eff.)        &              (${se_aggregate})       &           (${se_aggregate2})   \\
*tex p-value(aggregate eff.)       &              ${p_aggregate}        &           ${p_aggregate2}   \\

tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to security cooperation agreements signed with the governor. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

*CONCLUSION: SMALLER WHEN ALIGNED WITH PRESIDENT ON SIGNING AGREEMENTS WITH STATE. BOTH NEGATIVE
*CONCLUSION: IDENTICAL WHEN ALIGNED OR NOT WITH PRESIDENT ON SIGNING AGREEMENTS WITH STATE. BOTH NEGATIVE


*========================================================================
*1) AGREEMENTS WITH OTHER ACTORS

*CONCLUSION: IDENTICAL WHEN ALIGNED OR NO WITH PRESIDENT ON SIGNING AGREEMENTS WITH OTHER ACTORS. POSITIVE AND NON SIGNIFICANT THOUGH. 
*CONCLUSION: BIGGER WHEN ALIGNED WITH GOVERNOR ON SIGNING AGREEMENTS WITH OTHER ACTORS. POSITIVE AND NON SIGNIFICANT THOUGH.


************
***A: NOT ALIGNED
************	
est clear 
preserve
keep if $alignment_var2==0
*eststo: qui wildcorrection_as_trim3 $outcome2
xi: reghdfe  $outcome2  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols 
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)


***estimate linear combination by lead/lag:
foreach i in lag_4 {
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di  (_b[`i'_2018]*`d')
	test (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	 (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}
foreach i in lag_3 {
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2017]*`c') 
	test (_b[`i'_2017]*`c') =0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2017]*`c') 
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2017]*`c') 
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_2 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2016]*`b') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}
foreach i in  date_0{

	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2016]*`b')
	test (_b[`i'_2016]*`b')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`b')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2016]*`b')
	glo se_`i'_log: di %5.4f r(se)
}



foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
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
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	lincom 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
	glo aggregate: di %5.4f r(estimate)
	estadd local aggregate $aggregate
	glo se_aggregate: di %5.4f r(se)
	estadd local se_aggregate $se_aggregate
	test 	[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3	=0
	glo p_aggregate: di %5.4f r(p)
	estadd local p_aggregate $p_aggregate
				glo est_aggregate= "" 
			if (${p_aggregate}<=0.11) global est_aggregate = "*"
			if (${p_aggregate}<=0.05) global est_aggregate = "**"
			if (${p_aggregate}<=0.01) global est_aggregate = "***"	

restore
************
***B: ALIGNED
************	
preserve
keep if $alignment_var2==1
*eststo: qui wildcorrection_as_trim3 $outcome2
xi: reghdfe  $outcome2  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols 
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)


***estimate linear combination by lead/lag:

foreach i in lag_4 {
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di  (_b[`i'_2018]*`d')
	test (_b[`i'_2018]*`d')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	 (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}
foreach i in lag_3 {
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2017]*`c') 
	test (_b[`i'_2017]*`c') =0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2017]*`c') 
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2017]*`c') 
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_2 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2016]*`b') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}
foreach i in  date_0{

	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2016]*`b')
	test (_b[`i'_2016]*`b')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2016]*`b')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}



foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')
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

	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	lincom 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
	glo aggregate2: di %5.4f r(estimate)
	estadd local aggregate2 $aggregate2
	glo se_aggregate2: di %5.4f r(se)
	estadd local se_aggregate2 $se_aggregate2
	test 	[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3	=0
	glo p_aggregate2: di %5.4f r(p)
	estadd local p_aggregate2 $p_aggregate2
				glo est_aggregate2= "" 
			if (${p_aggregate2}<=0.11) global est_aggregate2 = "*"
			if (${p_aggregate2}<=0.05) global est_aggregate2 = "**"
			if (${p_aggregate2}<=0.01) global est_aggregate2 = "***"	

restore 

*Table
texdoc init  "../Tables/withotheractors_alignment.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements, \citet{chaisemarting_etal_2019} correction}
tex \label{tab:withotheractors_alignment}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{Security Cooperation Agreement w/ Governor$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 4 years &         $ ${beta_lag_4_log}^{${est_lag_4_log}} $ &      $ ${beta_lag_4_ihs}^{${est_lag_4_ihs}} $  \\
tex  & ($ ${se_lag_4_log}$) & ($ ${se_lag_4_ihs} $) \\
tex Lag 3 years &        $ ${beta_lag_3_log}^{${est_lag_3_log}} $ &     $ ${beta_lag_3_ihs}^{${est_lag_3_ihs}} $ \\
tex  & ($ ${se_lag_3_log}$) & ($ ${se_lag_3_ihs} $) \\
tex Lag 2 years &        $ ${beta_lag_2_log}^{${est_lag_2_log}} $ &    $ ${beta_lag_2_ihs}^{${est_lag_2_ihs}} $  \\
tex  & ($ ${se_lag_2_log}$) & ($ ${se_lag_2_ihs} $) \\
tex Reform, time 0 &        $ ${beta_date_0_log}^{${est_date_0_log}} $ &     $ ${beta_date_0_ihs}^{${est_date_0_ihs}} $ \\
tex  & ($ ${se_date_0_log}$) & ($ ${se_date_0_ihs} $) \\
tex Lead 1 year &         $ ${beta_lead_1_log}^{${est_lead_1_log}} $ &       $ ${beta_lead_1_ihs}^{${est_lead_1_ihs}} $ \\
tex  & ($ ${se_lead_1_log}$) & ($ ${se_lead_1_ihs} $) \\
tex Lead 3 years &        $ ${beta_lead_3_log}^{${est_lead_3_log}} $ &     $ ${beta_lead_3_ihs}^{${est_lead_3_ihs}} $ \\
tex  & ($ ${se_lead_3_log}$) & ($ ${se_lead_3_ihs} $) \\

tex \addlinespace
tex Observations       &            ${N_log}        &     ${N_ihs}  \\
tex R-squared        &              ${r2_log}        &           ${r2_ihs}   \\
tex Mun. FEs       &     \checkmark         &  \checkmark    \\
tex Year. FEs       &     \checkmark         &  \checkmark   \\
tex Controls$^b$   &      \checkmark       &      \checkmark    \\
tex Cohort weighted   &   \checkmark       &   \checkmark    \\
tex WILD CI   &          &       \\
tex Aggregate effect        &              $${aggregate}^{${est_aggregate}} $$         &            $${aggregate2}^{${est_aggregate2}} $$   \\
tex SE (aggregate eff.)        &              (${se_aggregate})       &           (${se_aggregate2})   \\
*tex p-value(aggregate eff.)       &              ${p_aggregate}        &           ${p_aggregate2}   \\

tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to security cooperation agreements signed with the governor. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*========================================================================
*Figure
est clear

*A) AGREEMENT WITH GOVERNOR
*1) Alignment with President==0
preserve
keep if $alignment_var==0
xi: reghdfe  $outcome  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	**estimate aggregate effect:
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	eststo: lincomest 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
restore 

*2) Alignment with President==1
preserve
keep if $alignment_var==1
xi: reghdfe  $outcome  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	**estimate aggregate effect:
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	eststo: lincomest 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
restore

*3) Alignment with Governor==0
preserve
keep if $alignment_var2==0
xi: reghdfe  $outcome  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	**estimate aggregate effect:
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	eststo: lincomest 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
restore 

*4) Alignment with Governor==1
preserve
keep if $alignment_var2==1
xi: reghdfe  $outcome  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	**estimate aggregate effect:
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	eststo: lincomest 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
restore

*B) AGREEMENT WITH OTHERS
*1) Alignment with President==0
preserve
keep if $alignment_var==0
xi: reghdfe  $outcome2  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	**estimate aggregate effect:
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	eststo: lincomest 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
restore 

*2) Alignment with President==1
preserve
keep if $alignment_var==1
xi: reghdfe  $outcome2  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	**estimate aggregate effect:
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	eststo: lincomest 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
restore

*3) Alignment with Governor==0
preserve
keep if $alignment_var2==0
xi: reghdfe  $outcome2  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	**estimate aggregate effect:
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	eststo: lincomest 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
restore 

*4) Alignment with Governor==1
preserve
keep if $alignment_var2==1
xi: reghdfe  $outcome2  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	**estimate aggregate effect:
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	
	eststo: lincomest 		[(_b[date_0_2016]*`b') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_3_2015]*`j')] / 3
restore





*create figure
preserve
label variable reform " "

coefplot (est1, rename((1) = "Alignment President = No") msize(large)  mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est2, rename((1) = "Alignment President = Yes") msize(large)   mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est3, rename((1) = "Alignment Governor = No") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est4, rename((1) = "Alignment Governor = Yes") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est5, rename((1) = "Alignment President = No") msize(large)  mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est6, rename((1) = "Alignment President = Yes") msize(large)  mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est7, rename((1) = "Alignment Governor = No") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est8, rename((1) = "Alignment Governor = Yes") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)  ///
ytitle(" ")  xtitle("Term Limit Reform Average Effect" "from t to t+3") ///
subtitle(" ") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures/average_effects_alignment_byagreementtype.png", as(png) replace
graph export "../Figures/average_effects_alignment_byagreementtype.pdf", as(pdf) replace
graph export "../Figures/average_effects_alignment_byagreementtype.tif", as(tif) replace
graph save "../Figures/average_effects_alignment_byagreementtype.gph", replace
restore




