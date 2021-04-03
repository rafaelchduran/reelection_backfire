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
	global narco ap4_2_3_lag_8 ap4_2_3_lag_7 ap4_2_3_lag_6 ap4_2_3_lag_5 ap4_2_3_lag_4 ap4_2_3_lag_3 ap4_2_3_lag_2
	global punishment ap4_2_11_lag_8 ap4_2_11_lag_7 ap4_2_11_lag_6 ap4_2_11_lag_5 ap4_2_11_lag_4 ap4_2_11_lag_3 ap4_2_11_lag_2
	global money ap4_12b_lag_8 ap4_12b_lag_7 ap4_12b_lag_6 ap4_12b_lag_5 ap4_12b_lag_4 ap4_12b_lag_3 ap4_12b_lag_2
	global police ap5_4_2_b_lag_8 ap5_4_2_b_lag_7 ap5_4_2_b_lag_6 ap5_4_2_b_lag_5 ap5_4_2_b_lag_4 ap5_4_2_b_lag_3 ap5_4_2_b_lag_2
	global army ap5_4_8_b_lag_8 ap5_4_8_b_lag_7 ap5_4_8_b_lag_6 ap5_4_8_b_lag_5 ap5_4_8_b_lag_4 ap5_4_8_b_lag_3 ap5_4_8_b_lag_2
	global citizens $narco $punishment $money $police $army
	global narco2 ap4_2_3
	global punishment2 ap4_2_11
	global money2 ap4_12b
	global police2 ap5_4_2_b
	global army2 ap5_4_8_b
	global citizens2  $narco2 $punishment2 $money2 $police2 $army2  
	
	global citizens3  ap4_2_11_pre ap4_12b_pre ap5_4_2_b_pre ap5_4_8_b_pre // ap4_2_3_pre
	
	global incumbent_adv inc_lag_8 inc_lag_7 inc_lag_6 inc_lag_5 inc_lag_4 inc_lag_3 inc_lag_2
	global incumbent_adv2 inc_party_runsfor1_lag_8 inc_party_runsfor1_lag_7 inc_party_runsfor1_lag_6 inc_party_runsfor1_lag_5 inc_party_runsfor1_lag_4 inc_party_runsfor1_lag_3 inc_party_runsfor1_lag_2
	global incumbent_adv3 inc_party_won_lag_8 inc_party_won_lag_7 inc_party_won_lag_6 inc_party_won_lag_5 inc_party_won_lag_4 inc_party_won_lag_3 inc_party_won_lag_2
	global num_parties numparties_eff_lag_8 numparties_eff_lag_7 numparties_eff_lag_6 numparties_eff_lag_5 numparties_eff_lag_4 numparties_eff_lag_3 numparties_eff_lag_2
	global num_parties2 numparties_eff_molinar_lag_8 numparties_eff_molinar_lag_7 numparties_eff_molinar_lag_6 numparties_eff_molinar_lag_5 numparties_eff_molinar_lag_4 numparties_eff_molinar_lag_3 numparties_eff_molinar_lag_2
	
	global incumbency $incumbent_adv $num_parties
	
	global controls winning_margin_governor  governor_notaligned pri_mayor2 morena_mayor2 $citizens2 winning_margin alignment_executive_strong
	global controls_pre winning_margin_governor_pre governor_alignment_pre  winning_margin_pre alignment_executive_strong_pre $citizens3

*NOTE: similar results using pre-treatment controls



*========================================================================
*Corrected-TWFE (Chaisemartin and D'Haultfoeuille, forthcoming AER) -Effect of Reform on Sec. Coop Agreement

/*Graph:
foreach outcome in logdefuncionespc  ihs_defuncionespc {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) seed(5675) ///
cluster(estado) robust_dynamic dynamic(3) placebo(4) longdiff_placebo 
graph export "../Figures/chaisemartin_`outcome'.png", as(png) replace
}
*/

************
*A) logdefuncionespc:
************
foreach outcome in logdefuncionespc {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
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
foreach i in 1 2 3 4{
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

	



************
*B) ihs_defuncionespc:
************
foreach outcome in ihs_defuncionespc {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
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
foreach i in 1 2 3 4{
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

*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVhomicides.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicides per capita)} & \multicolumn{1}{c}{IHS(homicides per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
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
tex Lead 3 years &        $ ${beta_t_3}^{${est_t_3}} $ &     $ ${beta2_t_3}^{${est2_t_3}} $ \\
tex  & ($ ${se_t_3}$) & ($ ${se2_t_3} $) \\

tex \addlinespace
tex Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Refers to the inverse hyperbolic sine transformation.  $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: Reform decreases the likelihood of signing security cooperation agreements.

**********************************************
*Table: EFFECT OF REFORM ON HOMICIDES, WITH LAGGED DV & COHORT WEIGHTS
**********************************************

use "../../Data/ConstructionDatabase/data_final.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 	
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 	logdefuncionespc_lag_8 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3	
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc
	xtset inegi year 
	

************
***A: log(homicides per capita) with covariates
************	

est clear
quietly	areg  logdefuncionespc  $saturated   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
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
	lincom 	((_b[`i'_2017]*`a') + (_b[`i'_2018]*`b'))
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
	lincom 	((_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c'))
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


************
***B: ihs(homicides per capita) with covariates
************	
quietly	areg  ihs_defuncionespc  $saturated    i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_7{
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
	
foreach i in lag_6{
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
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
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
*Table
	
texdoc init  "../Tables/abraham_sun_estimates_lagDV.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence}
tex \label{tab:abraham_sun_lagdv}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicide per capita)} & \multicolumn{1}{c}{ihs(homicide per capita)$^{a}$} \\
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
tex Controls$^b$   &          &       \\
tex Cohort weighted   &   \checkmark       &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

/*To get quantities of interest:

*/
preserve
use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc
	xtset inegi year 
	
center logdefuncionespc , inplace

eststo: areg  logdefuncionespc  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)
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





restore

**********************************************
*Table: RDD estimates
**********************************************
use "../Data/municipal_elections_incumbent_mexico_1989_present_v2.dta", clear

*Merge with Dube, Garcia-Ponce and Thoms (2016): data from 1990 to 2010
gen muncode=inegi

merge 1:1 muncode year using "../Mexico/Data/Dube, Garcia-Ponce and Thom (2016)/jeea12172-sup-0002-replication-data/Dube_Garcia_ Thom_ReplicationData/MaizeToHaze_JEEA_ReplicationData.dta"
drop if _merge==2
drop _merge

merge m:m inegi year using "../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011_2018.dta"
drop if _merge==2
drop _merge



est clear
foreach pol in 1 2 3 4{
eststo: quietly rdrobust incumbent_yesterday_w_tomorrow2 mv_incparty if reform==0 & year<2015, c(0) p(`pol') kernel(tri) bwselect(CCT) 
	estadd local postreform 
	
eststo: quietly rdrobust incumbent_yesterday_w_tomorrow2 mv_incparty if reform==1, c(0) p(`pol') kernel(tri) bwselect(CCT)
	estadd local postreform \checkmark



}
**the problem with splitting is that I am comparing those early vs late treated by the reform, and if there are het treatment effects then resutls are biased

esttab est*, keep(RD_Estimate) star(* 0.1 ** 0.05 *** 0.01) se

esttab using "../Tables/rdd_estimates.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N  postreform, fmt(%11.2gc 3) label("Observations" "Post Reform (2014)")) ///
keep(RD_Estimate) ///
mgroups("linear polynomial" "quadratic polynomial" "cubic polynomical" "quartic polynomial", ///
 pattern(1 0 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(RD_Estimate "Probability of victory at t+1$^a$") ///
collabels(none) nonotes booktabs nomtitles  nolines


**********************************************
*Table: Mechanisms: INCUMBENCY ADVANTAGE WITH POLYNOMIALS
**********************************************
use "../Data/ConstructionDatabase/data_wleads&lags_incumbency_weights.dta", clear

*GLOBALS: treatment
capture global saturated date_0_2015 date_0_2016 date_0_2017 date_0_2018 lag_2_2016 lag_3_2015 lag_3_2016 lag_3_2018 lag_4_2015 lag_4_2017 lag_5_2015 lag_5_2018 lag_6_2016 lag_6_2018 lag_8_2018 lead_1_2015 lead_1_2016
sum $saturated

*GLOBALS: controls
foreach i in margin_ HHI_ effectiveparties_ NP_ golosov_ governor_alignment_ pop_{
global `i'  `i'lag_6 `i'lag_5 `i'lag_4 `i'lag_3
}


global allcov2 $margin_  $governor_alignment_ 
global DV logdefuncionespc

*outcomes: incumbent_yesterday_w_tomorrow2 inc_party_runsfor1 numparties_eff numparties_eff_molinar inc_party_won
est clear
foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 1 2 3 4 {
*Estimate optimal bandwidth:
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
global optimal_75 = e(h_CCT)*.75
global optimal_half = e(h_CCT)*.5

di ${optimal}
di ${optimal_75}
di ${optimal_half}


*Polynomials:
*Generate polynomials:
gen pol`pol'=mv_incparty^`pol' if mv_incparty<${optimal} & mv_incparty>-${optimal}

foreach i in pol`pol'{
foreach var in $saturated{
gen `var'_`i'=`var'*`i'

}
}
}
}

*polynomial globals
capture global interacted_pol1 date_0_2015_pol1  date_0_2016_pol1  date_0_2017_pol1  date_0_2018_pol1  lag_5_2018_pol1  lag_4_2015_pol1  lag_3_2015_pol1  lag_3_2016_pol1  lag_3_2018_pol1
capture global interacted_pol2 date_0_2015_pol2  date_0_2016_pol2  date_0_2017_pol2  date_0_2018_pol2  lag_5_2018_pol2  lag_4_2015_pol2  lag_3_2015_pol2  lag_3_2016_pol2  lag_3_2018_pol2
capture global interacted_pol3 date_0_2015_pol3  date_0_2016_pol3  date_0_2017_pol3  date_0_2018_pol3  lag_5_2018_pol3  lag_4_2015_pol3  lag_3_2015_pol3  lag_3_2016_pol3  lag_3_2018_pol3
capture global interacted_pol4 date_0_2015_pol4  date_0_2016_pol4  date_0_2017_pol4  date_0_2018_pol4  lag_5_2018_pol4  lag_4_2015_pol4  lag_3_2015_pol4  lag_3_2016_pol4  lag_3_2018_pol4


*------------------
*A)WITH incumbent_yesterday_w_tomorrow2
*------------------
foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 1 2 3 4 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		*glo constant_`pol': di %5.4f _b[_cons]

***estimate linear combination by lead/lag:


foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c') 
	test (_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c')/5 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')/5
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/5
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/5 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


}
}

*get the partisan effect
foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	((((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/5)/2) -(_b[_cons]/5)/2 
	lincom (_b[_cons]/5)/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


/*****
test on partisan incumbency advantage
_b[reform] = difference between reelection and term limited elections: partisan(A) + personal(B) = A+B = .0714323 (sig 10%)
_b[_cons] = inc. advantage for term limited elections: partisan inc. advantage = A =  .1156307   (sig 5%)
_b[reform]-b[_cons]  = A+B-A=B=personal incumbency advantage =  -.0441984  (no sig but close to 10%)
*****/

eststo: areg  incumbent_yesterday_w_tomorrow2 reform  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	lincom 	(_b[reform]/2)-(_b[_cons]/2 )
	
	*generate group
	gen group=0
	replace group=1 if adopt_year==2015
	replace group=2 if adopt_year==2016
	replace group=3 if adopt_year==2017
	replace group=4 if adopt_year==2018

did_multiplegt incumbent_yesterday_w_tomorrow2 group year reform, placebo(1)  breps(50) cluster(estado)


*------------------
*B)WITH inc_party_won
*------------------
foreach j in inc_party_won{
foreach pol in 1 2 3 4 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


quietly areg  incumbent_yesterday_w_tomorrow2  $saturated pol`pol' $interacted_pol2 $allcov2 logdefuncionespc  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
keep if e(sample)==1


est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c') 
	test (_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a') =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
			}


}
}



texdoc init  "../Tables/abraham_sun_incumbency_wpolynomials.tex", replace force
*tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Event-in-Discontinuity in close elections model: Effect of 2014 Term Limit Reform on Incumbency Advantage}
tex \label{tab:incumbency_wpolynomials}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{Incumbent at t-1 won at t+1}  & \multicolumn{1}{c}{Incumbent at t won at t+1} \\
tex & \multicolumn{1}{c}{(indicator)}  & \multicolumn{1}{c}{(indicator)} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)}  \\
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \\
tex \addlinespace
*tex & \multicolumn{2}{c}{linear polynomial} \\
*tex \cmidrule(lrr){2-3} \\
*tex Lag 5 years &       $ ${beta_lag_5_ihsdet_1}^{${est_lag_5_ihsdet_1}} $ &       $ ${beta_lag_5_ihsdet2_1}^{${est_lag_5_ihsdet2_1}} $  \\
*tex & ($ ${se_lag_5_ihsdet_1} $ ) & ($ ${se_lag_5_ihsdet2_1} $ ) \\
*tex Lag 4 years &       $ ${beta_lag_4_ihsdet_1}^{${est_lag_4_ihsdet_1}} $ &        $ ${beta_lag_4_ihsdet2_1}^{${est_lag_4_ihsdet2_1}} $ \\  
*tex & ($ ${se_lag_4_ihsdet_1} $ ) & ($ ${se_lag_4_ihsdet2_1} $ ) \\
*tex Lag 3 years &          $ ${beta_lag_3_ihsdet_1}^{${est_lag_3_ihsdet_1}} $ &       $ ${beta_lag_3_ihsdet2_1}^{${est_lag_3_ihsdet2_1}} $ \\   
*tex & ($ ${se_lag_3_ihsdet_1} $ ) & ($ ${se_lag_3_ihsdet2_1} $ ) \\
*tex Reform, time 0 &         $ ${beta_date_0_ihsdet_1}^{${est_date_0_ihsdet_1}} $ &        $ ${beta_date_0_ihsdet2_1}^{${est_date_0_ihsdet2_1}} $ \\   
*tex & ($ ${se_date_0_ihsdet_1} $ ) & ($ ${se_date_0_ihsdet2_1} $ ) \\
*tex Observations          &        ${N_ihsdet_1}     &        ${N_ihsdet2_1} \\ 
*tex R-squared        &          ${r2_ihsdet_1}   &          ${r2_ihsdet2_1} \\  
*tex\\
tex & \multicolumn{2}{c}{quadratic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 5 years &       $ ${beta_lag_5_ihsdet_2}^{${est_lag_5_ihsdet_2}} $ &       $ ${beta_lag_5_ihsdet2_2}^{${est_lag_5_ihsdet2_2}} $  \\
tex & ($ ${se_lag_5_ihsdet_2} $ ) & ($ ${se_lag_5_ihsdet2_2} $ ) \\
tex Lag 4 years &       $ ${beta_lag_4_ihsdet_2}^{${est_lag_4_ihsdet_2}} $ &        $ ${beta_lag_4_ihsdet2_2}^{${est_lag_4_ihsdet2_2}} $ \\  
tex & ($ ${se_lag_4_ihsdet_2} $ ) & ($ ${se_lag_4_ihsdet2_2} $ ) \\
tex Lag 3 years &          $ ${beta_lag_3_ihsdet_2}^{${est_lag_3_ihsdet_2}} $ &       $ ${beta_lag_3_ihsdet2_2}^{${est_lag_3_ihsdet2_2}} $ \\   
tex & ($ ${se_lag_3_ihsdet_2} $ ) & ($ ${se_lag_3_ihsdet2_2} $ ) \\
tex Reform, time 0 &         $ ${beta_date_0_ihsdet_2}^{${est_date_0_ihsdet_2}} $ &        $ ${beta_date_0_ihsdet2_2}^{${est_date_0_ihsdet2_2}} $ \\   
tex & ($ ${se_date_0_ihsdet_2} $ ) & ($ ${se_date_0_ihsdet2_2} $ ) \\
tex Observations          &        ${N_ihsdet_2}     &        ${N_ihsdet2_2} \\ 
tex R-squared        &          ${r2_ihsdet_2}   &          ${r2_ihsdet2_2} \\  
tex\\
tex & \multicolumn{2}{c}{cubic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 5 years &       $ ${beta_lag_5_ihsdet_3}^{${est_lag_5_ihsdet_3}} $ &       $ ${beta_lag_5_ihsdet2_3}^{${est_lag_5_ihsdet2_3}} $  \\
tex & ($ ${se_lag_5_ihsdet_3} $ ) & ($ ${se_lag_5_ihsdet2_3} $ ) \\
tex Lag 4 years &       $ ${beta_lag_4_ihsdet_3}^{${est_lag_4_ihsdet_3}} $ &        $ ${beta_lag_4_ihsdet2_3}^{${est_lag_4_ihsdet2_3}} $ \\  
tex & ($ ${se_lag_4_ihsdet_3} $ ) & ($ ${se_lag_4_ihsdet2_3} $ ) \\
tex Lag 3 years &          $ ${beta_lag_3_ihsdet_3}^{${est_lag_3_ihsdet_3}} $ &       $ ${beta_lag_3_ihsdet2_3}^{${est_lag_3_ihsdet2_3}} $ \\   
tex & ($ ${se_lag_3_ihsdet_3} $ ) & ($ ${se_lag_3_ihsdet2_3} $ ) \\
tex Reform, time 0 &         $ ${beta_date_0_ihsdet_3}^{${est_date_0_ihsdet_3}} $ &        $ ${beta_date_0_ihsdet2_3}^{${est_date_0_ihsdet2_3}} $ \\   
tex & ($ ${se_date_0_ihsdet_3} $ ) & ($ ${se_date_0_ihsdet2_3} $ ) \\
tex Observations          &        ${N_ihsdet_3}     &        ${N_ihsdet2_3} \\ 
tex R-squared        &          ${r2_ihsdet_3}   &          ${r2_ihsdet2_3} \\  
tex\\
tex & \multicolumn{2}{c}{quartic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 5 years &       $ ${beta_lag_5_ihsdet_4}^{${est_lag_5_ihsdet_4}} $ &       $ ${beta_lag_5_ihsdet2_4}^{${est_lag_5_ihsdet2_4}} $  \\
tex & ($ ${se_lag_5_ihsdet_4} $ ) & ($ ${se_lag_5_ihsdet2_4} $ ) \\
tex Lag 4 years &       $ ${beta_lag_4_ihsdet_4}^{${est_lag_4_ihsdet_4}} $ &        $ ${beta_lag_4_ihsdet2_4}^{${est_lag_4_ihsdet2_4}} $ \\  
tex & ($ ${se_lag_4_ihsdet_4} $ ) & ($ ${se_lag_4_ihsdet2_4} $ ) \\
tex Lag 3 years &          $ ${beta_lag_3_ihsdet_4}^{${est_lag_3_ihsdet_4}} $ &       $ ${beta_lag_3_ihsdet2_4}^{${est_lag_3_ihsdet2_4}} $ \\   
tex & ($ ${se_lag_3_ihsdet_4} $ ) & ($ ${se_lag_3_ihsdet2_4} $ ) \\
tex Reform, time 0 &         $ ${beta_date_0_ihsdet_4}^{${est_date_0_ihsdet_4}} $ &        $ ${beta_date_0_ihsdet2_4}^{${est_date_0_ihsdet2_4}} $ \\   
tex & ($ ${se_date_0_ihsdet_4} $ ) & ($ ${se_date_0_ihsdet2_4} $ ) \\
tex Observations          &        ${N_ihsdet_4}     &        ${N_ihsdet2_4} \\ 
tex R-squared        &          ${r2_ihsdet_4}   &          ${r2_ihsdet2_4} \\  
tex\\
tex Mun. FEs        &     \checkmark         &  \checkmark   \\
tex Year. FEs     &     \checkmark         &  \checkmark  \\
tex State Controls$^a$  &    \checkmark     &       \checkmark \\
tex Cohort weighted  &         \checkmark &         \checkmark \\
tex \hline \hline      
tex \multicolumn{3}{p{0.9\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 6 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020} or because they are collinear or inexistent, like lag time period 2. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. Logged homicides per capita at the municipality level are also included as controls.}} \\
tex \end{tabular}
tex } 
tex \end{table}
*tex \end{landscape}
texdoc close

**********************************************
*Table: Mechanisms: ARMY AND LOCAL POLICE EFFORT
**********************************************
use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*GLOBALS: treatment
global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

*GLOBALS: controls
foreach i in margin_ HHI_ effectiveparties_ NP_ golosov_ governor_alignment_{
global `i' `i'lag_8  `i'lag_7 `i'lag_6 `i'lag_5 `i'lag_4 `i'lag_3 `i'lag_2
}

global allcov2 $margin_  $governor_alignment_
xtset inegi year
global DV l.logdefuncionespc

*Estimates:
******************
*1)LOG DETENIDOS PER CAPITA:
******************
est clear
eststo: areg  logdetenidospc  $saturated $allcov2 $DV  i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_logdet:  di %5.4f e(r2)
		glo N_logdet: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_logdet: di %5.4f r(se)
			
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
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
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
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_logdet: di %5.4f r(se)
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
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_logdet: di %5.4f r(se)
}

******************
*2)IHS DETENIDOS PER CAPITA:
******************
est clear
eststo: areg  logdetenidos_2pc  $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihsdet:  di %5.4f e(r2)
		glo N_ihsdet: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_ihsdet: di %5.4f r(se)
			
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
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
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
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet: di %5.4f r(se)
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
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_ihsdet: di %5.4f r(se)
}

******************
*3)LOG HEROINE KG:
******************
est clear
eststo: areg  logheroina_kg   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_her:  di %5.4f e(r2)
		glo N_her: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_her: di %5.4f r(se)
			
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
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
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
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_her: di %5.4f r(se)
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
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_her: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_her: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_her: di %5.4f r(se)
}

******************
*4)LOG HEROINE KG_2:
******************
est clear
eststo: areg  logheroina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_her_2:  di %5.4f e(r2)
		glo N_her_2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_her2: di %5.4f r(se)
			
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
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_her2: di %5.4f r(se)
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
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_her2: di %5.4f r(se)
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
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_her2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_her2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_her2: di %5.4f r(se)
}
******************
*5)LOG METANPHETAMINE KG:
******************
est clear
eststo: areg  logmetanfetamina_kg   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_met:  di %5.4f e(r2)
		glo N_met: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_met: di %5.4f r(se)
			
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
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_met: di %5.4f r(se)
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
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_met: di %5.4f r(se)
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
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_met: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_met: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_met: di %5.4f r(se)
}

******************
*6)LOG METANPHETAMINE KG_2:
******************
est clear
eststo: areg  logmetanfetamina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_met_2:  di %5.4f e(r2)
		glo N_met_2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_met2: di %5.4f r(se)
			
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
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_met2: di %5.4f r(se)
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
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_met2: di %5.4f r(se)
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
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_met2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_met2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_met2: di %5.4f r(se)
}

******************
*7)LOG LABORATORIO:
******************
est clear
eststo: areg  loglaboratorio   $saturated $allcov2  $DV  i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_lab:  di %5.4f e(r2)
		glo N_lab: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_lab: di %5.4f r(se)
			
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
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_lab: di %5.4f r(se)
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
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_lab: di %5.4f r(se)
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
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_lab: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_lab: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_lab: di %5.4f r(se)
}

******************
*8)LOG LABORATORIO_2:
******************
est clear
eststo: areg  loglaboratorio_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_lab_2:  di %5.4f e(r2)
		glo N_lab_2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_lab2: di %5.4f r(se)
			
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
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_lab2: di %5.4f r(se)
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
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_lab2: di %5.4f r(se)
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
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_lab2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_lab2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_lab2: di %5.4f r(se)
}



* You need to install the following packages:
	*ssc install texdoc, replace
	*ssc install texify, replace
	*ssc install estout, replace 
	
texdoc init  "../Tables/abraham_sun_mechanisms.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform Security Forces Effort, cohort weighted estimates}
tex \label{tab:effort}
tex \scalebox{0.70}{    
tex \begin{tabular}{lcccccccc}  
tex \hline \hline       
tex \\ \multicolumn{9}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{Local Police} & \multicolumn{6}{c}{Military}  \\
tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-9} \\
tex & \multicolumn{2}{c}{log(detained per capita)}  & \multicolumn{2}{c}{heroine, eradicated kg}  & \multicolumn{2}{c}{methamphetamine, eradicated kg} & \multicolumn{2}{c}{laboratories destroyed}  \\
tex &  & \multicolumn{1}{c}{(relative to none)} &  & \multicolumn{1}{c}{(relative to none)} &  & \multicolumn{1}{c}{(relative to none)} & & \multicolumn{1}{c}{(relative to none)} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} \\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \cmidrule(lrr){5-5} \cmidrule(lrr){6-6} \cmidrule(lrr){7-7} \cmidrule(lrr){8-8} \cmidrule(lrr){9-9}\\
tex \addlinespace

tex Lag 7 years &     $ ${beta_lag_7_logdet}^{${est_lag_7_logdet}} $ &     $ ${beta_lag_7_ihsdet}^{${est_lag_7_ihsdet}} $ & $ ${beta_lag_7_her}^{${est_lag_7_her}} $ & $ ${beta_lag_7_her2}^{${est_lag_7_her2}} $  &     $ ${beta_lag_7_met}^{${est_lag_7_met}} $   &     $ ${beta_lag_7_met2}^{${est_lag_7_met2}} $ & $ ${beta_lag_7_lab}^{${est_lag_7_lab}} $ & $ ${beta_lag_7_lab2}^{${est_lag_7_lab2}} $   \\
tex &     ($${se_lag_7_logdet}$) &     ($${se_lag_7_ihsdet}$) & ($${se_lag_7_her}$)& ($ ${se_lag_7_her2}$)  &    ($${se_lag_7_met}$)   &   ($${se_lag_7_met2}$) & ($${se_lag_7_lab}$) & ($${se_lag_7_lab2}$)   \\
tex Lag 6 years &     $ ${beta_lag_6_logdet}^{${est_lag_6_logdet}} $ &     $ ${beta_lag_6_ihsdet}^{${est_lag_6_ihsdet}} $ &  $ ${beta_lag_6_her}^{${est_lag_6_her}} $ &  $ ${beta_lag_6_her2}^{${est_lag_6_her2}} $  &     $ ${beta_lag_6_met}^{${est_lag_6_met}} $ &     $ ${beta_lag_6_met2}^{${est_lag_6_met2}} $ &  $ ${beta_lag_6_lab}^{${est_lag_6_lab}} $ &  $ ${beta_lag_6_lab2}^{${est_lag_6_lab2}} $  \\
tex &     ($${se_lag_6_logdet}$) &     ($${se_lag_6_ihsdet}$) & ($${se_lag_6_her}$)& ($ ${se_lag_6_her2}$)  &    ($${se_lag_6_met}$)   &   ($${se_lag_6_met2}$) & ($${se_lag_6_lab}$) & ($${se_lag_6_lab2}$)   \\
tex Lag 5 years &     $ ${beta_lag_5_logdet}^{${est_lag_5_logdet}} $ &     $ ${beta_lag_5_ihsdet}^{${est_lag_5_ihsdet}} $ &  $ ${beta_lag_5_her}^{${est_lag_5_her}} $ &  $ ${beta_lag_5_her2}^{${est_lag_5_her2}} $ &     $ ${beta_lag_5_met}^{${est_lag_5_met}} $  &     $ ${beta_lag_5_met2}^{${est_lag_5_met2}} $ &  $ ${beta_lag_5_lab}^{${est_lag_5_lab}} $ &  $ ${beta_lag_5_lab2}^{${est_lag_5_lab2}} $ \\
tex &     ($${se_lag_5_logdet}$) &     ($${se_lag_5_ihsdet}$) & ($${se_lag_5_her}$)& ($ ${se_lag_5_her2}$)  &    ($${se_lag_5_met}$)   &   ($${se_lag_5_met2}$) & ($${se_lag_5_lab}$) & ($${se_lag_5_lab2}$)   \\
tex Lag 4 years &     $ ${beta_lag_4_logdet}^{${est_lag_4_logdet}} $ &     $ ${beta_lag_4_ihsdet}^{${est_lag_4_ihsdet}} $ &   $ ${beta_lag_4_her}^{${est_lag_4_her}} $ &   $ ${beta_lag_4_her2}^{${est_lag_4_her2}} $  &     $ ${beta_lag_4_met}^{${est_lag_4_met}} $ &     $ ${beta_lag_4_met2}^{${est_lag_4_met2}} $ &   $ ${beta_lag_4_lab}^{${est_lag_4_lab}} $ &   $ ${beta_lag_4_lab2}^{${est_lag_4_lab2}} $  \\
tex &     ($${se_lag_4_logdet}$) &     ($${se_lag_4_ihsdet}$) & ($${se_lag_4_her}$)& ($ ${se_lag_4_her2}$)  &    ($${se_lag_4_met}$)   &   ($${se_lag_4_met2}$) & ($${se_lag_4_lab}$) & ($${se_lag_4_lab2}$)   \\
tex Lag 3 years &     $ ${beta_lag_3_logdet}^{${est_lag_3_logdet}} $ &     $ ${beta_lag_3_ihsdet}^{${est_lag_3_ihsdet}} $ &   $ ${beta_lag_3_her}^{${est_lag_3_her}} $ &   $ ${beta_lag_3_her2}^{${est_lag_3_her2}} $  &     $ ${beta_lag_3_met}^{${est_lag_3_met}} $ &     $ ${beta_lag_3_met2}^{${est_lag_3_met2}} $ &   $ ${beta_lag_3_lab}^{${est_lag_3_lab}} $ &   $ ${beta_lag_3_lab2}^{${est_lag_3_lab2}} $  \\
tex &     ($${se_lag_3_logdet}$) &     ($${se_lag_3_ihsdet}$) & ($${se_lag_3_her}$)& ($ ${se_lag_3_her2}$)  &    ($${se_lag_3_met}$)   &   ($${se_lag_3_met2}$) & ($${se_lag_3_lab}$) & ($${se_lag_3_lab2}$)   \\
tex Lag 2 years &     $ ${beta_lag_2_logdet}^{${est_lag_2_logdet}} $ &     $ ${beta_lag_2_ihsdet}^{${est_lag_2_ihsdet}} $ &  $ ${beta_lag_2_her}^{${est_lag_2_her}} $  &  $ ${beta_lag_2_her2}^{${est_lag_2_her2}} $  &     $ ${beta_lag_2_met}^{${est_lag_2_met}} $ &     $ ${beta_lag_2_met2}^{${est_lag_2_met2}} $ &  $ ${beta_lag_2_lab}^{${est_lag_2_lab}} $  &  $ ${beta_lag_2_lab2}^{${est_lag_2_lab2}} $ \\
tex &     ($${se_lag_2_logdet}$) &     ($${se_lag_2_ihsdet}$) & ($${se_lag_2_her}$)& ($ ${se_lag_2_her2}$)  &    ($${se_lag_2_met}$)   &   ($${se_lag_2_met2}$) & ($${se_lag_2_lab}$) & ($${se_lag_2_lab2}$)   \\
tex Reform, time 0 &     $ ${beta_date_0_logdet}^{${est_date_0_logdet}} $ &     $ ${beta_date_0_ihsdet}^{${est_date_0_ihsdet}} $ &   $ ${beta_date_0_her}^{${est_date_0_her}} $   &   $ ${beta_date_0_her2}^{${est_date_0_her2}} $  &     $ ${beta_date_0_met}^{${est_date_0_met}} $ &     $ ${beta_date_0_met2}^{${est_date_0_met2}} $ &   $ ${beta_date_0_her}^{${est_date_0_lab}} $   &   $ ${beta_date_0_lab2}^{${est_date_0_lab2}} $ \\
tex &     ($${se_date_0_logdet}$) &     ($${se_date_0_ihsdet}$) & ($${se_date_0_her}$)& ($ ${se_date_0_her2}$)  &    ($${se_date_0_met}$)   &   ($${se_date_0_met2}$) & ($${se_date_0_lab}$) & ($${se_date_0_lab2}$)   \\
tex Lead 1 year &     $ ${beta_lead_1_logdet}^{${est_lead_1_logdet}} $ &     $ ${beta_lead_1_ihsdet}^{${est_lead_1_ihsdet}} $ &    $ ${beta_lead_1_her}^{${est_lead_1_her}} $ &    $ ${beta_lead_1_her2}^{${est_lead_1_her2}} $ &     $ ${beta_lead_1_met}^{${est_lead_1_met}} $ &     $ ${beta_lead_1_met2}^{${est_lead_1_met2}} $ &    $ ${beta_lead_1_lab}^{${est_lead_1_lab}} $ &    $ ${beta_lead_1_lab2}^{${est_lead_1_lab2}} $ \\
tex &     ($${se_lead_1_logdet}$) &     ($${se_lead_1_ihsdet}$) & ($${se_lead_1_her}$)& ($ ${se_lead_1_her2}$)  &    ($${se_lead_1_met}$)   &   ($${se_lead_1_met2}$) & ($${se_lead_1_lab}$) & ($${se_lead_1_lab2}$)   \\
tex Lead 2 years &     $ ${beta_lead_2_logdet}^{${est_lead_2_logdet}} $ &     $ ${beta_lead_2_ihsdet}^{${est_lead_2_ihsdet}} $ &   $ ${beta_lead_2_her}^{${est_lead_2_her}} $  &   $ ${beta_lead_2_her2}^{${est_lead_2_her2}} $ &     $ ${beta_lead_2_met}^{${est_lead_2_met}} $ &     $ ${beta_lead_2_met2}^{${est_lead_2_met2}} $ &   $ ${beta_lead_2_lab}^{${est_lead_2_lab}} $  &   $ ${beta_lead_2_lab2}^{${est_lead_2_lab2}} $ \\
tex &     ($${se_lead_2_logdet}$) &     ($${se_lead_2_ihsdet}$) & ($${se_lead_2_her}$)& ($ ${se_lead_2_her2}$)  &    ($${se_lead_2_met}$)   &   ($${se_lead_2_met2}$) & ($${se_lead_2_lab}$) & ($${se_lead_2_lab2}$)   \\
tex Lead 3 years &     $ ${beta_lead_3_logdet}^{${est_lead_3_logdet}} $ &     $ ${beta_lead_3_ihsdet}^{${est_lead_3_ihsdet}} $ &   $ ${beta_lead_3_her}^{${est_lead_3_her}} $  &   $ ${beta_lead_3_her2}^{${est_lead_3_her2}} $ &     $ ${beta_lead_3_met}^{${est_lead_3_met}} $ &     $ ${beta_lead_3_met2}^{${est_lead_3_met2}} $ &   $ ${beta_lead_3_lab}^{${est_lead_3_lab}} $  &   $ ${beta_lead_3_lab2}^{${est_lead_3_lab2}} $ \\
tex &     ($${se_lead_3_logdet}$) &     ($${se_lead_3_ihsdet}$) & ($${se_lead_3_her}$)& ($ ${se_lead_3_her2}$)  &    ($${se_lead_3_met}$)   &   ($${se_lead_3_met2}$) & ($${se_lead_3_lab}$) & ($${se_lead_3_lab2}$)   \\
tex \\
tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}    &     ${N_her}      &     ${N_her_2}  &        ${N_met}    &        ${N_met_2}    &     ${N_lab}      &     ${N_lab_2} \\
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet}    &    ${r2_her}       &           ${r2_her_2} &          ${r2_met} &          ${r2_met_2}    &           ${r2_lab}       &           ${r2_lab_2}    \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark  &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^b$  &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark     \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\
tex Lag log(homicides per capita)  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{9}{p{1.5\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Even columns with outcomes with missing values where replaced by zeros assuming no activity was registered. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close



**********************************************
*Table: HET EFFECTS: ALINGMENT AND WINNING MARGIN
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear



*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	xtset inegi year
	global DV l.logdefuncionespc
	

************
***A: log(homicides per capita) with covariates // EXECUTIVE ALIGNMENT
************
quietly	areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster inegi)
keep if e(sample)==1

est clear
foreach i in alignment_executive_strong{

areg   logdefuncionespc c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'1:  di %5.4f e(r2)
		glo N_`i'1: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***B: ihs(homicides per capita) with covariates // EXECUTIVE ALIGNMENT
************


est clear
foreach i in alignment_executive_strong{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls $DV i.year, a(inegi) vce(cluster estado)
		glo r2_`i'2:  di %5.4f e(r2)
		glo N_`i'2: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***C: log(homicides per capita) with covariates // WINNING MARGIN
************
est clear
foreach i in winning_margin{

areg   logdefuncionespc c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'3:  di %5.4f e(r2)
		glo N_`i'3: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***D: ihs(homicides per capita) with covariates // WINNING MARGIN
************
est clear
foreach i in winning_margin{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'4:  di %5.4f e(r2)
		glo N_`i'4: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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



texdoc init  "../Tables/abraham_sun_estimates_heteffects.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect$^a$: the role of Alignment with Federal Government and Municipal Winning Margin}
tex \label{tab:abraham_sun_heteffects}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{b}$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace
tex Reform (t+3)*Alignment Fed. Gov. &     $ ${beta_alignment_executive_strong1}^{${est_alignment_executive_strong1}} $ &  &  $ ${beta_alignment_executive_strong2}^{${est_alignment_executive_strong2}} $  & \\
tex  &     ($ ${se_alignment_executive_strong1} $) &  & ($ ${se_alignment_executive_strong2} $)  & \\

tex Reform (t+3)*Winning Margin &    &   $ ${beta_winning_margin3}^{${est_winning_margin3}} $ &  &  $ ${beta_winning_margin4}^{${est_winning_margin4}} $ \\
tex  &    &     ($ ${se_winning_margin3} $) & & ($ ${se_winning_margin4} $)  \\

tex \\
tex \addlinespace
tex Observations       &        ${N_alignment_executive_strong1}    &        ${N_alignment_executive_strong2}    &     ${N_winning_margin3}      &     ${N_winning_margin4}  \\
tex R-squared       &        ${r2_alignment_executive_strong1}    &        ${r2_alignment_executive_strong2}    &     ${r2_winning_margin3}      &     ${r2_winning_margin4}  \\
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



**********************************************
*Table: HET EFFECTS: PRI AND PAN MAYORS
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	xtset inegi year 
	global DV l.logdefuncionespc

************
***A: log(homicides per capita) with covariates // PRI MAYOR
************
quietly	areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster inegi)
keep if e(sample)==1

est clear
foreach i in pri_mayor{

areg   logdefuncionespc c.${saturated}##c.`i' $controls  $DV  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'1:  di %5.4f e(r2)
		glo N_`i'1: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***B: ihs(homicides per capita) with covariates // PRI MAYOR
************


est clear
foreach i in pri_mayor{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls  $DV  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'2:  di %5.4f e(r2)
		glo N_`i'2: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***C: log(homicides per capita) with covariates // MORENA MAYOR
************
est clear
foreach i in pan_mayor{

areg   logdefuncionespc c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'3:  di %5.4f e(r2)
		glo N_`i'3: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***D: ihs(homicides per capita) with covariates // MORENA MAYOR
************
est clear
foreach i in pan_mayor{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'4:  di %5.4f e(r2)
		glo N_`i'4: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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


texdoc init  "../Tables/abraham_sun_estimates_pri.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect of Partisanship on Violence$^a$}
tex \label{tab:abraham_sun_pri}
tex \scalebox{1}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{b}$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace
tex Reform (t+3)*PRI &     $ ${beta_pri_mayor1}^{${est_pri_mayor1}} $ &  &  $ ${beta_pri_mayor2}^{${est_pri_mayor2}} $  & \\
tex  &     ($ ${se_pri_mayor1} $) &  & ($ ${se_pri_mayor2} $)  & \\

tex Reform (t+3)*PAN &    &   $ ${beta_pan_mayor3}^{${est_pan_mayor3}} $ &  &  $ ${beta_pan_mayor4}^{${est_pan_mayor4}} $ \\
tex  &    &     ($ ${se_pan_mayor3} $) & & ($ ${se_pan_mayor4} $)  \\
tex \\
tex \addlinespace
tex Observations       &        ${N_alignment_executive_strong1}    &        ${N_alignment_executive_strong2}    &     ${N_winning_margin3}      &     ${N_winning_margin4}  \\
tex R-squared       &        ${r2_alignment_executive_strong1}    &        ${r2_alignment_executive_strong2}    &     ${r2_winning_margin3}      &     ${r2_winning_margin4}  \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^c$  &  \checkmark       &       \checkmark  &  \checkmark        &   \checkmark    \\
tex Cohort weighted$^d$  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\

tex \hline \hline   
tex \multicolumn{5}{p{0.8\textwidth}}{\footnotesize{Notes:$^a$ Total interaction effect tests the linear hypothesis of the estimated coefficient of PRI mayor (PAN mayor) + estimated coefficient of the interaction of PRI (PAN)*lead t=3. This is a post-estimation test using the same specification as that of Table \ref{tab:abraham_sun_lagdv} column (1). Other leads and the indicator at time t=0 when reform came to effect are omitted due to collinearity. Standard errors of linear hypothesis test in parentheses with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided test with the null hypothesis equal to 0. Main regression with standard errors clustered at the state-level. $^b$ Refers to the inverse hyperbolic sine transformation. $^c$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. I also include the lag of the outcome, i.e. logged homicides per capita as control. $^d$ Estimates weighted by each cohort's relative share of the sample following \citet{abraham_sun_2020}.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

 
**********************************************
*Table: Het effects, PRI and Public Security Effort
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	xtset inegi year 
	global DV l.logdefuncionespc
	
************
***A: cocaine with covariates // PRI MAYOR
************
quietly	areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster inegi)
keep if e(sample)==1

est clear
foreach i in pri_mayor{

areg   logcocaina_kg c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'1:  di %5.4f e(r2)
		glo N_`i'1: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***B: heroine with covariates // PRI MAYOR
************


est clear
foreach i in pri_mayor{

areg   logheroina_kg c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'2:  di %5.4f e(r2)
		glo N_`i'2: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***C: cocaine with covariates // PAN MAYOR
************
est clear
foreach i in pan_mayor{

areg   logcocaina_kg c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'3:  di %5.4f e(r2)
		glo N_`i'3: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***D: heroine with covariates // MORENA MAYOR
************
est clear
foreach i in pan_mayor{

areg   logheroina_kg c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'4:  di %5.4f e(r2)
		glo N_`i'4: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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

*similar sign with laboratorio, mariguana, methanmphetamine

texdoc init  "../Tables/abraham_sun_estimates_pri_effort.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect of Partisanship on Public Security Effort$^a$}
tex \label{tab:abraham_sun_pri_effort}
tex \scalebox{1}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(cocaine)$^b$} & \multicolumn{2}{c}{log(heroine)$^b$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace
tex Reform (t+3)*PRI &     $ ${beta_pri_mayor1}^{${est_pri_mayor1}} $ &  &  $ ${beta_pri_mayor2}^{${est_pri_mayor2}} $  & \\
tex  &     ($ ${se_pri_mayor1} $) &  & ($ ${se_pri_mayor2} $)  & \\

tex Reform (t+3)*PAN &    &   $ ${beta_pan_mayor3}^{${est_pan_mayor3}} $ &  &  $ ${beta_pan_mayor4}^{${est_pan_mayor4}} $ \\
tex  &    &     ($ ${se_pan_mayor3} $) & & ($ ${se_pan_mayor4} $)  \\
tex \\
tex \addlinespace
tex Observations       &        ${N_pri_mayor1}    &        ${N_pan_mayor3}    &     ${N_pri_mayor2}      &     ${N_pan_mayor4}  \\
tex R-squared       &        ${r2_pri_mayor1}    &        ${r2_pan_mayor3}    &     ${r2_pri_mayor2}      &     ${r2_pan_mayor4}  \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^c$  &  \checkmark       &       \checkmark  &  \checkmark        &   \checkmark    \\
tex Cohort weighted$^d$  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\

tex \hline \hline  
tex \multicolumn{5}{p{0.65\textwidth}}{\footnotesize{Notes:$^a$ Total interaction effect tests the linear hypothesis of the estimated coefficient of PRI mayor (PAN mayor) + estimated coefficient of the interaction of PRI (PAN)*lead t=3. This is a post-estimation test using the same specification as that of Table \ref{tab:abraham_sun_lagdv} column (1). Other leads and the indicator at time t=0 when reform came to effect are omitted due to collinearity. Standard errors of linear hypothesis test in parentheses with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided test with the null hypothesis equal to 0. Main regression with standard errors clustered at the state-level. $^b$ Refers to the inverse hyperbolic sine transformation. $^c$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. I also include the lag of the outcome, i.e. logged homicides per capita as control. $^d$ Estimates weighted by each cohort's relative share of the sample following \citet{abraham_sun_2020}.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close



**********************************************
*Table: Mechanisms: INCUMBENCY QUALITY WITH POLYNOMIALS
**********************************************
use "../Data/ConstructionDatabase/data_wleads&lags_incumbency_weights.dta", clear

*GLOBALS: treatment
capture global saturated date_0_2015 date_0_2016 date_0_2017 date_0_2018 lag_2_2016 lag_3_2015 lag_3_2016 lag_3_2018 lag_4_2015 lag_4_2017 lag_5_2015 lag_5_2018 lag_6_2016 lag_6_2018 lag_8_2018 lead_1_2015 lead_1_2016
sum $saturated

*GLOBALS: controls
foreach i in margin_ HHI_ effectiveparties_ NP_ golosov_ governor_alignment_ pop_ logdefuncionespc_{
global `i'  `i'lag_6 `i'lag_5 `i'lag_4 `i'lag_3
}

global allcov2 $margin_  $governor_alignment_ 
*global allcov2 winning_margin_governor governor_alignment

xtset inegi year
global DV $logdefuncionespc_
global DV2 logdefuncionespc

*outcomes: incumbent_yesterday_w_tomorrow2 inc_party_runsfor1 numparties_eff numparties_eff_molinar inc_party_won
est clear
foreach j in incumbent_quality {
foreach pol in 1 2 3 4 {
*Estimate optimal bandwidth:
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
global optimal_75 = e(h_CCT)*.75
global optimal_half = e(h_CCT)*.5

di ${optimal}
di ${optimal_75}
di ${optimal_half}


*Polynomials:
*Generate polynomials:
gen pol`pol'=mv_incparty^`pol' if mv_incparty<${optimal} & mv_incparty>-${optimal}

foreach i in pol`pol'{
foreach var in $saturated{
gen `var'_`i'=`var'*`i'

}
}
}
}

*polynomial globals
capture global interacted_pol1 date_0_2015_pol1  date_0_2016_pol1  date_0_2017_pol1  date_0_2018_pol1  lag_5_2018_pol1  lag_4_2015_pol1  lag_3_2015_pol1  lag_3_2016_pol1  lag_3_2018_pol1
capture global interacted_pol2 date_0_2015_pol2  date_0_2016_pol2  date_0_2017_pol2  date_0_2018_pol2  lag_5_2018_pol2  lag_4_2015_pol2  lag_3_2015_pol2  lag_3_2016_pol2  lag_3_2018_pol2
capture global interacted_pol3 date_0_2015_pol3  date_0_2016_pol3  date_0_2017_pol3  date_0_2018_pol3  lag_5_2018_pol3  lag_4_2015_pol3  lag_3_2015_pol3  lag_3_2016_pol3  lag_3_2018_pol3
capture global interacted_pol4 date_0_2015_pol4  date_0_2016_pol4  date_0_2017_pol4  date_0_2018_pol4  lag_5_2018_pol4  lag_4_2015_pol4  lag_3_2015_pol4  lag_3_2016_pol4  lag_3_2018_pol4

preserve
foreach i in incumbent_yesterday_w_tomorrow2{
rdbwselect  `i' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal2 = e(h_CCT)

quietly areg  `i'  $saturated pol2 $interacted_pol2 $allcov $DV2  i.year if mv_incparty<${optimal2} & mv_incparty>-${optimal2}, a(inegi) vce(cluster inegi)
keep if e(sample)==1
}

*------------------
*A)SAMPLE INC. AT T-1 WINS AT T+1 :
*------------------
est clear
foreach j in incumbent_quality {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 

******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol1 $allcov2 $DV   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_logdet_`pol':  di %5.4f e(r2)
		glo N_logdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
			
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]+_b[`i'_2017]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}




******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol1 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)		
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}

foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}


}

foreach pol in 2 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet_`pol':  di %5.4f e(r2)
		glo N_logdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]+_b[`i'_2017]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}




******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)		
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}

foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}


}

foreach pol in 3 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol3 $allcov2 $DV   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet_`pol':  di %5.4f e(r2)
		glo N_logdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]+_b[`i'_2017]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}




******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol3 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)		
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}

foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}


}

foreach pol in 4 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol4 $allcov2 $DV   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet_`pol':  di %5.4f e(r2)
		glo N_logdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]+_b[`i'_2017]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}




******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol4 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)		
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}

foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}


}


}
restore


*------------------
*B)WITH HOMICIDES CONTROL
*------------------
preserve
foreach i in inc_party_won{


/*foreach j in incumbent_yesterday_w_tomorrow2{
rdbwselect  `j' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal2 = e(h_CCT)

quietly areg  `j'  $saturated pol2 $interacted_pol2 $allcov $DV2  i.year if mv_incparty<${optimal2} & mv_incparty>-${optimal2}, a(inegi) vce(cluster inegi)
keep if e(sample)==1
}
*/

rdbwselect  `i' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal2 = e(h_CCT)

quietly areg  `i'  $saturated pol2 $interacted_pol2 $allcov $DV2  i.year if mv_incparty<${optimal2} & mv_incparty>-${optimal2}, a(inegi) vce(cluster inegi)
keep if e(sample)==1
}

est clear
foreach j in incumbent_quality{
foreach pol in 1{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 



******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol1 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet2_`pol':  di %5.4f e(r2)
		glo N_logdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2017]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}



******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol1 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)		
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}

foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}


}


foreach pol in 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 



******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet2_`pol':  di %5.4f e(r2)
		glo N_logdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2017]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}



******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)		
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}

foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}


}

foreach pol in 3{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 



******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol3 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet2_`pol':  di %5.4f e(r2)
		glo N_logdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2017]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}



******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol3 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)		
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}

foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}


}

foreach pol in 4{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 



******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol4 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet2_`pol':  di %5.4f e(r2)
		glo N_logdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2017]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}



******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol4 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)		
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}

foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}


}
}

restore


texdoc init  "../Tables/abraham_sun_quality_wpolynomials_short.tex", replace force
*tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Event-in-Discontinuity in close elections model: Effect of 2014 Term Limit Reform on Incumbent's Quality }
tex \label{tab:incumbency_quality}
tex \scalebox{0.6}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{Incumbent quality indicator}   \\
tex & (1) & (2) \\
*tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}  \\
*tex \addlinespace
tex & \multicolumn{2}{c}{linear polynomial} \\
tex \cmidrule(lrr){2-3} \\

tex Lag 6 years &     $ ${beta_lag_6_ihsdet_1}^{${est_lag_6_ihsdet_1}} $ &     $ ${beta_lag_6_ihsdet2_1}^{${est_lag_6_ihsdet2_1}} $  \\
tex  &     ($${se_lag_6_ihsdet_1}$) &    ($${se_lag_6_ihsdet2_1}$)  \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_1}^{${est_lag_5_ihsdet_1}} $ &       $ ${beta_lag_5_ihsdet2_1}^{${est_lag_5_ihsdet2_1}} $  \\
tex  &     ($${se_lag_5_ihsdet_1}$) &    ($${se_lag_5_ihsdet2_1}$)  \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_1}^{${est_lag_4_ihsdet_1}} $ &       $ ${beta_lag_4_ihsdet2_1}^{${est_lag_4_ihsdet2_1}} $\\  
tex  &     ($${se_lag_4_ihsdet_1}$) &    ($${se_lag_4_ihsdet2_1}$)  \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_1}^{${est_lag_3_ihsdet_1}} $  &     $ ${beta_lag_3_ihsdet2_1}^{${est_lag_3_ihsdet2_1}} $ \\   
tex  &     ($${se_lag_3_ihsdet_1}$) &    ($${se_lag_3_ihsdet2_1}$)  \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_1}^{${est_date_0_ihsdet_1}} $ &     $ ${beta_date_0_ihsdet2_1}^{${est_date_0_ihsdet2_1}} $\\   
tex  &     ($${se_date_0_ihsdet_1}$) &    ($${se_date_0_ihsdet2_1}$)  \\
tex \\
tex Observations        &        ${N_ihsdet_1}    &        ${N_ihsdet2_1} \\ 
tex R-squared        &          ${r2_ihsdet_1}  &          ${r2_ihsdet2_1} \\  
tex\\
tex & \multicolumn{2}{c}{quadratic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 6 years &     $ ${beta_lag_6_ihsdet_2}^{${est_lag_6_ihsdet_2}} $ &     $ ${beta_lag_6_ihsdet2_2}^{${est_lag_6_ihsdet2_2}} $  \\
tex  &     ($${se_lag_6_ihsdet_2}$) &    ($${se_lag_6_ihsdet2_2}$)  \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_2}^{${est_lag_5_ihsdet_2}} $ &       $ ${beta_lag_5_ihsdet2_2}^{${est_lag_5_ihsdet2_2}} $  \\
tex  &     ($${se_lag_5_ihsdet_2}$) &    ($${se_lag_5_ihsdet2_2}$)  \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_2}^{${est_lag_4_ihsdet_2}} $ &       $ ${beta_lag_4_ihsdet2_2}^{${est_lag_4_ihsdet2_2}} $\\  
tex  &     ($${se_lag_4_ihsdet_2}$) &    ($${se_lag_4_ihsdet2_2}$)  \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_2}^{${est_lag_3_ihsdet_2}} $  &     $ ${beta_lag_3_ihsdet2_2}^{${est_lag_3_ihsdet2_2}} $ \\   
tex  &     ($${se_lag_3_ihsdet_2}$) &    ($${se_lag_3_ihsdet2_2}$)  \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_2}^{${est_date_0_ihsdet_2}} $ &     $ ${beta_date_0_ihsdet2_2}^{${est_date_0_ihsdet2_2}} $\\   
tex  &     ($${se_date_0_ihsdet_2}$) &    ($${se_date_0_ihsdet2_2}$)  \\
tex \\
tex Observations        &        ${N_ihsdet_2}    &        ${N_ihsdet2_2} \\ 
tex R-squared        &          ${r2_ihsdet_2}  &          ${r2_ihsdet2_2} \\  
tex \\
tex & \multicolumn{2}{c}{cubic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 6 years &     $ ${beta_lag_6_ihsdet_3}^{${est_lag_6_ihsdet_3}} $ &     $ ${beta_lag_6_ihsdet2_3}^{${est_lag_6_ihsdet2_3}} $  \\
tex  &     ($${se_lag_6_ihsdet_3}$) &    ($${se_lag_6_ihsdet2_3}$)  \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_3}^{${est_lag_5_ihsdet_3}} $ &       $ ${beta_lag_5_ihsdet2_3}^{${est_lag_5_ihsdet2_3}} $  \\
tex  &     ($${se_lag_5_ihsdet_3}$) &    ($${se_lag_5_ihsdet2_3}$)  \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_3}^{${est_lag_4_ihsdet_3}} $ &       $ ${beta_lag_4_ihsdet2_3}^{${est_lag_4_ihsdet2_3}} $\\  
tex  &     ($${se_lag_4_ihsdet_3}$) &    ($${se_lag_4_ihsdet2_3}$)  \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_3}^{${est_lag_3_ihsdet_3}} $  &     $ ${beta_lag_3_ihsdet2_3}^{${est_lag_3_ihsdet2_3}} $ \\   
tex  &     ($${se_lag_3_ihsdet_3}$) &    ($${se_lag_3_ihsdet2_3}$)  \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_3}^{${est_date_0_ihsdet_3}} $ &     $ ${beta_date_0_ihsdet2_3}^{${est_date_0_ihsdet2_3}} $\\   
tex  &     ($${se_date_0_ihsdet_3}$) &    ($${se_date_0_ihsdet2_3}$)  \\
tex \\
tex Observations        &        ${N_ihsdet_3}    &        ${N_ihsdet2_3} \\ 
tex R-squared        &          ${r2_ihsdet_3}  &          ${r2_ihsdet2_3} \\  
tex \\
tex & \multicolumn{2}{c}{quartic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 6 years &     $ ${beta_lag_6_ihsdet_4}^{${est_lag_6_ihsdet_4}} $ &     $ ${beta_lag_6_ihsdet2_4}^{${est_lag_6_ihsdet2_4}} $  \\
tex  &     ($${se_lag_6_ihsdet_4}$) &    ($${se_lag_6_ihsdet2_4}$)  \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_4}^{${est_lag_5_ihsdet_4}} $ &       $ ${beta_lag_5_ihsdet2_4}^{${est_lag_5_ihsdet2_4}} $  \\
tex  &     ($${se_lag_5_ihsdet_4}$) &    ($${se_lag_5_ihsdet2_4}$)  \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_4}^{${est_lag_4_ihsdet_4}} $ &       $ ${beta_lag_4_ihsdet2_4}^{${est_lag_4_ihsdet2_4}} $\\  
tex  &     ($${se_lag_4_ihsdet_4}$) &    ($${se_lag_4_ihsdet2_4}$)  \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_4}^{${est_lag_3_ihsdet_4}} $  &     $ ${beta_lag_3_ihsdet2_4}^{${est_lag_3_ihsdet2_4}} $ \\   
tex  &     ($${se_lag_3_ihsdet_4}$) &    ($${se_lag_3_ihsdet2_4}$)  \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_4}^{${est_date_0_ihsdet_4}} $ &     $ ${beta_date_0_ihsdet2_4}^{${est_date_0_ihsdet2_4}} $\\   
tex  &     ($${se_date_0_ihsdet_4}$) &    ($${se_date_0_ihsdet2_4}$)  \\
tex \\
tex Observations        &        ${N_ihsdet_4}    &        ${N_ihsdet2_4} \\ 
tex R-squared        &          ${r2_ihsdet_4}  &          ${r2_ihsdet2_4} \\  
tex\\

tex Mun. FEs      &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   \\
tex State Controls$^b$  &    \checkmark     &       \checkmark \\
tex Mun Controls$^c$  &    \checkmark     &       \checkmark \\
tex Cohort weighted  &     \checkmark &        \checkmark \\
tex Sample Inc. Adv. DV & Inc. at t-1 won at t+1 & Inc. at t won at t+1 \\
tex \hline \hline      
tex \multicolumn{3}{p{0.75\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 6 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020} or because they are collinear or inexistent like lag 2. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-levels: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Even columns with outcomes where missing values where replaced by zeros assuming no activity was registered. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. $^c$ Municipal controls include logged homicides per capita interacted with cohort fixed effects pre-treatment.}} \\
tex \end{tabular}
tex } 
tex \end{table}
*tex \end{landscape}
texdoc close




**********************************************
*Table: WITH LAGGED DV. & citizen demands as controlss
**********************************************
/*
ap4_2_3 - effect but too big; citizens that see narcotraffick as important
ap4_2_5 - effect but too big; citizens that see insecurity as important
ap4_2_11 - effect but too big; citizens that demand punishmnet
ap4_3_1 - effect but too big; citizens that see insecurity in their neighborhood
ap4_3_2 - effect but too big; citizens that see insecurity in their municipality
ap4_3_3 - effect but too big; citizens that see insecurity in their state
***effect of these last three diminishes strongly when moving away from home, i.e. externalities of war
ap4_7_1 - one year after will security better in neighborhood
ap4_7_2 - one year after will security better in municipality
ap4_7_3 - one year after will security better in state
ap4_7_1_b - dummy
ap4_7_2_b 
ap4_7_3_b

ap4_12b - no effect - money spent in security provision
ap5_1_6 - negative effect and strong -seguridad privada 
ap5_1_7 - negative effect and strong - policia barrial 
ap5_1_8 - negative effect and strong - operativo contra delincuencia
ap5_1_10 - negative effect and strong - patrullaje
ap5_1_12 - negative effect and strong - operativo contra narco
ap5_4_2_b policia preventiva municipal - negative effect and strong
ap5_4_3_b policia estatal - no effect
ap5_4_4_b policia federal - no effect
ap5_4_5_b policia ministerial o judicial - no effect
ap5_4_6_b MP, procuradurias estatales - no effect
ap5_4_7_b PGR - no effect
ap5_4_8_b ejercito - no effect
ap5_4_9_b marina - positive effect and strong

*I can use as placebo other unrelated questions

*next to do, run with pre-treatment split
*/

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

**create citizen demands as controls:
global insecurityperception ap4_2_3 ap4_2_5 ap4_2_11 ap4_3_1 ap4_3_2 ap4_3_3 ap4_7_1 ap4_7_2 ap4_7_3 ap4_12b ap5_1_6 ap5_1_7 ap5_1_8 ap5_1_10 ap5_1_12 ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_9 ap4_7_1_b ap4_7_2_b ap4_7_3_b ap5_4_2_b ap5_4_3_b ap5_4_4_b ap5_4_5_b ap5_4_6_b ap5_4_7_b ap5_4_8_b ap5_4_9_b
foreach var in $insecurityperception{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}


*Set globals: 
	global narco ap4_2_3_lag_8 ap4_2_3_lag_7 ap4_2_3_lag_6 ap4_2_3_lag_5 ap4_2_3_lag_4 ap4_2_3_lag_3 ap4_2_3_lag_2
	global punishment ap4_2_11_lag_8 ap4_2_11_lag_7 ap4_2_11_lag_6 ap4_2_11_lag_5 ap4_2_11_lag_4 ap4_2_11_lag_3 ap4_2_11_lag_2
	global money ap4_12b_lag_8 ap4_12b_lag_7 ap4_12b_lag_6 ap4_12b_lag_5 ap4_12b_lag_4 ap4_12b_lag_3 ap4_12b_lag_2
	global police ap5_4_2_b_lag_8 ap5_4_2_b_lag_7 ap5_4_2_b_lag_6 ap5_4_2_b_lag_5 ap5_4_2_b_lag_4 ap5_4_2_b_lag_3 ap5_4_2_b_lag_2
	global army ap5_4_8_b_lag_8 ap5_4_8_b_lag_7 ap5_4_8_b_lag_6 ap5_4_8_b_lag_5 ap5_4_8_b_lag_4 ap5_4_8_b_lag_3 ap5_4_8_b_lag_2
	global citizens $narco $punishment $money $police $army
	global narco2 ap4_2_3
	global punishment2 ap4_2_11
	global money2 ap4_12b
	global police2 ap5_4_2_b
	global army2 ap5_4_8_b
	global citizens2  $narco2 $punishment2 $money2 $police2 $army2  
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc
	xtset inegi year 
	

************
***A: log(homicides per capita) without covariates
************
quietly areg  logdefuncionespc  $saturated  $controls   i.year, a(inegi) vce(cluster estado)
keep if e(sample)==1
set matsize 11000

est clear
areg  logdefuncionespc  $saturated $lagDV  $controls   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log_nocov:  di %5.4f e(r2)
		glo N_log_nocov: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
	lincom (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log_nocov: di %5.4f r(se)
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
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
	lincom (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log_nocov: di %5.4f r(se)
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
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log_nocov: di %5.4f r(se)
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
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
	lincom (_b[`i'_2015]*`a')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}




************
***B: ihs(homicides per capita) without covariates
************	
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs_nocov:  di %5.4f e(r2)
		glo N_ihs_nocov: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
	lincom (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
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
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
	lincom (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
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
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
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
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
	lincom (_b[`i'_2015]*`a')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}



************
***C: log(homicides per capita) with covariates
************	

est clear
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
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
	lincom (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
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
	lincom (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
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
	lincom (_b[`i'_2015]*`a')
	glo se_`i'_log: di %5.4f r(se)
			}


************
***D: ihs(homicides per capita) with covariates
************	
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
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
	lincom (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
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
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
	lincom (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
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
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
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
	lincom (_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
			}



	
texdoc init  "../Tables/abraham_sun_estimates_lagDV&citizendemands.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence, controlling for citizens security perception}
tex \label{tab:abraham_sun_citizensdemands}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace
tex Lag 7 years &     $ ${beta_lag_7_log_nocov}^{${est_lag_7_log_nocov}} $ &      & $ ${beta_lag_7_ihs_nocov}^{${est_lag_7_ihs_nocov}} $ &   \\
tex  &     ($${se_lag_7_log_nocov}$) &    & ($${se_lag_7_ihs_nocov}$) & \\
tex Lag 6 years &     $ ${beta_lag_6_log_nocov}^{${est_lag_6_log_nocov}} $ &  $ ${beta_lag_6_log}^{${est_lag_6_log}} $    &  $ ${beta_lag_6_ihs_nocov}^{${est_lag_6_ihs_nocov}} $ &  $ ${beta_lag_6_ihs}^{${est_lag_6_ihs}} $  \\
tex  &     ($${se_lag_6_log_nocov}$) &   ($${se_lag_6_log}$) & ($${se_lag_6_ihs_nocov}$) & ($${se_lag_6_ihs}$) \\
tex Lag 5 years &     $ ${beta_lag_5_log_nocov}^{${est_lag_5_log_nocov}} $ &   $ ${beta_lag_5_log}^{${est_lag_5_log}} $   &  $ ${beta_lag_5_ihs_nocov}^{${est_lag_5_ihs_nocov}} $ &  $ ${beta_lag_5_ihs}^{${est_lag_5_ihs}} $ \\
tex  &     ($${se_lag_5_log_nocov}$) &   ($${se_lag_5_log}$) & ($${se_lag_5_ihs_nocov}$) & ($${se_lag_5_ihs}$) \\
tex Lag 4 years &     $ ${beta_lag_4_log_nocov}^{${est_lag_4_log_nocov}} $ &   $ ${beta_lag_4_log}^{${est_lag_4_log}} $   &   $ ${beta_lag_4_ihs_nocov}^{${est_lag_4_ihs_nocov}} $ &   $ ${beta_lag_4_ihs}^{${est_lag_4_ihs}} $  \\
tex  &     ($${se_lag_4_log_nocov}$) &   ($${se_lag_4_log}$) & ($${se_lag_4_ihs_nocov}$) & ($${se_lag_4_ihs}$) \\
tex Lag 3 years &     $ ${beta_lag_3_log_nocov}^{${est_lag_3_log_nocov}} $ &   $ ${beta_lag_3_log}^{${est_lag_3_log}} $   &   $ ${beta_lag_3_ihs_nocov}^{${est_lag_3_ihs_nocov}} $ &   $ ${beta_lag_3_ihs}^{${est_lag_3_ihs}} $ \\
tex  &     ($${se_lag_3_log_nocov}$) &   ($${se_lag_3_log}$) & ($${se_lag_3_ihs_nocov}$) & ($${se_lag_3_ihs}$) \\
tex Lag 2 years &     $ ${beta_lag_2_log_nocov}^{${est_lag_2_log_nocov}} $ &     $ ${beta_lag_2_log}^{${est_lag_2_log}} $ &  $ ${beta_lag_2_ihs_nocov}^{${est_lag_2_ihs_nocov}} $  &  $ ${beta_lag_2_ihs}^{${est_lag_2_ihs}} $  \\
tex  &     ($${se_lag_2_log_nocov}$) &   ($${se_lag_2_log}$) & ($${se_lag_2_ihs_nocov}$) & ($${se_lag_2_ihs}$) \\
tex Reform, time 0 &     $ ${beta_date_0_log_nocov}^{${est_date_0_log_nocov}} $ &     $ ${beta_date_0_log}^{${est_date_0_log}} $ &   $ ${beta_date_0_ihs_nocov}^{${est_date_0_ihs_nocov}} $   &   $ ${beta_date_0_ihs}^{${est_date_0_ihs}} $ \\
tex  &     ($${se_date_0_log_nocov}$) &   ($${se_date_0_log}$) & ($${se_date_0_ihs_nocov}$) & ($${se_date_0_ihs}$) \\
tex Lead 1 year &     $ ${beta_lead_1_log_nocov}^{${est_lead_1_log_nocov}} $ &     $ ${beta_lead_1_log}^{${est_lead_1_log}} $ &    $ ${beta_lead_1_ihs_nocov}^{${est_lead_1_ihs_nocov}} $ &    $ ${beta_lead_1_ihs}^{${est_lead_1_ihs}} $ \\
tex  &     ($${se_lead_1_log_nocov}$) &   ($${se_lead_1_log}$) & ($${se_lead_1_ihs_nocov}$) & ($${se_lead_1_ihs}$) \\
tex Lead 2 years &     $ ${beta_lead_2_log_nocov}^{${est_lead_2_log_nocov}} $ &     $ ${beta_lead_2_log}^{${est_lead_2_log}} $ &   $ ${beta_lead_2_ihs_nocov}^{${est_lead_2_ihs_nocov}} $  &   $ ${beta_lead_2_ihs}^{${est_lead_2_ihs}} $  \\
tex  &     ($${se_lead_2_log_nocov}$) &   ($${se_lead_2_log}$) & ($${se_lead_2_ihs_nocov}$) & ($${se_lead_2_ihs}$) \\
tex Lead 3 years &     $ ${beta_lead_3_log_nocov}^{${est_lead_3_log_nocov}} $ &     $ ${beta_lead_3_log}^{${est_lead_3_log}} $ &   $ ${beta_lead_3_ihs_nocov}^{${est_lead_3_ihs_nocov}} $ &   $ ${beta_lead_3_ihs}^{${est_lead_3_ihs}} $ \\
tex  &     ($${se_lead_3_log_nocov}$) &   ($${se_lead_3_log}$) & ($${se_lead_3_ihs_nocov}$) & ($${se_lead_3_ihs}$) \\
tex \\

tex \addlinespace
tex Observations       &        ${N_log_nocov}    &        ${N_log}    &     ${N_ihs_nocov}      &     ${N_ihs}  \\
tex R-squared        &          ${r2_log_nocov} &          ${r2_log}    &           ${r2_ihs_nocov}       &           ${r2_ihs}   \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^b$  &    \checkmark      &       \checkmark  &    \checkmark      &   \checkmark    \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\
tex Lag DV &  \checkmark      &       \checkmark  &    \checkmark      &   \checkmark    \\
tex Citizens' Security Perception$^c$ &    &   \checkmark  &         &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{5}{p{0.9\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}; lag 7 is removed in columns (2) and (4) due to collinearity. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. $^c$ Citizens' Security Perception are state-level covariates that include the percentage of citizens who see narcotraffick as the most worrisome issue in the country, the percentage of citizens who see a lack of punishment of criminals as the most worrisome public issue, the amount of money (in thousands) spend to protect from crime, and citizens trust on local police forces and the army.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


**********************************************
*Table: Abraham-Sun (2020) DTOs het effects
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

**create citizen demands as controls:
global dtos distEntradasPrinc eneCarteles hayCarteles nCarteles vCarteles
foreach var in $dtos{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

*Set globals: 
	global dtos_controls2 hayCarteles_lag_8 hayCarteles_lag_7 hayCarteles_lag_6 hayCarteles_lag_5 hayCarteles_lag_4 hayCarteles_lag_3 hayCarteles_lag_2
	global dtos_controls3 distEntradasPrinc_lag_8 distEntradasPrinc_lag_7 distEntradasPrinc_lag_6 distEntradasPrinc_lag_5 distEntradasPrinc_lag_4 distEntradasPrinc_lag_3 distEntradasPrinc_lag_2	
	global dtos_controls_all $dtos_controls2 $dtos_controls3

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc

	xtset inegi year 


/************
***A: log(homicides per capita) with covariates 
************
est clear
foreach i in hayCarteles{

eststo: areg   logdefuncionespc c.${saturated}##c.`i'##c.distEntradasPrinc $controls $lagDV    i.year & acuerdo2==0, a(inegi) vce(cluster inegi)

	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local statecontrols \checkmark
	estadd local cohortweights \checkmark
	estadd local lagdv \checkmark

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] + _b[distEntradasPrinc] + _b[1.lead_3_2015#c.distEntradasPrinc] + _b[1.lead_3_2015#c.`i'#c.distEntradasPrinc] + _b[c.`i'#c.distEntradasPrinc] 
	glo beta_`i'3: di %5.4f r(estimate)
	glo se_`i'3: di %5.4f r(se)
	glo g_`i'3: di r(df)
	glo t_`i'3: di  %5.4f ${beta_`i'3}/${se_`i'3}
	glo p_`i'3: di 2*ttail(${g_`i'3},abs(${t_`i'3})) 
	glo est_`i'3= "" 
			if (${p_`i'3}<=0.1) global est_`i'3  = "*"
			if (${p_`i'3}<=0.05) global est_`i'3 = "**"
			if (${p_`i'3}<=0.01) global est_`i'3 = "***"		
			
    estadd local fstat_`i'3 $${beta_`i'3}^{${est_`i'3}}
	estadd local error_`i'3 (${se_`i'3})



************
***B: ihs(homicides per capita) with covariates 
************

eststo: areg ihs_defuncionespc c.${saturated}##c.`i'##c.distEntradasPrinc $controls $lagDV2    i.year & acuerdo2==0, a(inegi) vce(cluster inegi)

	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local statecontrols \checkmark
	estadd local cohortweights \checkmark
	estadd local lagdv \checkmark

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] + _b[distEntradasPrinc] + _b[1.lead_3_2015#c.distEntradasPrinc] + _b[1.lead_3_2015#c.`i'#c.distEntradasPrinc] + _b[c.`i'#c.distEntradasPrinc] 
	glo beta_`i'3: di %5.4f r(estimate)
	glo se_`i'3: di %5.4f r(se)
	glo g_`i'3: di r(df)
	glo t_`i'3: di  %5.4f ${beta_`i'3}/${se_`i'3}
	glo p_`i'3: di 2*ttail(${g_`i'3},abs(${t_`i'3})) 
	glo est_`i'3= "" 
			if (${p_`i'3}<=0.1) global est_`i'3  = "*"
			if (${p_`i'3}<=0.05) global est_`i'3 = "**"
			if (${p_`i'3}<=0.01) global est_`i'3 = "***"		
			
    estadd local fstat_`i'3 $${beta_`i'3}^{${est_`i'3}}
	estadd local error_`i'3 (${se_`i'3})


esttab using "../Tables/abraham_sun_estimates_dtos_het_results.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///d
s(N r2 munfe yearfe statecontrols cohortweights lagdv fstat_`i'3 error_`i'3, fmt(0 3) label("Observations" "R-squared"  ///
"Mun. FEs" "Year FEs" "State Controls$^c$" "Cohort weighted$^d$" ///
"Lag DV" "Tot. int. effect$^a$" "Tot. int. effect, SE")) /// 
 keep( 1.lead_3_2015#c.`i' 1.lead_3_2015#c.distEntradasPrinc 1.lead_3_2015#c.`i'#c.distEntradasPrinc ) ///
coeflabel( ///
`i'  "Cartel presence" ///
1.lead_3_2015#c.`i' "Reform (t+3) X Cartel presence" ///
1.lead_3_2015#c.distEntradasPrinc "Reform (t+3) X Proximity to U.S." ///
c.`i'#c.distEntradasPrinc "Cartel presence X Proximity to U.S." ///
1.lead_3_2015#c.`i'#c.distEntradasPrinc "Reform (t+3) X Cartel presence X Proximity to U.S.") ///
mgroups("log(homicide per capita)" "ihs(homicide per capita)$^b$", ///
 pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
collabels(none) nonotes booktabs nomtitles  nolines

}


 */
 
************
***A: log(homicides per capita) with covariates 
quietly	areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster inegi)
keep if e(sample)==1

est clear
foreach i in distEntradasPrinc{

areg   logdefuncionespc c.${saturated}##c.`i' $controls $lagDV   i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'1:  di %5.4f e(r2)
		glo N_`i'1: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***B: ihs(homicides per capita) with covariates 
************


est clear
foreach i in distEntradasPrinc{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls $lagDV2   i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'2:  di %5.4f e(r2)
		glo N_`i'2: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***C: log(homicides per capita) with covariates 
************
est clear
foreach i in hayCarteles{

areg   logdefuncionespc c.${saturated}##c.`i' $controls $lagDV   i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'3:  di %5.4f e(r2)
		glo N_`i'3: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***D: ihs(homicides per capita) with covariates 
************
est clear
foreach i in hayCarteles{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls $lagDV2  i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'4:  di %5.4f e(r2)
		glo N_`i'4: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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




texdoc init  "../Tables/abraham_sun_estimates_dtos_het.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect of Term Limit Reform and Drug Trafficking Organization Presence on Violence$^a$}
tex \label{tab:abraham_sun_dto_het}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{b}$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace
tex Reform (t+3) X Proximity to U.S. &     $ ${beta_distEntradasPrinc1}^{${est_distEntradasPrinc1}} $ &  &  $ ${beta_distEntradasPrinc2}^{${est_distEntradasPrinc2}} $  & \\
tex  &     ($ ${se_distEntradasPrinc1} $) &  & ($ ${se_distEntradasPrinc2} $)  & \\

tex Reform (t+3) X Cartel presence (indicator) &    &   $ ${beta_hayCarteles3}^{${est_hayCarteles3}} $ &  &  $ ${beta_hayCarteles4}^{${est_hayCarteles4}} $ \\
tex  &    &     ($ ${se_hayCarteles3} $) & & ($ ${se_hayCarteles4} $)  \\

tex \addlinespace
tex Observations       &        ${N_distEntradasPrinc1}    &        ${N_distEntradasPrinc2}    &     ${N_hayCarteles3}      &     ${N_hayCarteles4}  \\
tex R-squared       &        ${r2_distEntradasPrinc1}    &        ${r2_distEntradasPrinc2}    &     ${r2_hayCarteles3}      &     ${r2_hayCarteles4}  \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^c$  &  \checkmark       &       \checkmark  &  \checkmark        &   \checkmark    \\
tex Cohort weighted$^d$  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\
tex Lag DV &  \checkmark      &       \checkmark  &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{5}{p{1.1\textwidth}}{\footnotesize{Notes:$^a$ Total interaction effect tests the linear hypothesis of the estimated coefficient of proximity to the US (Cartel presence indicator) + estimated coefficient of the interaction of proximity (cartel presence)*lead t=3. This is a post-estimation test using the same specification as that of Table \ref{tab:abraham_sun_lagdv}. Other leads and the indicator at time t=0 when reform came to effect are omitted due to collinearity. Standard errors of linear hypothesis test in parentheses with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided test with the null hypothesis equal to 0. $^b$ Refers to the inverse hyperbolic sine transformation. $^c$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. $^d$ Estimates weighted by each cohort's relative share of the sample following \citet{abraham_sun_2020}.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


**********************************************
*Table: EFFECT OF REFORM ON HOMICIDES: CONTROLLING FOR MANDO UNICO
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 acuerdo2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc
	xtset inegi year 
	

************
***A: log(homicides per capita) with covariates
************	

est clear
quietly	areg  logdefuncionespc  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
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


************
***B: ihs(homicides per capita) with covariates
************	
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_7{
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
	
foreach i in lag_6{
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
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
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
*Table
	
texdoc init  "../Tables/abraham_sun_estimates_lagDV_mandounico.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence, controlling for security cooperation agreements}
tex \label{tab:abraham_sun_lagdv_mandounico}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicide per capita)} & \multicolumn{1}{c}{ihs(homicide per capita)$^{a}$} \\
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
tex State Controls$^b$   &    \checkmark      &   \checkmark    \\
tex Cohort weighted   &   \checkmark       &   \checkmark    \\
tex Lag DV &          \checkmark         &   \checkmark    \\
tex Security Coop. Agreement &          \checkmark         &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{3}{p{0.9\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


**********************************************
*Table: EFFECT OF REFORM ON HOMICIDES: SPLITTING SAMPLE FOLLOWING MANDO UNICO STATUS
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc
	xtset inegi year 
	
	gen acuerdo2_pre=0
	replace acuerdo2_pre=1 if acuerdo2==1 & year<=2015
		gen acuerdo_pre=0
	replace acuerdo_pre=1 if acuerdo==1 & year<=2015
	global mandounico  acuerdo2_pre
************
***A: Acuerdo2 =0
************	

est clear
quietly	areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==0, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
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


************
***B: Acuerdo2 =1 
************	
quietly	areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==1, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_7{
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
	
foreach i in lag_6{
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
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
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



************
***C: ihs Acuerdo2 =0
************	

est clear
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year if $mandounico==0, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log2:  di %5.4f e(r2)
		glo N_log2: di %11.2gc e(N)

***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log2: di %5.4f r(se)
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
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log2: di %5.4f r(se)
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
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log2: di %5.4f r(se)
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
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_log2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_log2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_log2: di %5.4f r(se)
}


************
***D: IHS Acuerdo2 =1 
************	
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year if $mandounico==1, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs2:  di %5.4f e(r2)
		glo N_ihs2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs2: di %5.4f r(se)
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
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs2: di %5.4f r(se)
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
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs2: di %5.4f r(se)
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
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs2: di %5.4f r(se)
}





*Table 
	
texdoc init  "../Tables/abraham_sun_estimates_lagDV_splitmandounico.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence, comparing municipalities with security cooperation agreements with other levels of government}
tex \label{tab:abraham_sun_lagdv_splitmandounico}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^a$}\\
tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5} \\
tex & \multicolumn{1}{c}{w/o Security} & \multicolumn{1}{c}{w/ Security} & \multicolumn{1}{c}{w/o Security} & \multicolumn{1}{c}{w/ Security} \\ 
tex & \multicolumn{1}{c}{Coop. Agreement} & \multicolumn{1}{c}{Coop. Agreement} & \multicolumn{1}{c}{Coop. Agreement} & \multicolumn{1}{c}{Coop. Agreement}\\ 
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)}  & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)}\\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \cmidrule(lrr){5-5}\\
tex \addlinespace
tex Lag 7 years &      $ ${beta_lag_7_log}^{${est_lag_7_log}} $ &  $ ${beta_lag_7_ihs}^{${est_lag_7_ihs}} $   &      $ ${beta_lag_7_log2}^{${est_lag_7_log2}} $ &  $ ${beta_lag_7_ihs2}^{${est_lag_7_ihs2}} $ \\
tex  & ($ ${se_lag_7_log}$) & ($ ${se_lag_7_ihs} $)  & ($ ${se_lag_7_log2}$) & ($ ${se_lag_7_ihs2} $) \\
tex Lag 6 years &      $ ${beta_lag_6_log}^{${est_lag_6_log}} $ &  $ ${beta_lag_6_ihs}^{${est_lag_6_ihs}} $   &      $ ${beta_lag_6_log2}^{${est_lag_6_log2}} $ &  $ ${beta_lag_6_ihs2}^{${est_lag_6_ihs2}} $ \\
tex  & ($ ${se_lag_6_log}$) & ($ ${se_lag_6_ihs} $)  & ($ ${se_lag_6_log2}$) & ($ ${se_lag_6_ihs2} $) \\
tex Lag 5 years &      $ ${beta_lag_5_log}^{${est_lag_5_log}} $ &  $ ${beta_lag_5_ihs}^{${est_lag_5_ihs}} $   &      $ ${beta_lag_5_log2}^{${est_lag_5_log2}} $ &  $ ${beta_lag_5_ihs2}^{${est_lag_5_ihs2}} $ \\
tex  & ($ ${se_lag_5_log}$) & ($ ${se_lag_5_ihs} $)  & ($ ${se_lag_5_log2}$) & ($ ${se_lag_5_ihs2} $) \\
tex Lag 4 years &      $ ${beta_lag_4_log}^{${est_lag_4_log}} $ &  $ ${beta_lag_4_ihs}^{${est_lag_4_ihs}} $   &      $ ${beta_lag_4_log2}^{${est_lag_4_log2}} $ &  $ ${beta_lag_4_ihs2}^{${est_lag_4_ihs2}} $ \\
tex  & ($ ${se_lag_4_log}$) & ($ ${se_lag_4_ihs} $)  & ($ ${se_lag_4_log2}$) & ($ ${se_lag_4_ihs2} $) \\
tex Lag 3 years &      $ ${beta_lag_3_log}^{${est_lag_3_log}} $ &  $ ${beta_lag_3_ihs}^{${est_lag_3_ihs}} $   &      $ ${beta_lag_3_log2}^{${est_lag_3_log2}} $ &  $ ${beta_lag_3_ihs2}^{${est_lag_3_ihs2}} $ \\
tex  & ($ ${se_lag_3_log}$) & ($ ${se_lag_3_ihs} $)  & ($ ${se_lag_3_log2}$) & ($ ${se_lag_3_ihs2} $) \\
tex Lag 2 years &      $ ${beta_lag_2_log}^{${est_lag_2_log}} $ &  $ ${beta_lag_2_ihs}^{${est_lag_2_ihs}} $   &      $ ${beta_lag_2_log2}^{${est_lag_2_log2}} $ &  $ ${beta_lag_2_ihs2}^{${est_lag_2_ihs2}} $ \\
tex  & ($ ${se_lag_2_log}$) & ($ ${se_lag_2_ihs} $)  & ($ ${se_lag_2_log2}$) & ($ ${se_lag_2_ihs2} $) \\
tex Reform, time 0 &      $ ${beta_date_0_log}^{${est_date_0_log}} $ &  $ ${beta_date_0_ihs}^{${est_date_0_ihs}} $   &      $ ${beta_date_0_log2}^{${est_date_0_log2}} $ &  $ ${beta_date_0_ihs2}^{${est_date_0_ihs2}} $ \\
tex  & ($ ${se_date_0_log}$) & ($ ${se_date_0_ihs} $)  & ($ ${se_date_0_log2}$) & ($ ${se_date_0_ihs2} $) \\
tex Lead 1 year &      $ ${beta_lead_1_log}^{${est_lead_1_log}} $ &  $ ${beta_lead_1_ihs}^{${est_lead_1_ihs}} $   &      $ ${beta_lead_1_log2}^{${est_lead_1_log2}} $ &  $ ${beta_lead_1_ihs2}^{${est_lead_1_ihs2}} $ \\
tex  & ($ ${se_lead_1_log}$) & ($ ${se_lead_1_ihs} $)  & ($ ${se_lead_1_log2}$) & ($ ${se_lead_1_ihs2} $) \\
tex Lead 2 years &      $ ${beta_lead_2_log}^{${est_lead_2_log}} $ &  $ ${beta_lead_2_ihs}^{${est_lead_2_ihs}} $   &      $ ${beta_lead_2_log2}^{${est_lead_2_log2}} $ &  $ ${beta_lead_2_ihs2}^{${est_lead_2_ihs2}} $ \\
tex  & ($ ${se_lead_2_log}$) & ($ ${se_lead_2_ihs} $)  & ($ ${se_lead_2_log2}$) & ($ ${se_lead_2_ihs2} $) \\
tex Lead 3 years &      $ ${beta_lead_3_log}^{${est_lead_3_log}} $ &  $ ${beta_lead_3_ihs}^{${est_lead_3_ihs}} $   &      $ ${beta_lead_3_log2}^{${est_lead_3_log2}} $ &  $ ${beta_lead_3_ihs2}^{${est_lead_3_ihs2}} $ \\
tex  & ($ ${se_lead_3_log}$) & ($ ${se_lead_3_ihs} $)  & ($ ${se_lead_3_log2}$) & ($ ${se_lead_3_ihs2} $) \\

tex \addlinespace
tex Observations       &            ${N_log}        &     ${N_ihs} &            ${N_log2}        &     ${N_ihs2}  \\
tex R-squared        &              ${r2_log}        &           ${r2_ihs}  &              ${r2_log2}        &           ${r2_ihs2}  \\
tex Mun. FEs       &     \checkmark         &  \checkmark  &     \checkmark         &  \checkmark  \\
tex Year. FEs       &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark \\
tex State Controls$^b$   &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark   \\
tex Cohort weighted   &   \checkmark       &   \checkmark &   \checkmark       &   \checkmark    \\
tex Lag DV &          \checkmark         &   \checkmark &          \checkmark         &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{5}{p{1.1\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


**********************************************
*Table: HET EFFECTS: MANDO UNICO
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	xtset inegi year
	global DV l.logdefuncionespc
	

************
***A: log(homicides per capita) with covariates // ACUERDO
************
quietly	areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster inegi)
keep if e(sample)==1

est clear
foreach i in acuerdo{

areg   logdefuncionespc c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'1:  di %5.4f e(r2)
		glo N_`i'1: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***B: ihs(homicides per capita) with covariates // ACUERDO
************


est clear
foreach i in acuerdo{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls $DV i.year, a(inegi) vce(cluster estado)
		glo r2_`i'2:  di %5.4f e(r2)
		glo N_`i'2: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***C: log(homicides per capita) with covariates // ACUERDO2
************
est clear
foreach i in acuerdo2{

areg   logdefuncionespc c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'3:  di %5.4f e(r2)
		glo N_`i'3: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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
***D: ihs(homicides per capita) with covariates // WINNING MARGIN
************
est clear
foreach i in acuerdo2{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'4:  di %5.4f e(r2)
		glo N_`i'4: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
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



texdoc init  "../Tables/abraham_sun_estimates_hetmando_unico.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect$^a$: the role of security cooperation agreements}
tex \label{tab:abraham_sun_mando_unico}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{b}$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace

tex Reform (t+3)*Coop. Agreement &     $ ${beta_acuerdo1}^{${est_acuerdo1}} $ &  &  $ ${beta_acuerdo2}^{${est_acuerdo2}} $  & \\
tex  &     ($ ${se_acuerdo1} $) &  & ($ ${se_acuerdo2} $)  & \\

tex Reform (t+3)*Coop. Agreement (other measure)$^e$ &    &   $ ${beta_acuerdo23}^{${est_acuerdo23}} $ &  &  $ ${beta_acuerdo24}^{${est_acuerdo24}} $ \\
tex  &    &     ($ ${se_acuerdo23} $) & & ($ ${se_acuerdo24} $)  \\

tex \\
tex \addlinespace
tex Observations       &        ${N_acuerdo1}    &        ${N_acuerdo2}    &     ${N_acuerdo23}      &     ${N_acuerdo24}  \\
tex R-squared       &        ${r2_acuerdo1}    &        ${r2_acuerdo2}    &     ${r2_acuerdo23}      &     ${r2_acuerdo24}  \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^c$  &  \checkmark       &       \checkmark  &  \checkmark        &   \checkmark    \\
tex Cohort weighted$^d$  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{5}{p{1.2\textwidth}}{\footnotesize{Notes:$^a$ Total interaction effect tests the linear hypothesis of the estimated coefficient of alignment with Federal Government indicator (winning margin) + estimated coefficient of the interaction of alignment (winning margin)*lead t=3. This is a post-estimation test using the same specification as that of Table \ref{tab:abraham_sun_lagdv} column (1). Other leads and the indicator at time t=0 when reform came to effect are omitted due to collinearity. Standard errors of linear hypothesis test in parentheses with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided test with the null hypothesis equal to 0. Main regression with standard errors clustered at the state-level. $^b$ Refers to the inverse hyperbolic sine transformation. $^c$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. I also include the lag of the outcome, i.e. logged homicides per capita as control. $^d$ Estimates weighted by each cohort's relative share of the sample following \citet{abraham_sun_2020}.} $^e$ Measure taken from reasons to modify police structure questionnaire from the National Census of Municipal Governments from INEGI.} \\
tex \end{tabular}
tex } 
tex \end{table} 
texdoc close


