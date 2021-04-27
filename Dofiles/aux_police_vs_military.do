*Regressions: Reform & Sec. Cooperation Agreements, INTERACTIONS
*Paper: Reelection Backfire
*Author: Rafael Ch (rafael.ch@nyu.edu)
*========================================================================
/*

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

	global controls_mean logdefuncionespc_mean ap4_2_3_mean ap4_2_11_mean ap4_12b_mean ap5_4_2_b_mean ///
	ap5_4_8_b_mean alignment_executive_strong_mean alignment_governor_strong_mean winning_margin_mean ///
	winning_margin_governor_mean pan_mayor2_mean pri_mayor2_mean  acuerdo_mean
	 
	global controls2_mean logdefuncionespc_mean ap4_2_3_mean ap4_2_11_mean ap4_12b_mean ap5_4_2_b_mean ///
	ap5_4_8_b_mean alignment_executive_strong_mean alignment_governor_strong_mean winning_margin_mean ///
	winning_margin_governor_mean pan_mayor2_mean pri_mayor2_mean  acuerdo2_mean


	global controls $controls_mean
	global controls2 $controls2_mean

	/*global narco ap4_2_3_lag_8 ap4_2_3_lag_7 ap4_2_3_lag_6 ap4_2_3_lag_5 ap4_2_3_lag_4 ap4_2_3_lag_3 ap4_2_3_lag_2
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
	
	global incumbent_adv inc_lag_8 inc_lag_7 inc_lag_6 inc_lag_5 inc_lag_4 inc_lag_3 inc_lag_2
	global incumbent_adv2 inc_party_runsfor1_lag_8 inc_party_runsfor1_lag_7 inc_party_runsfor1_lag_6 inc_party_runsfor1_lag_5 inc_party_runsfor1_lag_4 inc_party_runsfor1_lag_3 inc_party_runsfor1_lag_2
	global incumbent_adv3 inc_party_won_lag_8 inc_party_won_lag_7 inc_party_won_lag_6 inc_party_won_lag_5 inc_party_won_lag_4 inc_party_won_lag_3 inc_party_won_lag_2
	global num_parties numparties_eff_lag_8 numparties_eff_lag_7 numparties_eff_lag_6 numparties_eff_lag_5 numparties_eff_lag_4 numparties_eff_lag_3 numparties_eff_lag_2
	global num_parties2 numparties_eff_molinar_lag_8 numparties_eff_molinar_lag_7 numparties_eff_molinar_lag_6 numparties_eff_molinar_lag_5 numparties_eff_molinar_lag_4 numparties_eff_molinar_lag_3 numparties_eff_molinar_lag_2
	
	global incumbency $incumbent_adv $num_parties


	global controls winning_margin_governor_pre governor_alignment_pre logdefuncionespc_prereform $citizens2
	*/


*========================================================================
*SET MATSIZE
set matsize 11000 
sort inegi year

*========================================================================
*1) TWFE model - Centralization on Effort
sort inegi year

est clear
*foreach outcome in logdetenidospc logdetenidos_2pc logheroina_kg logheroina_kg_2 logmetanfetamina_kg logmetanfetamina_kg_2 loglaboratorio loglaboratorio_2{
foreach outcome in  logdetenidos_2pc logheroina_kg_2  logmetanfetamina_kg_2  loglaboratorio_2{
foreach treatment in  acuerdo acuerdo2{
eststo: quietly xi: areg `outcome' `treatment'  i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}

esttab est*, keep(acuerdo acuerdo2) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/centralization_effort.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "State Cluster S.E.")) ///
keep(acuerdo acuerdo2) ///
mgroups("log(detained pc)" "log(heroine kg)" "log(methanphetamine kg)" "log(laboratories destroyed)", ///
pattern(1 0 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(acuerdo "Agreement A" acuerdo2 "Agreement B") ///
collabels(none) nonotes booktabs nomtitles  nolines


*========================================================================
*2) TWFE model - Reform on Effort
sort inegi year

est clear
*foreach outcome in logdetenidospc logdetenidos_2pc logheroina_kg logheroina_kg_2 logmetanfetamina_kg logmetanfetamina_kg_2 loglaboratorio loglaboratorio_2{
foreach outcome in  logdetenidos_2pc loglaboratorio_2 logmetanfetamina_kg_2{
foreach treatment in reform L.reform L2.reform{
eststo:  xi: areg `outcome' `treatment'  i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}

esttab est*, keep(reform L.reform L2.reform) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/reform_effort.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N r2 controls munfe yearfe clustermun, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "State Cluster S.E.")) ///
keep(reform L.reform L2.reform) ///
mgroups("log(detained pc)" "log(laboratories destroyed)" "log(methamphetamine errad. (kg))", ///
pattern(1 0 0 1 0 0 1 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(reform "Term Limit Reform" L.reform "Term Limit Reform (t-1)" L2.reform "Term Limit Reform (t-2)") ///
collabels(none) nonotes booktabs nomtitles  nolines

*========================================================================
*2) Chaisemartin and D'Haultfoeuille correction

*Graph:
foreach outcome in logdetenidos_2pc  loglaboratorio_2{
did_multiplegt `outcome' group year reform, breps(100) controls($controls)  seed(5675) ///
cluster(estado) robust_dynamic dynamic(2) placebo(2) longdiff_placebo 
graph export "../Figures/chaisemartin_`outcome'.png", as(png) replace
}

************
*A) Acuerdo:
************
foreach outcome in logdetenidos_2pc {
did_multiplegt `outcome' group year reform, breps(100) controls($controls)   placebo(5) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) average_effect ///
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

	

************
*B) Acuerdo2:
************
foreach outcome in loglaboratorio_2 {
did_multiplegt `outcome' group year reform, breps(100) controls($controls2) placebo(5) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
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

*Table
	
texdoc init  "../Tables/chaisemarting_estimates_effort.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on effort of security forces}
tex \label{tab:chaisemartin_effort}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{Local Police = log(detentions pc)} & \multicolumn{1}{c}{Military = log(laboratories errad.)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
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
tex Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Secondary version of security cooperation agreements. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: Reform decreases the likelihood of signing security cooperation agreements.







