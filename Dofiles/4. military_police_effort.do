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
*0) Naive event study

est clear
global variables logdetenidospc logdetenidos_2pc logheroina_kg logheroina_kg_2 logmetanfetamina_kg logmetanfetamina_kg_2 logcocaina_kg logcocaina_kg_2 logamapola_kghec logamapola_kghec_2 loglaboratorio loglaboratorio_2 logdrugs logdrugs_2
foreach i in $variables{
eststo: qui areg `i' $lagsleads $controls i.year, a(inegi) cluster(estado)
}
esttab est*, keep($lagsleads) t(%9.3f)  star(* 0.1 ** 0.05 *** 0.01)


*========================================================================

*1) Corrected-TWFE (Chaisemartin and D'Haultfoeuille, forthcoming AER) -Effect of Reform on Homicides

/*Graph:
foreach outcome in logdefuncionespc  ihs_defuncionespc {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) seed(5675) ///
cluster(estado) robust_dynamic dynamic(3) p2lacebo(4) longdiff_placebo 
graph export "../Figures/chaisemartin_`outcome'.png", as(png) replace
}
*/

************
*A) detenidos:
************
foreach outcome in logdetenidospc {
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
*B) 
************
foreach outcome in logdetenidos_2pc {
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVdetenidos.tex", replace force
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


*MESSAGE: detentions fall with the total number _2


************
*A) heroine:
************
foreach outcome in logheroina_kg {
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
*B) 
************
foreach outcome in logheroina_kg_2 {
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVheroine.tex", replace force
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


*MESSAGE: 


************
*A) meta:
************
foreach outcome in logmetanfetamina_kg {
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
*B) 
************
foreach outcome in logmetanfetamina_kg_2 {
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVmeta.tex", replace force
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


*MESSAGE: 



************
*A) laboratories:
************
foreach outcome in loglaboratorio {
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
*B) 
************
foreach outcome in loglaboratorio_2 {
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVlab.tex", replace force
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


*MESSAGE: 



************
*A) cocaine:
************
foreach outcome in logcocaina_kg {
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
*B) 
************
foreach outcome in logcocaina_kg_2 {
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVcocaine.tex", replace force
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


*MESSAGE: 




************
*A) Poppy:
************
foreach outcome in logamapola_kghec {
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
*B) 
************
foreach outcome in logamapola_kghec_2 {
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVpoppy.tex", replace force
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


*MESSAGE: 




************
*A) Drugs:
************
foreach outcome in logdrugs {
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
*B) 
************
foreach outcome in logdrugs_2 {
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVdrugsoverall.tex", replace force
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


*MESSAGE: 




*========================================================================

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

