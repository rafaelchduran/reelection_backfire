*Incumbency advantage
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
	ap5_4_8_b_mean alignment_executive_strong_mean alignment_governor_strong_mean  ///
	winning_margin_governor_mean pan_mayor2_mean pri_mayor2_mean 


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

	global controls logdefuncionespc_mean_y_1 logdefuncionespc_mean_y_2 logdefuncionespc_mean_y_3 logdefuncionespc_mean_y_4 logdefuncionespc_mean_y_5 logdefuncionespc_mean_y_6 logdefuncionespc_mean_y_7 logdefuncionespc_mean_y_8 ///
	align_gov_y_1 align_gov_y_2 align_gov_y_3 align_gov_y_4 align_gov_y_5 align_gov_y_6 align_gov_y_7 align_gov_y_8 ///
	margin_gov_y_1 margin_gov_y_2 margin_gov_y_3 margin_gov_y_4 margin_gov_y_5 margin_gov_y_6 margin_gov_y_7 margin_gov_y_8 
	*hayCarteles_y_1 hayCarteles_y_2 hayCarteles_y_3 hayCarteles_y_4 hayCarteles_y_5 hayCarteles_y_6 hayCarteles_y_7 hayCarteles_y_8 ///
	*pri_mayor2_y_1 pri_mayor2_y_2 pri_mayor2_y_3 pri_mayor2_y_4 pri_mayor2_y_5 pri_mayor2_y_6 pri_mayor2_y_7 pri_mayor2_y_8 ///
	*pan_mayor2_y_1 pan_mayor2_y_2 pan_mayor2_y_3 pan_mayor2_y_4 pan_mayor2_y_5 pan_mayor2_y_6 pan_mayor2_y_7 pan_mayor2_y_8 
	*winning_margin_mean_y_1 winning_margin_mean_y_2 winning_margin_mean_y_3 winning_margin_mean_y_4 winning_margin_mean_y_5 winning_margin_mean_y_6 winning_margin_mean_y_7 winning_margin_mean_y_8 
	*DOESN'T WORK WITH THIS [Nickel bias]: acuerdo3_mean_y_1 acuerdo3_mean_y_2 acuerdo3_mean_y_3 acuerdo3_mean_y_4 acuerdo3_mean_y_5 acuerdo3_mean_y_6 acuerdo3_mean_y_7 acuerdo3_mean_y_8
	*WORKS WITH THIS BUT POTENTIAL NICKEL BIAS: acuerdo_mean_y_1 acuerdo_mean_y_2 acuerdo_mean_y_3 acuerdo_mean_y_4 acuerdo_mean_y_5 acuerdo_mean_y_6 acuerdo_mean_y_7 acuerdo_mean_y_8
	*DOESN'T WORK WITH THIS [its the mechanism]: ap4_2_3_mean_y_1 ap4_2_3_mean_y_2 ap4_2_3_mean_y_3 ap4_2_3_mean_y_4 ap4_2_3_mean_y_5 ap4_2_3_mean_y_6 ap4_2_3_mean_y_7 ap4_2_3_mean_y_8 



*========================================================================
*SET MATSIZE
set matsize 11000 
sort inegi year
*========================================================================
*CONSTRUCT QUALITY MEASURE

*outcomes: incumbent_yesterday_w_tomorrow2 inc_party_runsfor1 numparties_eff numparties_eff_molinar inc_party_won
est clear
foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 1 2{
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


*incumbent_yesterday_w_tomorrow2

foreach i in incumbent_yesterday_w_tomorrow2{
rdbwselect  `i' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

areg  `i'  $saturated pol1 $interacted_pol1 $controls  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
*keep if e(sample)==1
}

foreach i in pol1 pol2{
foreach var in  reform{
gen `var'_`i'=`var'*`i'
}
}

*========================================================================
*Naive event study
preserve
est clear
global variables incumbent_yesterday_w_tomorrow2
foreach i in $variables{
rdbwselect  `i' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)/4
eststo: qui reghdfe `i' $lagsleads  pol1 reform_pol1 i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} ///
, a(inegi) vce(cluster estado)
eststo: qui reghdfe `i' $lagsleads $controls pol1 reform_pol1 i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} ///
, a(inegi) vce(cluster estado)

rdbwselect  `i' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)/4
eststo: qui reghdfe `i' $lagsleads  pol2 reform_pol2 i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} ///
, a(inegi)  vce(cluster estado)
eststo: qui reghdfe `i' $lagsleads $controls pol2 reform_pol2 i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} ///
, a(inegi)  vce(cluster estado)
}

esttab est*, keep($lagsleads) t(%9.3f)  star(* 0.1 ** 0.05 *** 0.01)
restore

*========================================================================
***1) Naive OLS
sort inegi year

est clear
foreach outcome in incumbent_yesterday_w_tomorrow2 {
foreach treatment in reform{
eststo: quietly xi: areg `outcome' `treatment' $controls    i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg f.`outcome' `treatment' $controls    i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg f2.`outcome' `treatment' $controls   i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg f3.`outcome' `treatment' $controls   i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}



esttab est*, keep(reform) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/naive_reform_quality.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "State Cluster S.E.")) ///
keep(reform ) ///
mgroups("Agreement A in t" "in t+1" "in t+2" "in t+3" , ///
pattern(1 1 1 1 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(reform "Term Limit Reform") ///
collabels(none) nonotes booktabs nomtitles  nolines



*========================================================================
*1) Chaisemartin and D'Haultfoeuille correction

************
*A) LINEAR:
************
/*foreach i in incumbent_yesterday_w_tomorrow2{
rdbwselect  `i' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

}
preserve
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach outcome in incumbent_quality {
did_multiplegt `outcome' group year reform, breps(100) controls(pol1 reform_pol1)   placebo(2) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) average_effect ///
save_results("../../Data/chaisemartin_`outcome'.dta")
}
*/

*Graph:
foreach i in incumbent_yesterday_w_tomorrow2{
rdbwselect  `i' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)/4
}
preserve
foreach outcome in  incumbent_yesterday_w_tomorrow2{
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
did_multiplegt `outcome' group year reform, breps(100) controls($controls2 pol1 reform_pol1)  seed(5675) ///
cluster(estado) robust_dynamic dynamic(2) placebo(2) longdiff_placebo
graph export "../Figures/chaisemartin_`outcome'_pol1.png", as(png) replace
}


ereturn list
return list

*save results to globals:
foreach i in 0 1 2 {
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
foreach i in 1 2 {
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
*B) QUADRATIC :
************
/*foreach i in incumbent_yesterday_w_tomorrow2{
rdbwselect  `i' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

}

preserve
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach outcome in incumbent_quality {
did_multiplegt `outcome' group year reform, breps(100) controls(pol2 reform_pol2)   placebo(2) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) average_effect ///
save_results("../../Data/chaisemartin_`outcome'.dta")
}
*/
*Graph:
foreach i in incumbent_yesterday_w_tomorrow2{
rdbwselect  `i' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
}

preserve
foreach outcome in  incumbent_yesterday_w_tomorrow2{
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
did_multiplegt `outcome' group year reform, breps(100) controls( pol2 reform_pol2)  seed(5675) ///
cluster(estado) robust_dynamic dynamic(3) placebo(2) longdiff_placebo
graph export "../Figures/chaisemartin_`outcome'_pol1.png", as(png) replace
}



ereturn list
return list

*save results to globals:
foreach i in 0 1 2 {
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
foreach i in 1 2 {
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVincumbencyadvantage_pol1&2.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of Term Limit Reform on Incumbency Advantage}
tex \label{tab:chaisemartin}
tex \scalebox{0.65}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable: incumbent party at t-1 won at t dummy}\\
tex & \multicolumn{1}{c}{linear polynomial} & \multicolumn{1}{c}{quadratic polynomial} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex 3 elections before &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) \\
tex 2 elections before &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) \\
tex t=0 reform &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) \\
tex 1 election after &         $ ${beta_t_2}^{${est_t_2}} $ &       $ ${beta2_t_2}^{${est2_t_2}} $ \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) \\

tex \addlinespace
tex Controls   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: State clustered standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%. Forcing variable=winning margin. Optimal bandwidth from \textcolor{blue}{Calonico et al. 2014}.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

*========================================================================
***1) Naive
sort inegi year


est clear
foreach outcome in incumbent_yesterday_w_tomorrow2 {
foreach treatment in reform{
eststo: quietly xi: areg `outcome' `treatment' $controls    i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg f.`outcome' `treatment' $controls    i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg f2.`outcome' `treatment' $controls   i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg f3.`outcome' `treatment' $controls   i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}



esttab est*, keep(reform) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/naive_reform_incumbencyadv.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "State Cluster S.E.")) ///
keep(reform ) ///
mgroups("Agreement A in t" "in t+1" "in t+2" "in t+3" , ///
pattern(1 1 1 1 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(reform "Term Limit Reform") ///
collabels(none) nonotes booktabs nomtitles  nolines




*========================================================================
*1) Chaisemartin and D'Haultfoeuille correction

*Graph:
preserve
foreach outcome in  incumbent_yesterday_w_tomorrow2{
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
did_multiplegt `outcome' group year reform, breps(100) controls($controls pol1 reform_pol1)  seed(5675) ///
cluster(estado) robust_dynamic dynamic(2) placebo(2) longdiff_placebo 
graph export "../Figures/chaisemartin_`outcome'_pol1.png", as(png) replace
}
restore

*Graph:
preserve
foreach outcome in  incumbent_yesterday_w_tomorrow2{
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
did_multiplegt `outcome' group year reform, breps(100) controls($controls pol2 reform_pol2)  seed(5675) ///
cluster(estado) robust_dynamic dynamic(2) placebo(2) longdiff_placebo 
graph export "../Figures/chaisemartin_`outcome'_pol2.png", as(png) replace
}
restore


*========================================================================
***3) INTERACTION WITH BEING FIRST OR SECOND TERM
est clear
*global interaction firstterm secondterm second_to_firsterm // 
foreach outcome in incumbent_yesterday_w_tomorrow2{
*foreach interaction in $interaction{
foreach treatment in L.reform{
*1)
eststo:  xi: areg `outcome' c.`treatment'##c.firstterm pol1 reform_firstterm_pol1  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.firstterm] +_b[firstterm] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.firstterm] +_b[firstterm]
global total_interaction_first: di %5.4f _b[`treatment']+_b[c.`treatment'#c.firstterm]  +_b[firstterm]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.firstterm] +_b[firstterm]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'
estadd local ci "[-.271, -0.035]" // for reform treatment
estadd local ci_total "[-0.087, 0.100]" // sum of all coefficients

*2) 
eststo:  xi: areg `outcome' c.`treatment'##c.secondterm pol1 reform_secondterm_pol1  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.secondterm] +_b[secondterm] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.secondterm] +_b[secondterm]
global total_interaction_second: di %5.4f _b[`treatment']+_b[c.`treatment'#c.secondterm] +_b[secondterm]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.secondterm] +_b[secondterm]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'
estadd local ci "[-.501, -0.286]" // for reform treatment
estadd local ci_total "[-.123, 0.0115]" // sum of all coefficients

/*3) 
eststo: quietly xi: areg `outcome' c.`treatment'##c.second_to_firsterm pol1 reform_second_to_firsterm_pol1  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#c.second_to_firsterm] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#c.second_to_firsterm]
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#c.second_to_firsterm]
global se_`i': di %5.4f r(se)
estadd local se $se_`i'


test ${total_interaction_second} - ${total_interaction_first}=0 
*/
}
}

*esttab est*, keep(reform c.reform#c.firstterm c.reform#c.secondterm  c.reform#c.second_to_firsterm ) t star(* 0.1 ** 0.05 *** 0.01)
esttab est*, keep(reform c.reform#c.firstterm c.reform#c.secondterm firstterm secondterm) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/incumbencyadv.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue ci, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)" "CI")) ///
keep(reform firstterm secondterm c.reform#c.firstterm c.reform#c.secondterm  ) ///
mgroups("DV: Agreement A" "DV: Agreement B", ///
pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(reform "Term Limit Reform"  ///
c.reform#c.firstterm "Term Limit Reform (t)*First Term Mayor" ///
c.reform#c.secondterm "Term Limit Reform (t)*Second Term Mayor" ///
firstterm "First Term Mayor" ///
secondterm "Second Term Mayor") ///
collabels(none) nonotes booktabs nomtitles  nolines


esttab est*, keep(L.reform cL.reform#c.firstterm cL.reform#c.secondterm  firstterm secondterm) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/L.reform_incumbencyadv.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue ci, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)" "CI")) ///
keep(L.reform firstterm secondterm cL.reform#c.firstterm cL.reform#c.secondterm  ) ///
mgroups("DV: Agreement A" "DV: Agreement B", ///
pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L.reform "Term Limit Reform"  ///
cL.reform#c.firstterm "Term Limit Reform (t)*First Term Mayor" ///
cL.reform#c.secondterm "Term Limit Reform (t)*Second Term Mayor" ///
firstterm "First Term Mayor" ///
secondterm "Second Term Mayor") ///
collabels(none) nonotes booktabs nomtitles  nolines

*=====================================================================================================
**Chaisemarting splitting

************
*A) Term limited vs ==0:
***********
preserve
foreach outcome in incumbent_yesterday_w_tomorrow2 {
keep if firstterm!=.
*keep if second_to_firsterm!=1
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
foreach outcome in incumbent_yesterday_w_tomorrow2 {
keep if secondterm!=.
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVmandounico_byfirst&secondterms.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin_alignmentpri}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{Term limited vs. First Term} & \multicolumn{1}{c}{Term Limited vs. Second Term$^{a}$} \\
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

/*CONFIDENCE INTERVALS:

*FIRST TERM
             |  Estimate         SE      LB CI      UB CI          N  Switchers 
-------------+------------------------------------------------------------------
    Effect_0 |   .000728   .0195468  -.0375837   .0390398       3933       1112 
    Effect_1 |  .0185761   .0085508   .0018166   .0353357       2540        814 
    Effect_2 |   .010922   .0338013  -.0553287   .0771726       1833        802 


*SECOND TERM:
             |  Estimate         SE      LB CI      UB CI          N  Switchers 
-------------+------------------------------------------------------------------
    Effect_0 |   .031189   .0175688  -.0032458   .0656238       3010        526 
    Effect_1 |  .0320859   .0183167  -.0038147   .0679866       2022        514 
    Effect_2 | -.0065139   .0218907  -.0494197   .0363918       1420        488 



*/


*=====================================================================================================
**Chaisemarting splitting with RDD

************
*A) Term limited vs ==0:
***********
preserve
foreach outcome in incumbent_yesterday_w_tomorrow2 {
keep if firstterm!=.
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
*keep if second_to_firsterm!=1
did_multiplegt `outcome' group year reform, breps(100) controls($controls pol1 reform_firstterm_pol1) placebo(4) seed(5675) ///
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
foreach outcome in incumbent_yesterday_w_tomorrow2 {
keep if secondterm!=.
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
did_multiplegt `outcome' group year reform, breps(100) controls($controls pol1 reform_secondterm_pol1) placebo(4) seed(5675) ///
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVmandounico_byfirst&secondterms_RDD.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin_alignmentpri}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{Term limited vs. First Term} & \multicolumn{1}{c}{Term Limited vs. Second Term$^{a}$} \\
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

/*CONFIDENCE INTERVALS:

*FIRST TERM
             |  Estimate         SE      LB CI      UB CI          N  Switchers 
-------------+------------------------------------------------------------------
    Effect_0 | -.0002478   .0256215  -.0504659   .0499704       2247        590 
    Effect_1 |  .0147767    .010905  -.0065971   .0361506       1414        434 
    Effect_2 |  .0025735   .0362999  -.0685743   .0737213        950        427 
    Effect_3 | -.0661846          0  -.0661846  -.0661846        504        337 


	*SECOND TERM
             |  Estimate         SE      LB CI      UB CI          N  Switchers 
-------------+------------------------------------------------------------------
    Effect_0 |   .035957    .014386   .0077604   .0641535       1679        271 
    Effect_1 |  .0332059   .0190885  -.0042076   .0706193       1083        265 
    Effect_2 | -.0274964   .0430154  -.1118065   .0568137        699        251 
    Effect_3 | -.0380936   1.81e-09  -.0380936  -.0380935        306        139 


*/


*=====================================================================================================
**Chaisemarting splitting with RDD

************
*A) POLYNOMIAL 1
***********
preserve
foreach outcome in incumbent_yesterday_w_tomorrow2 {
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
*keep if second_to_firsterm!=1
did_multiplegt `outcome' group year reform, breps(100) controls($controls pol1 reform_pol1) placebo(4) seed(5675) ///
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
*B) POLYNOMIAL 2
************
preserve
foreach outcome in incumbent_yesterday_w_tomorrow2 {
*keep if secondterm!=.
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
did_multiplegt `outcome' group year reform, breps(100) controls($controls pol2 reform_pol2) placebo(4) seed(5675) ///
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVincumbencyadv_pol1&2.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin_alignmentpri}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{Polynomial 1} & \multicolumn{1}{c}{Polynomial 2$^{a}$} \\
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

