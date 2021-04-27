*Regressions: Reform & Sec. Cooperation Agreements, MOTIVATIONS BEHIND AGREEMENTS
*Paper: Reelection Backfire
*Author: Rafael Ch (rafael.ch@nyu.edu)
*========================================================================
/*
Main Messages

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
use "../../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*========================================================================
*SET PANEL
xtset inegi year 

*========================================================================
*VARIABLE CLEANING
foreach i in reform{
gen `i'_pre=L.`i'
}

foreach i in winning_margin_governor governor_alignment logdefuncionespc{
gen `i'_pre=L2.`i'
}


*set controls 
*global controls winning_margin_governor governor_alignment logdefuncionespc
// create controls prereform
bysort estado inegi: gen logdefuncionespc_prereform=logdefuncionespc if year==2013
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n-1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.


*generate groups 
gen group=.
replace group=1 if adopt_year==.
replace group=2 if adopt_year==2015
replace group=3 if adopt_year==2016
replace group=4 if adopt_year==2017
replace group=5 if adopt_year==2018

*NOTE: similar results using pre-treatment controls

*create alignment variables and clean them 
order firstword win_governor alignment_executive_strong alignment_governor_strong double_alignment
*ssc install fillmissing, replace


*create alignment variables:
gen governor_alignment2=0 if firstword==win_governor & firstword!="pri" & firstword!="" // aligned but not the pri
order firstword win_governor governor_alignment2 alignment_executive_strong alignment_governor_strong double_alignment
replace governor_alignment2=1 if firstword==win_governor & firstword=="pri" // aligned and pri

replace governor_alignment2=2 if firstword!=win_governor // not aligned

*fill:
sort inegi year
foreach i in  governor_alignment2{
fillmissing `i', with(previous)
}
*this generates missing values of places that were not aligned. But that's exactly what I want to have aside. 
*Here PRI is acting as the way to measure higher clientelistic transfers, rather than credit claiming. 

*generate alignment with PRI president
gen president_alignment=0
replace president_alignment=1 if firstword=="pri" & year>2012 & year<2019



sort inegi year
foreach i in  alignment_executive_strong alignment_governor_strong double_alignment{
*bysort inegi year: replace `i'=`i'[_n-1] if `i'==.
fillmissing `i', with(previous)
}

**need to correct governor_alignment varaible: 
replace governor_alignment=. if win_governor==""  
sort inegi year
foreach i in  governor_alignment{
fillmissing `i', with(previous)
}

rename governor_alignment governor_pri

gen president_pri=0
replace president_pri=1 if year>2012 & year<2019


*========================================================================
*SET GLOBALS AND OTHER VARIABLES
global controls winning_margin_governor  governor_pri governor_alignment2 logdefuncionespc
global controls_pre winning_margin_governor_pre governor_alignment_pre logdefuncionespc_pre
global controls_lagged L.winning_margin_governor  L.governor_pri L.governor_alignment2 L.logdefuncionespc

*========================================================================
*SET MATSIZE
set matsize 11000 
sort inegi year

*========================================================================
*MOTIVO ACUERDO
sort inegi year

*xi: areg summotivos reform i.year, a(inegi) vce(cluster estado)
global motivos motivo_noaplica motivo_reformacons motivo_reformaley motivo_faltarecursos motivo_profesioalizacion motivo_coordinacion motivo_crimen motivo_otros

est clear
foreach outcome in $motivos {
foreach treatment in L.reform{
eststo:  xi: areg `outcome' `treatment' $controls_lagged i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}

esttab est*, keep(L1.reform ) t star(* 0.1 ** 0.05 *** 0.01)


esttab using "../Tables/twfe_reform_coop_agreements.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E.")) ///
keep(reform L.reform L2.reform L3.reform) ///
mgroups("DV: Security Cooperation Agreement A" "DV: Security Cooperation Agreement B", ///
pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(reform "Term Limit Reform"  L.reform "Lag" L2.reform "Lag 2" L3.reform "Lag 3") ///
collabels(none) nonotes booktabs nomtitles  nolines
	
*MESSAGE: seems that when the motivation is reforma de ley there is a decrease in likelihood.
*MESSAGE2: with L. we see a negative effect with reforma de ley and coordinacion. So they sign them less to coordinate and there are less changes in law, so no top down enforcement.


*========================================================================
***3) Corrected-TWFE (Chaisemartin and D'Haultfoeuille, forthcoming AER) 
global controls winning_margin_governor_pre governor_alignment_pre logdefuncionespc_prereform

*Graph:
foreach outcome in motivo_reformaley motivo_coordinacion {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls) seed(5675) ///
cluster(estado) robust_dynamic dynamic(3) placebo(4) longdiff_placebo 
graph export "../Figures/chaisemartin_`outcome'.png", as(png) replace
}

************
*A) motivo 1:
************
foreach outcome in motivo_reformaley {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls) placebo(2) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
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
*B) motivo 2:
************
foreach outcome in motivo_coordinacion {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls) placebo(2) seed(5675) ///
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVmotivations.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{Security Cooperation Agreement A} & \multicolumn{1}{c}{Security Cooperation Agreement B$^{a}$} \\
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
tex State Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Secondary version of security cooperation agreements. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: 
