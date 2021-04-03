*Regressions: HET EFFECTS BY SECURITY PREFERENCES
*Paper: Reelection Backfire
*Author: Rafael Ch (rafael.ch@nyu.edu)
*========================================================================
/*
ENVIPE variables: state-year level
ap4_2_3  = tema que más me preocupa - narcotráfico 
ap4_2_5  = tema que más me preocupa - inseguridad 
ap4_2_11 = tema que más me preocupa - falta castigo a delincuentes 
ap4_12b = ¿Cuánto gastaron en total por esas medidas durante 2018? 
ap5_4_2_b  = ¿Cuánta confianza le inspira la (el) (AUTORIDAD)? policia preventiva municipal
ap5_4_8_b = ¿Cuánta confianza le inspira la (el) (AUTORIDAD)? ejercito
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

	global controls2_mean logdefuncionespc_mean  ///
	 alignment_executive_strong_mean alignment_governor_strong_mean winning_margin_mean ///
	winning_margin_governor_mean pan_mayor2_mean pri_mayor2_mean  acuerdo2_mean

	global controls $controls_mean
	global controls $controls2_mean

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

*========================================================================
*SET MATSIZE
set matsize 11000 
sort inegi year

*========================================================================
***1) INTERACTION WITH DTO PRESENCE: 
est clear
global interaction ap4_2_3 ap4_2_5 ap4_2_11
foreach outcome in acuerdo acuerdo2{
foreach interaction in $interaction{
foreach treatment in L2.reform{
eststo: quietly xi: areg `outcome' c.`treatment'##c.`interaction' $controls i.year, a(inegi) vce(cluster estado)
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


esttab est*, keep(reform c.reform#c.ap4_2_3 c.reform#c.ap4_2_5  c.reform#c.ap4_2_11) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/DTO_presence.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)")) ///
keep(reform c.reform#c.ap4_2_3 c.reform#c.ap4_2_5  c.reform#c.ap4_2_11) ///
mgroups("DV: Agreement A" "DV: Agreement B", ///
pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(reform "Term Limit Reform"  ///
c.reform#c.ap4_2_3 "Term Limit Reform (t)*Worried about narcotraffick" ///
c.reform#c.ap4_2_5 "Term Limit Reform (t)*Worried about insecurity" ///
c.reform#c.ap4_2_11 "Term Limit Reform (t)*Punishment of criminals") ///
collabels(none) nonotes booktabs nomtitles  nolines


est clear
global interaction ap4_2_3 ap4_2_5 ap4_2_11
foreach outcome in acuerdo acuerdo2{
foreach interaction in $interaction{
foreach treatment in L.reform{
eststo: quietly xi: areg `outcome' c.`treatment'##cL.`interaction' $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#cL.`interaction'] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#cL.`interaction']
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#cL.`interaction']
global se_`i': di %5.4f r(se)
estadd local se $se_`i'
}
}
}



*esttab est*, keep(L.reform cL.reform#c.ap4_2_3 cL.reform#c.ap4_2_5  cL.reform#c.ap4_2_11) t star(* 0.1 ** 0.05 *** 0.01)
esttab est*, keep(L.reform cL.reform#cL.ap4_2_3 cL.reform#cL.ap4_2_5  cL.reform#cL.ap4_2_11) t star(* 0.1 ** 0.05 *** 0.01)

/*esttab using "../Tables/DTO_presence.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)")) ///
keep(L.reform cL.reform#c.ap4_2_3 cL.reform#c.ap4_2_5  cL.reform#c.ap4_2_11) ///
mgroups("DV: Agreement A" "DV: Agreement B", ///
pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L.reform "Term Limit Reform"  ///
cL.reform#c.ap4_2_3 "Term Limit Reform (t-1)*Worried about narcotraffick" ///
cL.reform#c.ap4_2_5 "Term Limit Reform (t-1)*Worried about insecurity" ///
cL.reform#c.ap4_2_11 "Term Limit Reform (t-1)*Punishment of criminals") ///
collabels(none) nonotes booktabs nomtitles  nolines
*/

esttab using "../Tables/DTO_presence.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)")) ///
keep(L.reform cL.reform#cL.ap4_2_3 cL.reform#cL.ap4_2_5  cL.reform#cL.ap4_2_11) ///
mgroups("DV: Agreement A" "DV: Agreement B", ///
pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L.reform "Term Limit Reform"  ///
cL.reform#cL.ap4_2_3 "Term Limit Reform (t-1)*Worried about narcotraffick" ///
cL.reform#cL.ap4_2_5 "Term Limit Reform (t-1)*Worried about insecurity" ///
cL.reform#cL.ap4_2_11 "Term Limit Reform (t-1)*Punishment of criminals") ///
collabels(none) nonotes booktabs nomtitles  nolines


*T-2
est clear
global interaction ap4_2_3 ap4_2_5 ap4_2_11
foreach outcome in acuerdo acuerdo2{
foreach interaction in $interaction{
foreach treatment in L2.reform{
eststo: quietly xi: areg `outcome' c.`treatment'##cL2.`interaction' $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

***this is the total interaction effect:
test  _b[`treatment']+_b[c.`treatment'#cL2.`interaction'] = 0
global total_interaction: di %5.4f _b[`treatment']+_b[c.`treatment'#cL2.`interaction']
estadd local tot_int $total_interaction
global pvalue: di %5.4f r(p)
estadd local pvalue $pvalue
lincom _b[`treatment']+_b[c.`treatment'#cL2.`interaction']
global se_`i': di %5.4f r(se)
estadd local se $se_`i'
}
}
}


esttab est*, keep(L2.reform cL2.reform#cL2.ap4_2_3 cL2.reform#cL2.ap4_2_5  cL2.reform#cL2.ap4_2_11) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/DTO_presence.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)")) ///
keep(L2.reform cL2.reform#cL2.ap4_2_3 cL2.reform#cL2.ap4_2_5  cL2.reform#cL2.ap4_2_11) ///
mgroups("DV: Agreement A" "DV: Agreement B", ///
pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L2.reform "Term Limit Reform"  ///
cL2.reform#cL2.ap4_2_3 "Term Limit Reform (t-2)*Worried about narcotraffick" ///
cL2.reform#cL2.ap4_2_5 "Term Limit Reform (t-2)*Worried about insecurity" ///
cL2.reform#cL2.ap4_2_11 "Term Limit Reform (t-2)*Punishment of criminals") ///
collabels(none) nonotes booktabs nomtitles  nolines

*MESSAGE: there are differential effects if there is cartel presence.

*========================================================================
**2) Robustness: CONCERN ABOUT NARCO. For Chaisemartin I can only do this by subsetting the data

************
*A) NARCO below mean:
***********
global split_variable ap4_2_3_mean

preserve
foreach outcome in acuerdo {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

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
*B) NARCO above mean:
************
preserve
foreach outcome in acuerdo {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

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

************
*A) INSECURITY below mean:
***********
global split_variable ap4_2_5_mean

preserve
foreach outcome in acuerdo {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados3_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta3_t_`i': di %5.3f e(effect_`i')
	glo se3_t_`i': di %5.3f e(se_effect_`i')
	glo t3_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval3_t_`i': di 2*ttail(${grados3_t_`i'},abs(${t3_t_`i'})) 
	glo est3_t_`i'= "" 
			if (${pval3_t_`i'}<=0.1) global est3_t_`i' = "*"
			if (${pval3_t_`i'}<=0.05) global est3_t_`i' = "**"
			if (${pval3_t_`i'}<=0.01) global est3_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados3_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta3_t_`i': di %5.3f e(placebo_`i')
	glo pse3_t_`i': di %5.3f e(se_placebo_`i')
	glo pt3_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval3_t_`i': di 2*ttail(${pgrados3_t_`i'},abs(${pt3_t_`i'})) 
	glo pest3_t_`i'= "" 
			if (${ppval3_t_`i'}<=0.1) global pest3_t_`i' = "*"
			if (${ppval3_t_`i'}<=0.05) global pest3_t_`i' = "**"
			if (${ppval3_t_`i'}<=0.01) global pest3_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	


restore

************
*B) INSECURITY above mean:
************
preserve
foreach outcome in acuerdo {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados4_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta4_t_`i': di %5.3f e(effect_`i')
	glo se4_t_`i': di %5.3f e(se_effect_`i')
	glo t4_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval4_t_`i': di 2*ttail(${grados4_t_`i'},abs(${t4_t_`i'})) 
	glo est4_t_`i'= "" 
			if (${pval4_t_`i'}<=0.1) global est4_t_`i' = "*"
			if (${pval4_t_`i'}<=0.05) global est4_t_`i' = "**"
			if (${pval4_t_`i'}<=0.01) global est4_t_`i' = "***"
			*if (${pval4_t_`i'}=0) global est4_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados4_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta4_t_`i': di %5.3f e(placebo_`i')
	glo pse4_t_`i': di %5.3f e(se_placebo_`i')
	glo pt4_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval4_t_`i': di 2*ttail(${pgrados4_t_`i'},abs(${pt4_t_`i'})) 
	glo pest4_t_`i'= "" 
			if (${ppval4_t_`i'}<=0.1) global pest4_t_`i' = "*"
			if (${ppval4_t_`i'}<=0.05) global pest4_t_`i' = "**"
			if (${ppval4_t_`i'}<=0.01) global pest4_t_`i' = "***"
			*if (${ppval4_t_`i'}=0) global pest4_t_`i' = "***"
}


restore

************
*A) PUNISHMENT below mean:
***********
global split_variable ap4_2_11_mean

preserve
foreach outcome in acuerdo {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados5_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta5_t_`i': di %5.3f e(effect_`i')
	glo se5_t_`i': di %5.3f e(se_effect_`i')
	glo t5_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval5_t_`i': di 2*ttail(${grados5_t_`i'},abs(${t5_t_`i'})) 
	glo est5_t_`i'= "" 
			if (${pval5_t_`i'}<=0.1) global est5_t_`i' = "*"
			if (${pval5_t_`i'}<=0.05) global est5_t_`i' = "**"
			if (${pval5_t_`i'}<=0.01) global est5_t_`i' = "***"
			*if (${pval5_t_`i'}=0) global est5_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados5_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta5_t_`i': di %5.3f e(placebo_`i')
	glo pse5_t_`i': di %5.3f e(se_placebo_`i')
	glo pt5_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval5_t_`i': di 2*ttail(${pgrados5_t_`i'},abs(${pt5_t_`i'})) 
	glo pest5_t_`i'= "" 
			if (${ppval5_t_`i'}<=0.1) global pest5_t_`i' = "*"
			if (${ppval5_t_`i'}<=0.05) global pest5_t_`i' = "**"
			if (${ppval5_t_`i'}<=0.01) global pest5_t_`i' = "***"
			*if (${ppval5_t_`i'}=0) global pest5_t_`i' = "***"
}

	


restore

************
*B) PUNISHMENT above mean:
************
preserve
foreach outcome in acuerdo {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados6_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta6_t_`i': di %5.3f e(effect_`i')
	glo se6_t_`i': di %5.3f e(se_effect_`i')
	glo t6_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval6_t_`i': di 2*ttail(${grados6_t_`i'},abs(${t6_t_`i'})) 
	glo est6_t_`i'= "" 
			if (${pval6_t_`i'}<=0.1) global est6_t_`i' = "*"
			if (${pval6_t_`i'}<=0.05) global est6_t_`i' = "**"
			if (${pval6_t_`i'}<=0.01) global est6_t_`i' = "***"
			*if (${pval6_t_`i'}=0) global est6_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados6_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta6_t_`i': di %5.3f e(placebo_`i')
	glo pse6_t_`i': di %5.3f e(se_placebo_`i')
	glo pt6_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval6_t_`i': di 2*ttail(${pgrados6_t_`i'},abs(${pt6_t_`i'})) 
	glo pest6_t_`i'= "" 
			if (${ppval6_t_`i'}<=0.1) global pest6_t_`i' = "*"
			if (${ppval6_t_`i'}<=0.05) global pest6_t_`i' = "**"
			if (${ppval6_t_`i'}<=0.01) global pest6_t_`i' = "***"
			*if (${ppval6_t_`i'}=0) global pest6_t_`i' = "***"
}


restore

************
*A) PRIVATE SECURITY EXPENSES below mean:
***********
global split_variable ap4_12b_mean

preserve
foreach outcome in acuerdo {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados7_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta7_t_`i': di %5.3f e(effect_`i')
	glo se7_t_`i': di %5.3f e(se_effect_`i')
	glo t7_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval7_t_`i': di 2*ttail(${grados7_t_`i'},abs(${t7_t_`i'})) 
	glo est7_t_`i'= "" 
			if (${pval7_t_`i'}<=0.1) global est7_t_`i' = "*"
			if (${pval7_t_`i'}<=0.05) global est7_t_`i' = "**"
			if (${pval7_t_`i'}<=0.01) global est7_t_`i' = "***"
			*if (${pval7_t_`i'}=0) global est7_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados7_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta7_t_`i': di %5.3f e(placebo_`i')
	glo pse7_t_`i': di %5.3f e(se_placebo_`i')
	glo pt7_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval7_t_`i': di 2*ttail(${pgrados7_t_`i'},abs(${pt7_t_`i'})) 
	glo pest7_t_`i'= "" 
			if (${ppval7_t_`i'}<=0.1) global pest7_t_`i' = "*"
			if (${ppval7_t_`i'}<=0.05) global pest7_t_`i' = "**"
			if (${ppval7_t_`i'}<=0.01) global pest7_t_`i' = "***"
			*if (${ppval7_t_`i'}=0) global pest7_t_`i' = "***"
}

	


restore

************
*B) PRIVATE SECURITY EXPENSES above mean:
************
preserve
foreach outcome in acuerdo {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados8_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta8_t_`i': di %5.3f e(effect_`i')
	glo se8_t_`i': di %5.3f e(se_effect_`i')
	glo t8_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval8_t_`i': di 2*ttail(${grados8_t_`i'},abs(${t8_t_`i'})) 
	glo est8_t_`i'= "" 
			if (${pval8_t_`i'}<=0.1) global est8_t_`i' = "*"
			if (${pval8_t_`i'}<=0.05) global est8_t_`i' = "**"
			if (${pval8_t_`i'}<=0.01) global est8_t_`i' = "***"
			*if (${pval8_t_`i'}=0) global est8_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados8_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta8_t_`i': di %5.3f e(placebo_`i')
	glo pse8_t_`i': di %5.3f e(se_placebo_`i')
	glo pt8_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval8_t_`i': di 2*ttail(${pgrados8_t_`i'},abs(${pt8_t_`i'})) 
	glo pest8_t_`i'= "" 
			if (${ppval8_t_`i'}<=0.1) global pest8_t_`i' = "*"
			if (${ppval8_t_`i'}<=0.05) global pest8_t_`i' = "**"
			if (${ppval8_t_`i'}<=0.01) global pest8_t_`i' = "***"
			*if (${ppval8_t_`i'}=0) global pest8_t_`i' = "***"
}


restore



*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVmandounico_by_sec_preferences.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Centralization, by security preferences}
tex \label{tab:chaisemartin_sec_preferences}
tex \scalebox{0.7}{    
tex \begin{tabular}{lcccccccc}  
tex \hline \hline       
tex \\ \multicolumn{9}{l}{Dependent variable: Signing Security Cooperation Agreement dummy}\\
tex \addlinespace
tex & \multicolumn{2}{c}{Worried about Narco} & \multicolumn{2}{c}{Worried about insecurity} & \multicolumn{2}{c}{Worried about low punishment} & \multicolumn{2}{c}{Home security expenses}
tex & & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} \\ 
tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5} \cmidrule(lrr){6-7} \cmidrule(lrr){8-9}  \\
tex \addlinespace
tex t-5 &        $ ${pbeta_t_4}^{${pest_t_4}} $ &     $ ${pbeta2_t_4}^{${pest2_t_4}} $ &     $ ${pbeta3_t_4}^{${pest3_t_4}} $ &     $ ${pbeta4_t_4}^{${pest4_t_4}} $ &    $ ${pbeta5_t_4}^{${pest5_t_4}} $ &     $ ${pbeta6_t_4}^{${pest6_t_4}} $ &     $ ${pbeta7_t_4}^{${pest7_t_4}} $ &     $ ${pbeta8_t_4}^{${pest8_t_4}} $ \\
tex  & ($ ${pse_t_4}$) & ($ ${pse2_t_4} $) & ($ ${pse3_t_4}$) & ($ ${pse4_t_4} $)  & ($ ${pse5_t_4}$) & ($ ${pse6_t_4} $) & ($ ${pse7_t_4}$) & ($ ${pse8_t_4} $) \\
tex t-4 &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ &     $ ${pbeta3_t_3}^{${pest3_t_3}} $ &     $ ${pbeta4_t_3}^{${pest4_t_3}} $ &    $ ${pbeta5_t_3}^{${pest5_t_3}} $ &     $ ${pbeta6_t_3}^{${pest6_t_3}} $ &     $ ${pbeta7_t_3}^{${pest7_t_3}} $ &     $ ${pbeta8_t_3}^{${pest8_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) & ($ ${pse3_t_3}$) & ($ ${pse4_t_3} $)  & ($ ${pse5_t_3}$) & ($ ${pse6_t_3} $) & ($ ${pse7_t_3}$) & ($ ${pse8_t_3} $) \\
tex t-3 &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ &     $ ${pbeta3_t_2}^{${pest3_t_2}} $ &     $ ${pbeta4_t_2}^{${pest4_t_2}} $ &    $ ${pbeta5_t_2}^{${pest5_t_2}} $ &     $ ${pbeta6_t_2}^{${pest6_t_2}} $ &     $ ${pbeta7_t_2}^{${pest7_t_2}} $ &     $ ${pbeta8_t_2}^{${pest8_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) & ($ ${pse3_t_2}$) & ($ ${pse4_t_2} $)  & ($ ${pse5_t_2}$) & ($ ${pse6_t_2} $) & ($ ${pse7_t_2}$) & ($ ${pse8_t_2} $) \\
tex t-2 &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ &     $ ${pbeta3_t_1}^{${pest3_t_1}} $ &     $ ${pbeta4_t_1}^{${pest4_t_1}} $ &    $ ${pbeta5_t_1}^{${pest5_t_1}} $ &     $ ${pbeta6_t_1}^{${pest6_t_1}} $ &     $ ${pbeta7_t_1}^{${pest7_t_1}} $ &     $ ${pbeta8_t_1}^{${pest8_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) & ($ ${pse3_t_1}$) & ($ ${pse4_t_1} $)  & ($ ${pse5_t_1}$) & ($ ${pse6_t_1} $) & ($ ${pse7_t_1}$) & ($ ${pse8_t_1} $) \\
tex t=0 (Reform) &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ &     $ ${beta3_t_0}^{${est3_t_0}} $ &     $ ${beta4_t_0}^{${est4_t_0}} $ &    $ ${beta5_t_0}^{${est5_t_0}} $ &     $ ${beta6_t_0}^{${est6_t_0}} $ &     $ ${beta7_t_0}^{${est7_t_0}} $ &     $ ${beta8_t_0}^{${est8_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) & ($ ${se3_t_0}$) & ($ ${se4_t_0} $)  & ($ ${se5_t_0}$) & ($ ${se6_t_0} $) & ($ ${se7_t_0}$) & ($ ${se8_t_0} $) \\
tex t+1 &        $ ${beta_t_1}^{${est_t_1}} $ &     $ ${beta2_t_1}^{${est2_t_1}} $ &     $ ${beta3_t_1}^{${est3_t_1}} $ &     $ ${beta4_t_1}^{${est4_t_1}} $ &    $ ${beta5_t_1}^{${est5_t_1}} $ &     $ ${beta6_t_1}^{${est6_t_1}} $ &     $ ${beta7_t_1}^{${est7_t_1}} $ &     $ ${beta8_t_1}^{${est8_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) & ($ ${se3_t_1}$) & ($ ${se4_t_1} $)  & ($ ${se5_t_1}$) & ($ ${se6_t_1} $) & ($ ${se7_t_1}$) & ($ ${se8_t_1} $) \\
tex t+2 &        $ ${beta_t_2}^{${est_t_2}} $ &     $ ${beta2_t_2}^{${est2_t_2}} $ &     $ ${beta3_t_2}^{${est3_t_2}} $ &     $ ${beta4_t_2}^{${est4_t_2}} $ &    $ ${beta5_t_2}^{${est5_t_2}} $ &     $ ${beta6_t_2}^{${est6_t_2}} $ &     $ ${beta7_t_2}^{${est7_t_2}} $ &     $ ${beta8_t_2}^{${est8_t_2}} $ \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) & ($ ${se3_t_2}$) & ($ ${se4_t_2} $)  & ($ ${se5_t_2}$) & ($ ${se6_t_2} $) & ($ ${se7_t_2}$) & ($ ${se8_t_2} $) \\


tex \addlinespace
tex Controls$^a$   &    \checkmark      &   \checkmark  &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark   \\
tex 95\% Diff. in CI  &    \checkmark      &   \checkmark  &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark   \\
tex \hline \hline      
tex \multicolumn{9}{p{1.5\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Pre-treatment controls include the following: governor winning margin, mayor winning margin, governor and president party alignment dummies, and logged homicides per capita.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

*MESSAGE:  larger effect when citizens hold stronger anti DTO views




*MESSAGE:  larger effect with cartel presence.

*========================================================================
***3) Robustness: Saturated Model

*Set globals: 
	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2  logdefuncionespc_lag_8 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3	 logdefuncionespc_lag_2
	global outcome acuerdo
	global outcome2 acuerdo2
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

************
***A: acuerdo w/o covariates // GOVERNOR ALIGNMENT
************

*	areg  $outcome  $saturated  i.year, a(inegi) vce(cluster inegi)
*keep if e(sample)==1


est clear
foreach i in hayCarteles{

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
foreach i in hayCarteles{

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
foreach i in nCarteles{

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
foreach i in nCarteles{

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




texdoc init  "../Tables/abraham_sun_estimates_heteffects_carteles.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect$^a$: Difference by Cartel Presence}
tex \label{tab:abraham_sun_heteffects}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{Agreement A} & \multicolumn{2}{c}{Agreement B$^{b}$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace

tex Reform (t+3)*Dummy Cartel presence &     $ ${beta_hayCarteles1}^{${est_hayCarteles1}} $ &  &  $ ${beta_hayCarteles2}^{${est_hayCarteles2}} $  & \\
tex  &     ($ ${se_hayCarteles1} $) &  & ($ ${se_hayCarteles2} $)  & \\

tex Reform (t+3)*Num. Carteles  &    &   $ ${beta_nCarteles3}^{${est_nCarteles3}} $ &  &  $ ${beta_nCarteles4}^{${est_nCarteles4}} $ \\
tex  &    &     ($ ${se_nCarteles3} $) & & ($ ${se_nCarteles4} $)  \\


tex \\
tex \addlinespace
tex Observations       &        ${N_hayCarteles1}    &        ${N_hayCarteles2}    &     ${N_nCarteles3}      &     ${N_nCarteles4}  \\
tex R-squared       &        ${r2_hayCarteles1}    &        ${r2_hayCarteles2}    &     ${r2_nCarteles3}      &     ${r2_nCarteles4}  \\


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

*MESSAGE: differential effect with cartel presence

