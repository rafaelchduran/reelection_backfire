*Regressions: Reform & Sec. Cooperation Agreements, INTERACTIONS
*Paper: Reelection Backfire
*Author: Rafael Ch (rafael.ch@nyu.edu)
*========================================================================
/*
Main Messages
1. Sec. Coop. Agreements decrease violence
2. Reform decreases the likelihood of signing sec. coop. agreements. Things look better with acuerdo than acuerdo2, and with IHS rather than logs.
-Wash hands-off < old the bull by the horns
-Why?
3. The fall in likelihood is stronger when there is political alignment. But there are two stories:
3.1 Transfers due to the alignment (Alignment literature, Dell (2015)). = capacity isolation
3.2 Blame/accountability from citizens (Sandra Ley 2017 on Mexico). = capacity isolation

ANOTHER MECHANISM IS INCUMBENCY ADVANTAGE: 
If incumbency disadvantage then this would be negative. But I find there is an inc. advantage. 
Now why is there an inc. advantage:
3.2.a. Clientelistic machinery-based incumbency advantage a la Fergusson et al (2019). Reelection incentives lead mayors to use the whole apparatus. 
3.2.b. Quality-based incumbency advantage a la Eggers (2017)
3.2.c. Information-based incumbency advantage a la BDM et a. (2020).


**IT'S WRONG FROM HERE ON:
4. Het effects of alignment with governor PRI (negative and significant) and president ALGINED (positive and significant)

5. Check type of agreements: only has this for control municipalities which is weird

Notes
1. check how acuerdo and acuerdo2 differ - done, use acuerdo
2. check how governor_alignment was constructed 
3. read Chaisemartin to understand the placebos 
4. read Sandra Ley paper on how citizens hold mayor accountables, and what happens with alignment


TODOS 
1. INCLUDE BOTH MODELS: OVERALL ALIGNMENT AND PRI ALIGNMENT TO COMPARE THE COEFFICIENTS

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
*SET GLOBALS AND OTHER VARIABLES
global controls winning_margin_governor  governor_alignment  

foreach i in reform{
gen `i'_pre=L.`i'
}

foreach i in winning_margin_governor governor_alignment logdefuncionespc{
gen `i'_pre=L2.`i'
}

global controls_pre winning_margin_governor_pre governor_alignment_pre logdefuncionespc_pre

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
*SET MATSIZE
set matsize 11000 
sort inegi year

*========================================================================
***1) INTERACTION WITH FEDERAL AND/OR STATE ALIGNMENT 
est clear
global interaction alignment_executive_strong alignment_governor_strong double_alignment
foreach interaction in $interaction{
foreach outcome in acuerdo{
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
esttab est*, keep(L.reform cL.reform#c.alignment_executive_strong cL.reform#c.alignment_governor_strong  cL.reform#c.double_alignment ) t star(* 0.1 ** 0.05 *** 0.01)
*esttab est*, keep(L2.reform cL2.reform#c.alignment_executive_strong cL2.reform#c.alignment_governor_strong  cL2.reform#c.double_alignment ) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/alignment.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun tot_int se pvalue, fmt(%11.2gc 3) label("Observations" ///
 "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E." "Tot.Int." "S.E.(Tot. Int.)" "p-value(Tot.Int.)")) ///
keep(L.reform cL.reform#c.alignment_executive_strong cL.reform#c.alignment_governor_strong  cL.reform#c.double_alignment ) ///
mgroups("DV: Security Cooperation Agreement A" "DV: Security Cooperation Agreement B", ///
pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L.reform "Term Limit Reform"  c "Term Limit Reform (t-1)*Alignment President" ///
cL.reform#c.alignment_governor_strong "Term Limit Reform (t-1)*Alignment Governor" ///
cL.reform#c.double_alignment "Term Limit Reform (t-1)*Alignment President \& Governor") ///
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
keep if alignment_executive_strong==0
did_multiplegt `outcome' group year reform, breps(1000) controls($controls) placebo(4) seed(5675) ///
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

	


restore


************
*B) alignment_executive_strong==1:
************
preserve
foreach outcome in acuerdo {
keep if alignment_executive_strong==1
did_multiplegt `outcome' group year reform, breps(1000) controls($controls) placebo(4) seed(5675) ///
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


restore
*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVmandounico_byalignment_president.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{NO ALIGNMENT W/ PRESIDENT'S PARTY} & \multicolumn{1}{c}{ALIGNMENT W/ PRESIDENT'S PARTY$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 4 years &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
*tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
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

*MESSAGE: ALIGNMENT SEEMS TO BE GREATER THAN NO ALIGNMENT BUT DON'T HAVE ENOUGH POWER TO DO THIS. CI OVERLAP. 


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


