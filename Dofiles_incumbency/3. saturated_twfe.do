*Regressions: Reform & Sec. Cooperation Agreements
*Paper: Reelection Backfire
*Author: Rafael Ch (rafael.ch@nyu.edu)
*========================================================================
/*NOTES
Tables:
1. Saturated TWFE model
2. With various bandwidths 
3. Abraham and Sun (2020)
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
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles_incumbency"

*========================================================================
*LOAD DATA
use "../../Data/ConstructionDatabase/data_final_incumbency.dta", clear

*========================================================================
*SET PANEL
xtset inegi year 

*========================================================================
*SET GLOBALS
*1) controls
*temporal globals with date_0
	global homicides  logdefuncionespc_lag_3 logdefuncionespc_lag_4 logdefuncionespc_lag_5 logdefuncionespc_lag_6  
	global executive  alignment_executive_strong_lag_3 alignment_executive_strong_lag_4 alignment_executive_strong_lag_5 alignment_executive_strong_lag_6  
	global governor  alignment_governor_strong_lag_3 alignment_governor_strong_lag_4 alignment_governor_strong_lag_5 alignment_governor_strong_lag_6  
	global margin  winning_margin_lag_3 winning_margin_lag_4 winning_margin_lag_5 winning_margin_lag_6  
	global margin_governor  winning_margin_governor_lag_3 winning_margin_governor_lag_4 winning_margin_governor_lag_5 winning_margin_governor_lag_6  
	global logpop  logpop_lag_3 logpop_lag_4 logpop_lag_5 logpop_lag_6  
	global carteles  hayCarteles_lag_3 hayCarteles_lag_4 hayCarteles_lag_5 hayCarteles_lag_6  
	global effectiveparties  effectiveparties_lag_3 effectiveparties_lag_4 effectiveparties_lag_5 effectiveparties_lag_6  
	global numparties_eff  numparties_eff_lag_3 numparties_eff_lag_4 numparties_eff_lag_5 numparties_eff_lag_6  
	global areakm2  areakm2_lag_3 areakm2_lag_4 areakm2_lag_5 areakm2_lag_6  
	global pan_mayor2  pan_mayor2_lag_3 pan_mayor2_lag_4 pan_mayor2_lag_5 pan_mayor2_lag_6  
	global pri_mayor2  pri_mayor2_lag_3 pri_mayor2_lag_4 pri_mayor2_lag_5 pri_mayor2_lag_6  
	gen presidential_election=0
	replace presidential_election=1 if year==2012 | year==2016
	gen pan_president=0
	replace pan_president=1 if year<=2012
	global incumbency  inc_lag_3 inc_lag_4 inc_lag_5 inc_lag_6  

	global controls_time_acuerdo presidential_election pan_president $incumbency $homicides $areakm2 $pan_mayor2 $pri_mayor2  $margin $governor $margin_governor $effectiveparties $logpop
	global controls_time_acuerdo  presidential_election pan_president $homicides $margin $governor $margin_governor $effectiveparties $logpop
	*global controls_time_acuerdo  

*2) treatment
	global lagsleads  lag_8 lag_6 lag_5 lag_4   date_0 // comparison group is lag_3
	global lagsleads2  lag_6   lag_5 lag_4  date_0 // comparison group is lag_3
	global saturated  lag_8_2018 lag_6_2018 lag_6_2016 lag_5_2018 lag_5_2015 lag_4_2018 lag_4_2017 lag_4_2015  date_0_2018 date_0_2017 date_0_2016 date_0_2015 // removed lag_3_2018 lag_3_2016 lag_3_2015 to be the comparison group AND lag_8_2018 because of collinearity following Abraham and Sun (2021)
	global saturated  lag_6_2018 lag_6_2016 lag_5_2018 lag_5_2015 lag_4_2018 lag_4_2017 lag_4_2015  date_0_2018 date_0_2017 date_0_2016 date_0_2015 // removed lag_3_2018 lag_3_2016 lag_3_2015 to be the comparison group AND lag_8_2018 because of collinearity following Abraham and Sun (2021)

*3) outcomes
	global outcome1 inc_party_won_tplus1
   	global outcome2 incumbent_yesterday_w_tomorrow2
	global outcome5 mv_incpartyfor1
	global outcomes  incumbent_yesterday_w_tomorrow2 mv_incpartyfor1
*========================================================================
*VARIABLE CREATION
*A) Generate interactions: 
*1) TWFE saturated model
gen interaction_ref=inc_party_won*reform
*2) Event Study 
*Generate interaction
foreach i in $lagsleads{
gen interaction_`i'=inc_party_won*`i'
}
*3) Abraham and Sun:
foreach i in $saturated{
gen interaction_`i'=inc_party_won*`i'
}

*B) Create polynomials and interactions:
foreach j in $outcome2 {
foreach pol in 1 2{
*Estimate optimal bandwidth:
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

*Polynomials:
*Generate polynomials:
gen pol`pol'=mv_incparty^`pol' if mv_incparty<${optimal} & mv_incparty>-${optimal}

foreach i in pol`pol'{
foreach var in reform inc_party_won interaction_ref{
gen `var'_`i'=`var'*`i'

}
}

foreach i in pol`pol'{
foreach var in $saturated{
gen `var'_`i'=`var'*`i'

}
}

foreach i in pol`pol'{
foreach var in $lagsleads{
gen `var'_`i'=`var'*`i'

}
}

}
}

*========================================================================
*SET POLYNOMIAL GLOBALS
*1) TWFE saturated model
global linearpolynomial reform_pol1 inc_party_won_pol1 interaction_ref_pol1
global quadraticpolynomial $linearpolynomial  reform_pol2 inc_party_won_pol2 interaction_ref_pol2
global linearpolynomial 
global quadraticpolynomial 

*2) Event study
*a) interaction with polynomials
global inter_lagsleads_pol1 lag_8_pol1 lag_6_pol1 lag_5_pol1 lag_4_pol1   date_0_pol1
global inter_lagsleads_pol2 $inter_lagsleads_pol1 lag_8_pol2 lag_6_pol2 lag_5_pol2 lag_4_pol2   date_0_pol2
*b) interaction of relative perios and reform
global interaction_reform interaction_lag_8 interaction_lag_6 interaction_lag_5 interaction_lag_4 interaction_date_0
global interaction_reform2  interaction_lag_6 interaction_lag_5 interaction_lag_4 interaction_date_0

*3) Abraham and Sun
*a) interaction with polynomials
global inter_pol1   lag_5_2018_pol1 lag_5_2015_pol1 lag_4_2018_pol1 lag_4_2017_pol1 lag_4_2015_pol1    date_0_2018_pol1 date_0_2017_pol1 date_0_2016_pol1 date_0_2015_pol1  
global inter_pol2  $inter_pol1 lag_5_2018_pol2 lag_5_2015_pol2 lag_4_2018_pol2 lag_4_2017_pol2 lag_4_2015_pol2    date_0_2018_pol2 date_0_2017_pol2 date_0_2016_pol2 date_0_2015_pol2  
*b) interaction of relative perios and reform
global interaction_reform_as interaction_lag_6_2018 interaction_lag_6_2016 interaction_lag_5_2018 interaction_lag_5_2015 interaction_lag_4_2018 interaction_lag_4_2017 interaction_lag_4_2015 interaction_date_0_2018 interaction_date_0_2017 interaction_date_0_2016 interaction_date_0_2015
*========================================================================
*Run .ado to run wild bootstrap
*do "wild_twfe_incumbency.do"
*========================================================================
*subset sample 
rdbwselect  $outcome5 mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
xi: reghdfe  $outcome5 pol1 reform inc_party_won interaction_ref   $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
*keep if e(sample)==1
*=============================================
*MAIN TABLE) Naive TWFE: constraint on the outcome/rating relationship to be identical on both sides of the cut-point
**NO COVARIATES
est clear

preserve
foreach band in CCT{
foreach n in 1{
foreach j in $outcomes{
*A) Linear polynomial
foreach pol in 1 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(`band') 
global optimal = e(h_CCT)/`n'

eststo: xi: reghdfe  `j'   reform inc_party_won interaction_ref    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
		estadd local fixed \checkmark
		estadd local year \checkmark
		estadd local controls  
		estadd local polynomial linear  
test 	(_b[interaction_ref]-_b[inc_party_won])=0
glo pvalue: di %5.3f r(p)
	estadd local pvalue $pvalue
	glo est= "" 
			if (${pvalue}<=0.1) global est = "*"
			if (${pvalue}<=0.05) global est = "**"
			if (${pvalue}<=0.01) global est = "***"

lincom (_b[interaction_ref]-_b[inc_party_won]) 
glo difference: di %5.3f r(estimate)
	estadd local difference $${difference}^{${est}}$
glo se: di %5.3f r(se)
	estadd local se $se		
}
*B) Quadratic polynomial
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(`band') 
*global optimal = e(h_CCT)/`n'

eststo: xi: reghdfe  `j'  reform inc_party_won interaction_ref  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
		estadd local fixed \checkmark
		estadd local year \checkmark
		estadd local controls  
		estadd local polynomial quadratic 
test 	(_b[interaction_ref]-_b[inc_party_won])=0
glo pvalue: di %5.3f r(p)
	estadd local pvalue $pvalue
	glo est= "" 
			if (${pvalue}<=0.1) global est = "*"
			if (${pvalue}<=0.05) global est = "**"
			if (${pvalue}<=0.01) global est = "***"

lincom (_b[interaction_ref]-_b[inc_party_won]) 
glo difference: di %5.3f r(estimate)
	estadd local difference $${difference}^{${est}}$
glo se: di %5.3f r(se)
	estadd local se $se		
}
}
}
}
restore
esttab est*, keep(reform inc_party_won interaction_ref) star(* 0.1 ** 0.05 *** 0.01) t

esttab using "../Tables_incumbency/naive_twfe_main.tex", replace f b(%9.3f) se(%9.3f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N r2 fixed year controls polynomial difference se, fmt(0 3) ///
label("Observations" "R-squared"  "Municipal FE" "Year FE" "Controls$^a$" ///
 "Polynomial" "Difference:Personal-Partisan" "SE (Difference)")) ///
keep(reform inc_party_won interaction_ref) ///
coeflabel(reform "Term Limit Reform" inc_party_won "\begin{tabular}[c]{@{}l@{}} Dummy win, Election at t \\ (Partisan Incumbency Advantage)\end{tabular}" ///
 interaction_ref "\begin{tabular}[c]{@{}l@{}} Interaction (Reform X Win), Election at t \\ (Personal Incumbency Advantage)\end{tabular}") ///
mgroups("\begin{tabular}[c]{@{}l@{}} Probability of winning, \\ Election at t+1\end{tabular}" ///
 "\begin{tabular}[c]{@{}l@{}}  Vote Share,  \\ Election at t+1 \end{tabular}" , ///
pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
collabels(none) nonotes booktabs nomtitles nolines

*=============================================
*0) Naive TWFE: constraint on the outcome/rating relationship to be identical on both sides of the cut-point
**NO COVARIATES
est clear

preserve
foreach band in CCT{
foreach n in 1{
foreach j in $outcomes{
*A) Linear polynomial
foreach pol in 1 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(`band') 
global optimal = e(h_CCT)/`n'

eststo: xi: reghdfe  `j'  pol`pol' reform inc_party_won interaction_ref    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
		estadd local fixed \checkmark
		estadd local year \checkmark
		estadd local controls  
		estadd local polynomial linear  
test 	(_b[interaction_ref]-_b[inc_party_won])=0
glo pvalue: di %5.3f r(p)
	estadd local pvalue $pvalue
	glo est= "" 
			if (${pvalue}<=0.1) global est = "*"
			if (${pvalue}<=0.05) global est = "**"
			if (${pvalue}<=0.01) global est = "***"

lincom (_b[interaction_ref]-_b[inc_party_won]) 
glo difference: di %5.3f r(estimate)
	estadd local difference $${difference}^{${est}}$
glo se: di %5.3f r(se)
	estadd local se $se		
}
*B) Quadratic polynomial
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(`band') 
*global optimal = e(h_CCT)/`n'

eststo: xi: reghdfe  `j'  reform inc_party_won interaction_ref pol1  pol`pol'  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
		estadd local fixed \checkmark
		estadd local year \checkmark
		estadd local controls  
		estadd local polynomial quadratic 
test 	(_b[interaction_ref]-_b[inc_party_won])=0
glo pvalue: di %5.3f r(p)
	estadd local pvalue $pvalue
	glo est= "" 
			if (${pvalue}<=0.1) global est = "*"
			if (${pvalue}<=0.05) global est = "**"
			if (${pvalue}<=0.01) global est = "***"

lincom (_b[interaction_ref]-_b[inc_party_won]) 
glo difference: di %5.3f r(estimate)
	estadd local difference $${difference}^{${est}}$
glo se: di %5.3f r(se)
	estadd local se $se		
}
}
}
}
restore
esttab est*, keep(reform inc_party_won interaction_ref) star(* 0.1 ** 0.05 *** 0.01) t

esttab using "../Tables_incumbency/naive_twfe_nocov.tex", replace f b(%9.3f) se(%9.3f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N r2 fixed year controls polynomial difference se, fmt(0 3) ///
label("Observations" "R-squared"  "Municipal FE" "Year FE" "Controls$^a$" ///
 "Polynomial" "Difference:Personal-Partisan" "SE (Difference)")) ///
keep(reform inc_party_won interaction_ref) ///
coeflabel(reform "Term Limit Reform" inc_party_won "\begin{tabular}[c]{@{}l@{}} Dummy win, Election at t \\ (Partisan Incumbency Advantage)\end{tabular}" ///
 interaction_ref "\begin{tabular}[c]{@{}l@{}} Interaction (Reform X Win), Election at t \\ (Personal Incumbency Advantage)\end{tabular}") ///
mgroups("\begin{tabular}[c]{@{}l@{}} Probability of winning, \\ Election at t+1\end{tabular}" ///
 "\begin{tabular}[c]{@{}l@{}}  Vote Share,  \\ Election at t+1 \end{tabular}" , ///
pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
collabels(none) nonotes booktabs nomtitles nolines

*=============================================
*1) Naive TWFE: constraint on the outcome/rating relationship to be identical on both sides of the cut-point
*WITH COVARIATES
est clear

preserve
foreach band in CCT{
foreach n in 1{
foreach j in $outcomes{
*A) Linear polynomial
foreach pol in 1 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(`band') 
global optimal = e(h_CCT)/`n'

eststo: xi: reghdfe  `j'  pol`pol' reform inc_party_won interaction_ref  $controls_time_acuerdo    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
		estadd local fixed \checkmark
		estadd local year \checkmark
		estadd local controls  
		estadd local polynomial linear  
test 	(_b[interaction_ref]-_b[inc_party_won])=0
glo pvalue: di %5.3f r(p)
	estadd local pvalue $pvalue
	glo est= "" 
			if (${pvalue}<=0.1) global est = "*"
			if (${pvalue}<=0.05) global est = "**"
			if (${pvalue}<=0.01) global est = "***"

lincom (_b[interaction_ref]-_b[inc_party_won]) 
glo difference: di %5.3f r(estimate)
	estadd local difference $${difference}^{${est}}$
glo se: di %5.3f r(se)
	estadd local se $se		
}
*B) Quadratic polynomial
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(`band') 
*global optimal = e(h_CCT)/`n'

eststo: xi: reghdfe  `j'  reform inc_party_won interaction_ref pol1  pol`pol' $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
		estadd local fixed \checkmark
		estadd local year \checkmark
		estadd local controls  
		estadd local polynomial quadratic 
test 	(_b[interaction_ref]-_b[inc_party_won])=0
glo pvalue: di %5.3f r(p)
	estadd local pvalue $pvalue
	glo est= "" 
			if (${pvalue}<=0.1) global est = "*"
			if (${pvalue}<=0.05) global est = "**"
			if (${pvalue}<=0.01) global est = "***"

lincom (_b[interaction_ref]-_b[inc_party_won]) 
glo difference: di %5.3f r(estimate)
	estadd local difference $${difference}^{${est}}$
glo se: di %5.3f r(se)
	estadd local se $se		
}
}
}
}
restore
esttab est*, keep(reform inc_party_won interaction_ref) star(* 0.1 ** 0.05 *** 0.01) t

esttab using "../Tables_incumbency/naive_twfe.tex", replace f b(%9.3f) se(%9.3f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N r2 fixed year controls polynomial difference se, fmt(0 3) ///
label("Observations" "R-squared"  "Municipal FE" "Year FE" "Controls$^a$" ///
 "Polynomial" "Difference:Personal-Partisan" "SE (Difference)")) ///
keep(reform inc_party_won interaction_ref) ///
coeflabel(reform "Term Limit Reform" inc_party_won "\begin{tabular}[c]{@{}l@{}} Dummy win, Election at t \\ (Partisan Incumbency Advantage)\end{tabular}" ///
 interaction_ref "\begin{tabular}[c]{@{}l@{}} Interaction (Reform X Win), Election at t \\ (Personal Incumbency Advantage)\end{tabular}") ///
mgroups("\begin{tabular}[c]{@{}l@{}} Probability of winning, \\ Election at t+1\end{tabular}" ///
 "\begin{tabular}[c]{@{}l@{}}  Vote Share,  \\ Election at t+1 \end{tabular}" , ///
pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
collabels(none) nonotes booktabs nomtitles nolines

*=============================================
*2) Naive TWFE-Saturated: polynomial interacting the treatment
/*
Including an interaction between the forcing variable and the treatment can account for the fact that the treatment
may impact not only the intercept, but also the slop of the regression line. This is important where data that are 
very far away from the cutpoint are included in the analysis or in which there is nolinearity in the relationship
between the outcome and the forcing variable. Jacob and Zhu (2012). 
*/
est clear
*pol1
preserve
foreach n in 1{
foreach j in $outcomes{
*subset sample:
foreach pol in 1 {
rdbwselect  $outcome5 mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)/`n'

xi: reghdfe  $outcome5 pol`pol' inc_party_won inc_party_won_pol1  reform reform_pol1  interaction_ref interaction_ref_pol1  $controls_time_acuerdo    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
*keep if e(sample)==1
}


foreach pol in 1 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)/`n'

eststo: xi: reghdfe  `j' pol`pol' inc_party_won inc_party_won_pol1  reform reform_pol1  interaction_ref interaction_ref_pol1  $controls_time_acuerdo    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
		estadd local fixed \checkmark
		estadd local year \checkmark
		estadd local controls  
		estadd local polynomial linear 
test 	(_b[interaction_ref]-_b[inc_party_won])=0
glo pvalue: di %5.3f r(p)
	estadd local pvalue $pvalue
	glo est= "" 
			if (${pvalue}<=0.1) global est = "*"
			if (${pvalue}<=0.05) global est = "**"
			if (${pvalue}<=0.01) global est = "***"

lincom (_b[interaction_ref]-_b[inc_party_won]) 
glo difference: di %5.3f r(estimate)
	estadd local difference $${difference}^{${est}}$
glo se: di %5.3f r(se)
	estadd local se $se		
}
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)/`n'

eststo: xi: reghdfe  `j' pol1 pol`pol' inc_party_won inc_party_won_pol1 inc_party_won_pol2  reform reform_pol1 reform_pol2  interaction_ref interaction_ref_pol1 interaction_ref_pol2  $controls_time_acuerdo    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
		estadd local fixed \checkmark
		estadd local year \checkmark
		estadd local controls  
		estadd local polynomial quadratic
test 	(_b[interaction_ref]-_b[inc_party_won])=0
glo pvalue: di %5.3f r(p)
	estadd local pvalue $pvalue
	glo est= "" 
			if (${pvalue}<=0.1) global est = "*"
			if (${pvalue}<=0.05) global est = "**"
			if (${pvalue}<=0.01) global est = "***"

lincom (_b[interaction_ref]-_b[inc_party_won]) 
glo difference: di %5.3f r(estimate)
	estadd local difference $${difference}^{${est}}$
glo se: di %5.3f r(se)
	estadd local se $se		  
}
}
}
restore
esttab est*, keep(reform inc_party_won interaction_ref) star(* 0.1 ** 0.05 *** 0.01) t

esttab using "../Tables_incumbency/naive_twfe_saturated.tex", replace f b(%9.3f) se(%9.3f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N r2 fixed year controls polynomial difference se, fmt(0 3) ///
label("Observations" "R-squared"  "Municipal FE" "Year FE" "Controls$^a$" ///
 "Polynomial" "Difference:Personal-Partisan" "SE (Difference)")) ///
keep(reform inc_party_won interaction_ref) ///
coeflabel(reform "Term Limit Reform" inc_party_won "\begin{tabular}[c]{@{}l@{}} Dummy win, Election at t \\ (Partisan Incumbency Advantage)\end{tabular}" ///
 interaction_ref "\begin{tabular}[c]{@{}l@{}} Term Limit Reform X Dummy Win, Election at t \\ (Personal Incumbency Advantage)\end{tabular}") ///
mgroups("\begin{tabular}[c]{@{}l@{}} Probability of winning, \\ Election at t+1\end{tabular}" ///
 "\begin{tabular}[c]{@{}l@{}}  Vote Share,  \\ Election at t+1 \end{tabular}" , ///
pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
collabels(none) nonotes booktabs nomtitles nolines

*=============================================
*3) Naive Event Study
label variable lag_8 "t-8"
label variable lag_6"t-6"
label variable lag_5 "t-5"
label variable lag_4 "t-4"
label variable date_0 "Term Limit vs. Reelection"
label variable inc_party_won "Partisan"
label variable interaction_lag_6 "t-6"
label variable interaction_lag_5 "t-5"
label variable interaction_lag_4 "t-4"
label variable interaction_date_0 "Personal, t=0"

est clear
*pol1
foreach j in $outcomes {

foreach pol in 1 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

eststo: xi: reghdfe  `j'  $lagsleads2 $interaction_reform2 inc_party_won   pol`pol'  $controls_time_acuerdo    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
}
}
esttab est*, keep($lagsleads2 inc_party_won $interaction_reform2) star(* 0.1 ** 0.05 *** 0.01) t

*Figure: Probability of Winning in t+1. Pol1 and Pol2			   
coefplot (est1, msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
(est2, msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
keep($interaction_reform2) ///
order(interaction_lag_6 interaction_lag_5 interaction_lag_4 interaction_date_0) ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel A: Probability of winning at Election t+1") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 8 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/new_event_study_incumbency.png", as(png) replace
*graph export "../Figures_incumbency/new_event_study_incumbency.pdf", as(pdf) replace
*graph export "../Figures_incumbency/new_event_study_incumbency.tif", as(tif) replace
graph save "../Figures_incumbency/new_event_study_incumbency.gph", replace

*Figure: Vote Share in t+1. Pol1 and Pol2			   
coefplot (est3, msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
(est4, msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
keep($interaction_reform2) ///
order(interaction_lag_6 interaction_lag_5 interaction_lag_4 interaction_date_0) ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel B: Vote Share at Election t+1") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 8 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/new_event_study_incumbency_margin.png", as(png) replace
*graph export "../Figures_incumbency/new_event_study_incumbency_margin.pdf", as(pdf) replace
*graph export "../Figures_incumbency/new_event_study_incumbency_margin.tif", as(tif) replace
graph save "../Figures_incumbency/new_event_study_incumbency_margin.gph", replace

*=============================================
*Combine figures:
grc1leg "../Figures_incumbency/new_event_study_incumbency.gph" "../Figures_incumbency/new_event_study_incumbency_margin.gph" , ///
scheme(s1color)  imargin(vsmall)   col(1)
graph export "../Figures_incumbency/new_paralleltrend_eventstudy_incumbency.png", as(png) replace
*graph export "../Figures_incumbency/new_paralleltrend_eventstudy_incumbency.pdf", as(pdf) replace
*graph export "../Figures_incumbency/new_paralleltrend_eventstudy_incumbency.tif", as(tif) replace


*=============================================
*4) Abraham and Sun (2021)
est clear
*pol1
foreach j in $outcomes {

*A) Quadratic Polynomial
foreach pol in 1 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
*lags and leads
foreach i in lag_6 {
xi: reghdfe  `j'  $saturated inc_party_won pol`pol' $inter_pol1 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

	sum perc if interaction_`i'_2016==1, meanonly
	local b = r(mean)
	sum perc if interaction_`i'_2018==1, meanonly
	local d = r(mean)
	di (_b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2018]*`d') 
	test (_b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2018]*`d')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2018]*`d')
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated inc_party_won pol`pol' $inter_pol1 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2018]*`d')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_5 {
xi: reghdfe  `j'  $saturated inc_party_won pol`pol' $inter_pol1 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

	sum perc if interaction_`i'_2015==1, meanonly
	local a = r(mean)
	sum perc if interaction_`i'_2018==1, meanonly
	local d = r(mean)
	di (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2018]*`d') 
	test (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2018]*`d')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2018]*`d')
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated inc_party_won pol`pol' $inter_pol1 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2018]*`d')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
xi: reghdfe  `j'  $saturated inc_party_won pol`pol' $inter_pol1 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

	sum perc if interaction_`i'_2015==1, meanonly
	local a = r(mean)
	sum perc if interaction_`i'_2017==1, meanonly
	local c = r(mean)
	di (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2017]*`c') 
	test (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2017]*`c')    =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2017]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated inc_party_won pol`pol' $inter_pol1 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2017]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
xi: reghdfe  `j'  $saturated inc_party_won pol`pol' $inter_pol1 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if interaction_`i'_2015==1, meanonly
	local a = r(mean)
	sum perc if interaction_`i'_2016==1, meanonly
	local b = r(mean)
	sum perc if interaction_`i'_2017==1, meanonly
	local c = r(mean)
	di (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2017]*`c') 
	test (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2017]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2017]*`c')
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated inc_party_won pol`pol' $inter_pol1 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2017]*`c')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}
}
*B) Quadratic Polynomial
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
*lags and leads
foreach i in lag_6 {
xi: reghdfe  `j'  $saturated inc_party_won pol1 pol`pol' $inter_pol2 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

	sum perc if interaction_`i'_2016==1, meanonly
	local b = r(mean)
	sum perc if interaction_`i'_2018==1, meanonly
	local d = r(mean)
	di (_b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2018]*`d') 
	test (_b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2018]*`d')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2018]*`d')
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
xi: reghdfe  `j'  $saturated inc_party_won pol1 pol`pol' $inter_pol2 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2018]*`d')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_5 {
xi: reghdfe  `j'  $saturated inc_party_won pol1 pol`pol' $inter_pol2 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

	sum perc if interaction_`i'_2015==1, meanonly
	local a = r(mean)
	sum perc if interaction_`i'_2018==1, meanonly
	local d = r(mean)
	di (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2018]*`d') 
	test (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2018]*`d')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2018]*`d')
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
xi: reghdfe  `j'  $saturated inc_party_won pol1 pol`pol' $inter_pol2 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2018]*`d')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
xi: reghdfe  `j'  $saturated inc_party_won pol1 pol`pol' $inter_pol2 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

	sum perc if interaction_`i'_2015==1, meanonly
	local a = r(mean)
	sum perc if interaction_`i'_2017==1, meanonly
	local c = r(mean)
	di (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2017]*`c') 
	test (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2017]*`c')    =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2017]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
xi: reghdfe  `j'  $saturated inc_party_won pol1 pol`pol' $inter_pol2 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2017]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
xi: reghdfe  `j'  $saturated inc_party_won pol1 pol`pol' $inter_pol2 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if interaction_`i'_2015==1, meanonly
	local a = r(mean)
	sum perc if interaction_`i'_2016==1, meanonly
	local b = r(mean)
	sum perc if interaction_`i'_2017==1, meanonly
	local c = r(mean)
	di (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2017]*`c') 
	test (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2017]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2017]*`c')
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
xi: reghdfe  `j'  $saturated inc_party_won pol1 pol`pol' $inter_pol2 $interaction_reform_as $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[interaction_`i'_2015]*`a' + _b[interaction_`i'_2016]*`b' + _b[interaction_`i'_2017]*`c')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

}
}

*Figure: Probability of Winning in t+1. Pol1 and Pol2			   
coefplot (est1, rename((1) = "t-6") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "t-5") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "t-4") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est4, rename((1) = "Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "t-6") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "t-5") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est7, rename((1) = "t-4") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est8, rename((1) = "Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel A: Probability of winning at Election t+1") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/new_incumbency_as.png", as(png) replace
*raph export "../Figures_incumbency/new_incumbency_as.pdf", as(pdf) replace
*graph export "../Figures_incumbency/new_incumbency_as.tif", as(tif) replace
graph save "../Figures_incumbency/new_incumbency_as.gph", replace

*Figure: Vote share in t+1. Pol1 and Pol2			   
coefplot (est9, rename((1) = "t-6") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est10, rename((1) = "t-5") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est11, rename((1) = "t-4") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est12, rename((1) = "Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est13, rename((1) = "t-6") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est14, rename((1) = "t-5") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est15, rename((1) = "t-4") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est16, rename((1) = "Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel B: Vote Share at Election t+1") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/new_incumbency_as_margin.png", as(png) replace
*graph export "../Figures_incumbency/new_incumbency_as_margin.pdf", as(pdf) replace
*graph export "../Figures_incumbency/new_incumbency_as_margin.tif", as(tif) replace
graph save "../Figures_incumbency/new_incumbency_as_margin.gph", replace

*=============================================
*Combine figures:
grc1leg "../Figures_incumbency/new_incumbency_as.gph" "../Figures_incumbency/new_incumbency_as_margin.gph" , ///
scheme(s1color)  imargin(vsmall)  row(1)
graph export "../Figures_incumbency/new_parallel_incumbency_as.png", as(png) replace
*graph export "../Figures_incumbency/new_parallel_incumbency_as.pdf", as(pdf) replace
*graph export "../Figures_incumbency/new_parallel_incumbency_as.tif", as(tif) replace

*===================================================================================================
*===================================================================================================
/*===================================================================================================
*Callaway and Sant'Ana (2020)
*help did
*net install did, from("https://raw.githubusercontent.com/NickCH-K/did/master/") replace
*didsetup
rcall clear
gen group2=.
replace group2=0 if group==1
replace group2=1 if group==2
replace group2=2 if group==3
replace group2=3 if group==4
replace group2=4 if group==5

att_gt $outcome5 year group2, idname(inegi) allow_unbalanced_panel  anticipation(0) 
// doesn't work	
