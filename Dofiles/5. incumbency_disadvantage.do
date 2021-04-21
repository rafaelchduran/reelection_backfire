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
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_incparty[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_incparty[_n+1]

*========================================================================
*SET GLOBALS
*1) controls
*temporal globals with date_0
	global homicides logdefuncionespc_lag_2 logdefuncionespc_lag_3 logdefuncionespc_lag_4 logdefuncionespc_lag_5 logdefuncionespc_lag_6 logdefuncionespc_lag_7 logdefuncionespc_lag_8
	global executive alignment_executive_strong_lag_2 alignment_executive_strong_lag_3 alignment_executive_strong_lag_4 alignment_executive_strong_lag_5 alignment_executive_strong_lag_6 alignment_executive_strong_lag_7 alignment_executive_strong_lag_8
	global governor alignment_governor_strong_lag_2 alignment_governor_strong_lag_3 alignment_governor_strong_lag_4 alignment_governor_strong_lag_5 alignment_governor_strong_lag_6 alignment_governor_strong_lag_7 alignment_governor_strong_lag_8
	global margin winning_margin_lag_2 winning_margin_lag_3 winning_margin_lag_4 winning_margin_lag_5 winning_margin_lag_6 winning_margin_lag_7 winning_margin_lag_8
	global margin_governor winning_margin_governor_lag_2 winning_margin_governor_lag_3 winning_margin_governor_lag_4 winning_margin_governor_lag_5 winning_margin_governor_lag_6 winning_margin_governor_lag_7 winning_margin_governor_lag_8
	global acuerdo acuerdo_lag_2 acuerdo_lag_3 acuerdo_lag_4 acuerdo_lag_5 acuerdo_lag_6 acuerdo_lag_7 acuerdo_lag_8
	global acuerdo2 acuerdo2_lag_2 acuerdo2_lag_3 acuerdo2_lag_4 acuerdo2_lag_5 acuerdo2_lag_6 acuerdo2_lag_7 acuerdo2_lag_8
	global logpop logpop_lag_2 logpop_lag_3 logpop_lag_4 logpop_lag_5 logpop_lag_6 logpop_lag_7 logpop_lag_8
	global carteles hayCarteles_lag_2 hayCarteles_lag_3 hayCarteles_lag_4 hayCarteles_lag_5 hayCarteles_lag_6 hayCarteles_lag_7 hayCarteles_lag_8

	global controls_time_acuerdo  $homicides $executive $governor $margin_governor $logpop $carteles

	
*2) treatment
	global lagsleads   lag_7 lag_6 lag_5 lag_4 lag_3  date_0
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
    global saturated2 lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 lag_1_2015 lag_1_2016 lag_1_2017 lag_1_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
    global trim lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

*3) outcomes
	global incumbency incumbent_today_w_tomorrow2 incumbent_yesterday_w_tomorrow2 inc_party_won  inc_party_won_tplus1 party_won_nextelec
	*inc_party_won_tplus1  should be identical to incumbent_today_w_tomorrow2. I find they are highly correlated. Former constructed with raceafter and latter with winning margins comparison. I believe the second one is better. 
	*inc_party_won = incumbent at t-1 won at t (could be used as a placebo or to see imediate reform effects)
	global outcome inc_party_won_tplus1 // this is another experiment: where former incumbent wins what happens to new incumbent: in term limited places they win. in reelection nothing.
   	global outcome2 incumbent_yesterday_w_tomorrow2
	global outcome3 party_won_nextelec // for unconditional case 

	
*========================================================================
*Run .ado 
do "wild_areg.do"
do "macros_tables.do"
*========================================================================
*NAIVE RDD: CONDITIONAL ON BEING INCUMBENT IN T-1
preserve
use "../../Data/municipal_elections_incumbent_mexico_1989_present_v2.dta", clear

*Merge with Dube, Garcia-Ponce and Thoms (2016): data from 1990 to 2010
gen muncode=inegi

merge 1:1 muncode year using "../../Mexico/Data/Dube, Garcia-Ponce and Thom (2016)/jeea12172-sup-0002-replication-data/Dube_Garcia_ Thom_ReplicationData/MaizeToHaze_JEEA_ReplicationData.dta"
drop if _merge==2
drop _merge

merge m:m inegi year using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011_2018.dta"
drop if _merge==2
drop _merge

est clear
foreach pol in 1 2 {
eststo: quietly rdrobust $outcome2  mv_incparty if reform==0 & year<2015, c(0) p(`pol') kernel(tri) bwselect(CCT) 
	estadd local postreform 
eststo: quietly rdrobust $outcome2  mv_incparty if reform==1 & year<2018, c(0) p(`pol') kernel(tri) bwselect(CCT)
	estadd local postreform \checkmark


}
**the problem with splitting is that I am comparing those early vs late treated by the reform, and if there are het treatment effects then resutls are biased

esttab est*, keep(RD_Estimate) star(* 0.1 ** 0.05 *** 0.01) se

esttab est1 est2 est3 est4 using "../Tables/rdd_estimates_conditional.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N  postreform, fmt(%11.2gc 3) label("Observations" "Post Reform (2014)")) ///
keep(RD_Estimate) ///
mgroups("linear polynomial" "quadratic polynomial", ///
 pattern(1 0 1 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(RD_Estimate "Conditional Probability of victory at t+1") ///
collabels(none) nonotes booktabs nomtitles  nolines

*McCarty Test:
foreach pol in 1 2{
rdbwselect  $outcome2 mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal_`pol' = e(h_CCT)
di ${optimal_`pol'}
}
foreach pol in 1 2{
rddensity mv_incparty, c(0) p(`pol') h(${optimal_`pol'}) plot vce(jackknife) ///
graph_options(legend(off) title("") xtitle("Party vote share margin at t (municipal elections)") ytitle("Density") scheme(s1color))
graph export "../Figures/conditional_mccrary_test_pol`pol'.png", as(png) replace
graph save "../Figures/conditional_mccrary_test_pol`pol'.gph", replace
}

restore

*========================================================================
*NAIVE RDD: UNCONDITIONAL ON BEING INCUMBENT IN T-1
preserve
use "../../Data/ConstructionDatabase/data_final_incumbency_advantage.dta", clear
foreach pol in 1 2 {
eststo: quietly rdrobust $outcome3 mv_party if reform==0 & year<2015, c(0) p(`pol') kernel(tri) bwselect(CCT) 
	estadd local postreform 
eststo: quietly rdrobust $outcome3  mv_party if reform==1 & year<2018, c(0) p(`pol') kernel(tri) bwselect(CCT)
	estadd local postreform \checkmark
}

esttab est*, keep(RD_Estimate) star(* 0.1 ** 0.05 *** 0.01) se

esttab est5 est6 est7 est8 using "../Tables/rdd_estimates_unconditional.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N  postreform, fmt(%11.2gc 3) label("Observations" "Post Reform (2014)")) ///
keep(RD_Estimate) ///
mgroups("linear polynomial" "quadratic polynomial", ///
 pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(RD_Estimate "Unconditional Probability of victory at t+1") ///
collabels(none) nonotes booktabs nomtitles  nolines


esttab est1 est2 est5 est6 est3 est4 est7 est8 using "../Tables/rdd_estimates_cond_vs_unconditional.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N  postreform, fmt(%11.2gc 3) label("Observations" "Post Reform (2014)")) ///
keep(RD_Estimate) ///
mgroups("linear polynomial" "quadratic polynomial", ///
 pattern(1 0 0 0 1 0 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(RD_Estimate "Probability of victory at t+1") ///
collabels(none) nonotes booktabs nomtitles  nolines


*McCarty Test:
foreach pol in 1 2{
rdbwselect  $outcome3 mv_party, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
di ${optimal}
}

rddensity mv_party, c(0) p(`pol') h(${optimal}) plot vce(jackknife) ///
graph_options(legend(off) title("") xtitle("Party vote share margin at t (municipal elections)") ytitle("Density") scheme(s1color))
graph export "../Figures/unconditional_mccrary_test_pol`pol'.png", as(png) replace
graph save "../Figures/unconditional_mccrary_test_pol`pol'.gph", replace
}
restore

*DCdensity mv_party, breakpoint(0) generate(Xj Yj r0 fhat se_fhat)
*========================================================================
*Naive lagsleads
foreach pol in 1 2{

rdbwselect  $outcome2 mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global cond_optimal_`pol' = e(h_CCT)
di ${optimal_`pol'}
gen pol`pol'=mv_incparty^`pol' if mv_incparty<${optimal_`pol'} & mv_incparty>-${optimal_`pol'}
}

foreach pol in 1 2{
foreach i in pol`pol'{
foreach var in $lagsleads{
gen `var'_`i'=`var'*`i'
}
}
}

global int_lagsleads_pol1  lag_6_pol1 lag_5_pol1 lag_4_pol1 lag_3_pol1 lag_2_pol1 date_0_pol1 lead_1_pol1 lead_2_pol1 lead_3_pol1
global int_lagsleads_pol2  lag_6_pol2 lag_5_pol2 lag_4_pol2 lag_3_pol2 lag_2_pol2 date_0_pol2 lead_1_pol2 lead_2_pol2 lead_3_pol2

*Estimates
est clear
*eststo: xi: reghdfe  $outcome2  $lagsleads pol1 $int_lagsleads_pol1 $controls_time_acuerdo i.year if mv_incparty<${optimal_1}/2 & mv_incparty>-${optimal_1}/2, a(inegi) vce(cluster estado)
*eststo: xi: reghdfe  $outcome2  $lagsleads pol2 $int_lagsleads_pol2 $controls_time_acuerdo i.year if mv_incparty<${optimal_2}/2 & mv_incparty>-${optimal_2}/2, a(inegi) vce(cluster estado)
eststo: wildcorrection_incumbency_pol1  $outcome2
macros_tables3
eststo: wildcorrection_incumbency_pol2  $outcome2
macros_tables3

esttab est*, keep($lagsleads) t(%9.3f)  star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/incumbency_advantage_naive_event_study.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N r2 controls munfe yearfe clustermun wildci , fmt(%11.2gc 3) ///
 label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "State Cluster S.E." "Wild corr." )) ///
keep($lagsleads ) ///
mgroups("linear polynomical" "quadratic polynomical", ///
pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(lag_7 "t-7" lag_6 "t-6" lag_5 "t-5" lag_4 "t-4" lag_3 "t-3"  date_0 "Reform t=0") ///
collabels(none) nonotes booktabs nomtitles  nolines

*CONCLUSION 1: MANDO UNICO DECREASES AND ITS SIGNIFICANT; SO IS THE AGGREGATE EFFECT



*========================================================================
*AS (2021): CONDITIONAL ON BEING INCUMBENT IN T-1
**generate variables:
foreach pol in 1 2{
foreach i in pol`pol'{
foreach var in $saturated{
gen `var'_`i'=`var'*`i'
}
}
}

*GLOBALS
global interacted_pol1 lag_7_2017_pol1 lag_7_2018_pol1 lag_6_2016_pol1 lag_6_2017_pol1 lag_6_2018_pol1 lag_5_2015_pol1 lag_5_2016_pol1 lag_5_2017_pol1 lag_5_2018_pol1 lag_4_2015_pol1 lag_4_2016_pol1 lag_4_2017_pol1 lag_4_2018_pol1 lag_3_2015_pol1 lag_3_2016_pol1 lag_3_2017_pol1 lag_3_2018_pol1 lag_2_2015_pol1 lag_2_2016_pol1 lag_2_2017_pol1 lag_2_2018_pol1 date_0_2015_pol1 date_0_2016_pol1 date_0_2017_pol1 date_0_2018_pol1 lead_1_2015_pol1 lead_1_2016_pol1 lead_1_2017_pol1 lead_2_2015_pol1 lead_2_2016_pol1 lead_3_2015_pol1
global interacted_pol2 lag_7_2017_pol2 lag_7_2018_pol2 lag_6_2016_pol2 lag_6_2017_pol2 lag_6_2018_pol2 lag_5_2015_pol2 lag_5_2016_pol2 lag_5_2017_pol2 lag_5_2018_pol2 lag_4_2015_pol2 lag_4_2016_pol2 lag_4_2017_pol2 lag_4_2018_pol2 lag_3_2015_pol2 lag_3_2016_pol2 lag_3_2017_pol2 lag_3_2018_pol2 lag_2_2015_pol2 lag_2_2016_pol2 lag_2_2017_pol2 lag_2_2018_pol2 date_0_2015_pol2 date_0_2016_pol2 date_0_2017_pol2 date_0_2018_pol2 lead_1_2015_pol2 lead_1_2016_pol2 lead_1_2017_pol2 lead_2_2015_pol2 lead_2_2016_pol2 lead_3_2015_pol2


*ESTIMATES:
est clear
eststo: xi: reghdfe  $outcome2  $saturated pol1 $interacted_pol1 $controls_time_acuerdo i.year if mv_incparty<${optimal_1}/3 & mv_incparty>-${optimal_1}/3, a(inegi) vce(cluster estado)
		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	di (_b[`i'_2017]*`a') 
	test (_b[`i'_2017]*`a')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2017]*`a')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2017]*`a') 
	glo se_`i'_log: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_5    {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+ (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+ (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+ (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2015]*`a')+ (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in  lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_3{
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

foreach i in date_0{
	sum perc if `i'_2018==1, meanonly
	local a = r(mean)
	di (_b[`i'_2018]*`a') 
	test (_b[`i'_2018]*`a') = 0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2018]*`a')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2018]*`a') 
	glo se_`i'_log: di %5.4f r(se)
}

	sum perc if date_0_2018==1, meanonly
	local a = r(mean)

eststo:lincomest (_b[date_0_2018]*`a') /2


*Pol2:
eststo: xi: reghdfe  $outcome2  $saturated pol2 $interacted_pol2 $controls_time_acuerdo i.year if mv_incparty<${optimal_2} & mv_incparty>-${optimal_2}, a(inegi) vce(cluster estado)
		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	di (_b[`i'_2017]*`a') 
	test (_b[`i'_2017]*`a')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2017]*`a')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2017]*`a') 
	glo se_`i'_log: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_5    {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+ (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+ (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+ (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2015]*`a')+ (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in  lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_3{
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

foreach i in date_0{
	sum perc if `i'_2018==1, meanonly
	local a = r(mean)
	di (_b[`i'_2018]*`a') 
	test (_b[`i'_2018]*`a') = 0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2018]*`a')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2018]*`a') 
	glo se_`i'_log: di %5.4f r(se)
}

	sum perc if date_0_2018==1, meanonly
	local a = r(mean)

eststo:lincomest (_b[date_0_2018]*`a') /2
			

*========================================================================
*========================================================================
*========================================================================
*=============================OLD========================================
*========================================================================
*========================================================================
*========================================================================

**********************************************
*Table: RDD estimates
**********************************************
use "../../Data/municipal_elections_incumbent_mexico_1989_present_v2.dta", clear

*Merge with Dube, Garcia-Ponce and Thoms (2016): data from 1990 to 2010
gen muncode=inegi

merge 1:1 muncode year using "../../Mexico/Data/Dube, Garcia-Ponce and Thom (2016)/jeea12172-sup-0002-replication-data/Dube_Garcia_ Thom_ReplicationData/MaizeToHaze_JEEA_ReplicationData.dta"
drop if _merge==2
drop _merge

merge m:m inegi year using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011_2018.dta"
drop if _merge==2
drop _merge

est clear
foreach pol in 1 2 {
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
use "../../Data/ConstructionDatabase/data_wleads&lags_incumbency_weights.dta", clear

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
*A)WITH incumbent_yesterday_w_tomorrow2 !!!! NEED TO ADD CONTROL VARIABLES
*------------------
est clear
foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 1 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal}/2 & mv_incparty>-${optimal}/2, a(inegi) vce(cluster inegi)

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
	lincom 	(_b[`i'_2018]*`c')
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
	lincom 	(_b[`i'_2015]*`a')
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			

 areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') +(_b[`i'_2018]*`d')
	

*get the partisan effect
*foreach i in date_0 {
	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') -(_b[_cons]))/2 

	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
		sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)

	eststo: lincomest (_b[_cons])/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

}
}
/*****
test on partisan incumbency advantage
_b[reform] = difference between reelection and term limited elections: partisan(A) + personal(B) = A+B = -.1982682    (sig 1%)
_b[_cons] = inc. advantage for term limited elections: partisan inc. advantage = A =  .2323985    (sig 1%)
_b[reform]-b[_cons]  = A+B-A=B=personal incumbency advantage =  -.3315326  (sig 1%)

*****/

*------------------
*B)WITH inc_party_won
*------------------
foreach j in inc_party_won {
foreach pol in 1 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal}/2 & mv_incparty>-${optimal}/2, a(inegi) vce(cluster inegi)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		*glo constant_`pol': di %5.4f _b[_cons]

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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
			

 areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') +(_b[`i'_2018]*`d')
	

*get the partisan effect
*foreach i in date_0 {
	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') -(_b[_cons]))/2 

	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
		sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)

	eststo: lincomest (_b[_cons])/2
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

}
}
/*****
**SEPARATION IS IN THE PAPER BY ANDY HALL:
test on partisan incumbency advantage
_b[reform] = difference between reelection and term limited elections: partisan(A) + personal(B) = A+B =  -.0120322   (nosig)
_b[_cons] = inc. advantage for term limited elections: partisan inc. advantage = A = .2407415    (sig 1%)
_b[reform]-b[_cons]  = A+B-A=B=personal incumbency advantage =  -.2467576   (sig 1%)

*****/



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
tex & \multicolumn{2}{c}{linear polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex 4 Elections prior &       $ ${beta_lag_5_ihsdet_1}^{${est_lag_5_ihsdet_1}} $ &       $ ${beta_lag_5_ihsdet2_1}^{${est_lag_5_ihsdet2_1}} $  \\
tex & ($ ${se_lag_5_ihsdet_1} $ ) & ($ ${se_lag_5_ihsdet2_1} $ ) \\
tex 3 Elections prior &       $ ${beta_lag_4_ihsdet_1}^{${est_lag_4_ihsdet_1}} $ &        $ ${beta_lag_4_ihsdet2_1}^{${est_lag_4_ihsdet2_1}} $ \\  
tex & ($ ${se_lag_4_ihsdet_1} $ ) & ($ ${se_lag_4_ihsdet2_1} $ ) \\
tex 2 Elections prior &          $ ${beta_lag_3_ihsdet_1}^{${est_lag_3_ihsdet_1}} $ &       $ ${beta_lag_3_ihsdet2_1}^{${est_lag_3_ihsdet2_1}} $ \\   
tex & ($ ${se_lag_3_ihsdet_1} $ ) & ($ ${se_lag_3_ihsdet2_1} $ ) \\
tex Election after Reform &         $ ${beta_date_0_ihsdet_1}^{${est_date_0_ihsdet_1}} $ &        $ ${beta_date_0_ihsdet2_1}^{${est_date_0_ihsdet2_1}} $ \\   
tex & ($ ${se_date_0_ihsdet_1} $ ) & ($ ${se_date_0_ihsdet2_1} $ ) \\
tex Observations          &        ${N_ihsdet_1}     &        ${N_ihsdet2_1} \\ 
tex R-squared        &          ${r2_ihsdet_1}   &          ${r2_ihsdet2_1} \\  
tex\\
tex & \multicolumn{2}{c}{quadratic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex 4 Elections prior &       $ ${beta_lag_5_ihsdet_2}^{${est_lag_5_ihsdet_2}} $ &       $ ${beta_lag_5_ihsdet2_2}^{${est_lag_5_ihsdet2_2}} $  \\
tex & ($ ${se_lag_5_ihsdet_2} $ ) & ($ ${se_lag_5_ihsdet2_2} $ ) \\
tex 3 Elections prior &       $ ${beta_lag_4_ihsdet_2}^{${est_lag_4_ihsdet_2}} $ &        $ ${beta_lag_4_ihsdet2_2}^{${est_lag_4_ihsdet2_2}} $ \\  
tex & ($ ${se_lag_4_ihsdet_2} $ ) & ($ ${se_lag_4_ihsdet2_2} $ ) \\
tex 2 Elections prior &          $ ${beta_lag_3_ihsdet_2}^{${est_lag_3_ihsdet_2}} $ &       $ ${beta_lag_3_ihsdet2_2}^{${est_lag_3_ihsdet2_2}} $ \\   
tex & ($ ${se_lag_3_ihsdet_2} $ ) & ($ ${se_lag_3_ihsdet2_2} $ ) \\
tex Election after Reform &         $ ${beta_date_0_ihsdet_2}^{${est_date_0_ihsdet_2}} $ &        $ ${beta_date_0_ihsdet2_2}^{${est_date_0_ihsdet2_2}} $ \\   
tex & ($ ${se_date_0_ihsdet_2} $ ) & ($ ${se_date_0_ihsdet2_2} $ ) \\
tex Observations          &        ${N_ihsdet_2}     &        ${N_ihsdet2_2} \\ 
tex R-squared        &          ${r2_ihsdet_2}   &          ${r2_ihsdet2_2} \\  
tex\\
tex Mun. FEs        &     \checkmark         &  \checkmark   \\
tex Year. FEs     &     \checkmark         &  \checkmark  \\
tex Controls$^a$  &    \checkmark     &       \checkmark \\
tex Cohort weighted  &         \checkmark &         \checkmark \\
tex \hline \hline      
tex \multicolumn{3}{p{0.9\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 6 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020} or because they are collinear or inexistent, like lag time period 2. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. Logged homicides per capita at the municipality level are also included as controls.}} \\
tex \end{tabular}
tex } 
tex \end{table}
*tex \end{landscape}
texdoc close

*===================================================================================================
*Figure COMPOSITION OF THE INCUMBENCY ADVANTAGE
coefplot (est1, rename((1) = "Partisan + Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Partisan") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est4, rename((1) = "Partisan + Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "Partisan") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle("Probability of Victory at t+1")  xtitle("Term Limit Reform Average Effect") ///
subtitle(" ") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures/partisan_personal_inc_advantage.png", as(png) replace
graph export "../Figures/partisan_personal_inc_advantage.pdf", as(pdf) replace
graph export "../Figures/partisan_personal_inc_advantage.tif", as(tif) replace
graph save "../Figures/partisan_personal_inc_advantage.gph", replace


*Both polynomials: 
**POLYNOMIAL 1
coefplot (est1, rename((1) = "Partisan + Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Partisan") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est7, rename((1) = "Partisan + Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est8, rename((1) = "Partisan") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est9, rename((1) = "Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle("linear polynomial") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" ///
4 "Conditional" 20 "Unconditional" ) rows(2)) 
graph export "../Figures/pol1.png", as(png) replace
graph export "../Figures/pol1.pdf", as(pdf) replace
graph export "../Figures/pol1.tif", as(tif) replace
graph save "../Figures/pol1.gph", replace

**POLYNOMIAL 2
coefplot (est4, rename((1) = "Partisan + Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "Partisan") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est10, rename((1) = "Partisan + Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est11, rename((1) = "Partisan") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est12, rename((1) = "Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle("quadratic polynomial") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" ///
4 "Conditional" 20 "Unconditional" ) rows(2)) 
graph export "../Figures/pol2.png", as(png) replace
graph export "../Figures/pol2.pdf", as(pdf) replace
graph export "../Figures/pol2.tif", as(tif) replace
graph save "../Figures/pol2.gph", replace


*combine:
grc1leg "../Figures/pol1.gph" "../Figures/pol2.gph" , ///
scheme(s1color)  imargin(vsmall) ycommon  col(2) row(1) l1("Probability of winning at t+1") b1("Reform Average Effect")
graph export "../Figures/personalvspartisan_advantage.png", as(png) replace
graph export "../Figures/personalvspartisan_advantage.pdf", as(pdf) replace
graph export "../Figures/personalvspartisan_advantage.tif", as(tif) replace


*=============================================
*Parallel trend figure: seems there is an incumbency disadvantage
est clear
foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 1 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

***estimate linear combination by lead/lag:
foreach i in lag_5 {
	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal}/2 & mv_incparty>-${optimal}/2, a(inegi) vce(cluster inegi)
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
	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal}/2 & mv_incparty>-${optimal}/2, a(inegi) vce(cluster inegi)
	eststo: lincomest 	(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal}/2 & mv_incparty>-${optimal}/2, a(inegi) vce(cluster inegi)
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
	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal}/2 & mv_incparty>-${optimal}/2, a(inegi) vce(cluster inegi)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal}/2 & mv_incparty>-${optimal}/2, a(inegi) vce(cluster inegi)
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
	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal}/2 & mv_incparty>-${optimal}/2, a(inegi) vce(cluster inegi)
	eststo: lincomest  	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal}/2 & mv_incparty>-${optimal}/2, a(inegi) vce(cluster inegi)
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
	qui areg  `j'  $saturated pol`pol' $interacted_pol2   i.year if mv_incparty<${optimal}/2 & mv_incparty>-${optimal}/2, a(inegi) vce(cluster inegi)
	eststo: lincomest  	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			
}

}
}

*Figure
			   
coefplot (est1, rename((1) = "4 Elections prior") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "3 Elections prior") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "2 Elections prior") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est4, rename((1) = "Election after Reform") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est5, rename((1) = "4 Elections prior") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est6, rename((1) = "3 Elections prior") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est7, rename((1) = "2 Elections prior") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est8, rename((1) = "Election after Reform") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle("Probability of winning at t+1")  xtitle("Time period of elections") ///
subtitle(" ") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures/incumbency.png", as(png) replace
graph export "../Figures/incumbency.pdf", as(pdf) replace
graph export "../Figures/incumbency.tif", as(tif) replace
graph save "../Figures/incumbency.gph", replace

*=============================================
*ROBUSTNESS DECREASING OPTIMAL BANDWIDTH. TRY MULTIPLE LIKE IN THE BOLSA FAMILIA PAPER. 
