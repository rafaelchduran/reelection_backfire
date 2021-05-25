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
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles_incumbency"

*========================================================================
*LOAD DATA
use "../../Data/ConstructionDatabase/data_final_incumbency.dta", clear

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
	global controls_naive logdefuncionespc_mean alignment_executive_strong_mean alignment_governor_strong_mean winning_margin_mean winning_margin_governor_mean pan_mayor2_mean pri_mayor2_mean hayCarteles_mean logpop_mean


*2) treatment
	global lagsleads  lag_8 lag_6 lag_5 lag_4   date_0 // comparison group is lag_3
	global saturated  lag_5_2018 lag_5_2015 lag_4_2018 lag_4_2017 lag_4_2015  date_0_2018 date_0_2017 date_0_2016 date_0_2015 // removed lag_3_2018 lag_3_2016 lag_3_2015 to be the comparison group AND lag_8_2018 because of collinearity following Abraham and Sun (2021)
	global saturated2  lag_8_2018 lag_6_2018 lag_6_2016 lag_5_2018 lag_5_2015 lag_4_2018 lag_4_2017 lag_4_2015  date_0_2018 date_0_2017 date_0_2016 date_0_2015 // removed lag_3_2018 lag_3_2016 lag_3_2015 to be the comparison group AND lag_8_2018 because of collinearity following Abraham and Sun (2021)

*3) outcomes
	global incumbency incumbent_today_w_tomorrow2 incumbent_yesterday_w_tomorrow2 inc_party_won  inc_party_won_tplus1 party_won_nextelec
	*inc_party_won_tplus1  should be identical to incumbent_today_w_tomorrow2. I find they are highly correlated. Former constructed with raceafter and latter with winning margins comparison. I believe the second one is better. 
	*inc_party_won = incumbent at t-1 won at t (could be used as a placebo or to see imediate reform effects)
	global outcome inc_party_won_tplus1 // this is another experiment: where former incumbent wins what happens to new incumbent: in term limited places they win. in reelection nothing.
   	global outcome2 incumbent_yesterday_w_tomorrow2
	global outcome3 party_won_nextelec // for unconditional case 
	global outcome4 inc_party_won // for unconditional case 
	global outcome5 mv_incpartyfor1
*========================================================================
*VARIABLE CREATION
est clear
foreach j in $outcome2 {
foreach pol in 1 {
*Estimate optimal bandwidth:
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

*Polynomials:
*Generate polynomials:
gen pol`pol'=mv_incparty^`pol' if mv_incparty<${optimal} & mv_incparty>-${optimal}

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

foreach i in pol`pol'{
foreach var in reform{
gen `var'_`i'=`var'*`i'

}
}




}
}

foreach j in  $outcome2 {
foreach pol in 2 {
*Estimate optimal bandwidth:
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

*Polynomials:
*Generate polynomials:
gen pol`pol'=mv_incparty^`pol' if mv_incparty<${optimal} & mv_incparty>-${optimal}

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

foreach i in pol`pol'{
foreach var in reform{
gen `var'_`i'=`var'*`i'

}
}

}
}

*

*polynomial globals
global inter_pol1   lag_5_2018_pol1 lag_5_2015_pol1 lag_4_2018_pol1 lag_4_2017_pol1 lag_4_2015_pol1    date_0_2018_pol1 date_0_2017_pol1 date_0_2016_pol1 date_0_2015_pol1  
global inter_pol2   lag_5_2018_pol2 lag_5_2015_pol2 lag_4_2018_pol2 lag_4_2017_pol2 lag_4_2015_pol2    date_0_2018_pol2 date_0_2017_pol2 date_0_2016_pol2 date_0_2015_pol2  
*global inter_pol1 lag_6_2018_pol1 lag_6_2016_pol1 lag_5_2018_pol1 lag_5_2015_pol1 lag_4_2018_pol1 lag_4_2017_pol1 lag_4_2015_pol1    date_0_2018_pol1 date_0_2017_pol1 date_0_2016_pol1 date_0_2015_pol1  
*global inter_pol2 lag_6_2018_pol2 lag_6_2016_pol2 lag_5_2018_pol2 lag_5_2015_pol2 lag_4_2018_pol2 lag_4_2017_pol2 lag_4_2015_pol2    date_0_2018_pol2 date_0_2017_pol2 date_0_2016_pol2 date_0_2015_pol2  

global inter_lagsleads_pol1 lag_8_pol1 lag_6_pol1 lag_5_pol1 lag_4_pol1   date_0_pol1
global inter_lagsleads_pol2 lag_8_pol2 lag_6_pol2 lag_5_pol2 lag_4_pol2   date_0_pol2

global inter_reform_pol1 reform_pol1
global inter_reform_pol2 reform_pol2

*foreach i in $controls logdetenidos_2pc acuerdo acuerdo2  acuerdo3 acuerdo4{
*bysort inegi: carryforward $outcome2, replace

*========================================================================
*Run .ado 
do "wild_areg_incumbency.do"
do "macros_tables_incumbency.do"
*========================================================================
*TWFE: Probability of winning in t+1
foreach j in $outcome2 {
foreach pol in 1 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
xi: reghdfe `j' reform pol`pol' $inter_reform_pol1 $controls_naive  i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
boottest {reform}, bootcluster(estado) seed(5675) level(95) boottype(wild)   nograph 
*i) Partisan + Personal 
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol1 $controls_naive  i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest 	_b[reform]/2 
*ii) Personal
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol1 $controls_naive  i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest 	(_b[reform]-_b[_cons])/2 
*iii) Partisan
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol1 $controls_naive  i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest _b[_cons]/2 
}
}

foreach j in $outcome2 {
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
xi: reghdfe `j' reform pol`pol' $inter_reform_pol2 $controls_naive i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
boottest {reform}, bootcluster(estado) seed(5675) level(95) boottype(wild)   nograph 
*i) Partisan + Personal 
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol2 $controls_naive i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest 	_b[reform]/2 
*ii) Personal
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol2 $controls_naive i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest 	(_b[reform]-_b[_cons])/2 
*iii) Partisan
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol2 $controls_naive i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest _b[_cons]/2 
}
}

*FIGURE:
coefplot (est1, rename((1) = "Partisan + Personal") mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Personal") mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Partisan") mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est4, rename((1) = "Partisan + Personal") mfcolor(white)  msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Personal") mfcolor(white) msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "Partisan") mfcolor(white) msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle("Term Limit Reform Effect") ///
subtitle("Probability of winning at t+1") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/twfe_partisan_personal_inc_advantage.png", as(png) replace
graph export "../Figures_incumbency/twfe_partisan_personal_inc_advantage.pdf", as(pdf) replace
graph export "../Figures_incumbency/twfe_partisan_personal_inc_advantage.tif", as(tif) replace
graph save "../Figures_incumbency/twfe_partisan_personal_inc_advantage.gph", replace

*========================================================================
*TWFE: Winning margin in t+1 
est clear
foreach j in $outcome5 {
foreach pol in 1 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
xi: reghdfe `j' reform pol`pol' $inter_reform_pol1  $controls_naive i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
boottest {reform}, bootcluster(estado) seed(5675) level(95) boottype(wild)   nograph 
*i) Partisan + Personal 
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol1 $controls_naive  i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest 	_b[reform]/2 
*ii) Personal
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol1 $controls_naive  i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest 	(_b[reform]-_b[_cons])/2 
*iii) Partisan
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol1 $controls_naive  i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest _b[_cons]/2 
}
}

foreach j in $outcome5 {
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
xi: reghdfe `j' reform pol`pol' $inter_reform_pol2  i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
boottest {reform}, bootcluster(estado) seed(5675) level(95) boottype(wild)   nograph 
*i) Partisan + Personal 
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol2 $controls_naive i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest 	_b[reform]/2 
*ii) Personal
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol2 $controls_naive i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest 	(_b[reform]-_b[_cons])/2 
*iii) Partisan
qui xi: reghdfe `j' reform pol`pol' $inter_reform_pol2 $controls_naive i.year  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
eststo: lincomest _b[_cons]/2 
}
}

*FIGURE:
coefplot (est1, rename((1) = "Partisan + Personal") mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Personal") mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Partisan") mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est4, rename((1) = "Partisan + Personal") mfcolor(white)  msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "Personal") mfcolor(white) msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Partisan") mfcolor(white) msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle("Term Limit Reform Effect") ///
subtitle("Winning margin at t+1") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/twfe_partisan_personal_inc_advantage_margin.png", as(png) replace
graph export "../Figures_incumbency/twfe_partisan_personal_inc_advantage_margin.pdf", as(pdf) replace
graph export "../Figures_incumbency/twfe_partisan_personal_inc_advantage_margin.tif", as(tif) replace
graph save "../Figures_incumbency/twfe_partisan_personal_inc_advantage_margin.gph", replace

*=============================================
*Combine figures:
grc1leg "../Figures_incumbency/twfe_partisan_personal_inc_advantage.gph" "../Figures_incumbency/twfe_partisan_personal_inc_advantage_margin.gph" , ///
scheme(s1color)  imargin(vsmall) ycommon xcommon  col(2) row(1)
graph export "../Figures_incumbency/twfe_personalvspartisan_advantage.png", as(png) replace
graph export "../Figures_incumbency/twfe_personalvspartisan_advantage.pdf", as(pdf) replace
graph export "../Figures_incumbency/twfe_personalvspartisan_advantage.tif", as(tif) replace

*=============================================
*=============================================
*=============================================










