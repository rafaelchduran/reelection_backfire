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
	global homicides logdefuncionespc_lag_2 logdefuncionespc_lag_3 logdefuncionespc_lag_4 logdefuncionespc_lag_5 logdefuncionespc_lag_6 logdefuncionespc_lag_7 logdefuncionespc_lag_8
	global executive alignment_executive_strong_lag_2 alignment_executive_strong_lag_3 alignment_executive_strong_lag_4 alignment_executive_strong_lag_5 alignment_executive_strong_lag_6 alignment_executive_strong_lag_7 alignment_executive_strong_lag_8
	global governor alignment_governor_strong_lag_2 alignment_governor_strong_lag_3 alignment_governor_strong_lag_4 alignment_governor_strong_lag_5 alignment_governor_strong_lag_6 alignment_governor_strong_lag_7 alignment_governor_strong_lag_8
	global margin winning_margin_lag_2 winning_margin_lag_3 winning_margin_lag_4 winning_margin_lag_5 winning_margin_lag_6 winning_margin_lag_7 winning_margin_lag_8
	global margin_governor winning_margin_governor_lag_2 winning_margin_governor_lag_3 winning_margin_governor_lag_4 winning_margin_governor_lag_5 winning_margin_governor_lag_6 winning_margin_governor_lag_7 winning_margin_governor_lag_8
	global acuerdo acuerdo_lag_2 acuerdo_lag_3 acuerdo_lag_4 acuerdo_lag_5 acuerdo_lag_6 acuerdo_lag_7 acuerdo_lag_8
	global acuerdo2 acuerdo2_lag_2 acuerdo2_lag_3 acuerdo2_lag_4 acuerdo2_lag_5 acuerdo2_lag_6 acuerdo2_lag_7 acuerdo2_lag_8
	global logpop logpop_lag_2 logpop_lag_3 logpop_lag_4 logpop_lag_5 logpop_lag_6 logpop_lag_7 logpop_lag_8
	global carteles hayCarteles_lag_2 hayCarteles_lag_3 hayCarteles_lag_4 hayCarteles_lag_5 hayCarteles_lag_6 hayCarteles_lag_7 hayCarteles_lag_8

	global controls_time_acuerdo  $homicides $executive $governor $margin $margin_governor $logpop $carteles
	global controls_time_acuerdo  

*2) treatment
	global lagsleads   lag_7 lag_6 lag_5 lag_4 lag_3 lag_2  date_0 lead_1 lead_2 lead_3
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
    global saturated2 lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 lag_1_2015 lag_1_2016 lag_1_2017 lag_1_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
    global trim lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

*3) outcomes
	global outcomes seguridad_egr2 seguridad_ing2 desarrollo_soc_egr2 obras_egr2 remun_egr2 ingresos2 impuesto_predial2 impuestos2 patrimonio2 produccion2 tenencia2 carros2
	foreach i in $outcomes{
	replace `i'=`i'/1000000
	}
	global outcome1 seguridad_egr2
	global outcome2 seguridad_ing2 
	global outcome3 desarrollo_soc_egr2
	global outcome4 obras_egr2
	global outcome5 remun_egr2
	global outcome6 ingresos2
	global outcome7 impuesto_predial2
	global outcome8 impuestos2
	global outcome9 patrimonio2
	global outcome10 produccion2
	global outcome11 tenencia2
	global outcome12 carros2

*========================================================================
*VARIABLE CREATION
est clear
foreach j in $outcome1 {
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
}
}

foreach j in  $outcome1 {
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


}
}

*polynomial globals
global inter_pol1   lag_5_2018_pol1 lag_5_2015_pol1 lag_4_2018_pol1 lag_4_2017_pol1 lag_4_2015_pol1    date_0_2018_pol1 date_0_2017_pol1 date_0_2016_pol1 date_0_2015_pol1  
global inter_pol2   lag_5_2018_pol2 lag_5_2015_pol2 lag_4_2018_pol2 lag_4_2017_pol2 lag_4_2015_pol2    date_0_2018_pol2 date_0_2017_pol2 date_0_2016_pol2 date_0_2015_pol2  
*global inter_pol1 lag_6_2018_pol1 lag_6_2016_pol1 lag_5_2018_pol1 lag_5_2015_pol1 lag_4_2018_pol1 lag_4_2017_pol1 lag_4_2015_pol1    date_0_2018_pol1 date_0_2017_pol1 date_0_2016_pol1 date_0_2015_pol1  
*global inter_pol2 lag_6_2018_pol2 lag_6_2016_pol2 lag_5_2018_pol2 lag_5_2015_pol2 lag_4_2018_pol2 lag_4_2017_pol2 lag_4_2015_pol2    date_0_2018_pol2 date_0_2017_pol2 date_0_2016_pol2 date_0_2015_pol2  
global inter_pol1_2   date_0_2018_pol1 date_0_2017_pol1 date_0_2016_pol1 date_0_2015_pol1  
global inter_pol2_2   date_0_2017_pol2 date_0_2016_pol2 date_0_2015_pol2  

global inter_lagsleads_pol1 lag_7_pol1 lag_6_pol1 lag_5_pol1 lag_4_pol1 lag_3_pol1  date_0_pol1
global inter_lagsleads_pol2 lag_7_pol2 lag_6_pol2 lag_5_pol2 lag_4_pol2  lag_3_pol2 date_0_pol2

foreach i in pol1 pol2{
foreach var in  reform {
gen `var'_`i'=`var'*`i'
}
}
*========================================================================
*SET MATSIZE
set matsize 11000 
sort inegi year
*========================================================================
/*Naive event study
preserve
est clear
foreach i in $outcomes{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
eststo: qui reghdfe `i' $lagsleads $controls_time_acuerdo pol1 $inter_lagsleads_pol1 i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} ///
, a(inegi) vce(cluster estado)

rdbwselect  `j' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
eststo: qui reghdfe `i' $lagsleads $controls_time_acuerdo pol2 $inter_lagsleads_pol2 i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} ///
, a(inegi)  vce(cluster estado)
}

esttab est*, keep($lagsleads) t(%9.3f)  star(* 0.1 ** 0.05 *** 0.01)
restore
*/
*=============================================
*Decrease sample

*=============================================
*AS PARALLEL TREND: 

*Expenses security
preserve
est clear
foreach j in $outcome1{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel A: Expenses in " "public security") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/exp_security_allyears.png", as(png) replace
graph export "../Figures_incumbency/exp_security_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/exp_security_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/exp_security_allyears.gph", replace
restore


**Expenses: social security
preserve
est clear
foreach j in $outcome3{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel B: Expenses in social" "development") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/expenses_social_allyears.png", as(png) replace
graph export "../Figures_incumbency/expenses_social_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/expenses_social_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/expenses_social_allyears.gph", replace
restore

** Infrastructure
preserve
est clear
foreach j in $outcome4{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel C: Expenses in" "infrastructure") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/expenses_infrastructure_allyears.png", as(png) replace
graph export "../Figures_incumbency/expenses_infrastructure_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/expenses_infrastructure_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/expenses_infrastructure_allyears.gph", replace
restore


**Wages
preserve
est clear
foreach j in $outcome5{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel D: Wages to bureaucrats") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/expenses_wages_allyears.png", as(png) replace
graph export "../Figures_incumbency/expenses_wages_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/expenses_wages_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/expenses_wages_allyears.gph", replace
restore



***REVENUES
** Reveneus
preserve
est clear
foreach j in $outcome6{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  $outcome1  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel A: Total Municipal Revenues") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/revenues_allyears.png", as(png) replace
graph export "../Figures_incumbency/revenues_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/revenues_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/revenues_allyears.gph", replace
restore


**Revenues Security
preserve
est clear
foreach j in $outcome2{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel H: Public security Revenues") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/ing_security_allyears.png", as(png) replace
graph export "../Figures_incumbency/ing_security_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/ing_security_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/ing_security_allyears.gph", replace
restore

** Predial
preserve
est clear
foreach j in $outcome7{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  $outcome1  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel C: Property Tax Revenues") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/propertytax_allyears.png", as(png) replace
graph export "../Figures_incumbency/propertytax_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/propertytax_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/propertytax_allyears.gph", replace
restore

** Taxes
preserve
est clear
foreach j in $outcome8{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  $outcome1  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel B: Tax Revenues") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/taxes_allyears.png", as(png) replace
graph export "../Figures_incumbency/taxes_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/taxes_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/taxes_allyears.gph", replace
restore

** Patrimonio
preserve
est clear
foreach j in $outcome9{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  $outcome1  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel D: Estate Tax Revenues") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/estate_allyears.png", as(png) replace
graph export "../Figures_incumbency/estate_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/estate_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/estate_allyears.gph", replace
restore

** Production
preserve
est clear
foreach j in $outcome10{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  $outcome1  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel E: Production, Consumption and" "Transactions Tax Revenues") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/production_allyears.png", as(png) replace
graph export "../Figures_incumbency/production_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/production_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/production_allyears.gph", replace
restore

** Tenencia
preserve
est clear
foreach j in $outcome11{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  $outcome1  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel F: Vehicle Ownership" "Tax Revenues (Tenencia)") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/tenencia_allyears.png", as(png) replace
graph export "../Figures_incumbency/tenencia_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/tenencia_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/tenencia_allyears.gph", replace
restore


** New cars
preserve
est clear
foreach j in $outcome12{
rdbwselect  `j' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
keep if mv_incparty<${optimal} & mv_incparty>-${optimal}
foreach i in lag_7{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
xi: reghdfe  $outcome1  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
xi: reghdfe  `j'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}


			   

			   

}			   
coefplot (est1, rename((1) = "t-7") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est2, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est3, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est4, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est5, rename((1) = "t-3") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est6, rename((1) = "t-2") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black))) ///
 (est7, rename((1) = "t=0") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est8, rename((1) = "t+1") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est9, rename((1) = "t+2") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 (est10, rename((1) = "t+3") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.1 *1 *2) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel G: New Cars Tax Revenues") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures_incumbency/newcars_allyears.png", as(png) replace
graph export "../Figures_incumbency/newcars_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/newcars_allyears.tif", as(tif) replace
graph save "../Figures_incumbency/newcars_allyears.gph", replace
restore


****COMBINE: EXPENSES
*combine:
grc1leg "../Figures_incumbency/exp_security_allyears.gph" "../Figures_incumbency/expenses_wages_allyears.gph", ///
scheme(s1color)  imargin(vsmall)  col(2)  l1("Million pesos ") b1(" ")
graph export "../Figures_incumbency/expenses_allyears.png", as(png) replace
graph export "../Figures_incumbency/expenses_allyears.pdf", as(pdf) replace
graph export "../Figures_incumbency/expenses_allyears.tif", as(tif) replace
   
****COMBINE: REVENUES
*combine:
grc1leg   "../Figures_incumbency/revenues_allyears.gph" "../Figures_incumbency/taxes_allyears.gph"  ///
"../Figures_incumbency/propertytax_allyears.gph"  "../Figures_incumbency/estate_allyears.gph", ///
scheme(s1color)  imargin(vsmall)  col(2)  l1("Million pesos ") b1(" ")
graph export "../Figures_incumbency/revenues_allyears1.png", as(png) replace
graph export "../Figures_incumbency/revenues_allyears1.pdf", as(pdf) replace
graph export "../Figures_incumbency/revenues_allyears1.tif", as(tif) replace

*combine:
grc1leg   "../Figures_incumbency/production_allyears.gph" "../Figures_incumbency/tenencia_allyears.gph"  ///
"../Figures_incumbency/newcars_allyears.gph" "../Figures_incumbency/ing_security_allyears.gph", ///
scheme(s1color)  imargin(vsmall)  col(2)  l1("Million pesos ") b1(" ")
graph export "../Figures_incumbency/revenues_allyears2.png", as(png) replace
graph export "../Figures_incumbency/revenues_allyears2.pdf", as(pdf) replace
graph export "../Figures_incumbency/revenues_allyears2.tif", as(tif) replace





