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
	global homicides logdefuncionespc_lag_2 logdefuncionespc_lag_3 logdefuncionespc_lag_4 logdefuncionespc_lag_5 logdefuncionespc_lag_6 logdefuncionespc_lag_7 
	global executive alignment_executive_strong_lag_2 alignment_executive_strong_lag_3 alignment_executive_strong_lag_4 alignment_executive_strong_lag_5 alignment_executive_strong_lag_6 alignment_executive_strong_lag_7 
	global governor alignment_governor_strong_lag_2 alignment_governor_strong_lag_3 alignment_governor_strong_lag_4 alignment_governor_strong_lag_5 alignment_governor_strong_lag_6 alignment_governor_strong_lag_7 
	global margin winning_margin_lag_2 winning_margin_lag_3 winning_margin_lag_4 winning_margin_lag_5 winning_margin_lag_6 winning_margin_lag_7 
	global margin_governor winning_margin_governor_lag_2 winning_margin_governor_lag_3 winning_margin_governor_lag_4 winning_margin_governor_lag_5 winning_margin_governor_lag_6 winning_margin_governor_lag_7 
	global logpop logpop_lag_2 logpop_lag_3 logpop_lag_4 logpop_lag_5 logpop_lag_6 logpop_lag_7 
	global carteles hayCarteles_lag_2 hayCarteles_lag_3 hayCarteles_lag_4 hayCarteles_lag_5 hayCarteles_lag_6 hayCarteles_lag_7 
	global effectiveparties effectiveparties_lag_2 effectiveparties_lag_3 effectiveparties_lag_4 effectiveparties_lag_5 effectiveparties_lag_6 effectiveparties_lag_7 
	global numparties_eff numparties_eff_lag_2 numparties_eff_lag_3 numparties_eff_lag_4 numparties_eff_lag_5 numparties_eff_lag_6 numparties_eff_lag_7 
	global areakm2 areakm2_lag_2 areakm2_lag_3 areakm2_lag_4 areakm2_lag_5 areakm2_lag_6 areakm2_lag_7 
	global pan_mayor2 pan_mayor2_lag_2 pan_mayor2_lag_3 pan_mayor2_lag_4 pan_mayor2_lag_5 pan_mayor2_lag_6 pan_mayor2_lag_7 
	global pri_mayor2 pri_mayor2_lag_2 pri_mayor2_lag_3 pri_mayor2_lag_4 pri_mayor2_lag_5 pri_mayor2_lag_6 pri_mayor2_lag_7 

	global controls_time_acuerdo   $homicides $executive $governor $margin_governor $margin $carteles $logpop $numparties_eff $areakm2 $pan_mayor2 $pri_mayor2
	global controls_time_acuerdo   $homicides $executive $governor $margin_governor $margin $carteles $logpop  $areakm2 $pan_mayor2 $pri_mayor2 // works with pol2
	global controls_time_acuerdo   $homicides $executive $governor $margin_governor $margin $carteles $logpop  // works with pol2

*2) treatment
	global lagsleads    lag_6 lag_5 lag_4 lag_3 lag_2 date_0
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 

	*global saturated date_0_2015 date_0_2016 date_0_2017 date_0_2018 lag_2_2016 lag_3_2015 lag_3_2016 lag_3_2018 lag_4_2015 lag_4_2017 lag_5_2015 lag_5_2018 lag_6_2016 lag_6_2018 lag_8_2018 lead_1_2015 lead_1_2016
	*global saturated2 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lag_2_2016 lag_3_2015 lag_3_2016 lag_3_2018 lag_4_2015 lag_4_2017 lag_5_2015 lag_5_2018 lag_6_2016 lag_6_2018 lead_1_2015 lead_1_2016

*3) outcomes
	global incumbency incumbent_today_w_tomorrow2 incumbent_yesterday_w_tomorrow2 inc_party_won  inc_party_won_tplus1 party_won_nextelec
	*inc_party_won_tplus1  should be identical to incumbent_today_w_tomorrow2. I find they are highly correlated. Former constructed with raceafter and latter with winning margins comparison. I believe the second one is better. 
	*inc_party_won = incumbent at t-1 won at t (could be used as a placebo or to see imediate reform effects)
	global outcome inc_party_won_tplus1 // this is another experiment: where former incumbent wins what happens to new incumbent: in term limited places they win. in reelection nothing.
   	global outcome2 incumbent_yesterday_w_tomorrow2
	global outcome3 party_won_nextelec // for unconditional case 
	global outcome4 inc_party_won // for unconditional case 
	
*========================================================================
*VARIABLE CREATION
est clear
foreach j in incumbent_yesterday_w_tomorrow2 {
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

foreach j in incumbent_yesterday_w_tomorrow2 {
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
global interlags_pol1 lag_7_pol1 lag_6_pol1 lag_5_pol1 lag_4_pol1 lag_3_pol1 lag_2_pol1 date_0_pol1
global interlags_pol2 lag_7_pol2 lag_6_pol2 lag_5_pol2 lag_4_pol2 lag_3_pol2 lag_2_pol2 date_0_pol2
global inter_pol1 date_0_2015_pol1  date_0_2016_pol1  date_0_2017_pol1  date_0_2018_pol1  lag_5_2018_pol1  lag_4_2015_pol1  lag_3_2015_pol1  lag_3_2016_pol1  lag_3_2018_pol1
global inter_pol2 date_0_2015_pol2  date_0_2016_pol2  date_0_2017_pol2  date_0_2018_pol2  lag_5_2018_pol2  lag_4_2015_pol2  lag_3_2015_pol2  lag_3_2016_pol2  lag_3_2018_pol2

*========================================================================
*Run .ado 
do "wild_areg.do"
do "macros_tables.do"
*========================================================================
*NAIVE EVENT STUDY
preserve
xi: reghdfe  acuerdo_estcom lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015 ///
 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
keep if e(sample)==1

est clear
foreach j in $outcome2 {
foreach pol in 1{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
 eststo: xi: reghdfe  `j'  $lagsleads pol`pol' $interlags_pol1  $controls_time_acuerdo i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
}
}

foreach j in $outcome2 {
foreach pol in 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
 eststo: xi: reghdfe  `j'  $lagsleads pol`pol' $interlags_pol2  $controls_time_acuerdo i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
}
}

esttab est*, keep($lagsleads) t(%9.3f)  star(* 0.1 ** 0.05 *** 0.01)
restore 

*========================================================================
*DIFF-IN-DISCONTINUITY OF CLOSE ELECTIONS 

*------------------
*A) CONDITIONAL ON T-1
*------------------
*pol1
est clear
foreach j in $outcome2 {
foreach pol in 1{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
 xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
}
}
		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		*glo constant_`pol': di %5.4f _b[_cons]

***estimate linear combination by lead/lag:


/*foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c')/2 
	test (_b[`i'_2018]*`c')/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2018]*`c')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')/2
	test (_b[`i'_2015]*`a')/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}
*/

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
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
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	
*get the personal effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest [((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')) -(_b[_cons])]/2
*get the partisan effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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


*pol2 
*est clear
foreach j in $outcome2 {
foreach pol in 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		*glo constant_`pol': di %5.4f _b[_cons]

***estimate linear combination by lead/lag:


/*foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c')/2 
	test (_b[`i'_2018]*`c')/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2018]*`c')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')/2
	test (_b[`i'_2015]*`a')/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}
*/

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
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
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	

*get the personal effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest [((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')) -(_b[_cons])]/2
*get the partisan effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
*B) UNCONDITIONAL ON T-1
*------------------
*pol1
est clear
foreach j in $outcome4 {
foreach pol in 1{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
 xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		*glo constant_`pol': di %5.4f _b[_cons]

***estimate linear combination by lead/lag:


foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c')/2 
	test (_b[`i'_2018]*`c')/2  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2018]*`c')/2
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')/2
	test (_b[`i'_2015]*`a')/2 =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')/2
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2 =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
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
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
			

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	

*get the partisan effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') -(_b[_cons]))/2

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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


*pol2 
*est clear
foreach j in $outcome4 {
foreach pol in 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		*glo constant_`pol': di %5.4f _b[_cons]

***estimate linear combination by lead/lag:


foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c')/2 
	test (_b[`i'_2018]*`c')/2  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2018]*`c')/2
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')/2
	test (_b[`i'_2015]*`a')/2 =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a')/2
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2 =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
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
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
			

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	

*get the partisan effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') -(_b[_cons]))/2

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
 (est2, rename((1) = "Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Partisan") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est4, rename((1) = "Partisan + Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Partisan") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
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
 (est2, rename((1) = "Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Partisan") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est7, rename((1) = "Partisan + Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est8, rename((1) = "Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est9, rename((1) = "Partisan") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
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
 (est5, rename((1) = "Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Partisan") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est10, rename((1) = "Partisan + Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est11, rename((1) = "Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est12, rename((1) = "Partisan") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
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
*PARALLEL TREND FIGURE:
est clear
*pol1
foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

***estimate linear combination by lead/lag:
foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal_1} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest  	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest  	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			
}

}
}

*pol2
foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 2 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

***estimate linear combination by lead/lag:
foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest  	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest  	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2
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
ytitle("Probability of winning at t+1")  xtitle(" ") ///
subtitle(" ") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures/incumbency.png", as(png) replace
graph export "../Figures/incumbency.pdf", as(pdf) replace
graph export "../Figures/incumbency.tif", as(tif) replace
graph save "../Figures/incumbency.gph", replace

*=============================================
*McCrary test:
foreach pol in 1 2{
rdbwselect  $outcome2 mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal_`pol' = e(h_CCT)
di ${optimal_`pol'}/6
}


foreach pol in 1{
preserve
set scheme plottig 
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo $margin $homicides incumbent_quality   i.year if mv_incparty<${optimal_`pol'} & mv_incparty>-${optimal_`pol'}, a(inegi) vce(cluster estado)
keep if e(sample)==1
DCdensity mv_incparty, breakpoint(0) generate(Xj Yj r0 fhat se_fhat)
graph export "../Figures/conditional_mccrary_test_pol`pol'.png", as(png) replace
graph save "../Figures/conditional_mccrary_test_pol`pol'.gph", replace
restore
}


foreach pol in 2{
preserve
set scheme plottig 
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo $margin  $homicides  incumbent_quality  i.year if mv_incparty<${optimal_`pol'} & mv_incparty>-${optimal_`pol'}, a(inegi) vce(cluster estado)
keep if e(sample)==1
DCdensity mv_incparty, breakpoint(0) generate(Xj Yj r0 fhat se_fhat) 
graph export "../Figures/conditional_mccrary_test_pol`pol'.png", as(png) replace
graph save "../Figures/conditional_mccrary_test_pol`pol'.gph", replace
restore
}


*combine:
graph combine "../Figures/conditional_mccrary_test_pol1.gph" "../Figures/conditional_mccrary_test_pol2.gph", ///
scheme(s1color)  imargin(vsmall) ycommon  col(1) row(1) l1("Density") b1("Incumbent Party Vote Margin at t") // l2title("linear polynomial") l2title("quadratic polynomial")
graph export "../Figures/conditional_mccrary_test_pol1_final.png", as(png) replace
graph export "../Figures/conditional_mccrary_test_pol1_final.pdf", as(pdf) replace
graph export "../Figures/conditional_mccrary_test_pol1_final.tif", as(tif) replace

*=============================================
*No discontinuous jump of pretreatment covariates
global covariates 	logdefuncionespc alignment_executive_strong alignment_governor_strong  logpop numparties_eff numparties_eff_molinar pri_mayor2 pan_mayor2 morena_mayor2

est clear
foreach j in $covariates {
foreach pol in 1{

***estimate linear combination by lead/lag:
foreach i in date_0 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2    i.year if mv_incparty<${optimal_1} & mv_incparty>-${optimal_1}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2   i.year if mv_incparty<${optimal_1} & mv_incparty>-${optimal_1}, a(inegi) vce(cluster estado)
	eststo: lincomest  	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))
			
}

}
}

*Figure		   
coefplot (est1, rename((1) = "Homicides per capita") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Alignment President") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Alignment Governor") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est4, rename((1) = "log(population)") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est7, rename((1) = "PRI mayor") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est8, rename((1) = "PAN mayor") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est9, rename((1) = "MORENA mayor") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle(" ") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures/nojump1.png", as(png) replace
graph export "../Figures/nojump1.pdf", as(pdf) replace
graph export "../Figures/nojump1.tif", as(tif) replace
graph save "../Figures/nojump1.gph", replace


coefplot  (est5, rename((1) = "Effective Number of Parties") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black)))  ///
 (est6, rename((1) = "Effective Number of Parties-Molinar") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle(" ") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures/nojump2.png", as(png) replace
graph export "../Figures/nojump2.pdf", as(pdf) replace
graph export "../Figures/nojump2.tif", as(tif) replace
graph save "../Figures/nojump2.gph", replace


*combine:
grc1leg "../Figures/nojump1.gph" "../Figures/nojump2.gph" , ///
scheme(s1color)  imargin(vsmall) ycommon  col(1) row(2) l1("Pretreatment covariates") b1("Reform Average Effect in close elections")
graph export "../Figures/nojump.png", as(png) replace
graph export "../Figures/nojump.pdf", as(pdf) replace
graph export "../Figures/nojump.tif", as(tif) replace
*=============================================
*DIFFERENT BANDWIDTHS

*------------------
*A) LINEAR
*------------------
*create splits
cap drop split1 split2 split3 split4 split6
foreach i in 1 2 3 4  6 {
gen split`i'=${optimal_1}/`i'
}

global split split1 split2 split3 split4 split6

est clear
foreach optimal in $split{
foreach j in $outcome2 {
foreach pol in 1 {
foreach i in date_0 {		
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo   i.year if mv_incparty<`optimal' & mv_incparty>-`optimal', a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	

*get the partisan effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo   i.year if mv_incparty<`optimal' & mv_incparty>-`optimal', a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') -(_b[_cons]))/2

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo   i.year if mv_incparty<`optimal' & mv_incparty>-`optimal', a(inegi) vce(cluster estado)
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
}

*Figure COMPOSITION OF THE INCUMBENCY ADVANTAGE ACROSS THRESHOLDS
coefplot (est1, rename((1) = "Partisan + Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Partisan") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est4, rename((1) = "Partisan + Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Partisan") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est7, rename((1) = "Partisan + Personal") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est8, rename((1) = "Personal") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est9, rename((1) = "Partisan") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est10, rename((1) = "Partisan + Personal") msize(large) mcolor(orange) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est11, rename((1) = "Personal") msize(large) mcolor(orange) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est12, rename((1) = "Partisan") msize(large) mcolor(orange) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est13, rename((1) = "Partisan + Personal") msize(large) mcolor(yellow) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est14, rename((1) = "Personal") msize(large) mcolor(yellow) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est15, rename((1) = "Partisan") msize(large) mcolor(yellow) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle("Probability of Victory" "at t+1")  xtitle("Term Limit Reform Average Effect") ///
subtitle("linear polynomial") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" ///
4 "optimal=17%" 20 "optimal/2=8.5%" 32 "optimal/3=5.7%" 40 "optimal/4=4.3%" 60 "optimal/6=2.8%") size(small) rows(3) region(col(white))) 
graph export "../Figures/many_bandwidths_linear.png", as(png) replace
graph export "../Figures/many_bandwidths_linear.pdf", as(pdf) replace
graph export "../Figures/many_bandwidths_linear.tif", as(tif) replace
graph save "../Figures/many_bandwidths_linear.gph", replace


*------------------
*B) QUADRATIC
*------------------
*create splits
cap drop split1 split2 split3 split4 split6
foreach i in 1 2 3 4  6 {
gen split`i'=${optimal_2}/`i'
}


global split split1 split2 split3 split4 split6

est clear
foreach optimal in $split{
foreach j in $outcome2 {
foreach pol in 2 {
foreach i in date_0 {		
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<`optimal' & mv_incparty>-`optimal', a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	

*get the partisan effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<`optimal' & mv_incparty>-`optimal', a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') -(_b[_cons]))/2

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<`optimal' & mv_incparty>-`optimal', a(inegi) vce(cluster estado)
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
}

*Figure COMPOSITION OF THE INCUMBENCY ADVANTAGE ACROSS THRESHOLDS
coefplot (est1, rename((1) = "Partisan + Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Personal") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Partisan") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est4, rename((1) = "Partisan + Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "Personal") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Partisan") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est7, rename((1) = "Partisan + Personal") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est8, rename((1) = "Personal") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est9, rename((1) = "Partisan") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est10, rename((1) = "Partisan + Personal") msize(large) mcolor(orange) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est11, rename((1) = "Personal") msize(large) mcolor(orange) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est12, rename((1) = "Partisan") msize(large) mcolor(orange) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est13, rename((1) = "Partisan + Personal") msize(large) mcolor(yellow) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est14, rename((1) = "Personal") msize(large) mcolor(yellow) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est15, rename((1) = "Partisan") msize(large) mcolor(yellow) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle("Probability of Victory" "at t+1")  xtitle("Term Limit Reform Average Effect") ///
subtitle("quadratic polynomial") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" ///
4 "optimal=17%" 20 "optimal/2=8.5%" 32 "optimal/3=5.7%" 40 "optimal/4=4.3%" 60 "optimal/6=2.8%") size(small) rows(3) region(col(white))) 
graph export "../Figures/many_bandwidths_quadratic.png", as(png) replace
graph export "../Figures/many_bandwidths_quadratic.pdf", as(pdf) replace
graph export "../Figures/many_bandwidths_quadratic.tif", as(tif) replace
graph save "../Figures/many_bandwidths_quadratic.gph", replace

*combine:
grc1leg "../Figures/many_bandwidths_linear.gph" "../Figures/many_bandwidths_quadratic.gph"  , ///
scheme(s1color)  imargin(vsmall) ycommon  col(2) l1("Probability of Victory at t+1") b1(" ")
graph export "../Figures/many_bandwidths.png", as(png) replace
graph export "../Figures/many_bandwidths.pdf", as(pdf) replace
graph export "../Figures/many_bandwidths.tif", as(tif) replace

*=============================================
*INTERACTION WITH PRETREATMENT HOMICIDES PER CAPITA 

est clear
*generate interaction
cap drop inter_*
global split_variable logdefuncionespc_mean
foreach i in $split_variable{
foreach j in $saturated2{
gen inter_`j'=`i'*`j'
}
}

global interaction inter_date_0_2015 inter_date_0_2016 inter_date_0_2017 inter_date_0_2018 inter_lag_2_2016 inter_lag_3_2015 inter_lag_3_2016 inter_lag_3_2018 inter_lag_4_2015 inter_lag_4_2017 inter_lag_5_2015 inter_lag_5_2018 inter_lag_6_2016 inter_lag_6_2018 inter_lead_1_2015 inter_lead_1_2016

*A. LINEAR:
est clear
foreach j in $outcome2 {
foreach pol in 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

 xi: reghdfe  `j'  $saturated2 pol`pol' $inter_pol1 $split_variable $interaction  $controls_time_acuerdo  i.year if mv_incparty<${optimal_1} & mv_incparty>-${optimal_1}, a(inegi) vce(cluster estado)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)

		

***estimate linear combination by lead/lag:
foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c')/2 
	test (_b[`i'_2018]*`c')/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2018]*`c')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')/2
	test (_b[`i'_2015]*`a')/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
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
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
	
	*1) PARTISAN + PERSONAL EFFECT = REFORM AND LOW HOMICIDES
	*store base:
	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d')]/ 2	
	
	*2) PARTISAN + PERSONAL EFFECT = REFORM AND HIGH HOMICIDES
 qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1 $split_variable $interaction  $controls_time_acuerdo  i.year if mv_incparty<${optimal_1} & mv_incparty>-${optimal_1}, a(inegi) vce(cluster estado)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest [(_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d')]/2	
	
	*3) PARTISAN + PERSONAL EFFECT = TOTAL INTERACTION
 qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1 $split_variable $interaction  $controls_time_acuerdo  i.year if mv_incparty<${optimal_1} & mv_incparty>-${optimal_1}, a(inegi) vce(cluster estado)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
	+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d')]/2

}

}
}
*B. QUADRATIC:

foreach j in $outcome2 {
foreach pol in 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

 xi: reghdfe  `j'  $saturated2 pol`pol' $inter_pol2 $split_variable $interaction  $controls_time_acuerdo  i.year if mv_incparty<${optimal_2} & mv_incparty>-${optimal_2}, a(inegi) vce(cluster estado)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)

		

***estimate linear combination by lead/lag:
foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c')/2 
	test (_b[`i'_2018]*`c')/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2018]*`c')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')/2
	test (_b[`i'_2015]*`a')/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
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
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
	
	*1) PARTISAN + PERSONAL EFFECT = REFORM AND LOW HOMICIDES
	*store base:
	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d')]/ 2	
	
	*2) PARTISAN + PERSONAL EFFECT = REFORM AND HIGH HOMICIDES
 qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1 $split_variable $interaction  $controls_time_acuerdo  i.year if mv_incparty<${optimal_1} & mv_incparty>-${optimal_1}, a(inegi) vce(cluster estado)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest [(_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d')]/2	
	
	*3) PARTISAN + PERSONAL EFFECT = TOTAL INTERACTION
 qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1 $split_variable $interaction  $controls_time_acuerdo  i.year if mv_incparty<${optimal_1} & mv_incparty>-${optimal_1}, a(inegi) vce(cluster estado)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
	+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d')]/2

}

}
}

*Figure
coefplot (est1, rename((1) = "Reform & Low Violence") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Interaction (Reform & High Violence)") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
  (est3, rename((1) = "Total Interaction") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
  (est4, rename((1) = "Reform & Low Violence") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
  (est5, rename((1) = "Interaction (Reform & High Violence)") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
  (est6, rename((1) = "Total Interaction") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)  ///
ytitle(" ")  xtitle(" ") ///
subtitle(" ") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) size(small) rows(2) region(col(white))) 
graph export "../Figures/did_rd_homicide_interaction.png", as(png) replace
graph export "../Figures/did_rd_homicide_interaction.pdf", as(pdf) replace
graph export "../Figures/did_rd_homicide_interaction.tif", as(tif) replace
graph save "../Figures/did_rd_homicide_interaction.gph", replace

*=============================================
*INTERACTION WITH PRETREATMENT HOMICIDES PER CAPITA: INCUMBENCY PARTISAN AND PERSONSAL DECOMPOSITION

est clear
*generate interaction
cap drop inter_*
global split_variable logdefuncionespc_mean
foreach i in $split_variable{
foreach j in $saturated2{
gen inter_`j'=`i'*`j'
}
}

global interaction inter_date_0_2015 inter_date_0_2016 inter_date_0_2017 inter_date_0_2018 inter_lag_2_2016 inter_lag_3_2015 inter_lag_3_2016 inter_lag_3_2018 inter_lag_4_2015 inter_lag_4_2017 inter_lag_5_2015 inter_lag_5_2018 inter_lag_6_2016 inter_lag_6_2018 inter_lead_1_2015 inter_lead_1_2016

*A. LINEAR:
est clear
foreach j in $outcome2 {
foreach pol in 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

 xi: reghdfe  `j'  $saturated2 pol`pol' $inter_pol1 $split_variable $interaction  $controls_time_acuerdo  i.year if mv_incparty<${optimal_1} & mv_incparty>-${optimal_1}, a(inegi) vce(cluster estado)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)

		

***estimate linear combination by lead/lag:
foreach i in date_0 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
	
	*1) PARTISAN + PERSONAL EFFECT = REFORM AND LOW HOMICIDES
	*store base:
	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d')]/ 2	
	
	*2) PARTISAN + PERSONAL EFFECT = REFORM AND HIGH HOMICIDES
 qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1 $split_variable $interaction  $controls_time_acuerdo  i.year if mv_incparty<${optimal_1} & mv_incparty>-${optimal_1}, a(inegi) vce(cluster estado)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest [(_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d')]/2	
	
	*3) PARTISAN + PERSONAL EFFECT = TOTAL INTERACTION
 qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1 $split_variable $interaction  $controls_time_acuerdo  i.year if mv_incparty<${optimal_1} & mv_incparty>-${optimal_1}, a(inegi) vce(cluster estado)
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	eststo: lincomest 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
	+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d')]/2

}

}
}



*=============================================
*Refinements: multiply by percentage of reelected
			

*========================================================================
*========================================================================
*=============================OLD========================================
*========================================================================
*========================================================================
/*========================================================================
*========================================================================
*SPLITTING SAMPLE FOR HOMICIDES PRE-TREATMENT

*------------------
*A) BELOW HOMICIDES MEAN
*------------------
est clear
foreach j in $outcome2 {
foreach pol in 1{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		*glo constant_`pol': di %5.4f _b[_cons]

***estimate linear combination by lead/lag:


foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c')/2 
	test (_b[`i'_2018]*`c')/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2018]*`c')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')/2
	test (_b[`i'_2015]*`a')/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
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
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	

*get the partisan effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') -(_b[_cons]))/2

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
*------------------
*B) CONDITIONAL
*------------------
foreach j in $outcome2 {
foreach pol in 1{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo $homicides i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		*glo constant_`pol': di %5.4f _b[_cons]

***estimate linear combination by lead/lag:


foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c')/2 
	test (_b[`i'_2018]*`c')/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2018]*`c')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')/2
	test (_b[`i'_2015]*`a')/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
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
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo $homicides i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	

*get the partisan effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo $homicides i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') -(_b[_cons]))/2

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo $homicides i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
4 "UNCONDITIONAL" 20 "CONDITIONAL" ) rows(2)) 
graph export "../Figures/condition_by_homicides.png", as(png) replace
graph export "../Figures/condition_by_homicides.pdf", as(pdf) replace
graph export "../Figures/condition_by_homicides.tif", as(tif) replace
graph save "../Figures/condition_by_homicides.gph", replace
*========================================================================
*SPLITTING SAMPLE FOR HOMICIDES PRE-TREATMENT

*------------------
*A) BELOW HOMICIDES MEAN
*------------------
est clear
preserve
sum logdefuncionespc_mean
return list
local outcome_mean = r(mean)
keep if logdefuncionespc_mean < `outcome_mean'

foreach j in $outcome2 {
foreach pol in 1{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		*glo constant_`pol': di %5.4f _b[_cons]

***estimate linear combination by lead/lag:


foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c')/2 
	test (_b[`i'_2018]*`c')/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2018]*`c')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')/2
	test (_b[`i'_2015]*`a')/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
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
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	

*get the partisan effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') -(_b[_cons]))/2

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
restore
*------------------
*B) ABOVE HOMICIDES MEAN
*------------------
preserve
sum logdefuncionespc_mean
return list
local outcome_mean = r(mean)
keep if logdefuncionespc_mean >= `outcome_mean'

foreach j in $outcome2 {
foreach pol in 1{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		*glo constant_`pol': di %5.4f _b[_cons]

***estimate linear combination by lead/lag:


foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c')/2 
	test (_b[`i'_2018]*`c')/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2018]*`c')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	(_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')/2
	test (_b[`i'_2015]*`a')/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	(_b[`i'_2015]*`a')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2 =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c'))/2
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
	di ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	test ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2 
	

*get the partisan effect
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	eststo: lincomest ((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') -(_b[_cons]))/2

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
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
restore

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
4 "BELOW MEAN" 20 "ABOVE MEAN" ) rows(2)) 
graph export "../Figures/split_by_homicides.png", as(png) replace
graph export "../Figures/split_by_homicides.pdf", as(pdf) replace
graph export "../Figures/split_by_homicides.tif", as(tif) replace
graph save "../Figures/split_by_homicides.gph", replace



*========================================================================
*LOAD DATASET:
use "../../Data/ConstructionDatabase/data_final.dta", clear

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
global inter_pol1 lag_7_2017_pol1 lag_7_2018_pol1 lag_6_2016_pol1 lag_6_2017_pol1 lag_6_2018_pol1 lag_5_2015_pol1 lag_5_2016_pol1 lag_5_2017_pol1 lag_5_2018_pol1 lag_4_2015_pol1 lag_4_2016_pol1 lag_4_2017_pol1 lag_4_2018_pol1 lag_3_2015_pol1 lag_3_2016_pol1 lag_3_2017_pol1 lag_3_2018_pol1 lag_2_2015_pol1 lag_2_2016_pol1 lag_2_2017_pol1 lag_2_2018_pol1 date_0_2015_pol1 date_0_2016_pol1 date_0_2017_pol1 date_0_2018_pol1 lead_1_2015_pol1 lead_1_2016_pol1 lead_1_2017_pol1 lead_2_2015_pol1 lead_2_2016_pol1 lead_3_2015_pol1
global inter_pol2 lag_7_2017_pol2 lag_7_2018_pol2 lag_6_2016_pol2 lag_6_2017_pol2 lag_6_2018_pol2 lag_5_2015_pol2 lag_5_2016_pol2 lag_5_2017_pol2 lag_5_2018_pol2 lag_4_2015_pol2 lag_4_2016_pol2 lag_4_2017_pol2 lag_4_2018_pol2 lag_3_2015_pol2 lag_3_2016_pol2 lag_3_2017_pol2 lag_3_2018_pol2 lag_2_2015_pol2 lag_2_2016_pol2 lag_2_2017_pol2 lag_2_2018_pol2 date_0_2015_pol2 date_0_2016_pol2 date_0_2017_pol2 date_0_2018_pol2 lead_1_2015_pol2 lead_1_2016_pol2 lead_1_2017_pol2 lead_2_2015_pol2 lead_2_2016_pol2 lead_3_2015_pol2


*ESTIMATES:
est clear
eststo: xi: reghdfe  $outcome2  $saturated pol1 $inter_pol1 $controls_time_acuerdo i.year if mv_incparty<${optimal_1}/3 & mv_incparty>-${optimal_1}/3, a(inegi) vce(cluster estado)
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
eststo: xi: reghdfe  $outcome2  $saturated pol2 $inter_pol2 $controls_time_acuerdo i.year if mv_incparty<${optimal_2} & mv_incparty>-${optimal_2}, a(inegi) vce(cluster estado)
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


