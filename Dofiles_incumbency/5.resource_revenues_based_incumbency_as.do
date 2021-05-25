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
	global controls_time_acuerdo  presidential_election pan_president  $margin $governor $margin_governor $effectiveparties $logpop
	global controls_naive logdefuncionespc_mean alignment_executive_strong_mean alignment_governor_strong_mean winning_margin_mean winning_margin_governor_mean pan_mayor2_mean pri_mayor2_mean hayCarteles_mean logpop_mean

*2) treatment
	global lagsleads  lag_8 lag_6 lag_5 lag_4   date_0 // comparison group is lag_3
	*global saturated  lag_5_2018 lag_5_2015 lag_4_2018 lag_4_2017 lag_4_2015  date_0_2018 date_0_2017 date_0_2016 date_0_2015 // removed lag_3_2018 lag_3_2016 lag_3_2015 to be the comparison group AND lag_8_2018 because of collinearity following Abraham and Sun (2021)
	*global saturated  lag_6_2018 lag_6_2016 lag_5_2018 lag_5_2015 lag_4_2018 lag_4_2017 lag_4_2015  date_0_2018 date_0_2017 date_0_2016 date_0_2015 // removed lag_3_2018 lag_3_2016 lag_3_2015 to be the comparison group AND lag_8_2018 because of collinearity following Abraham and Sun (2021)
	global saturated  lag_8_2018 lag_6_2018 lag_6_2016 lag_5_2018 lag_5_2015 lag_4_2018 lag_4_2017 lag_4_2015  date_0_2018 date_0_2017 date_0_2016 date_0_2015 // removed lag_3_2018 lag_3_2016 lag_3_2015 to be the comparison group AND lag_8_2018 because of collinearity following Abraham and Sun (2021)
	global saturated2  date_0_2018 date_0_2017 date_0_2016 date_0_2015 // removed lag_3_2018 lag_3_2016 lag_3_2015 to be the comparison group AND lag_8_2018 because of collinearity following Abraham and Sun (2021)

*3) outcomes
	global outcomes participaciones2 participables2 fomento2 aportaciones2 fa_infra2 fa_fortalecer2
	foreach i in $outcomes{
	replace `i'=`i'/1000000
	}
	global outcome1 participaciones2
	global outcome2 participables2 
	global outcome3 fomento2
	global outcome4 aportaciones2
	global outcome5 fa_infra2
	global outcome6 fa_fortalecer2

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

global inter_lagsleads_pol1 lag_8_pol1 lag_6_pol1 lag_5_pol1 lag_4_pol1   date_0_pol1
global inter_lagsleads_pol2 lag_8_pol2 lag_6_pol2 lag_5_pol2 lag_4_pol2   date_0_pol2

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
*Naive event study
preserve
est clear
foreach i in $outcomes{
rdbwselect  `i' mv_incparty, c(0) p(1) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
eststo: qui reghdfe `i' $lagsleads $controls_time_acuerdo pol1 $inter_lagsleads_pol1 i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} ///
, a(inegi) vce(cluster estado)

rdbwselect  `i' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
eststo: qui reghdfe `i' $lagsleads $controls_time_acuerdo pol2 $inter_lagsleads_pol2 i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} ///
, a(inegi)  vce(cluster estado)
}

esttab est*, keep($lagsleads) t(%9.3f)  star(* 0.1 ** 0.05 *** 0.01)
restore

*=============================================
*AS PARALLEL TREND: 

*participaciones
est clear
*pol1
foreach j in $outcome1 {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}
foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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



*pol2
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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


*Figure: 
coefplot (est1, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est2, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est3, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est4, rename((1) = "Reform") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est5, rename((1) = "t-6") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est6, rename((1) = "t-5") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est7, rename((1) = "t-4") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est8, rename((1) = "Reform") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel A: General Participation Fund") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/participaciones.png", as(png) replace
graph export "../Figures_incumbency/participaciones.pdf", as(pdf) replace
graph export "../Figures_incumbency/participaciones.tif", as(tif) replace
graph save "../Figures_incumbency/participaciones.gph", replace

**participables
est clear
*pol1
foreach j in $outcome2 {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
 xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}
foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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



*pol2
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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
coefplot (est1, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est2, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est2, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est4, rename((1) = "Reform") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est5, rename((1) = "t-6") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est6, rename((1) = "t-5") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
  (est6, rename((1) = "t-4") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
(est8, rename((1) = "Reform") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel B: Participating Funds") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/participables.png", as(png) replace
graph export "../Figures_incumbency/participables.pdf", as(pdf) replace
graph export "../Figures_incumbency/participables.tif", as(tif) replace
graph save "../Figures_incumbency/participables.gph", replace

**fomento
est clear
*pol1
foreach j in $outcome3 {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}
foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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



*pol2
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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
coefplot (est1, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est2, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est3, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est4, rename((1) = "Reform") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est5, rename((1) = "t-6") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est6, rename((1) = "t-5") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est6, rename((1) = "t-4") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est8, rename((1) = "Reform") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel C: Municipal Development Fund") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/fomento.png", as(png) replace
graph export "../Figures_incumbency/fomento.pdf", as(pdf) replace
graph export "../Figures_incumbency/fomento.tif", as(tif) replace
graph save "../Figures_incumbency/fomento.gph", replace

** Aportaciones federales y estatales
est clear
*pol1
foreach j in $outcome4 {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}
foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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



*pol2
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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
coefplot (est1, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est2, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est3, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est4, rename((1) = "Reform") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est5, rename((1) = "t-6") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est6, rename((1) = "t-5") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est7, rename((1) = "t-4") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est8, rename((1) = "Reform") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel D: Federal and state contributions") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/aportaciones.png", as(png) replace
graph export "../Figures_incumbency/aportaciones.pdf", as(pdf) replace
graph export "../Figures_incumbency/aportaciones.tif", as(tif) replace
graph save "../Figures_incumbency/aportaciones.gph", replace

**FA para el Fortalecimiento de los Municipios
est clear
*pol1
foreach j in $outcome6 {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}
foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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



*pol2
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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
coefplot (est1, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est2, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est3, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est4, rename((1) = "Reform") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est5, rename((1) = "t-6") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est6, rename((1) = "t-5") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
  (est7, rename((1) = "t-4") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est8, rename((1) = "Reform") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel E: Contribution Fund to Strengthen Municipalities") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/fortalecimiento.png", as(png) replace
graph export "../Figures_incumbency/fortalecimiento.pdf", as(pdf) replace
graph export "../Figures_incumbency/fortalecimiento.tif", as(tif) replace
graph save "../Figures_incumbency/fortalecimiento.gph", replace

** FA para la Infraestructura Social Municipal
est clear
*pol1
foreach j in $outcome5 {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)

***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}
foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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



*pol2
foreach pol in 2 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
***estimate linear combination by lead/lag:
foreach i in lag_6 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2016]*`b'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_5 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')   =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	(_b[`i'_2015]*`a'+_b[`i'_2018]*`c')/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}


foreach i in lag_4 {
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"
qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol2  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	eststo: lincomest 	((_b[`i'_2015]*`a')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') )/2
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
coefplot (est1, rename((1) = "t-6") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est2, rename((1) = "t-5") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est3, rename((1) = "t-4") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est4, rename((1) = "Reform") msize(small) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est5, rename((1) = "t-6") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est6, rename((1) = "t-5") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black))) ///
 (est7, rename((1) = "t-4") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 (est8, rename((1) = "Reform") msize(small) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *1 *3) color(black black black)))  ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle(" ")  xtitle(" ") ///
subtitle("Panel F: Contribution Fund for Municipal Social Infrastructure") legend(order(1 "99% CI" 1 "95% CI" 3 "90% CI" ///
4 "linear" 20 "quadratic" ) rows(2)) 
graph export "../Figures_incumbency/infra_social.png", as(png) replace
graph export "../Figures_incumbency/infra_social.pdf", as(pdf) replace
graph export "../Figures_incumbency/infra_social.tif", as(tif) replace
graph save "../Figures_incumbency/infra_social.gph", replace



****COMBINE
*combine:
grc1leg "../Figures_incumbency/participaciones.gph" "../Figures_incumbency/participables.gph" ///
"../Figures_incumbency/fomento.gph" "../Figures_incumbency/aportaciones.gph" ///
"../Figures_incumbency/fortalecimiento.gph" "../Figures_incumbency/infra_social.gph", ///
scheme(s1color)  imargin(vsmall)  col(2)  l1("Million pesos ") b1(" ")
graph export "../Figures_incumbency/resouce_based_incumbency.png", as(png) replace
graph export "../Figures_incumbency/resouce_based_incumbency.pdf", as(pdf) replace
graph export "../Figures_incumbency/resouce_based_incumbency.tif", as(tif) replace



