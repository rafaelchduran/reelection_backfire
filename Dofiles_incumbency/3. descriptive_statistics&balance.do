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
*KEEP REAL DATASET
est clear

foreach band in CCT{
foreach n in 1{
foreach j in $outcome2{
*A) Linear polynomial
foreach pol in 1 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(`band') 
global optimal = e(h_CCT)/`n'

eststo: xi: reghdfe  `j'  pol`pol' reform inc_party_won interaction_ref    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
}

}
}
}
esttab est*, keep(reform inc_party_won interaction_ref) star(* 0.1 ** 0.05 *** 0.01) t

keep if e(sample)==1

*========================================================================
*SET PANEL
xtset inegi year 

*========================================================================
*TABLES
est clear
*--------------------*
*Incumbency advantage:
*--------------------*
label variable incumbent_yesterday_w_tomorrow2 "Probability of winning, election at t+1"
label variable mv_incpartyfor1 "Vote share, election at t+1"

global outcomes incumbent_yesterday_w_tomorrow2 mv_incpartyfor1

estpost sum  $outcomes
est store table1

esttab using "../Tables_incumbency/inc_advantage.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

*--------------------*
*Treatments and Forcing Variable:
*--------------------*
label variable reform "Term Limit Removed=1; 0 otherwise"
label variable inc_party_won "Win election at t=1; 0 otherwise"
global treatments reform inc_party_won 
global forcing winning_margin
estpost sum  $treatments
est store table_treatments

esttab using "../Tables_incumbency/treatments.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(0)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

estpost sum  $forcing
est store table_forcing

esttab using "../Tables_incumbency/forcing.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

*--------------------*
*Controls:
*--------------------*
label variable pan_mayor2 "PAN mayor=1; 0 otherwise"
label variable pri_mayor2 "PRI mayor=1; 0 otherwise"
label variable hayCarteles "Incidence of Cartel Presence"

global controls winning_margin_governor alignment_executive_strong alignment_governor_strong  pan_mayor2 pri_mayor2 
estpost sum  $controls
est store table_controls

esttab using "../Tables_incumbency/covariates_statelevel.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(0)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

label variable pop "Population (INEGI and CONAPO projections)"

estpost sum pop 
est store table5

esttab using "../Tables_incumbency/population.tex", replace  cells("mean(fmt(%11.2gc)) sd(fmt(%11.2gc)) min(fmt(0)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)


*--------------------*
*Mechanisms: transfers
*--------------------*
global transfers participaciones2 participables2 fomento2 aportaciones2 fa_infra2 fa_fortalecer2
	foreach i in $transfers{
	replace `i'=`i'/1000000
	}
global outcome1 participaciones2
global outcome2 participables2 
global outcome3 fomento2
global outcome4 aportaciones2
global outcome5 fa_infra2
global outcome6 fa_fortalecer2
	
label variable participaciones2 "General Participations Fund (Mill. pesos)"
label variable participables2 "Participating Fund (Mill. pesos)"
label variable fomento2 "Municipal Development Fund (Mill. pesos)"
label variable aportaciones2 "Federal and state contributions (Mill. pesos)"
label variable fa_infra2 "Contribution Fund for Municipal Social Infrastructure (Mill. pesos)"
label variable fa_fortalecer2 "Contribution Fund to Strengthen Municipalities (Mill. pesos)"
	
estpost sum $transfers
est store table3

esttab using "../Tables_incumbency/transfers.tex", replace  cells("mean(fmt(%11.2gc)) sd(fmt(%11.2gc)) min(fmt(%11.2gc)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

*--------------------*
*Mechanisms: revenues
*--------------------*
	global revenues ingresos2 impuestos2 impuesto_predial2 patrimonio2 produccion2 tenencia2 carros2 seguridad_ing2
	foreach i in $revenues{
	replace `i'=`i'/1000000
	}

label variable ingresos2 "Total Municipal Revenues (Mill. pesos)"
label variable impuestos2 "Tax Revenues (Mill. pesos)"
label variable impuesto_predial2 "Property Tax Rrevenues (Mill. pesos)"
label variable patrimonio2 "Estate Tax Revenues (Mill. pesos)"
label variable produccion2 "Prod., Cons. and Trans. Tax Revenues (Mill. pesos)"
label variable tenencia2 "Vehicle Ownership Tax Revenues (Mill. pesos)"
label variable carros2 "New Cars Tax Revenues (Mill. pesos)"
label variable seguridad_ing2 "Public Security Revenues (Mill. pesos)"

estpost sum $revenues
est store table4

esttab using "../Tables_incumbency/revenues.tex", replace  cells("mean(fmt(%11.2gc)) sd(fmt(%11.2gc)) min(fmt(%11.2gc)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

*--------------------*
*Mechanisms: other resources
*--------------------*
	foreach i in t_person{
	replace `i'=(`i'/pop)*100000
	}
	
label variable t_person "Bureaucrats per 100,000 inhabitants"
label variable tot_ses "Number of City Council Sessions"
label variable tot_inic "Number of Approved Initiatives of Law"
label variable por_egre "Percentage of Municipal Budget Spend"
global other tot_ses tot_inic por_egre t_person

estpost sum $other
est store table4

esttab using "../Tables_incumbency/other.tex", replace  cells("mean(fmt(%11.2gc)) sd(fmt(%11.2gc)) min(fmt(%11.2gc)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

	
*--------------------*
*Alternative mechanisms:
*--------------------*
label variable incumbent_quality "Incumbent undergraduate or graduate title (indicator)"

global quality incumbent_quality
estpost sum  $quality
est store table4

esttab using "../Tables_incumbency/desc_quality.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

  

*========================================================================
*========================================================================
*========================================================================
*========================================================================
*========================================================================
