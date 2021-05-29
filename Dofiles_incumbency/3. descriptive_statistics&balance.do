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
*VARIABLE CREATION AND GLOBALS
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

foreach j in  incumbent_yesterday_w_tomorrow2 {
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
global inter_pol1   lag_5_2018_pol1 lag_5_2015_pol1 lag_4_2018_pol1 lag_4_2017_pol1 lag_4_2015_pol1    date_0_2018_pol1 date_0_2017_pol1 date_0_2016_pol1 date_0_2015_pol1  
global inter_pol2   lag_5_2018_pol2 lag_5_2015_pol2 lag_4_2018_pol2 lag_4_2017_pol2 lag_4_2015_pol2    date_0_2018_pol2 date_0_2017_pol2 date_0_2016_pol2 date_0_2015_pol2  
global inter_pol1 lag_6_2018_pol1 lag_6_2016_pol1 lag_5_2018_pol1 lag_5_2015_pol1 lag_4_2018_pol1 lag_4_2017_pol1 lag_4_2015_pol1    date_0_2018_pol1 date_0_2017_pol1 date_0_2016_pol1 date_0_2015_pol1  
global inter_pol2 lag_6_2018_pol2 lag_6_2016_pol2 lag_5_2018_pol2 lag_5_2015_pol2 lag_4_2018_pol2 lag_4_2017_pol2 lag_4_2015_pol2    date_0_2018_pol2 date_0_2017_pol2 date_0_2016_pol2 date_0_2015_pol2  
global saturated  lag_6_2018 lag_6_2016 lag_5_2018 lag_5_2015 lag_4_2018 lag_4_2017 lag_4_2015  date_0_2018 date_0_2017 date_0_2016 date_0_2015 // removed lag_3_2018 lag_3_2016 lag_3_2015 to be the comparison group AND lag_8_2018 because of collinearity following Abraham and Sun (2021)
global controls_time_acuerdo   $governor $margin_governor $effectiveparties 

*========================================================================
*KEEP REAL DATASET
est clear
*pol1
foreach j in incumbent_yesterday_w_tomorrow2 {

foreach pol in 1 {
rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)

qui xi: reghdfe  `j'  $saturated pol`pol' $inter_pol1  $controls_time_acuerdo  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
}
}
keep if e(sample)==1

*========================================================================
*SET PANEL
xtset inegi year 

*========================================================================
*TABLES
*--------------------*
*Incumbency advantage:
*--------------------*
label variable incumbent_yesterday_w_tomorrow2 "Probability of winning in the election at t+1"
label variable mv_incpartyfor1 "Winning margin in the election at t+1"

global outcomes incumbent_yesterday_w_tomorrow2 mv_incpartyfor1

estpost sum  $outcomes
est store table1

esttab using "../Tables_incumbency/inc_advantage.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

*--------------------*
*Controls:
*--------------------*
label variable pan_mayor2 "PAN mayor=1; 0 otherwise"
label variable pri_mayor2 "PRI mayor=1; 0 otherwise"
label variable hayCarteles "Incidence of Cartel Presence"

global controls winning_margin_governor alignment_executive_strong alignment_governor_strong winning_margin pan_mayor2 pri_mayor2 
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
	replace `i'=(`i'/pop)*1000
	}
	
label variable t_person "Bureaucrats per 1,000 inhabitants"
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
