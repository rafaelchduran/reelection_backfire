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
/*Things to control for:
0. homicides
1. heterogeneity in citizens security preferences 
2. alignment 
3. winning margin mayor
4. winning margin governor
5. parties
*/

*========================================================================
*MISCELANEOUS
*1) party variables:
gen pri_mayor2=.
replace  pri_mayor2=1 if firstword=="pri"
replace pri_mayor2=0 if firstword!="pri" & firstword!=""

gen morena_mayor2=.
replace  morena_mayor2=1 if firstword=="morena"
replace morena_mayor2=0 if firstword!="morena" & firstword!=""

gen pan_mayor2=.
replace  pan_mayor2=1 if firstword=="pan"
replace pan_mayor2=0 if firstword!="pan" & firstword!=""

/*fill:
sort inegi year
foreach i in pri_mayor2  morena_mayor2 pan_mayor2{
fillmissing `i', with(previous)
}

bysort estado inegi: gen logdefuncionespc_prereform=logdefuncionespc if year==2013
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n-1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
*/

*2) alignment variables:
*order firstword win_governor alignment_executive_strong alignment_governor_strong double_alignment

gen governor_alignment2=0 if firstword==win_governor & firstword!="pri" & firstword!="" // aligned but not the pri
*order firstword win_governor governor_alignment2 alignment_executive_strong alignment_governor_strong double_alignment
replace governor_alignment2=1 if firstword==win_governor & firstword=="pri" // aligned and pri
replace governor_alignment2=2 if firstword!=win_governor // not aligned

*this generates missing values of places that were not aligned. But that's exactly what I want to have aside. 
*Here PRI is acting as the way to measure higher clientelistic transfers, rather than credit claiming. 

tab governor_alignment2, gen(governor_align_)
rename governor_align_1 governor_aligned_notpri
rename governor_align_2 governor_aligned_pri
rename governor_align_3 governor_notaligned
 

*generate alignment with PRI president
gen president_alignment=0
replace president_alignment=1 if firstword=="pri" & year>2012 & year<2019

*generate president is from PRI
gen president_pri=0
replace president_pri=1 if year>2012 & year<2019

*Municipality fixed effects 
tab inegi, gen(inegi_)

*some last changes:
rename governor_alignment governor_pri

global alignment alignment_executive_strong alignment_governor_strong double_alignment governor_pri ///
 governor_aligned_notpri governor_aligned_pri governor_notaligned president_pri

*========================================================================
*LEADS AND LAGS
**create citizen demands as controls:
global insecurityperception ap4_2_3 ap4_2_5 ap4_2_11 ap4_3_1 ap4_3_2 ap4_3_3 ap4_7_1 ap4_7_2 ap4_7_3 ap4_12b ap5_1_6 ap5_1_7 ap5_1_8 ap5_1_10 ap5_1_12 ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_9 ap4_7_1_b ap4_7_2_b ap4_7_3_b ap5_4_2_b ap5_4_3_b ap5_4_4_b ap5_4_5_b ap5_4_6_b ap5_4_7_b ap5_4_8_b ap5_4_9_b
foreach var in $insecurityperception{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

global incumbency inc_party_runsfor1 inc_party_won numparties_eff numparties_eff_molinar 
foreach var in $incumbency{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

gen inc = incumbent_yesterday_w_tomorrow2
global incumbency inc 
foreach var in $incumbency{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

foreach var in $alignment{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
} 

global parties numparties_eff numparties_eff_molinar
foreach var in $parties{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

global margin winning_margin winning_margin_governor
foreach var in $margin{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

*Benchmark model:
tab year, gen(year_)
foreach var in winning_margin_governor HHI effectiveparties  NP golosov dcoal{
foreach y in 1 2 3 4 5 6 7 8 9 10{
capture gen `var'_year_`y'=year_`y'*`var'
}
}

*Event case regression: 
**set controls
foreach i in 1 2 3 4 5 6 7 8{
capture gen margin_lag_`i'=lag_`i'*winning_margin_governor
capture gen margin_lead_`i'=lead_`i'*winning_margin_governor
capture gen margin_date0=date_0*winning_margin_governor
}


foreach var in ncand effectiveparties HHI num_parties NP golosov dcoal governor_alignment pop logdefuncionespc ihs_defuncionespc{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

foreach var in areakm2{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}


*global agreements acuerdo acuerdo2 acuerdo3 acuerdo4
global agreements acuerdo acuerdo2 
foreach var in $agreements{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

*========================================================================
*GENERATE GROUPS
gen group=.
replace group=1 if adopt_year==.
replace group=2 if adopt_year==2015
replace group=3 if adopt_year==2016
replace group=4 if adopt_year==2017
replace group=5 if adopt_year==2018

*========================================================================
*VARIABLES FOR WILD CLUSTERING
sort inegi year

foreach i in reform{
gen `i'_pre=L.`i'
}

sort inegi year
*foreach i in acuerdo acuerdo2 acuerdo3 acuerdo4{
foreach i in acuerdo acuerdo2 logdefuncionespc{
gen `i'_post=F.`i'
gen `i'_post2=F2.`i'
gen `i'_post3=F3.`i'
}

*========================================================================
*CONTROL VARIABLES (PRE-TREATMENT) FOR NAIVE REGRESSION
*========================================================================
*1) CONTEMPORARY TO TREATMENT

global  controls logdefuncionespc ap4_2_3 ap4_2_5 ap4_2_11 ap4_12b ap5_4_2_b ap5_4_8_b ///
alignment_executive_strong alignment_governor_strong winning_margin winning_margin_governor ///
pan_mayor2 pri_mayor2 morena_mayor2

*========================================================================
*2) LAGGED ONE PERIODS
foreach i in  $controls{
gen `i'_t_1=L.`i'
}

*========================================================================
*3) LAGGED TWO PERIODS
foreach i in  $controls{
gen `i'_t_2=L2.`i'
}

*========================================================================
*4) LAGGED THREE PERIODS
foreach i in  $controls{
gen `i'_t_3=L3.`i'
}

*========================================================================
*5)Create measures for 2014

*foreach i in $controls logdetenidos_2pc acuerdo acuerdo2  acuerdo3 acuerdo4{
foreach i in $controls logdetenidos_2pc acuerdo acuerdo2  {
gen `i'_2014=`i' if year==2014
foreach j in  `i'_2014{
xfill `j', i(inegi)
}
}

*========================================================================
*6) Average prior to treatment
foreach i in $controls  logdetenidos_2pc acuerdo acuerdo2 acuerdo3 acuerdo4 acuerdo5 acuerdo_federal acuerdo_total {
bysort inegi: egen `i'_mean=mean(`i') if year<=2014
foreach j in  `i'_mean{
xfill `j', i(inegi)
}
}

/*foreach i in pri_mayor2  morena_mayor2 pan_mayor2{
fillmissing `i', with(previous)
}
*/

*multiply by year dummies

gen align_pres=alignment_executive_strong_mean
gen align_gov=alignment_governor_strong_mean
gen margin_gov=winning_margin_governor_mean

global  controls logdefuncionespc_mean ap4_2_3_mean ap4_2_5_mean ap4_2_11_mean ap4_12b_mean ap5_4_2_b_mean ap5_4_8_b_mean ///
align_pres align_gov winning_margin_mean margin_gov ///
pan_mayor2 pri_mayor2 morena_mayor2 hayCarteles nCarteles acuerdo_mean acuerdo2_mean acuerdo3_mean acuerdo4_mean acuerdo5_mean acuerdo_federal_mean acuerdo_total_mean

tab year, gen(y_)

global years y_1 y_2 y_3 y_4 y_5 y_6 y_7 y_8 y_9
foreach i in $controls {
foreach year in $years{
gen `i'_`year'=`i'*`year'
}
}

*========================================================================
*ERASE TIME PERIODS PRIOR TO 2010
drop if year<2010

*========================================================================
*SAVE DATA
save "../../Data/ConstructionDatabase/data_final.dta", replace



*========================================================================
*FOR R
use "../../Data/ConstructionDatabase/data_wleads&lags2.dta", clear
*MISCELANEOUS
*1) party variables:
gen pri_mayor2=.
replace  pri_mayor2=1 if firstword=="pri"
replace pri_mayor2=0 if firstword!="pri" & firstword!=""

gen morena_mayor2=.
replace  morena_mayor2=1 if firstword=="morena"
replace morena_mayor2=0 if firstword!="morena" & firstword!=""

gen pan_mayor2=.
replace  pan_mayor2=1 if firstword=="pan"
replace pan_mayor2=0 if firstword!="pan" & firstword!=""

/*fill:
sort inegi year
foreach i in pri_mayor2  morena_mayor2 pan_mayor2{
fillmissing `i', with(previous)
}

bysort estado inegi: gen logdefuncionespc_prereform=logdefuncionespc if year==2013
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n-1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
*/

*2) alignment variables:
*order firstword win_governor alignment_executive_strong alignment_governor_strong double_alignment

gen governor_alignment2=0 if firstword==win_governor & firstword!="pri" & firstword!="" // aligned but not the pri
*order firstword win_governor governor_alignment2 alignment_executive_strong alignment_governor_strong double_alignment
replace governor_alignment2=1 if firstword==win_governor & firstword=="pri" // aligned and pri
replace governor_alignment2=2 if firstword!=win_governor // not aligned

*this generates missing values of places that were not aligned. But that's exactly what I want to have aside. 
*Here PRI is acting as the way to measure higher clientelistic transfers, rather than credit claiming. 

tab governor_alignment2, gen(governor_align_)
rename governor_align_1 governor_aligned_notpri
rename governor_align_2 governor_aligned_pri
rename governor_align_3 governor_notaligned
 

*generate alignment with PRI president
gen president_alignment=0
replace president_alignment=1 if firstword=="pri" & year>2012 & year<2019

*generate president is from PRI
gen president_pri=0
replace president_pri=1 if year>2012 & year<2019

*Municipality fixed effects 
tab inegi, gen(inegi_)

*some last changes:
rename governor_alignment governor_pri

global alignment alignment_executive_strong alignment_governor_strong double_alignment governor_pri ///
 governor_aligned_notpri governor_aligned_pri governor_notaligned president_pri

*========================================================================
*LEADS AND LAGS
**create citizen demands as controls:
global insecurityperception ap4_2_3 ap4_2_5 ap4_2_11 ap4_3_1 ap4_3_2 ap4_3_3 ap4_7_1 ap4_7_2 ap4_7_3 ap4_12b ap5_1_6 ap5_1_7 ap5_1_8 ap5_1_10 ap5_1_12 ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_9 ap4_7_1_b ap4_7_2_b ap4_7_3_b ap5_4_2_b ap5_4_3_b ap5_4_4_b ap5_4_5_b ap5_4_6_b ap5_4_7_b ap5_4_8_b ap5_4_9_b
foreach var in $insecurityperception{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

global incumbency inc_party_runsfor1 inc_party_won numparties_eff numparties_eff_molinar 
foreach var in $incumbency{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

gen inc = incumbent_yesterday_w_tomorrow2
global incumbency inc 
foreach var in $incumbency{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

foreach var in $alignment{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
} 

global parties numparties_eff numparties_eff_molinar
foreach var in $parties{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

global margin winning_margin winning_margin_governor
foreach var in $margin{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

*Benchmark model:
tab year, gen(year_)
foreach var in winning_margin_governor HHI effectiveparties  NP golosov dcoal{
foreach y in 1 2 3 4 5 6 7 8 9 10{
capture gen `var'_year_`y'=year_`y'*`var'
}
}

*Event case regression: 
**set controls
foreach i in 1 2 3 4 5 6 7 8{
capture gen margin_lag_`i'=lag_`i'*winning_margin_governor
capture gen margin_lead_`i'=lead_`i'*winning_margin_governor
capture gen margin_date0=date_0*winning_margin_governor
}


foreach var in ncand effectiveparties HHI num_parties NP golosov dcoal governor_alignment pop logdefuncionespc ihs_defuncionespc{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

foreach var in areakm2{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}


global agreements acuerdo acuerdo2 acuerdo3 acuerdo4 acuerdo5 acuerdo_federal acuerdo_total
*global agreements acuerdo acuerdo2 
foreach var in $agreements{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

*========================================================================
*GENERATE GROUPS
gen group=.
replace group=1 if adopt_year==.
replace group=2 if adopt_year==2015
replace group=3 if adopt_year==2016
replace group=4 if adopt_year==2017
replace group=5 if adopt_year==2018

*========================================================================
*VARIABLES FOR WILD CLUSTERING
sort inegi year

foreach i in reform{
gen `i'_pre=L.`i'
}

sort inegi year
*foreach i in acuerdo acuerdo2 acuerdo3 acuerdo4{
foreach i in acuerdo acuerdo2 acuerdo3 acuerdo4 acuerdo5 logdefuncionespc{
gen `i'_post=F.`i'
gen `i'_post2=F2.`i'
gen `i'_post3=F3.`i'
}

*========================================================================
*CONTROL VARIABLES (PRE-TREATMENT) FOR NAIVE REGRESSION
*========================================================================
*1) CONTEMPORARY TO TREATMENT

global  controls logdefuncionespc ap4_2_3 ap4_2_5 ap4_2_11 ap4_12b ap5_4_2_b ap5_4_8_b ///
alignment_executive_strong alignment_governor_strong winning_margin winning_margin_governor ///
pan_mayor2 pri_mayor2 morena_mayor2

*========================================================================
*2) LAGGED ONE PERIODS
foreach i in  $controls{
gen `i'_t_1=L.`i'
}

*========================================================================
*3) LAGGED TWO PERIODS
foreach i in  $controls{
gen `i'_t_2=L2.`i'
}

*========================================================================
*4) LAGGED THREE PERIODS
foreach i in  $controls{
gen `i'_t_3=L3.`i'
}

*========================================================================
*5)Create measures for 2014

*foreach i in $controls logdetenidos_2pc acuerdo acuerdo2  acuerdo3 acuerdo4{
foreach i in $controls logdetenidos_2pc acuerdo acuerdo2  {
gen `i'_2014=`i' if year==2014
foreach j in  `i'_2014{
xfill `j', i(inegi)
}
}

*========================================================================
*6) Average prior to treatment
*foreach i in $controls ap4_2_5 logdetenidos_2pc acuerdo acuerdo2 acuerdo3 acuerdo4{
foreach i in $controls logdetenidos_2pc acuerdo acuerdo2 acuerdo3 acuerdo4 acuerdo5 acuerdo_federal acuerdo_total{
bysort inegi: egen `i'_mean=mean(`i') if year<=2014
foreach j in  `i'_mean{
xfill `j', i(inegi)
}
}

/*foreach i in pri_mayor2  morena_mayor2 pan_mayor2{
fillmissing `i', with(previous)
}
*/

*multiply by year dummies

gen align_pres=alignment_executive_strong_mean
gen align_gov=alignment_governor_strong_mean
gen margin_gov=winning_margin_governor_mean

global  controls logdefuncionespc_mean ap4_2_3_mean ap4_2_5_mean ap4_2_11_mean ap4_12b_mean ap5_4_2_b_mean ap5_4_8_b_mean ///
align_pres align_gov winning_margin_mean margin_gov ///
pan_mayor2 pri_mayor2 morena_mayor2 hayCarteles nCarteles acuerdo_mean acuerdo2_mean acuerdo3_mean acuerdo4_mean acuerdo5_mean acuerdo_federal_mean acuerdo_total_mean

tab year, gen(y_)

global years y_1 y_2 y_3 y_4 y_5 y_6 y_7 y_8 y_9
foreach i in $controls {
foreach year in $years{
gen `i'_`year'=`i'*`year'
}
}
*========================================================================
*SAVE DATA
save "../../Data/ConstructionDatabase/data_final2_forR.dta", replace

*ssc install xtbalance
*ssc install xtbalance2
xtset inegi year
xtbalance2 $controls logdefuncionespc ihs_defuncionespc *_y_* reform, gen(balance)
keep if balance==1
keep inegi year logdefuncionespc ihs_defuncionespc logdefuncionespc_mean_y_1 logdefuncionespc_mean_y_2 logdefuncionespc_mean_y_3 logdefuncionespc_mean_y_4 logdefuncionespc_mean_y_5 logdefuncionespc_mean_y_6 logdefuncionespc_mean_y_7 logdefuncionespc_mean_y_8 logdefuncionespc_mean_y_9  ///
 align_gov_y_1 align_gov_y_2 align_gov_y_3 align_gov_y_4 align_gov_y_5 align_gov_y_6 align_gov_y_7 align_gov_y_8 align_gov_y_9 ///
 margin_gov_y_1 margin_gov_y_2 margin_gov_y_3 margin_gov_y_4 margin_gov_y_5 margin_gov_y_6 margin_gov_y_7 margin_gov_y_8 margin_gov_y_9 ///
 hayCarteles_y_1 hayCarteles_y_2 hayCarteles_y_3 hayCarteles_y_4 hayCarteles_y_5 hayCarteles_y_6 hayCarteles_y_7 hayCarteles_y_8 hayCarteles_y_9 ///
ap4_2_3_mean_y_1 ap4_2_3_mean_y_2 ap4_2_3_mean_y_3 ap4_2_3_mean_y_4 ap4_2_3_mean_y_5 ap4_2_3_mean_y_6 ap4_2_3_mean_y_7 ap4_2_3_mean_y_8 ap4_2_3_mean_y_9

*xtbalance, range(2010 2018) miss($controls logdefuncionespc ihs_defuncionespc *_y_* reform)
*keep inegi year $controls logdefuncionespc ihs_defuncionespc *_y_* reform
save "../../Data/ConstructionDatabase/data_final2_forR_balanced.dta", replace
