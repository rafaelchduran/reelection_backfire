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
use "../../Data/ConstructionDatabase/data_wleads&lags_incumbency_weights.dta", clear
**N=4562
*========================================================================
*SET PANEL
xtset inegi year 
*========================================================================
*Drop irrelevant time periods
drop if lead_1_2015==1 // no observations 
drop if lead_1_2016==1 // no observations
drop if lag_2_2016==1 // no observations
drop lead_1_2015
drop lead_1_2016
drop lag_2_2016
drop if lag_7_2017==1 // no observations
drop if lag_7_2018==1 // no observations
drop lag_7_2017
drop lag_7_2018
*========================================================================
*MISCELANEOUS

*0) pop
gen logpop=ln(pop)

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
*tab inegi, gen(inegi_)

*some last changes:
rename governor_alignment governor_pri

global alignment alignment_executive_strong alignment_governor_strong double_alignment governor_pri ///
 governor_aligned_notpri governor_aligned_pri governor_notaligned president_pri

*========================================================================
*LEADS AND LAGS
** homicides
global homicides logdefunciones ihs_defunciones loghomicide loghomicide_old loghomicidecombined
foreach var in $homicides{
foreach i in 3 4 5 6  8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

**create citizen demands as controls:
global insecurityperception ap4_2_3 ap4_2_5 ap4_2_11 ap4_2_8 ap4_2_1 ap4_2_2 ap4_2_4 ap4_2_6 ap4_2_7 ap4_2_9 ap4_2_10 ap4_3_1 ap4_3_2 ap4_3_3 ap4_7_1 ap4_7_2 ap4_7_3 ap4_12b ap5_1_6 ap5_1_7 ap5_1_8 ap5_1_10 ap5_1_12 ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_9 ap4_7_1_b ap4_7_2_b ap4_7_3_b ap5_4_2_b ap5_4_3_b ap5_4_4_b ap5_4_5_b ap5_4_6_b ap5_4_7_b ap5_4_8_b ap5_4_9_b
foreach var in $insecurityperception{
foreach i in 3 4 5 6  8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

global incumbency inc_party_runsfor1 inc_party_won numparties_eff numparties_eff_molinar 
foreach var in $incumbency{
foreach i in 3 4 5 6  8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

gen inc = incumbent_yesterday_w_tomorrow2
global incumbency inc 
foreach var in $incumbency{
foreach i in 3 4 5 6  8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

foreach var in $alignment{
foreach i in 3 4 5 6  8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
} 

global parties numparties_eff numparties_eff_molinar
foreach var in $parties{
foreach i in 3 4 5 6  8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

global margin winning_margin winning_margin_governor
foreach var in $margin{
foreach i in 3 4 5 6  8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

*Benchmark model:
tab year, gen(year_)
foreach var in winning_margin_governor HHI effectiveparties  NP golosov dcoal{
foreach i in 3 4 5 6  8{
capture gen `var'_year_`y'=year_`y'*`var'
}
}

*Event case regression: 
**set controls
foreach i in 3 4 5 6  8{
capture gen margin_lag_`i'=lag_`i'*winning_margin_governor
capture gen margin_date0=date_0*winning_margin_governor
}


foreach var in ncand effectiveparties HHI num_parties NP golosov dcoal governor_alignment pop logdefuncionespc ihs_defuncionespc{
foreach i in 3 4 5 6  8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

foreach var in areakm2 logpop hayCarteles{
foreach i in 3 4 5 6  8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

foreach var in pri_mayor2 pan_mayor2{
foreach i in 3 4 5 6  8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

global agreements acuerdo acuerdo2nacuerdo3 acuerdo4 acuerdo5 acuerdo_total acuerdo_gobestatal acuerdo_gobestatal2 acuerdo_gobestatal3 acuerdo_gobestatal4 acuerdo_gobfederal acuerdo_gobfederal2 acuerdo_gobfederal3 acuerdo_gobfederal4 acuerdo_gobestatal_federal acuerdo_estcom
foreach var in $agreements{
foreach i in 3 4 5 6  8{
capture gen `var'_lag_`i'=lag_`i'*`var'
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
pan_mayor2 pri_mayor2 morena_mayor2 hayCarteles logpop mv_incpartyfor1 

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
cap foreach i in $controls logdetenidos_2pc acuerdo acuerdo2 acuerdo3 acuerdo4 acuerdo5 acuerdo_federal acuerdo_total ap4_2_1 ap4_2_2 ap4_2_4 ap4_2_6 ap4_2_7 ap4_2_8 ap4_2_9 ap4_2_10 ///
 ap5_3_1 ap5_3_2 ap5_3_3 ap5_3_4 ap5_3_5 ap5_3_6 ap5_3_7 ap5_3_8 ap5_3_9 ap5_3_1_b ap5_3_2_b ap5_3_3_b ap5_3_4_b ap5_3_5_b ap5_3_6_b ap5_3_7_b ap5_3_8_b ap5_3_9_b ///
 ap5_4_1 ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_9 ap5_4_1_b ap5_4_2_b ap5_4_3_b ap5_4_4_b ap5_4_5_b ap5_4_6_b ap5_4_7_b ap5_4_8_b ap5_4_9_b ///
 ap5_5_1 ap5_5_2 ap5_5_3 ap5_5_4 ap5_5_5 ap5_5_6 ap5_5_7 ap5_5_8 ap5_5_9 ap5_5_1_b ap5_5_2_b ap5_5_3_b ap5_5_4_b ap5_5_5_b ap5_5_6_b ap5_5_7_b ap5_5_8_b ap5_5_9_b ///
 ap5_6_1 ap5_6_2 ap5_6_3 ap5_6_4 ap5_6_5 ap5_6_6 ap5_6_7 ap5_6_8 ap5_6_9 ap5_6_1_b ap5_6_2_b ap5_6_3_b ap5_6_4_b ap5_6_5_b ap5_6_6_b ap5_6_7_b ap5_6_8_b ap5_6_9_b{
cap bysort inegi: egen `i'_mean=mean(`i') if year<=2014
cap foreach j in  `i'_mean{
cap xfill `j', i(inegi)
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

cap global  controls logdefuncionespc_mean ap4_2_3_mean ap4_2_5_mean ap4_2_11_mean ap4_12b_mean ap5_4_2_b_mean ap5_4_8_b_mean ///
align_pres align_gov winning_margin_mean margin_gov logpop_mean ///
pan_mayor2 pri_mayor2 morena_mayor2 hayCarteles nCarteles acuerdo_mean acuerdo2_mean acuerdo3_mean acuerdo4_mean acuerdo5_mean acuerdo_federal_mean acuerdo_total_mean

tab year, gen(y_)

cap global years y_1 y_2 y_3 y_4 y_5 y_6 y_7 y_8 y_9
*cap global years y_1 y_2 y_3 y_4 y_5 y_6 
cap foreach i in $controls {
cap foreach year in $years{
cap gen `i'_`year'=`i'*`year'
}
}

*========================================================================
*Create various agreement measures:

gen acuerdo_estyfed=0
replace acuerdo_estyfed=1 if acuerdo_gobestatal2==1 & acuerdo_gobfederal==1
replace acuerdo_estyfed=. if acuerdo_gobestatal2==. | acuerdo_gobfederal==.

gen acuerdo_estnofed=0
replace acuerdo_estnofed=1 if acuerdo_gobestatal2==1 & acuerdo_gobfederal==0
replace acuerdo_estnofed=. if acuerdo_gobestatal2==. | acuerdo_gobfederal==.

gen acuerdo_fednoest=0
replace acuerdo_fednoest=1 if acuerdo_gobestatal2==0 & acuerdo_gobfederal==1
replace acuerdo_fednoest=. if acuerdo_gobestatal2==. | acuerdo_gobfederal==.

gen acuerdo_estyfed2=0
replace acuerdo_estyfed2=1 if acuerdo_gobestatal3==1 & acuerdo_gobfederal==1
replace acuerdo_estyfed2=. if acuerdo_gobestatal3==. | acuerdo_gobfederal==.

gen acuerdo_estnofed2=0
replace acuerdo_estnofed2=1 if acuerdo_gobestatal3==1 & acuerdo_gobfederal==0
replace acuerdo_estnofed2=. if acuerdo_gobestatal3==. | acuerdo_gobfederal==.

gen acuerdo_fednoest2=0
replace acuerdo_fednoest2=1 if acuerdo_gobestatal3==0 & acuerdo_gobfederal==1
replace acuerdo_fednoest2=. if acuerdo_gobestatal3==. | acuerdo_gobfederal==.

gen acuerdo_estyfed3=0
replace acuerdo_estyfed3=1 if acuerdo_gobestatal4==1 & acuerdo_gobfederal==1
replace acuerdo_estyfed3=. if acuerdo_gobestatal4==. | acuerdo_gobfederal==.

gen acuerdo_estnofed3=0
replace acuerdo_estnofed3=1 if acuerdo_gobestatal4==1 & acuerdo_gobfederal==0
replace acuerdo_estnofed3=. if acuerdo_gobestatal4==. | acuerdo_gobfederal==.

gen acuerdo_fednoest3=0
replace acuerdo_fednoest3=1 if acuerdo_gobestatal4==0 & acuerdo_gobfederal==1
replace acuerdo_fednoest3=. if acuerdo_gobestatal4==. | acuerdo_gobfederal==.



*========================================================================
*ERASE TIME PERIODS PRIOR TO 2010
drop if year<2010


*========================================================================
*SAVE DATA
save "../../Data/ConstructionDatabase/data_final_incumbency.dta", replace
/*========================================================================
*Incumbency advantage: win or loose at t and compare to t+1
use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_02 mv_party_03 mv_party_04 mv_party_05 mv_party_06 mv_party_07 mv_party_08 mv_party_09 mv_party_10 mv_party_11 mv_party_12 mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename mv_party_01 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop1
save `drop1'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01 mv_party_03 mv_party_04 mv_party_05 mv_party_06 mv_party_07 mv_party_08 mv_party_09 mv_party_10 mv_party_11 mv_party_12 mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename mv_party_02 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop2
save `drop2'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02  mv_party_04 mv_party_05 mv_party_06 mv_party_07 mv_party_08 mv_party_09 mv_party_10 mv_party_11 mv_party_12 mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename mv_party_03 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop3
save `drop3'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03 mv_party_05 mv_party_06 mv_party_07 mv_party_08 mv_party_09 mv_party_10 mv_party_11 mv_party_12 mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_04 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop4
save `drop4'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_06 mv_party_07 mv_party_08 mv_party_09 mv_party_10 mv_party_11 mv_party_12 mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_05 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop5
save `drop5'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_07 mv_party_08 mv_party_09 mv_party_10 mv_party_11 mv_party_12 mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_06 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop6
save `drop6'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06  mv_party_08 mv_party_09 mv_party_10 mv_party_11 mv_party_12 mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_07 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop7
save `drop7'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07  mv_party_09 mv_party_10 mv_party_11 mv_party_12 mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_08 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop8
save `drop8'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07 mv_party_08  mv_party_10 mv_party_11 mv_party_12 mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_09 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop9
save `drop9'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07 mv_party_08 mv_party_09   mv_party_11 mv_party_12 mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_10 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop10
save `drop10'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07 mv_party_08 mv_party_09  mv_party_10 ///
   mv_party_12 mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_11 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop11
save `drop11'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07 mv_party_08 mv_party_09  mv_party_10 ///
   mv_party_11  mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_12 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop12
save `drop12'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07 mv_party_08 mv_party_09  mv_party_10 ///
   mv_party_11  mv_party_13 mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_12 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop12
save `drop12'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07 mv_party_08 mv_party_09  mv_party_10 ///
   mv_party_11  mv_party_12  mv_party_14 mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_13 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop13
save `drop13'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07 mv_party_08 mv_party_09  mv_party_10 ///
   mv_party_11  mv_party_12  mv_party_13  mv_party_15 mv_party_16 mv_party_17 mv_party_18
rename  mv_party_14 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop14
save `drop14'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07 mv_party_08 mv_party_09  mv_party_10 ///
   mv_party_11  mv_party_12  mv_party_13 mv_party_14  mv_party_16 mv_party_17 mv_party_18
rename  mv_party_15 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop15
save `drop15'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07 mv_party_08 mv_party_09  mv_party_10 ///
   mv_party_11  mv_party_12  mv_party_13 mv_party_14  mv_party_15  mv_party_17 mv_party_18
rename  mv_party_16 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop16
save `drop16'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07 mv_party_08 mv_party_09  mv_party_10 ///
   mv_party_11  mv_party_12  mv_party_13 mv_party_14  mv_party_15  mv_party_16  mv_party_18
rename  mv_party_17 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop17
save `drop17'

use "../../Data/ConstructionDatabase/data_final.dta", clear
drop  mv_party_01  mv_party_02 mv_party_03  mv_party_04  mv_party_05  mv_party_06 mv_party_07 mv_party_08 mv_party_09  mv_party_10 ///
   mv_party_11  mv_party_12  mv_party_13 mv_party_14  mv_party_15  mv_party_17  
rename  mv_party_18 mv_party
xtset inegi year
gen party_won_nextelec=.
by inegi: replace party_won_nextelec=0 if winning_margin[_n+1]!=mv_party[_n+1]
by inegi: replace party_won_nextelec=1 if winning_margin[_n+1]==mv_party[_n+1]
tempfile drop18
save `drop18'

use `drop1'
append using `drop1'
append using `drop2'
append using `drop3'
append using `drop4'
append using `drop5'
append using `drop6'
append using `drop7'
append using `drop8'
append using `drop9'
append using `drop10'
append using `drop11'
append using `drop12'
append using `drop13'
append using `drop14'
append using `drop15'
append using `drop16'
append using `drop17'
append using `drop18'
save "../../Data/ConstructionDatabase/data_final_incumbency_advantage.dta", replace


*========================================================================
*========================================================================
