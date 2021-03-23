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
*GENERATE VARIABLES
**1) dummies for party incumbents:
gen pri_mayor2=.
replace  pri_mayor2=1 if firstword=="pri"
replace pri_mayor2=0 if firstword!="pri" & firstword!=""

gen morena_mayor2=.
replace  morena_mayor2=1 if firstword=="morena"
replace morena_mayor2=0 if firstword!="morena" & firstword!=""

gen pan_mayor2=.
replace  pan_mayor2=1 if firstword=="pan"
replace pan_mayor2=0 if firstword!="pan" & firstword!=""

*fill:
sort inegi year
foreach i in pri_mayor2  morena_mayor2 pan_mayor2{
fillmissing `i', with(previous)
}

order pri_mayor2 morena_mayor2 pan_mayor2

**2) lags for wild clustering and other regressions
sort inegi year

foreach i in reform{
gen `i'_pre=L.`i'
}

foreach i in winning_margin_governor governor_alignment logdefuncionespc winning_margin alignment_executive_strong{
gen `i'_pre=L2.`i'
}

**need to fill winning_margin alignment_executive_strong
foreach i in  winning_margin winning_margin_pre alignment_executive_strong_pre{
*bysort inegi year: replace `i'=`i'[_n-1] if `i'==.
fillmissing `i', with(previous)
}

bysort estado inegi: gen logdefuncionespc_prereform=logdefuncionespc if year==2013
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n-1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.
bysort estado inegi: replace logdefuncionespc_prereform=logdefuncionespc_prereform[_n+1] if logdefuncionespc_prereform==.


*generate groups 
gen group=.
replace group=1 if adopt_year==.
replace group=2 if adopt_year==2015
replace group=3 if adopt_year==2016
replace group=4 if adopt_year==2017
replace group=5 if adopt_year==2018

**create citizen demands as controls:
global insecurityperception ap4_2_3 ap4_2_5 ap4_2_11 ap4_3_1 ap4_3_2 ap4_3_3 ap4_7_1 ap4_7_2 ap4_7_3 ap4_12b ap5_1_6 ap5_1_7 ap5_1_8 ap5_1_10 ap5_1_12 ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_9 ap4_7_1_b ap4_7_2_b ap4_7_3_b ap5_4_2_b ap5_4_3_b ap5_4_4_b ap5_4_5_b ap5_4_6_b ap5_4_7_b ap5_4_8_b ap5_4_9_b
foreach var in $insecurityperception{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

foreach i in  inc_party_runsfor1 inc_party_won numparties_eff numparties_eff_molinar {
fillmissing `i', with(previous)
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

*alignment variables
order firstword win_governor alignment_executive_strong alignment_governor_strong double_alignment

gen governor_alignment2=0 if firstword==win_governor & firstword!="pri" & firstword!="" // aligned but not the pri
order firstword win_governor governor_alignment2 alignment_executive_strong alignment_governor_strong double_alignment
replace governor_alignment2=1 if firstword==win_governor & firstword=="pri" // aligned and pri

replace governor_alignment2=2 if firstword!=win_governor // not aligned

*fill:
sort inegi year
foreach i in  governor_alignment2{
fillmissing `i', with(previous)
}
*this generates missing values of places that were not aligned. But that's exactly what I want to have aside. 
*Here PRI is acting as the way to measure higher clientelistic transfers, rather than credit claiming. 

tab governor_alignment2, gen(governor_align_)
rename governor_align_1 governor_aligned_notpri
rename governor_align_2 governor_aligned_pri
rename governor_align_3 governor_notaligned

*generate alignment with PRI president
gen president_alignment=0
replace president_alignment=1 if firstword=="pri" & year>2012 & year<2019

sort inegi year
foreach i in  alignment_executive_strong alignment_governor_strong double_alignment{
fillmissing `i', with(previous)
}

**need to correct governor_alignment variable: 
replace governor_alignment=. if win_governor==""  
sort inegi year
foreach i in  governor_alignment{
fillmissing `i', with(previous)
}

rename governor_alignment governor_pri

gen president_pri=0
replace president_pri=1 if year>2012 & year<2019



*========================================================================
*SAVE DATA
save "../../Data/ConstructionDatabase/data_final.dta", replace

