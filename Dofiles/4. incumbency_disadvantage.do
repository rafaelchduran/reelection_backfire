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
use "../../Data/ConstructionDatabase/data_final.dta", clear

*========================================================================
*SET PANEL
xtset inegi year 

*========================================================================
*SET GLOBALS
	global narco ap4_2_3_lag_8 ap4_2_3_lag_7 ap4_2_3_lag_6 ap4_2_3_lag_5 ap4_2_3_lag_4 ap4_2_3_lag_3 ap4_2_3_lag_2
	global punishment ap4_2_11_lag_8 ap4_2_11_lag_7 ap4_2_11_lag_6 ap4_2_11_lag_5 ap4_2_11_lag_4 ap4_2_11_lag_3 ap4_2_11_lag_2
	global money ap4_12b_lag_8 ap4_12b_lag_7 ap4_12b_lag_6 ap4_12b_lag_5 ap4_12b_lag_4 ap4_12b_lag_3 ap4_12b_lag_2
	global police ap5_4_2_b_lag_8 ap5_4_2_b_lag_7 ap5_4_2_b_lag_6 ap5_4_2_b_lag_5 ap5_4_2_b_lag_4 ap5_4_2_b_lag_3 ap5_4_2_b_lag_2
	global army ap5_4_8_b_lag_8 ap5_4_8_b_lag_7 ap5_4_8_b_lag_6 ap5_4_8_b_lag_5 ap5_4_8_b_lag_4 ap5_4_8_b_lag_3 ap5_4_8_b_lag_2
	global citizens $narco $punishment $money $police $army
	global narco2 ap4_2_3
	global punishment2 ap4_2_11
	global money2 ap4_12b
	global police2 ap5_4_2_b
	global army2 ap5_4_8_b
	global citizens2  $narco2 $punishment2 $money2 $police2 $army2  
	
	global citizens3  ap4_2_11_pre ap4_12b_pre ap5_4_2_b_pre ap5_4_8_b_pre // ap4_2_3_pre
	
	global incumbent_adv inc_lag_8 inc_lag_7 inc_lag_6 inc_lag_5 inc_lag_4 inc_lag_3 inc_lag_2
	global incumbent_adv2 inc_party_runsfor1_lag_8 inc_party_runsfor1_lag_7 inc_party_runsfor1_lag_6 inc_party_runsfor1_lag_5 inc_party_runsfor1_lag_4 inc_party_runsfor1_lag_3 inc_party_runsfor1_lag_2
	global incumbent_adv3 inc_party_won_lag_8 inc_party_won_lag_7 inc_party_won_lag_6 inc_party_won_lag_5 inc_party_won_lag_4 inc_party_won_lag_3 inc_party_won_lag_2
	global num_parties numparties_eff_lag_8 numparties_eff_lag_7 numparties_eff_lag_6 numparties_eff_lag_5 numparties_eff_lag_4 numparties_eff_lag_3 numparties_eff_lag_2
	global num_parties2 numparties_eff_molinar_lag_8 numparties_eff_molinar_lag_7 numparties_eff_molinar_lag_6 numparties_eff_molinar_lag_5 numparties_eff_molinar_lag_4 numparties_eff_molinar_lag_3 numparties_eff_molinar_lag_2
	
	global incumbency $incumbent_adv $num_parties
	
	global controls winning_margin_governor  governor_notaligned pri_mayor2 morena_mayor2 $citizens2 winning_margin alignment_executive_strong
	global controls_pre winning_margin_governor_pre governor_alignment_pre  winning_margin_pre alignment_executive_strong_pre $citizens3

*NOTE: similar results using pre-treatment controls

*========================================================================

**********************************************
*Table: RDD estimates
**********************************************
use "../Data/municipal_elections_incumbent_mexico_1989_present_v2.dta", clear

*Merge with Dube, Garcia-Ponce and Thoms (2016): data from 1990 to 2010
gen muncode=inegi

merge 1:1 muncode year using "../Mexico/Data/Dube, Garcia-Ponce and Thom (2016)/jeea12172-sup-0002-replication-data/Dube_Garcia_ Thom_ReplicationData/MaizeToHaze_JEEA_ReplicationData.dta"
drop if _merge==2
drop _merge

merge m:m inegi year using "../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011_2018.dta"
drop if _merge==2
drop _merge



est clear
foreach pol in 1 2 3 4{
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
use "../Data/ConstructionDatabase/data_wleads&lags_incumbency_weights.dta", clear

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
*A)WITH incumbent_yesterday_w_tomorrow2
*------------------
foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 1 2 3 4 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	lincom 	(_b[`i'_2018]*`c')/5 
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
	lincom 	(_b[`i'_2015]*`a')/5
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c'))/5
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/5 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


}
}

*get the partisan effect
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
	lincom 	((((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/5)/2) -(_b[_cons]/5)/2 
	lincom (_b[_cons]/5)/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


/*****
test on partisan incumbency advantage
_b[reform] = difference between reelection and term limited elections: partisan(A) + personal(B) = A+B = .0714323 (sig 10%)
_b[_cons] = inc. advantage for term limited elections: partisan inc. advantage = A =  .1156307   (sig 5%)
_b[reform]-b[_cons]  = A+B-A=B=personal incumbency advantage =  -.0441984  (no sig but close to 10%)
*****/

eststo: areg  incumbent_yesterday_w_tomorrow2 reform  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster estado)
	lincom 	(_b[reform]/2)-(_b[_cons]/2 )
	
	*generate group
	gen group=0
	replace group=1 if adopt_year==2015
	replace group=2 if adopt_year==2016
	replace group=3 if adopt_year==2017
	replace group=4 if adopt_year==2018

did_multiplegt incumbent_yesterday_w_tomorrow2 group year reform, placebo(1)  breps(50) cluster(estado)


*------------------
*B)WITH inc_party_won
*------------------
foreach j in inc_party_won{
foreach pol in 1 2 3 4 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


quietly areg  incumbent_yesterday_w_tomorrow2  $saturated pol`pol' $interacted_pol2 $allcov2 logdefuncionespc  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
keep if e(sample)==1


est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
			}


}
}



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
*tex & \multicolumn{2}{c}{linear polynomial} \\
*tex \cmidrule(lrr){2-3} \\
*tex Lag 5 years &       $ ${beta_lag_5_ihsdet_1}^{${est_lag_5_ihsdet_1}} $ &       $ ${beta_lag_5_ihsdet2_1}^{${est_lag_5_ihsdet2_1}} $  \\
*tex & ($ ${se_lag_5_ihsdet_1} $ ) & ($ ${se_lag_5_ihsdet2_1} $ ) \\
*tex Lag 4 years &       $ ${beta_lag_4_ihsdet_1}^{${est_lag_4_ihsdet_1}} $ &        $ ${beta_lag_4_ihsdet2_1}^{${est_lag_4_ihsdet2_1}} $ \\  
*tex & ($ ${se_lag_4_ihsdet_1} $ ) & ($ ${se_lag_4_ihsdet2_1} $ ) \\
*tex Lag 3 years &          $ ${beta_lag_3_ihsdet_1}^{${est_lag_3_ihsdet_1}} $ &       $ ${beta_lag_3_ihsdet2_1}^{${est_lag_3_ihsdet2_1}} $ \\   
*tex & ($ ${se_lag_3_ihsdet_1} $ ) & ($ ${se_lag_3_ihsdet2_1} $ ) \\
*tex Reform, time 0 &         $ ${beta_date_0_ihsdet_1}^{${est_date_0_ihsdet_1}} $ &        $ ${beta_date_0_ihsdet2_1}^{${est_date_0_ihsdet2_1}} $ \\   
*tex & ($ ${se_date_0_ihsdet_1} $ ) & ($ ${se_date_0_ihsdet2_1} $ ) \\
*tex Observations          &        ${N_ihsdet_1}     &        ${N_ihsdet2_1} \\ 
*tex R-squared        &          ${r2_ihsdet_1}   &          ${r2_ihsdet2_1} \\  
*tex\\
tex & \multicolumn{2}{c}{quadratic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 5 years &       $ ${beta_lag_5_ihsdet_2}^{${est_lag_5_ihsdet_2}} $ &       $ ${beta_lag_5_ihsdet2_2}^{${est_lag_5_ihsdet2_2}} $  \\
tex & ($ ${se_lag_5_ihsdet_2} $ ) & ($ ${se_lag_5_ihsdet2_2} $ ) \\
tex Lag 4 years &       $ ${beta_lag_4_ihsdet_2}^{${est_lag_4_ihsdet_2}} $ &        $ ${beta_lag_4_ihsdet2_2}^{${est_lag_4_ihsdet2_2}} $ \\  
tex & ($ ${se_lag_4_ihsdet_2} $ ) & ($ ${se_lag_4_ihsdet2_2} $ ) \\
tex Lag 3 years &          $ ${beta_lag_3_ihsdet_2}^{${est_lag_3_ihsdet_2}} $ &       $ ${beta_lag_3_ihsdet2_2}^{${est_lag_3_ihsdet2_2}} $ \\   
tex & ($ ${se_lag_3_ihsdet_2} $ ) & ($ ${se_lag_3_ihsdet2_2} $ ) \\
tex Reform, time 0 &         $ ${beta_date_0_ihsdet_2}^{${est_date_0_ihsdet_2}} $ &        $ ${beta_date_0_ihsdet2_2}^{${est_date_0_ihsdet2_2}} $ \\   
tex & ($ ${se_date_0_ihsdet_2} $ ) & ($ ${se_date_0_ihsdet2_2} $ ) \\
tex Observations          &        ${N_ihsdet_2}     &        ${N_ihsdet2_2} \\ 
tex R-squared        &          ${r2_ihsdet_2}   &          ${r2_ihsdet2_2} \\  
tex\\
tex & \multicolumn{2}{c}{cubic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 5 years &       $ ${beta_lag_5_ihsdet_3}^{${est_lag_5_ihsdet_3}} $ &       $ ${beta_lag_5_ihsdet2_3}^{${est_lag_5_ihsdet2_3}} $  \\
tex & ($ ${se_lag_5_ihsdet_3} $ ) & ($ ${se_lag_5_ihsdet2_3} $ ) \\
tex Lag 4 years &       $ ${beta_lag_4_ihsdet_3}^{${est_lag_4_ihsdet_3}} $ &        $ ${beta_lag_4_ihsdet2_3}^{${est_lag_4_ihsdet2_3}} $ \\  
tex & ($ ${se_lag_4_ihsdet_3} $ ) & ($ ${se_lag_4_ihsdet2_3} $ ) \\
tex Lag 3 years &          $ ${beta_lag_3_ihsdet_3}^{${est_lag_3_ihsdet_3}} $ &       $ ${beta_lag_3_ihsdet2_3}^{${est_lag_3_ihsdet2_3}} $ \\   
tex & ($ ${se_lag_3_ihsdet_3} $ ) & ($ ${se_lag_3_ihsdet2_3} $ ) \\
tex Reform, time 0 &         $ ${beta_date_0_ihsdet_3}^{${est_date_0_ihsdet_3}} $ &        $ ${beta_date_0_ihsdet2_3}^{${est_date_0_ihsdet2_3}} $ \\   
tex & ($ ${se_date_0_ihsdet_3} $ ) & ($ ${se_date_0_ihsdet2_3} $ ) \\
tex Observations          &        ${N_ihsdet_3}     &        ${N_ihsdet2_3} \\ 
tex R-squared        &          ${r2_ihsdet_3}   &          ${r2_ihsdet2_3} \\  
tex\\
tex & \multicolumn{2}{c}{quartic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 5 years &       $ ${beta_lag_5_ihsdet_4}^{${est_lag_5_ihsdet_4}} $ &       $ ${beta_lag_5_ihsdet2_4}^{${est_lag_5_ihsdet2_4}} $  \\
tex & ($ ${se_lag_5_ihsdet_4} $ ) & ($ ${se_lag_5_ihsdet2_4} $ ) \\
tex Lag 4 years &       $ ${beta_lag_4_ihsdet_4}^{${est_lag_4_ihsdet_4}} $ &        $ ${beta_lag_4_ihsdet2_4}^{${est_lag_4_ihsdet2_4}} $ \\  
tex & ($ ${se_lag_4_ihsdet_4} $ ) & ($ ${se_lag_4_ihsdet2_4} $ ) \\
tex Lag 3 years &          $ ${beta_lag_3_ihsdet_4}^{${est_lag_3_ihsdet_4}} $ &       $ ${beta_lag_3_ihsdet2_4}^{${est_lag_3_ihsdet2_4}} $ \\   
tex & ($ ${se_lag_3_ihsdet_4} $ ) & ($ ${se_lag_3_ihsdet2_4} $ ) \\
tex Reform, time 0 &         $ ${beta_date_0_ihsdet_4}^{${est_date_0_ihsdet_4}} $ &        $ ${beta_date_0_ihsdet2_4}^{${est_date_0_ihsdet2_4}} $ \\   
tex & ($ ${se_date_0_ihsdet_4} $ ) & ($ ${se_date_0_ihsdet2_4} $ ) \\
tex Observations          &        ${N_ihsdet_4}     &        ${N_ihsdet2_4} \\ 
tex R-squared        &          ${r2_ihsdet_4}   &          ${r2_ihsdet2_4} \\  
tex\\
tex Mun. FEs        &     \checkmark         &  \checkmark   \\
tex Year. FEs     &     \checkmark         &  \checkmark  \\
tex State Controls$^a$  &    \checkmark     &       \checkmark \\
tex Cohort weighted  &         \checkmark &         \checkmark \\
tex \hline \hline      
tex \multicolumn{3}{p{0.9\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 6 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020} or because they are collinear or inexistent, like lag time period 2. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. Logged homicides per capita at the municipality level are also included as controls.}} \\
tex \end{tabular}
tex } 
tex \end{table}
*tex \end{landscape}
texdoc close

