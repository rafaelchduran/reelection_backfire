*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*Additional Tables   
*****************************************************

clear all
set more off  
set varabbrev off 

*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Dofiles"

**********************************************
*Table: NAIVE TWFE REFORM ON VIOLENCE
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc

	xtset inegi year 

est clear
eststo: areg  logdefuncionespc reform $lagDV i.year, a(inegi) vce(cluster estado)
 estadd local fixed \checkmark
 estadd local yearfe	\checkmark
 estadd local lagged  \checkmark
 
*eststo: areg  ihs_defuncionespc reform $lagDV2 i.year, a(inegi) vce(cluster estado)

 
esttab using "../Tables/naive_twfe.tex", replace f b(%9.4f) se(%9.4f) se  star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
 s(N r2 fixed yearfe lagged, fmt(0 3) label("Observations" "R-squared" "Mun. FE" "Year FE" "Lag DV")) ///
keep(reform) ///
coeflabel(reform "Term Limit Reform") ///
collabels(none) nonotes booktabs nomtitles nolines

**********************************************
*Table: Mechanisms: INCUMBENT GENDER WITH POLYNOMIALS
**********************************************
use "../Data/ConstructionDatabase/data_wleads&lags_incumbency_weights.dta", clear

*GLOBALS: treatment
capture global saturated date_0_2015 date_0_2016 date_0_2017 date_0_2018 lag_2_2016 lag_3_2015 lag_3_2016 lag_3_2018 lag_4_2015 lag_4_2017 lag_5_2015 lag_5_2018 lag_6_2016 lag_6_2018 lag_8_2018 lead_1_2015 lead_1_2016
sum $saturated

*GLOBALS: controls
foreach i in margin_ HHI_ effectiveparties_ NP_ golosov_ governor_alignment_ pop_ logdefuncionespc_{
global `i'  `i'lag_6 `i'lag_5 `i'lag_4 `i'lag_3
}

global allcov2 $margin_  $governor_alignment_ 
*global allcov2 winning_margin_governor governor_alignment

xtset inegi year
global DV $logdefuncionespc_
global DV2 logdefuncionespc

*outcomes: incumbent_yesterday_w_tomorrow2 inc_party_runsfor1 numparties_eff numparties_eff_molinar inc_party_won
est clear
foreach j in incumbent_quality {
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

*generate outcome
gen gender=0 if sexo=="H"
replace gender=1 if sexo=="M"

*------------------
*A)SAMPLE INC. AT T-1 WINS AT T+1 :
*------------------
preserve
foreach i in incumbent_yesterday_w_tomorrow2{
rdbwselect  `i' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal2 = e(h_CCT)

quietly areg  `i'  $saturated pol2 $interacted_pol2 $allcov $DV2  i.year if mv_incparty<${optimal2} & mv_incparty>-${optimal2}, a(inegi) vce(cluster inegi)
keep if e(sample)==1
}

est clear
foreach j in gender {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 

******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol1 $allcov2 $DV   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_logdet_`pol':  di %5.4f e(r2)
		glo N_logdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
			
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]+_b[`i'_2017]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}




******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol1 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
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
}


}

foreach pol in 2 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet_`pol':  di %5.4f e(r2)
		glo N_logdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]+_b[`i'_2017]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}




******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
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
}


}

foreach pol in 3 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol3 $allcov2 $DV   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet_`pol':  di %5.4f e(r2)
		glo N_logdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]+_b[`i'_2017]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}




******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol3 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
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
}


}

foreach pol in 4 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol4 $allcov2 $DV   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet_`pol':  di %5.4f e(r2)
		glo N_logdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015]+_b[`i'_2017]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet_`pol': di r(p)
	glo beta_`i'_logdet_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet_`pol'= "" 
			if (${p_`i'_logdet_`pol'}<=0.1) global est_`i'_logdet_`pol' = "*"
			if (${p_`i'_logdet_`pol'}<=0.05) global est_`i'_logdet_`pol' = "**"
			if (${p_`i'_logdet_`pol'}<=0.01) global est_`i'_logdet_`pol' = "***"	
}




******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol4 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet_`pol': di r(p)
	glo beta_`i'_ihsdet_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet_`pol'= "" 
			if (${p_`i'_ihsdet_`pol'}<=0.1) global est_`i'_ihsdet_`pol' = "*"
			if (${p_`i'_ihsdet_`pol'}<=0.05) global est_`i'_ihsdet_`pol' = "**"
			if (${p_`i'_ihsdet_`pol'}<=0.01) global est_`i'_ihsdet_`pol' = "***"	
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
}


}


}
restore


*------------------
*B)WITH
*------------------
preserve
foreach i in inc_party_won{


/*foreach j in incumbent_yesterday_w_tomorrow2{
rdbwselect  `j' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal2 = e(h_CCT)

quietly areg  `j'  $saturated pol2 $interacted_pol2 $allcov $DV2  i.year if mv_incparty<${optimal2} & mv_incparty>-${optimal2}, a(inegi) vce(cluster inegi)
keep if e(sample)==1
}
*/

rdbwselect  `i' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal2 = e(h_CCT)

quietly areg  `i'  $saturated pol2 $interacted_pol2 $allcov $DV2  i.year if mv_incparty<${optimal2} & mv_incparty>-${optimal2}, a(inegi) vce(cluster inegi)
keep if e(sample)==1
}

est clear
foreach j in gender{
foreach pol in 1{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 



******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol1 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet2_`pol':  di %5.4f e(r2)
		glo N_logdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2017]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}



******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol1 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
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
}

}


foreach pol in 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 



******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet2_`pol':  di %5.4f e(r2)
		glo N_logdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2017]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}



******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
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
}

}

foreach pol in 3{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 



******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol3 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet2_`pol':  di %5.4f e(r2)
		glo N_logdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2017]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}



******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol3 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
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
}

}

foreach pol in 4{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 



******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol4 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

	
		glo r2_logdet2_`pol':  di %5.4f e(r2)
		glo N_logdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_6 {
	di (_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}


foreach i in lag_5 {
	di (_b[`i'_2015]) +(_b[`i'_2018]) 
	test (_b[`i'_2015]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015]+_b[`i'_2017])
	test (_b[`i'_2015]+_b[`i'_2017])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2017]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet2_`pol': di r(p)
	glo beta_`i'_logdet2_`pol': di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  
	glo est_`i'_logdet2_`pol'= "" 
			if (${p_`i'_logdet2_`pol'}<=0.1) global est_`i'_logdet2_`pol' = "*"
			if (${p_`i'_logdet2_`pol'}<=0.05) global est_`i'_logdet2_`pol' = "**"
			if (${p_`i'_logdet2_`pol'}<=0.01) global est_`i'_logdet2_`pol' = "***"	
}



******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol4 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_6 {
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	test (_b[`i'_2016]*`b'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
}


foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a'+_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  )
	test (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) =0
	glo p_`i'_ihsdet2_`pol': di r(p)
	glo beta_`i'_ihsdet2_`pol': di %5.4f (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c') )
	glo est_`i'_ihsdet2_`pol'= "" 
			if (${p_`i'_ihsdet2_`pol'}<=0.1) global est_`i'_ihsdet2_`pol' = "*"
			if (${p_`i'_ihsdet2_`pol'}<=0.05) global est_`i'_ihsdet2_`pol' = "**"
			if (${p_`i'_ihsdet2_`pol'}<=0.01) global est_`i'_ihsdet2_`pol' = "***"	
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
}

}
}

restore


texdoc init  "../Tables/abraham_sun_gender.tex", replace force
*tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Event-in-Discontinuity in close elections model: Effect of 2014 Term Limit Reform on Incumbent's Quality }
tex \label{tab:incumbency_gender}
tex \scalebox{0.6}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{Incumbent quality indicator}   \\
tex & (1) & (2) \\
*tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}  \\
*tex \addlinespace
tex & \multicolumn{2}{c}{linear polynomial} \\
tex \cmidrule(lrr){2-3} \\
*tex Lag 6 years &     $ ${beta_lag_6_ihsdet_1}^{${est_lag_6_ihsdet_1}} $ &     $ ${beta_lag_6_ihsdet2_1}^{${est_lag_6_ihsdet2_1}} $  \\
*tex \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_1}^{${est_lag_5_ihsdet_1}} $ &       $ ${beta_lag_5_ihsdet2_1}^{${est_lag_5_ihsdet2_1}} $  \\
tex \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_1}^{${est_lag_4_ihsdet_1}} $ &       $ ${beta_lag_4_ihsdet2_1}^{${est_lag_4_ihsdet2_1}} $\\  
tex \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_1}^{${est_lag_3_ihsdet_1}} $  &     $ ${beta_lag_3_ihsdet2_1}^{${est_lag_3_ihsdet2_1}} $ \\   
tex \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_1}^{${est_date_0_ihsdet_1}} $ &     $ ${beta_date_0_ihsdet2_1}^{${est_date_0_ihsdet2_1}} $\\   
tex \\
tex Observations        &        ${N_ihsdet_1}    &        ${N_ihsdet2_1} \\ 
tex R-squared        &          ${r2_ihsdet_1}  &          ${r2_ihsdet2_1} \\  
tex\\
tex & \multicolumn{2}{c}{quadratic polynomial} \\
tex \cmidrule(lrr){2-3} \\
*tex Lag 6 years &     $ ${beta_lag_6_ihsdet_2}^{${est_lag_6_ihsdet_2}} $ &     $ ${beta_lag_6_ihsdet2_2}^{${est_lag_6_ihsdet2_2}} $  \\
*tex \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_2}^{${est_lag_5_ihsdet_2}} $ &       $ ${beta_lag_5_ihsdet2_2}^{${est_lag_5_ihsdet2_2}} $  \\
tex \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_2}^{${est_lag_4_ihsdet_2}} $ &       $ ${beta_lag_4_ihsdet2_2}^{${est_lag_4_ihsdet2_2}} $\\  
tex \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_2}^{${est_lag_3_ihsdet_2}} $  &     $ ${beta_lag_3_ihsdet2_2}^{${est_lag_3_ihsdet2_2}} $ \\   
tex \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_2}^{${est_date_0_ihsdet_2}} $ &     $ ${beta_date_0_ihsdet2_2}^{${est_date_0_ihsdet2_2}} $\\   
tex \\
tex Observations        &        ${N_ihsdet_2}    &        ${N_ihsdet2_2} \\ 
tex R-squared        &          ${r2_ihsdet_2}  &          ${r2_ihsdet2_2} \\  
tex \\
tex & \multicolumn{2}{c}{cubic polynomial} \\
tex \cmidrule(lrr){2-3} \\
*tex Lag 6 years &     $ ${beta_lag_6_ihsdet_3}^{${est_lag_6_ihsdet_3}} $ &     $ ${beta_lag_6_ihsdet2_3}^{${est_lag_6_ihsdet2_3}} $  \\
*tex \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_3}^{${est_lag_5_ihsdet_3}} $ &       $ ${beta_lag_5_ihsdet2_3}^{${est_lag_5_ihsdet2_3}} $  \\
tex \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_3}^{${est_lag_4_ihsdet_3}} $ &       $ ${beta_lag_4_ihsdet2_3}^{${est_lag_4_ihsdet2_3}} $\\  
tex \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_3}^{${est_lag_3_ihsdet_3}} $  &     $ ${beta_lag_3_ihsdet2_3}^{${est_lag_3_ihsdet2_3}} $ \\   
tex \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_3}^{${est_date_0_ihsdet_3}} $ &     $ ${beta_date_0_ihsdet2_3}^{${est_date_0_ihsdet2_3}} $\\   
tex \\
tex Observations        &        ${N_ihsdet_3}    &        ${N_ihsdet2_3} \\ 
tex R-squared        &          ${r2_ihsdet_3}  &          ${r2_ihsdet2_3} \\  
tex \\
tex & \multicolumn{2}{c}{quartic polynomial} \\
tex \cmidrule(lrr){2-3} \\
*tex Lag 6 years &     $ ${beta_lag_6_ihsdet_4}^{${est_lag_6_ihsdet_4}} $ &     $ ${beta_lag_6_ihsdet2_4}^{${est_lag_6_ihsdet2_4}} $  \\
*tex \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_4}^{${est_lag_5_ihsdet_4}} $ &       $ ${beta_lag_5_ihsdet2_4}^{${est_lag_5_ihsdet2_4}} $  \\
tex \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_4}^{${est_lag_4_ihsdet_4}} $ &       $ ${beta_lag_4_ihsdet2_4}^{${est_lag_4_ihsdet2_4}} $\\  
tex \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_4}^{${est_lag_3_ihsdet_4}} $  &     $ ${beta_lag_3_ihsdet2_4}^{${est_lag_3_ihsdet2_4}} $ \\   
tex \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_4}^{${est_date_0_ihsdet_4}} $ &     $ ${beta_date_0_ihsdet2_4}^{${est_date_0_ihsdet2_4}} $\\   
tex \\
tex Observations        &        ${N_ihsdet_4}    &        ${N_ihsdet2_4} \\ 
tex R-squared        &          ${r2_ihsdet_4}  &          ${r2_ihsdet2_4} \\  
tex\\
tex Mun. FEs      &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   \\
tex State Controls$^b$  &    \checkmark     &       \checkmark \\
tex Mun Controls$^c$  &    \checkmark     &       \checkmark \\
tex Cohort weighted  &     \checkmark &        \checkmark \\
tex Sample Inc. Adv. DV & Inc. at t-1 won at t+1 & Inc. at t won at t+1 \\
tex \hline \hline      
tex \multicolumn{3}{p{0.75\textwidth}}{\footnotesize{Notes: Linear combination of the Cohort Average Treatment Effects (CATTs) for reach relative time period, weighting by each cohort's relative share of the sample following \citet{abraham_sun_2020}. Two relative time periods (lag 6, 3 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020} or because they are collinear or inexistent. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-levels: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Even columns with outcomes where missing values where replaced by zeros assuming no activity was registered. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. $^c$ Municipal controls include logged homicides per capita interacted with cohort fixed effects pre-treatment.}} \\
tex \end{tabular}
tex } 
tex \end{table}
*tex \end{landscape}
texdoc close



**********************************************
*Table: WITH LAGGED DV & CITIZEN DEMANDS
**********************************************

/*Questions of interest
PERCEPCION DE INSEGURIDAD
Temas que mas preocupan
ap4_2_3 narcotrafico
ap4_2_5 inseguridad
ap4_2_11 falta castigo a delincuentes

¿En términos de delincuencia, considera que vivir en ... es… [use this to measure distance]
ap4_3_1 colonia
ap4_3_2 municipio
ap4_3_3 estado

4.7 De acuerdo con su experiencia, ¿considera que en lo que resta de 2019 la seguridad pública en (ÁMBITO GEOGRÁFICO) …
ap4_7_1 colonia
ap4_7_2 municipio
ap4_7_3 estado


For 2011, 
4.11 ¿Cuánto gastaron en total por esas medidas durante 2010?
ap4_11

For 2012-2019
4.12 ¿Cuánto gastaron en total por esas medidas durante 2018?
ap4_12

DESEMPENO INSTITUCIONAL
¿Sabe usted si alguna de las siguientes acciones se realizaron en el 2018 en su (MUNICIPIO/LOCALIDAD), como… 
ap5_1_6 contratar seguridad privada?
ap5_1_7 policias barriales
ap5_1_8 operativo contra delincuencia
ap5_1_10 patrullaje
ap5_1_12 combatir narco


5.4 ¿Cuánta confianza le inspira la (el) (AUTORIDAD)?
ap5_4_2 policia preventiva municipal
ap5_4_3 policia estatal
ap5_4_4 policia federal
ap5_4_5 policia ministerial o judicial
ap5_4_6 MP, procuradurias estatales
ap5_4_7 PGR
ap5_4_8 ejercito
ap5_4_9 marina


ap4_2_3 ap4_2_5 ap4_2_11 
ap4_3_1 ap4_3_2 ap4_3_3 
ap4_7_1 ap4_7_2 ap4_7_3 
ap4_12b 
ap5_1_6 ap5_1_7 ap5_1_8 
ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_9 
ap4_7_1_b ap4_7_2_b ap4_7_3_b

*/
use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

replace ap4_2_11=. if ap4_2_11==11

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc ap4_2_11
	global lagDV2 l.ihs_defuncionespc
	xtset inegi year 
	
************
***A: log(homicides per capita) without covariates
************
quietly	areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster estado)
keep if e(sample)==1
set matsize 11000

est clear
eststo: 	areg  logdefuncionespc $controls  $saturated  i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log_nocov:  di %5.4f e(r2)
		glo N_log_nocov: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'r2_log_nocov: di r(p)
	glo beta_`i'r2_log_nocov: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'r2_log_nocov= "" 
			if (${p_`i'r2_log_nocov}<=0.1) global est_`i'r2_log_nocov = "*"
			if (${p_`i'r2_log_nocov}<=0.05) global est_`i'r2_log_nocov = "**"
			if (${p_`i'r2_log_nocov}<=0.01) global est_`i'r2_log_nocov = "***"	
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
}


************
***B: ihs(homicides per capita) without covariates
************	
quietly	areg  ihs_defuncionespc  $saturated $controls   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs_nocov:  di %5.4f e(r2)
		glo N_ihs_nocov: di %11.2gc e(N)
***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'r2_ihs_nocov: di r(p)
	glo beta_`i'r2_ihs_nocov: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'r2_ihs_nocov= "" 
			if (${p_`i'r2_ihs_nocov}<=0.1) global est_`i'r2_ihs_nocov = "*"
			if (${p_`i'r2_ihs_nocov}<=0.05) global est_`i'r2_ihs_nocov = "**"
			if (${p_`i'r2_ihs_nocov}<=0.01) global est_`i'r2_ihs_nocov = "***"	
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
}


************
***C: log(homicides per capita) with covariates
************	

est clear
quietly	areg  logdefuncionespc  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)

***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'r2_log: di r(p)
	glo beta_`i'r2_log: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'r2_log= "" 
			if (${p_`i'r2_log}<=0.1) global est_`i'r2_log = "*"
			if (${p_`i'r2_log}<=0.05) global est_`i'r2_log = "**"
			if (${p_`i'r2_log}<=0.01) global est_`i'r2_log = "***"	
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
}


foreach i in lead_2{
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
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
}


************
***D: ihs(homicides per capita) with covariates
************	
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'r2_ihs: di r(p)
	glo beta_`i'r2_ihs: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'r2_ihs= "" 
			if (${p_`i'r2_ihs}<=0.1) global est_`i'r2_ihs = "*"
			if (${p_`i'r2_ihs}<=0.05) global est_`i'r2_ihs = "**"
			if (${p_`i'r2_ihs}<=0.01) global est_`i'r2_ihs = "***"	
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
}

* You need to install the following packages:
	*ssc install texdoc, replace
	*ssc install texify, replace
	*ssc install estout, replace 
	
texdoc init  "../Tables/abraham_sun_estimates_lagDV&citizens.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence, cohort's weighted estimates and controlling for the lag of the dependent variable}
tex \label{tab:abraham_sun_lagdv}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace
*tex Lag 7 years &     $ ${beta_lag_7_log_nocov}^{${est_lag_7_log_nocov}} $ &     $ ${beta_lag_7_log}^{${est_lag_7_log}} $ & $ ${beta_lag_7_ihs_nocov}^{${est_lag_7_ihs_nocov}} $ & $ ${beta_lag_7_ihs}^{${est_lag_7_ihs}} $   \\
*tex \\
tex Lag 6 years &     $ ${beta_lag_6_log_nocov}^{${est_lag_6_log_nocov}} $ &     $ ${beta_lag_6_log}^{${est_lag_6_log}} $ &  $ ${beta_lag_6_ihs_nocov}^{${est_lag_6_ihs_nocov}} $ &  $ ${beta_lag_6_ihs}^{${est_lag_6_ihs}} $  \\
tex \\
tex Lag 5 years &     $ ${beta_lag_5_log_nocov}^{${est_lag_5_log_nocov}} $ &     $ ${beta_lag_5_log}^{${est_lag_5_log}} $ &  $ ${beta_lag_5_ihs_nocov}^{${est_lag_5_ihs_nocov}} $ &  $ ${beta_lag_5_ihs}^{${est_lag_5_ihs}} $ \\
tex \\
tex Lag 4 years &     $ ${beta_lag_4_log_nocov}^{${est_lag_4_log_nocov}} $ &     $ ${beta_lag_4_log}^{${est_lag_4_log}} $ &   $ ${beta_lag_4_ihs_nocov}^{${est_lag_4_ihs_nocov}} $ &   $ ${beta_lag_4_ihs}^{${est_lag_4_ihs}} $  \\
tex \\
tex Lag 3 years &     $ ${beta_lag_3_log_nocov}^{${est_lag_3_log_nocov}} $ &     $ ${beta_lag_3_log}^{${est_lag_3_log}} $ &   $ ${beta_lag_3_ihs_nocov}^{${est_lag_3_ihs_nocov}} $ &   $ ${beta_lag_3_ihs}^{${est_lag_3_ihs}} $ \\
tex \\
tex Lag 2 years &     $ ${beta_lag_2_log_nocov}^{${est_lag_2_log_nocov}} $ &     $ ${beta_lag_2_log}^{${est_lag_2_log}} $ &  $ ${beta_lag_2_ihs_nocov}^{${est_lag_2_ihs_nocov}} $  &  $ ${beta_lag_2_ihs}^{${est_lag_2_ihs}} $  \\
tex \\
tex Reform, time 0 &     $ ${beta_date_0_log_nocov}^{${est_date_0_log_nocov}} $ &     $ ${beta_date_0_log}^{${est_date_0_log}} $ &   $ ${beta_date_0_ihs_nocov}^{${est_date_0_ihs_nocov}} $   &   $ ${beta_date_0_ihs}^{${est_date_0_ihs}} $ \\
tex \\
tex Lead 1 year &     $ ${beta_lead_1_log_nocov}^{${est_lead_1_log_nocov}} $ &     $ ${beta_lead_1_log}^{${est_lead_1_log}} $ &    $ ${beta_lead_1_ihs_nocov}^{${est_lead_1_ihs_nocov}} $ &    $ ${beta_lead_1_ihs}^{${est_lead_1_ihs}} $ \\
tex \\
tex Lead 2 years &     $ ${beta_lead_2_log_nocov}^{${est_lead_2_log_nocov}} $ &     $ ${beta_lead_2_log}^{${est_lead_2_log}} $ &   $ ${beta_lead_2_ihs_nocov}^{${est_lead_2_ihs_nocov}} $  &   $ ${beta_lead_2_ihs}^{${est_lead_2_ihs}} $  \\
tex \\
tex Lead 3 years &     $ ${beta_lead_3_log_nocov}^{${est_lead_3_log_nocov}} $ &     $ ${beta_lead_3_log}^{${est_lead_3_log}} $ &   $ ${beta_lead_3_ihs_nocov}^{${est_lead_3_ihs_nocov}} $  &   $ ${beta_lead_3_ihs}^{${est_lead_3_ihs}} $ \\

tex \addlinespace
tex Observations       &        ${N_log_nocov}    &        ${N_log}    &     ${N_ihs_nocov}      &     ${N_ihs}  \\
tex R-squared        &          ${r2_log_nocov} &          ${r2_log}    &           ${r2_ihs_nocov}       &           ${r2_ihs}   \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^b$  &    \checkmark      &       \checkmark  &    \checkmark      &   \checkmark    \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\
tex Lag DV &       &       \checkmark  &         &   \checkmark    \\
tex State. Ins. Perc. &       &       \checkmark  &         &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{5}{p{0.75\textwidth}}{\footnotesize{Notes: Linear combination of the Cohort Average Treatment Effects (CATTs) for reach relative time period, weighting by each cohort's relative share of the sample following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed in columns (2) and (4) to avoid collinearity problems noted by \citet{abraham_sun_2020}; lag 7 is removed due to collinearity in columns (1) and (3). Standard errors in parentheses are clustered at the state level, with the following significance-levels: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


**********************************************
*Table: Abraham-Sun (2020) event-study and citizens demands: interactions
**********************************************
/*
ap4_2_3 - effect but too big; citizens that see narcotraffick as important
ap4_2_5 - effect but too big; citizens that see insecurity as important
ap4_2_11 - effect but too big; citizens that demand punishmnet
ap4_3_1 - effect but too big; citizens that see insecurity in their neighborhood
ap4_3_2 - effect but too big; citizens that see insecurity in their municipality
ap4_3_3 - effect but too big; citizens that see insecurity in their state
***effect of these last three diminishes strongly when moving away from home, i.e. externalities of war
ap4_7_1 
ap4_7_2 
ap4_7_3 
ap4_7_1_b 
ap4_7_2_b 
ap4_7_3_b

ap4_12b - no effect - money spent in security provision
ap5_1_6 - negative effect and strong -seguridad privada 
ap5_1_7 - negative effect and strong - policia barrial 
ap5_1_8 - negative effect and strong - operativo contra delincuencia
ap5_1_10 - negative effect and strong - patrullaje
ap5_1_12 - negative effect and strong - operativo contra narco
ap5_4_2_b policia preventiva municipal - negative effect and strong
ap5_4_3_b policia estatal - no effect
ap5_4_4_b policia federal - no effect
ap5_4_5_b policia ministerial o judicial - no effect
ap5_4_6_b MP, procuradurias estatales - no effect
ap5_4_7_b PGR - no effect
ap5_4_8_b ejercito - no effect
ap5_4_9_b marina - positive effect and strong

*I can use as placebo other unrelated questions

*next to do, run with pre-treatment split
*/

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear


*variable transformation
**money in thousands
replace ap4_12b=ap4_12b/1000

**create pretreatment citizens demands




*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc

	xtset inegi year 
************
***A: log(homicides per capita) with covariates 
************
areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster inegi)
keep if e(sample)==1

est clear
foreach i in ap5_4_6_b{

areg   logdefuncionespc c.${saturated}##c.`i' $controls  i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'1:  di %5.4f e(r2)
		glo N_`i'1: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
	glo beta_`i'1: di %5.4f r(estimate)
	glo se_`i'1: di %5.4f r(se)
	glo g_`i'1: di r(df)
	glo t_`i'1: di  %5.4f ${beta_`i'1}/${se_`i'1}
	glo p_`i'1: di 2*ttail(${g_`i'1},abs(${t_`i'1})) 
	glo est_`i'1= "" 
			if (${p_`i'1}<=0.1) global est_`i'1  = "*"
			if (${p_`i'1}<=0.05) global est_`i'1 = "**"
			if (${p_`i'1}<=0.01) global est_`i'1 = "***"	
}
			

************
***B: ihs(homicides per capita) with covariates 
************


est clear
foreach i in ap5_4_7_b{

areg   logdefuncionespc c.${saturated}##c.`i' $controls  i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'2:  di %5.4f e(r2)
		glo N_`i'2: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
	glo beta_`i'2: di %5.4f r(estimate)
	glo se_`i'2: di %5.4f r(se)
	glo g_`i'2: di r(df)
	glo t_`i'2: di  %5.4f ${beta_`i'2}/${se_`i'2}
	glo p_`i'2: di 2*ttail(${g_`i'2},abs(${t_`i'2})) 
	glo est_`i'2= "" 
			if (${p_`i'2}<=0.1) global est_`i'2  = "*"
			if (${p_`i'2}<=0.05) global est_`i'2 = "**"
			if (${p_`i'2}<=0.01) global est_`i'2 = "***"	
}


************
***C: log(homicides per capita) with covariates 
************
est clear
foreach i in ap5_4_8_b{

areg   logdefuncionespc c.${saturated}##c.`i' $controls  i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'3:  di %5.4f e(r2)
		glo N_`i'3: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
	glo beta_`i'3: di %5.4f r(estimate)
	glo se_`i'3: di %5.4f r(se)
	glo g_`i'3: di r(df)
	glo t_`i'3: di  %5.4f ${beta_`i'3}/${se_`i'3}
	glo p_`i'3: di 2*ttail(${g_`i'3},abs(${t_`i'3})) 
	glo est_`i'3= "" 
			if (${p_`i'3}<=0.1) global est_`i'3  = "*"
			if (${p_`i'3}<=0.05) global est_`i'3 = "**"
			if (${p_`i'3}<=0.01) global est_`i'3 = "***"	
}


************
***D: ihs(homicides per capita) with covariates 
************
est clear
foreach i in ap5_4_9_b{

areg   logdefuncionespc c.${saturated}##c.`i' $controls  i.year, a(inegi) vce(cluster inegi)
		glo r2_`i'4:  di %5.4f e(r2)
		glo N_`i'4: di %11.2gc e(N)	

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] 
	glo beta_`i'4: di %5.4f r(estimate)
	glo se_`i'4: di %5.4f r(se)
	glo g_`i'4: di r(df)
	glo t_`i'4: di  %5.4f ${beta_`i'4}/${se_`i'4}
	glo p_`i'4: di 2*ttail(${g_`i'4},abs(${t_`i'4})) 
	glo est_`i'4= "" 
			if (${p_`i'4}<=0.1) global est_`i'4  = "*"
			if (${p_`i'4}<=0.05) global est_`i'4 = "**"
			if (${p_`i'4}<=0.01) global est_`i'4 = "***"	
}





texdoc init  "../Tables/abraham_sun_estimates_citizens.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect: Citizens Security Perception$^a$}
tex \label{tab:abraham_sun_citizens}
tex \scalebox{1}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{b}$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace

tex Reform (t+3)*patrullaje &     $ ${beta_ap5_4_6_b1}^{${est_ap5_4_6_b1}} $ &  &  $ ${beta_ap5_4_7_b2}^{${est_ap5_473_b2}} $  & \\
tex  &     ($ ${se_ap5_4_61} $) &  & ($ ${se_ap5_4_7_b2} $)  & \\

tex Reform (t+3)*narco &    &   $ ${beta_ap5_4_8_b3}^{${est_ap5_4_8_b3}} $ &  &  $ ${beta_ap5_4_9_b4}^{${est_ap5_4_9_b4}} $ \\
tex  &    &     ($ ${se_ap5_4_8_b3} $) & & ($ ${se_ap5_4_9_b4} $)  \\

tex \addlinespace
tex Observations       &        ${N_alignment_executive_strong1}    &        ${N_alignment_executive_strong2}    &     ${N_winning_margin3}      &     ${N_winning_margin4}  \\
tex R-squared       &        ${r2_alignment_executive_strong1}    &        ${r2_alignment_executive_strong2}    &     ${r2_winning_margin3}      &     ${r2_winning_margin4}  \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^c$  &  \checkmark       &       \checkmark  &  \checkmark        &   \checkmark    \\
tex Cohort weighted$^d$  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{5}{p{0.9\textwidth}}{\footnotesize{Notes:$^a$ Total interaction effect tests the linear hypothesis of the estimated coefficient of alignment with Federal Government indicator (winning margin) + estimated coefficient of the interaction of alignment (winning margin)*lead t=3. This is a post-estimation test using the same specification as that of Table \ref{abraham_sun}. Other leads and the indicator at time t=0 when reform came to effect are omitted due to collinearity. Standard errors of linear hypothesis test in parentheses with the following significance-levels: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided test with the null hypothesis equal to 0. $^b$ Refers to the inverse hyperbolic sine transformation. $^c$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. $^d$ Estimates weighted by each cohort's relative share of the sample following \citet{abraham_sun_2020}.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

**********************************************
*Table: Mechanisms: INCUMBENCY ADVANTAGE 
*get more significance if I drop incumbent_yesterday_w_tomorrow instead of incumbent_yesterday_w_tomorrow2
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

global DV 
*logdefuncionespc


*outcomes: incumbent_yesterday_w_tomorrow2 inc_party_runsfor1 numparties_eff numparties_eff_molinar inc_party_won
est clear
foreach j in incumbent_yesterday_w_tomorrow2{
*Estimate optimal bandwidth:
rdbwselect  incumbent_yesterday_w_tomorrow2 mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
global optimal_75 = e(h_CCT)*.75
global optimal_half = e(h_CCT)*.5

di ${optimal}
di ${optimal_75}
di ${optimal_half}


*Estimates:


******************
*1) no weights
******************
est clear
eststo: areg  `j'  $saturated $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_logdet:  di %5.4f e(r2)
		glo N_logdet: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:


foreach i in lag_5 {
	di (_b[`i'_2018]) 
	test (_b[`i'_2018])  =0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2018]) 
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
}

foreach i in lag_4 {
	di (_b[`i'_2015])
	test (_b[`i'_2015])  =0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015])+(_b[`i'_2017]) 
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
}

foreach i in lag_3 {
	di (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	test (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018])  =0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015])+(_b[`i'_2016]) +(_b[`i'_2018]) 
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
}

foreach i in date_0 {
	di (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])*-1 
	test (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])  =0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015])+(_b[`i'_2016])+(_b[`i'_2017])  +(_b[`i'_2018])*-1  
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
}



******************
*2) w/ weights
******************
est clear
eststo: areg  `j'  $saturated $allcov2 $DV   i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
	
		glo r2_ihsdet:  di %5.4f e(r2)
		glo N_ihsdet: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:


foreach i in lag_5 {
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2018]*`c') 
	test (_b[`i'_2018]*`c')  =0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)

	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a') =0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
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
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
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
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') *-1
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') =0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d')]*-1 
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
}


}



texdoc init  "../Tables/abraham_sun_incumbency.tex", replace force
*tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Event-in-Discontinuity in close elections model: Effect of 2014 Term Limit Reform on Incumbency Advantage}
tex \label{tab:incumbency}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{incumbency advantage}   \\
tex & \multicolumn{1}{c}{(w/o cohort weights)} & \multicolumn{1}{c}{(w/ cohort weights)} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \\
tex \addlinespace

*tex Lag 7 years &     $ ${beta_lag_7_logdet}^{${est_lag_7_logdet}} $ &     $ ${beta_lag_7_ihsdet}^{${est_lag_7_ihsdet}} $ \\
*tex \\
*tex Lag 6 years &     $ ${beta_lag_6_logdet}^{${est_lag_6_logdet}} $ &     $ ${beta_lag_6_ihsdet}^{${est_lag_6_ihsdet}} $  \\
*tex \\
tex Lag 5 years &     $ ${beta_lag_5_logdet}^{${est_lag_5_logdet}} $ &     $ ${beta_lag_5_ihsdet}^{${est_lag_5_ihsdet}} $  \\
tex \\
tex Lag 4 years &     $ ${beta_lag_4_logdet}^{${est_lag_4_logdet}} $ &     $ ${beta_lag_4_ihsdet}^{${est_lag_4_ihsdet}} $ \\  
tex \\
tex Lag 3 years &     $ ${beta_lag_3_logdet}^{${est_lag_3_logdet}} $ &     $ ${beta_lag_3_ihsdet}^{${est_lag_3_ihsdet}} $  \\   
tex \\
*tex Lag 2 years &     $ ${beta_lag_2_logdet}^{${est_lag_2_logdet}} $ &     $ ${beta_lag_2_ihsdet}^{${est_lag_2_ihsdet}} $ \\  
*tex \\
tex Reform, time 0 &     $ ${beta_date_0_logdet}^{${est_date_0_logdet}} $ &     $ ${beta_date_0_ihsdet}^{${est_date_0_ihsdet}} $ \\   
tex \\
*tex Lead 1 year &     $ ${beta_lead_1_logdet}^{${est_lead_1_logdet}} $ &     $ ${beta_lead_1_ihsdet}^{${est_lead_1_ihsdet}} $  \\    
*tex \\
*tex Lead 2 years &     $ ${beta_lead_2_logdet}^{${est_lead_2_logdet}} $ &     $ ${beta_lead_2_ihsdet}^{${est_lead_2_ihsdet}} $ \\  
*tex \\
*tex Lead 3 years &     $ ${beta_lead_3_logdet}^{${est_lead_3_logdet}} $ &     $ ${beta_lead_3_ihsdet}^{${est_lead_3_ihsdet}} $  \\  

tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}  \\ 
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet} \\  

tex Mun. FEs      &     \checkmark         &  \checkmark  \\
tex Year. FEs    &     \checkmark         &  \checkmark  \\
tex State Controls$^b$  &    \checkmark     &       \checkmark \\
tex Cohort weighted  &   \checkmark      &       \checkmark  \\

tex \hline \hline      
tex \multicolumn{3}{p{0.65\textwidth}}{\footnotesize{Notes: Linear combination of the Cohort Average Treatment Effects (CATTs) for reach relative time period, weighting by each cohort's relative share of the sample following \citet{abraham_sun_2020}. Two relative time periods (lag 7 and 1) are removed in columns (2) and (4) to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-levels: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Even columns with outcomes where missing values where replaced by zeros assuming no activity was registered. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
*tex \end{landscape}
texdoc close



**********************************************
*Table: Mechanisms: INCUMBENCY ADVANTAGE OTHER ESTIMATIONS
**********************************************
use "../Data/ConstructionDatabase/data_wleads&lags_incumbency_weights.dta", clear

*GLOBALS: treatment
capture global saturated date_0_2015 date_0_2016 date_0_2017 date_0_2018 lag_2_2016 lag_3_2015 lag_3_2016 lag_3_2018 lag_4_2015 lag_4_2017 lag_5_2015 lag_5_2018 lag_6_2016 lag_6_2018 lag_8_2018 lead_1_2015 lead_1_2016

*GLOBALS: controls
foreach i in margin_ HHI_ effectiveparties_ NP_ golosov_ governor_alignment_{
global `i' `i'lag_6 `i'lag_5 `i'lag_4 `i'lag_3 
}

global allcov2 $margin_  $governor_alignment_


*outcomes: incumbent_yesterday_w_tomorrow2 inc_party_runsfor1 numparties_eff numparties_eff_molinar inc_party_won
est clear
foreach j in incumbent_yesterday_w_tomorrow2{
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

*Interacted with reform indicator
foreach i in pol1 pol2 pol3 pol4 {
foreach var in reform{
gen `var'_`i'=`var'*`i'
}
}

*polynomial globals
capture global interacted_pol1 date_0_2015_pol1  date_0_2016_pol1  date_0_2017_pol1  date_0_2018_pol1  lag_5_2018_pol1  lag_4_2015_pol1  lag_3_2015_pol1  lag_3_2016_pol1  lag_3_2018_pol1
capture global interacted_pol2 date_0_2015_pol2  date_0_2016_pol2  date_0_2017_pol2  date_0_2018_pol2  lag_5_2018_pol2  lag_4_2015_pol2  lag_3_2015_pol2  lag_3_2016_pol2  lag_3_2018_pol2
capture global interacted_pol3 date_0_2015_pol3  date_0_2016_pol3  date_0_2017_pol3  date_0_2018_pol3  lag_5_2018_pol3  lag_4_2015_pol3  lag_3_2015_pol3  lag_3_2016_pol3  lag_3_2018_pol3
capture global interacted_pol4 date_0_2015_pol4  date_0_2016_pol4  date_0_2017_pol4  date_0_2018_pol4  lag_5_2018_pol4  lag_4_2015_pol4  lag_3_2015_pol4  lag_3_2016_pol4  lag_3_2018_pol4



*Estimates: by treatment group (but I'm comparing early vs late adopters)
est clear
foreach outcome in incumbent_yesterday_w_tomorrow2{
foreach j in pol1{
*foreach j in pol1 pol2 pol3 pol4 pol5{
eststo: areg  `outcome'  reform `j' reform_`j'    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
eststo: areg  `outcome'  reform `j' reform_`j'    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} & year<2016, a(inegi) vce(cluster inegi)
eststo: areg  `outcome'  reform `j' reform_`j'    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} & year<2017 & year!=2015, a(inegi) vce(cluster inegi)
eststo: areg  `outcome'  reform `j' reform_`j'    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} & year<2018 & year!=2015 & year!=2016, a(inegi) vce(cluster inegi)
eststo: areg  `outcome'  reform `j' reform_`j'    i.year if mv_incparty<${optimal} & mv_incparty>-${optimal} & year<2019 & year!=2015 & year!=2016 & year!=2017, a(inegi) vce(cluster inegi)
}
}

esttab est*, keep(reform) star(* 0.1 ** 0.05 *** 0.01) t


*Interact year dummies with pol
foreach i in pol1 pol2 pol3 pol4{
foreach y in 1 2 3 4 5 6 7 8 9 10{
gen year_`y'_`i'=year_`y'*`i'
}
}

*create globals 
global years year_1 year_2 year_3 year_4 year_5 year_6 year_7 year_8 year_9 year_10
global year_pol1  year_1_pol1 year_2_pol1 year_3_pol1 year_4_pol1 year_5_pol1 year_6_pol1 year_7_pol1 year_8_pol1 year_9_pol1 year_10_pol1
global year_pol2  year_1_pol2 year_2_pol2 year_3_pol2 year_4_pol2 year_5_pol2 year_6_pol2 year_7_pol2 year_8_pol2 year_9_pol2 year_10_pol2
global year_pol3  year_1_pol3 year_2_pol3 year_3_pol3 year_4_pol3 year_5_pol3 year_6_pol3 year_7_pol3 year_8_pol3 year_9_pol3 year_10_pol3
global year_pol4  year_1_pol4 year_2_pol4 year_3_pol4 year_4_pol4 year_5_pol4 year_6_pol4 year_7_pol4 year_8_pol4 year_9_pol4 year_10_pol4
global year_pol5  year_1_pol5 year_2_pol5 year_3_pol5 year_4_pol5 year_5_pol5 year_6_pol5 year_7_pol5 year_8_pol5 year_9_pol5 year_10_pol5

est clear
foreach outcome in incumbent_yesterday_w_tomorrow2{
foreach pol in 1{
*Estimate optimal bandwidth:
rdbwselect  `outcome' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 
global optimal = e(h_CCT)
global optimal_75 = e(h_CCT)*.75
global optimal_half = e(h_CCT)*.5

di ${optimal}
di ${optimal_75}
di ${optimal_half}

eststo: areg  `outcome'  $years pol`pol' $year_pol4  if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
}
}

esttab est*, keep($years) star(* 0.1 ** 0.05 *** 0.01) t

******************
*Event study (no saturated model), no weights
**Interact lead/lags dummies with pol
foreach i in pol pol2 pol3 pol4 {
foreach y in 1 2 3 4 5 6 7 8{
gen lag_`y'_`i'=lag_`y'*`i'
}
}

foreach i in pol pol2 pol3 pol4 {
foreach y in 1 2 3 {
gen lead_`y'_`i'=lag_`y'*`i'
}
}

foreach i in pol pol2 pol3 pol4 {
foreach y in 0 {
gen date_`y'_`i'=date_`y'*`i'
}
}

**create globals:
global leadlagspol lag_8_pol lag_7_pol lag_6_pol lag_5_pol lag_4_pol lag_3_pol lag_2_pol date_0_pol lead_1_pol lead_2_pol lead_3_pol
global leadlagspol2 lag_8_pol2 lag_7_pol2 lag_6_pol2 lag_5_pol2 lag_4_pol2 lag_3_pol2 lag_2_pol2 date_0_pol2 lead_1_pol2 lead_2_pol2 lead_3_pol2
global leadlagspol3 lag_8_pol3 lag_7_pol3 lag_6_pol3 lag_5_pol3 lag_4_pol3 lag_3_pol3 lag_2_pol3 date_0_pol3 lead_1_pol3 lead_2_pol3 lead_3_pol3
global leadlagspol4 lag_8_pol4 lag_7_pol4 lag_6_pol4 lag_5_pol4 lag_4_pol4 lag_3_pol4 lag_2_pol4 date_0_pol4 lead_1_pol4 lead_2_pol4 lead_3_pol4
global leadlagspol5 lag_8_pol5 lag_7_pol5 lag_6_pol5 lag_5_pol5 lag_4_pol5 lag_3_pol5 lag_2_pol5 date_0_pol5 lead_1_pol5 lead_2_pol5 lead_3_pol5

global treatments   lag_6 lag_5 lag_4 lag_3  date_0 lead_1 lead_2 lead_3

est clear
foreach outcome in incumbent_yesterday_w_tomorrow2{
foreach j in pol pol2 pol3 pol4 pol5{
eststo: areg  `outcome'  $treatments `j' $leadlags_`j' $allcov2 , a(inegi) vce(cluster inegi)
eststo: areg  `outcome'  $treatments `j' $leadlags_`j' $allcov2 if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
eststo: areg  `outcome'  $treatments `j' $leadlags_`j' $allcov2 if mv_incparty<${optimal_75} & mv_incparty>-${optimal_75}, a(inegi) vce(cluster inegi)
}
}

esttab est*, keep($treatments) star(* 0.1 ** 0.05 *** 0.01) t

******************
*Do cohort-year weighted estimate

*1) PARTY INCUMBENCY ADVANTAGE: with no coalitions
******************
foreach outcome in incumbent_yesterday_w_tomorrow2{
foreach j in pol{
est clear
eststo: areg  `outcome' `j' $interacted_`j'  $saturated $allcov2  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihsdet:  di %5.4f e(r2)
		glo N_ihsdet: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
}

foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2018]*`c') =0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') =0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') 
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
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
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
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
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
}



foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
}


foreach i in lead_2{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	di (_b[`i'_2016]*`a')
	test (_b[`i'_2016]*`a')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2016]*`a')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
}




******************
*2) PARTY INCUMBENCY ADVANTAGE: with no coalitions
******************
est clear
eststo: areg  `outcome' `j' $interacted_`j'  $saturated $allcov2  i.year if mv_incparty<${optimal_75} & mv_incparty>-${optimal_75}, a(inegi) vce(cluster inegi)
*eststo: areg  incumbent_yesterday_w_tomorrow2 `j' $interacted_`j'  $saturated $allcov2  i.year if mv_incparty<${optimal_half} & mv_incparty>-${optimal_half}, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_logdet:  di %5.4f e(r2)
		glo N_logdet: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2018]*`c')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
}

foreach i in lag_5 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2018]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2018]*`c') =0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2018]*`c') 
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
}

foreach i in lag_4 {
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') =0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2017]*`c') 
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
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
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2018]*`c') 
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
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
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')+(_b[`i'_2018]*`d') 
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
}



foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
}


foreach i in lead_2{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	di (_b[`i'_2016]*`a')
	test (_b[`i'_2016]*`a')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2016]*`a')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
}

}
}

texdoc init  "../Tables/abraham_sun_incumbency_wpolynomials.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Event-in-Discontinuity in close elections model: Effect of 2014 Term Limit Reform on Incumbency Advantage}
tex \label{effort}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex \\
tex & \multicolumn{2}{c}{incumbency advantage}   \\
tex & \multicolumn{1}{c}{(w/o cohort weights)} & \multicolumn{1}{c}{(w/ cohort weights)} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \\
tex \addlinespace

*tex Lag 7 years &     $ ${beta_lag_7_logdet}^{${est_lag_7_logdet}} $ &     $ ${beta_lag_7_ihsdet}^{${est_lag_7_ihsdet}} $ \\
*tex \\
*tex Lag 6 years &     $ ${beta_lag_6_logdet}^{${est_lag_6_logdet}} $ &     $ ${beta_lag_6_ihsdet}^{${est_lag_6_ihsdet}} $  \\
*tex \\
tex Lag 5 years &     $ ${beta_lag_5_logdet}^{${est_lag_5_logdet}} $ &     $ ${beta_lag_5_ihsdet}^{${est_lag_5_ihsdet}} $  \\
tex \\
tex Lag 4 years &     $ ${beta_lag_4_logdet}^{${est_lag_4_logdet}} $ &     $ ${beta_lag_4_ihsdet}^{${est_lag_4_ihsdet}} $ \\  
tex \\
tex Lag 3 years &     $ ${beta_lag_3_logdet}^{${est_lag_3_logdet}} $ &     $ ${beta_lag_3_ihsdet}^{${est_lag_3_ihsdet}} $  \\   
tex \\
*tex Lag 2 years &     $ ${beta_lag_2_logdet}^{${est_lag_2_logdet}} $ &     $ ${beta_lag_2_ihsdet}^{${est_lag_2_ihsdet}} $ \\  
*tex \\
tex Reform, time 0 &     $ ${beta_date_0_logdet}^{${est_date_0_logdet}} $ &     $ ${beta_date_0_ihsdet}^{${est_date_0_ihsdet}} $ \\   
tex \\
*tex Lead 1 year &     $ ${beta_lead_1_logdet}^{${est_lead_1_logdet}} $ &     $ ${beta_lead_1_ihsdet}^{${est_lead_1_ihsdet}} $  \\    
*tex \\
*tex Lead 2 years &     $ ${beta_lead_2_logdet}^{${est_lead_2_logdet}} $ &     $ ${beta_lead_2_ihsdet}^{${est_lead_2_ihsdet}} $ \\  
*tex \\
*tex Lead 3 years &     $ ${beta_lead_3_logdet}^{${est_lead_3_logdet}} $ &     $ ${beta_lead_3_ihsdet}^{${est_lead_3_ihsdet}} $  \\  

tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}  \\ 
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet} \\  

tex Mun. FEs      &     \checkmark         &  \checkmark  \\
tex Year. FEs    &     \checkmark         &  \checkmark  \\
tex State Controls$^b$  &    \checkmark     &       \checkmark \\
tex Cohort weighted  &   \checkmark      &       \checkmark  \\

tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Linear combination of the Cohort Average Treatment Effects (CATTs) for reach relative time period, weighting by each cohort's relative share of the sample following \citet{abraham_sun_2020}. Two relative time periods (lag 7 and 1) are removed in columns (2) and (4) to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-levels: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Even columns with outcomes where missing values where replaced by zeros assuming no activity was registered. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close




**********************************************
*Table: Chaisemartin and D'Haultfoeuille (forthcoming AER)
**********************************************
**ssc install twowayfeweights
**ssc install did_multipleGT
use "../Data/ConstructionDatabase/data_wleads&lags.dta", clear

*generate groups 
gen group=.
replace group=1 if adopt_year==.
replace group=2 if adopt_year==2015
replace group=3 if adopt_year==2016
replace group=4 if adopt_year==2017
replace group=5 if adopt_year==2018

*run following do file with corrected program for multiperiod did: 

do "did_multipleperiod_July27_2020.do"

*estimate multiperiod did: 
xtset inegi year
gen l_logdefunciones=logdefunciones[_n-1]
global controls  winning_margin_governor governor_alignment
*twowayfeweights logdefuncionespc group year reform, type(feTR) controls(l_logdefunciones winning_margin_governor governor_alignment) breps(1000) brepscluster(estado) path("../Data/group_time_weight.dta")
*did_multipleperiod logdefuncionespc group year reform, breps(50) placebo(5)  dynamic(3) cluster(inegi) covariances average_effect(simple)  save_results("../Data/did_chaisemartin.dta")
did_multipleperiod logdefuncionespc group year reform, breps(50) controls($controls) placebo(5)  dynamic(3) cluster(inegi) covariances average_effect(simple)  save_results("../Data/did_chaisemartin.dta")
graph export "../Figures/chaisemartin_twfe_did.png", as(png) replace
ereturn list


**the thing is that it uses block bootstrap, while I should be using wild given the differences in N by state.
**cannot drop the lag_1 to compare
**so let's change the program did_multiperiod to allow for wild bootstrap:

**********************************************
*Table: INCUMBENT QUALITY: ADVERSE SELECTION
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global controls2 governor_alignment winning_margin_governor
	global outcome incumbent_quality
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l3.incumbent_quality
	xtset inegi year 
	
************
***A: log(homicides per capita) without covariates
************
/*quietly	areg  $outcome  $saturated   i.year, a(inegi) vce(cluster estado)
keep if e(sample)==1
set matsize 11000
*/

est clear
eststo: 	areg  $outcome   $saturated  i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log_nocov:  di %5.4f e(r2)
		glo N_log_nocov: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
	lincom (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log_nocov: di %5.4f r(se)		
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"
	lincom (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log_nocov: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
	lincom (_b[`i'_2015]*`a')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


************
***B: ihs(homicides per capita) without covariates
************	
eststo:	areg  $outcome   $saturated $lagDV  i.year, a(inegi) vce(cluster inegi)

*quietly	areg  ihs_defuncionespc  $saturated $controls   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs_nocov:  di %5.4f e(r2)
		glo N_ihs_nocov: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
	lincom (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs_nocov: di %5.4f r(se)		
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"
	lincom (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
	lincom (_b[`i'_2015]*`a')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


************
***C: log(homicides per capita) with covariates
************	
eststo:	areg  $outcome  $saturated $lagDV2  i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)

***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
	lincom (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log: di %5.4f r(se)		
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
			}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_log: di %5.4f r(se)
			}


foreach i in lead_2{
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_log: di %5.4f r(se)
			}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
	lincom (_b[`i'_2015]*`a')
	glo se_`i'_log: di %5.4f r(se)
			}

************
***D: ihs(homicides per capita) with covariates
************	
quietly	areg  $outcome  $saturated $lagDV $controls2  i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
	lincom (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)		
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
			}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
			}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
			}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
	lincom (_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
			}
	
texdoc init  "../Tables/abraham_sun_quality_lagDV.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence, cohort's weighted estimates and controlling for the lag of the dependent variable}
tex \label{tab:abraham_sun_lagdv}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace
tex Lag 7 years &     $ ${beta_lag_7_log_nocov}^{${est_lag_7_log_nocov}} $ &     $ ${beta_lag_7_log}^{${est_lag_7_log}} $ & $ ${beta_lag_7_ihs_nocov}^{${est_lag_7_ihs_nocov}} $ & $ ${beta_lag_7_ihs}^{${est_lag_7_ihs}} $   \\
tex  &     ($${se_lag_7_log_nocov}$) &   ($${se_lag_7_log}$) & ($${se_lag_7_ihs_nocov}$) & ($${se_lag_7_ihs}$) \\
tex Lag 6 years &     $ ${beta_lag_6_log_nocov}^{${est_lag_6_log_nocov}} $ &      &  $ ${beta_lag_6_ihs_nocov}^{${est_lag_6_ihs_nocov}} $ &  $ ${beta_lag_6_ihs}^{${est_lag_6_ihs}} $  \\
tex  &     ($${se_lag_6_log_nocov}$) &   ($${se_lag_6_log}$) & ($${se_lag_6_ihs_nocov}$) & ($${se_lag_6_ihs}$) \\
tex Lag 5 years &     $ ${beta_lag_5_log_nocov}^{${est_lag_5_log_nocov}} $ &      &  $ ${beta_lag_5_ihs_nocov}^{${est_lag_5_ihs_nocov}} $ &  $ ${beta_lag_5_ihs}^{${est_lag_5_ihs}} $ \\
tex  &     ($${se_lag_5_log_nocov}$) &   ($${se_lag_5_log}$) & ($${se_lag_5_ihs_nocov}$) & ($${se_lag_5_ihs}$) \\
tex Lag 4 years &     $ ${beta_lag_4_log_nocov}^{${est_lag_4_log_nocov}} $ &      &   $ ${beta_lag_4_ihs_nocov}^{${est_lag_4_ihs_nocov}} $ &   $ ${beta_lag_4_ihs}^{${est_lag_4_ihs}} $  \\
tex  &     ($${se_lag_4_log_nocov}$) &   ($${se_lag_4_log}$) & ($${se_lag_4_ihs_nocov}$) & ($${se_lag_4_ihs}$) \\
tex Lag 3 years &     $ ${beta_lag_3_log_nocov}^{${est_lag_3_log_nocov}} $ &      &   $ ${beta_lag_3_ihs_nocov}^{${est_lag_3_ihs_nocov}} $ &   $ ${beta_lag_3_ihs}^{${est_lag_3_ihs}} $ \\
tex  &     ($${se_lag_3_log_nocov}$) &   ($${se_lag_3_log}$) & ($${se_lag_3_ihs_nocov}$) & ($${se_lag_3_ihs}$) \\
tex Lag 2 years &     $ ${beta_lag_2_log_nocov}^{${est_lag_2_log_nocov}} $ &     $ ${beta_lag_2_log}^{${est_lag_2_log}} $ &  $ ${beta_lag_2_ihs_nocov}^{${est_lag_2_ihs_nocov}} $  &  $ ${beta_lag_2_ihs}^{${est_lag_2_ihs}} $  \\
tex  &     ($${se_lag_2_log_nocov}$) &   ($${se_lag_2_log}$) & ($${se_lag_2_ihs_nocov}$) & ($${se_lag_2_ihs}$) \\
tex Reform, time 0 &     $ ${beta_date_0_log_nocov}^{${est_date_0_log_nocov}} $ &     $ ${beta_date_0_log}^{${est_date_0_log}} $ &   $ ${beta_date_0_ihs_nocov}^{${est_date_0_ihs_nocov}} $   &   $ ${beta_date_0_ihs}^{${est_date_0_ihs}} $ \\
tex  &     ($${se_date_0_log_nocov}$) &   ($${se_date_0_log}$) & ($${se_date_0_ihs_nocov}$) & ($${se_date_0_ihs}$) \\
tex Lead 1 year &     $ ${beta_lead_1_log_nocov}^{${est_lead_1_log_nocov}} $ &     $ ${beta_lead_1_log}^{${est_lead_1_log}} $ &    $ ${beta_lead_1_ihs_nocov}^{${est_lead_1_ihs_nocov}} $ &    $ ${beta_lead_1_ihs}^{${est_lead_1_ihs}} $ \\
tex  &     ($${se_lead_1_log_nocov}$) &   ($${se_lead_1_log}$) & ($${se_lead_1_ihs_nocov}$) & ($${se_lead_1_ihs}$) \\
tex Lead 2 years &     $ ${beta_lead_2_log_nocov}^{${est_lead_2_log_nocov}} $ &     $ ${beta_lead_2_log}^{${est_lead_2_log}} $ &   $ ${beta_lead_2_ihs_nocov}^{${est_lead_2_ihs_nocov}} $  &   $ ${beta_lead_2_ihs}^{${est_lead_2_ihs}} $  \\
tex  &     ($${se_lead_2_log_nocov}$) &   ($${se_lead_2_log}$) & ($${se_lead_2_ihs_nocov}$) & ($${se_lead_2_ihs}$) \\
tex Lead 3 years &     $ ${beta_lead_3_log_nocov}^{${est_lead_3_log_nocov}} $ &     $ ${beta_lead_3_log}^{${est_lead_3_log}} $ &    &   $ ${beta_lead_3_ihs}^{${est_lead_3_ihs}} $ \\
tex  &     ($${se_lead_3_log_nocov}$) &   ($${se_lead_3_log}$) & ($${se_lead_3_ihs_nocov}$) & ($${se_lead_3_ihs}$) \\
tex \\
tex \addlinespace
tex Observations       &        ${N_log_nocov}    &        ${N_log}    &     ${N_ihs_nocov}      &     ${N_ihs}  \\
tex R-squared        &          ${r2_log_nocov} &          ${r2_log}    &           ${r2_ihs_nocov}       &           ${r2_ihs}   \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^b$  &    \checkmark      &       \checkmark  &    \checkmark      &   \checkmark    \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\
tex Lag DV &       &       \checkmark  &         &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{5}{p{0.75\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

**********************************************
*Table: CONTROLLING FOR DTOs presence, proxied by distance from the nearest US entry point
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

**create citizen demands as controls:
global dtos distEntradasPrinc eneCarteles hayCarteles nCarteles vCarteles
foreach var in $dtos{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}


*Set globals: 
	global dtos_controls2 hayCarteles_lag_8 hayCarteles_lag_7 hayCarteles_lag_6 hayCarteles_lag_5 hayCarteles_lag_4 hayCarteles_lag_3 hayCarteles_lag_2
	global dtos_controls3 distEntradasPrinc_lag_8 distEntradasPrinc_lag_7 distEntradasPrinc_lag_6 distEntradasPrinc_lag_5 distEntradasPrinc_lag_4 distEntradasPrinc_lag_3 distEntradasPrinc_lag_2	
	global dtos_controls_all $dtos_controls2 $dtos_controls3
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc
	xtset inegi year 

	
************
***A: log(homicides per capita) without covariates
************
quietly areg  logdefuncionespc  $saturated  $controls   i.year, a(inegi) vce(cluster estado)
keep if e(sample)==1
set matsize 11000

est clear
areg  logdefuncionespc  $saturated $lagDV  $controls   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log_nocov:  di %5.4f e(r2)
		glo N_log_nocov: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'r2_log_nocov: di r(p)
	glo beta_`i'r2_log_nocov: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'r2_log_nocov= "" 
			if (${p_`i'r2_log_nocov}<=0.1) global est_`i'r2_log_nocov = "*"
			if (${p_`i'r2_log_nocov}<=0.05) global est_`i'r2_log_nocov = "**"
			if (${p_`i'r2_log_nocov}<=0.01) global est_`i'r2_log_nocov = "***"	
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log_nocov: di r(p)
	glo beta_`i'_log_nocov: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log_nocov= "" 
			if (${p_`i'_log_nocov}<=0.1) global est_`i'_log_nocov = "*"
			if (${p_`i'_log_nocov}<=0.05) global est_`i'_log_nocov = "**"
			if (${p_`i'_log_nocov}<=0.01) global est_`i'_log_nocov = "***"	
}


************
***B: ihs(homicides per capita) without covariates
************	

quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs_nocov:  di %5.4f e(r2)
		glo N_ihs_nocov: di %11.2gc e(N)
***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'r2_ihs_nocov: di r(p)
	glo beta_`i'r2_ihs_nocov: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'r2_ihs_nocov= "" 
			if (${p_`i'r2_ihs_nocov}<=0.1) global est_`i'r2_ihs_nocov = "*"
			if (${p_`i'r2_ihs_nocov}<=0.05) global est_`i'r2_ihs_nocov = "**"
			if (${p_`i'r2_ihs_nocov}<=0.01) global est_`i'r2_ihs_nocov = "***"	
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihs_nocov: di r(p)
	glo beta_`i'_ihs_nocov: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihs_nocov= "" 
			if (${p_`i'_ihs_nocov}<=0.1) global est_`i'_ihs_nocov = "*"
			if (${p_`i'_ihs_nocov}<=0.05) global est_`i'_ihs_nocov = "**"
			if (${p_`i'_ihs_nocov}<=0.01) global est_`i'_ihs_nocov = "***"	
}


************
***C: log(homicides per capita) with covariates
************	

est clear
	areg  logdefuncionespc  $saturated $lagDV  $controls $dtos_controls_all   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)

***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'r2_log: di r(p)
	glo beta_`i'r2_log: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'r2_log= "" 
			if (${p_`i'r2_log}<=0.1) global est_`i'r2_log = "*"
			if (${p_`i'r2_log}<=0.05) global est_`i'r2_log = "**"
			if (${p_`i'r2_log}<=0.01) global est_`i'r2_log = "***"	
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
}


foreach i in lead_2{
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
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
}


************
***D: ihs(homicides per capita) with covariates
************	
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $dtos_controls_all i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'r2_ihs: di r(p)
	glo beta_`i'r2_ihs: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'r2_ihs= "" 
			if (${p_`i'r2_ihs}<=0.1) global est_`i'r2_ihs = "*"
			if (${p_`i'r2_ihs}<=0.05) global est_`i'r2_ihs = "**"
			if (${p_`i'r2_ihs}<=0.01) global est_`i'r2_ihs = "***"	
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
}



* You need to install the following packages:
	*ssc install texdoc, replace
	*ssc install texify, replace
	*ssc install estout, replace 
	
texdoc init  "../Tables/abraham_sun_estimates_dtos_capture.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence, cohort's weighted estimates controlling for distance from neares US entry point}
tex \label{tab:abraham_sun_dto}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace
*tex Lag 7 years &     $ ${beta_lag_7_log_nocov}^{${est_lag_7_log_nocov}} $ &     $ ${beta_lag_7_log}^{${est_lag_7_log}} $ & $ ${beta_lag_7_ihs_nocov}^{${est_lag_7_ihs_nocov}} $ & $ ${beta_lag_7_ihs}^{${est_lag_7_ihs}} $   \\
*tex \\
tex Lag 6 years &     $ ${beta_lag_6_log_nocov}^{${est_lag_6_log_nocov}} $ &     $ ${beta_lag_6_log}^{${est_lag_6_log}} $ &  $ ${beta_lag_6_ihs_nocov}^{${est_lag_6_ihs_nocov}} $ &  $ ${beta_lag_6_ihs}^{${est_lag_6_ihs}} $  \\
tex \\
tex Lag 5 years &     $ ${beta_lag_5_log_nocov}^{${est_lag_5_log_nocov}} $ &     $ ${beta_lag_5_log}^{${est_lag_5_log}} $ &  $ ${beta_lag_5_ihs_nocov}^{${est_lag_5_ihs_nocov}} $ &  $ ${beta_lag_5_ihs}^{${est_lag_5_ihs}} $ \\
tex \\
tex Lag 4 years &     $ ${beta_lag_4_log_nocov}^{${est_lag_4_log_nocov}} $ &     $ ${beta_lag_4_log}^{${est_lag_4_log}} $ &   $ ${beta_lag_4_ihs_nocov}^{${est_lag_4_ihs_nocov}} $ &   $ ${beta_lag_4_ihs}^{${est_lag_4_ihs}} $  \\
tex \\
tex Lag 3 years &     $ ${beta_lag_3_log_nocov}^{${est_lag_3_log_nocov}} $ &     $ ${beta_lag_3_log}^{${est_lag_3_log}} $ &   $ ${beta_lag_3_ihs_nocov}^{${est_lag_3_ihs_nocov}} $ &   $ ${beta_lag_3_ihs}^{${est_lag_3_ihs}} $ \\
tex \\
tex Lag 2 years &     $ ${beta_lag_2_log_nocov}^{${est_lag_2_log_nocov}} $ &     $ ${beta_lag_2_log}^{${est_lag_2_log}} $ &  $ ${beta_lag_2_ihs_nocov}^{${est_lag_2_ihs_nocov}} $  &  $ ${beta_lag_2_ihs}^{${est_lag_2_ihs}} $  \\
tex \\
tex Reform, time 0 &     $ ${beta_date_0_log_nocov}^{${est_date_0_log_nocov}} $ &     $ ${beta_date_0_log}^{${est_date_0_log}} $ &   $ ${beta_date_0_ihs_nocov}^{${est_date_0_ihs_nocov}} $   &   $ ${beta_date_0_ihs}^{${est_date_0_ihs}} $ \\
tex \\
tex Lead 1 year &     $ ${beta_lead_1_log_nocov}^{${est_lead_1_log_nocov}} $ &     $ ${beta_lead_1_log}^{${est_lead_1_log}} $ &    $ ${beta_lead_1_ihs_nocov}^{${est_lead_1_ihs_nocov}} $ &    $ ${beta_lead_1_ihs}^{${est_lead_1_ihs}} $ \\
tex \\
tex Lead 2 years &     $ ${beta_lead_2_log_nocov}^{${est_lead_2_log_nocov}} $ &     $ ${beta_lead_2_log}^{${est_lead_2_log}} $ &   $ ${beta_lead_2_ihs_nocov}^{${est_lead_2_ihs_nocov}} $  &   $ ${beta_lead_2_ihs}^{${est_lead_2_ihs}} $  \\
tex \\
tex Lead 3 years &     $ ${beta_lead_3_log_nocov}^{${est_lead_3_log_nocov}} $ &     $ ${beta_lead_3_log}^{${est_lead_3_log}} $ &   $ ${beta_lead_3_ihs_nocov}^{${est_lead_3_ihs_nocov}} $  &   $ ${beta_lead_3_ihs}^{${est_lead_3_ihs}} $ \\

tex \addlinespace
tex Observations       &        ${N_log_nocov}    &        ${N_log}    &     ${N_ihs_nocov}      &     ${N_ihs}  \\
tex R-squared        &          ${r2_log_nocov} &          ${r2_log}    &           ${r2_ihs_nocov}       &           ${r2_ihs}   \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^b$  &    \checkmark      &       \checkmark  &    \checkmark      &   \checkmark    \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\
tex Lag DV &  \checkmark      &       \checkmark  &    \checkmark      &   \checkmark    \\
tex DTOs presence$^c$ &    &   \checkmark  &         &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{5}{p{0.9\textwidth}}{\footnotesize{Notes: Linear combination of the Cohort Average Treatment Effects (CATTs) for reach relative time period, weighting by each cohort's relative share of the sample following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed in columns (2) and (4) to avoid collinearity problems noted by \citet{abraham_sun_2020}; lag 7 is removed due to collinearity in columns (1) and (3). Standard errors in parentheses are clustered at the state level, with the following significance-levels: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. $^c$ Citizens' Security Perception are state-level covariates that include the percentage of citizens who see narcotraffick as the most worrisome issue in the country, the percentage of citizens who see a lack of punishment as the most worrisome issue in security provision, the amount of money (in thousands) spent to face insecurity threats, and citizens trust on local police forces and the army.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

**********************************************
*Table: EFFECT OF REFORM ON MANDO UNICO
**********************************************
/*
with acuerdo2 there are no pretrends and no effects
with acuerdo there are pretrends and positive effect but increasing

*/

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 logdefuncionespc_lag_8 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3	 logdefuncionespc_lag_2
	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 logdefuncionespc_lag_8 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3	
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	*global lagDV  
	*global lagDV2  l.acuerdo2
	xtset inegi year 
	

************
***A: log(homicides per capita) with covariates
************	

est clear
quietly	areg  acuerdo  $saturated  $controls i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)

***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_log: di %5.4f r(se)
}


foreach i in lead_2{
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


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log: di r(p)
	glo beta_`i'_log: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log= "" 
			if (${p_`i'_log}<=0.1) global est_`i'_log = "*"
			if (${p_`i'_log}<=0.05) global est_`i'_log = "**"
			if (${p_`i'_log}<=0.01) global est_`i'_log = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_log: di %5.4f r(se)
}


************
***B: ihs(homicides per capita) with covariates
************	
quietly	areg  acuerdo2  $saturated  $controls i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihs: di r(p)
	glo beta_`i'_ihs: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihs= "" 
			if (${p_`i'_ihs}<=0.1) global est_`i'_ihs = "*"
			if (${p_`i'_ihs}<=0.05) global est_`i'_ihs = "**"
			if (${p_`i'_ihs}<=0.01) global est_`i'_ihs = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}
*Table
	
texdoc init  "../Tables/abraham_sun_estimates_DVmandounico.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence}
tex \label{tab:abraham_sun_lagdv}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicide per capita)} & \multicolumn{1}{c}{ihs(homicide per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 7 years &      $ ${beta_lag_7_log}^{${est_lag_7_log}} $ &  $ ${beta_lag_7_ihs}^{${est_lag_7_ihs}} $   \\
tex  & ($ ${se_lag_7_log}$) & ($ ${se_lag_7_ihs} $) \\
tex Lag 6 years &          $ ${beta_lag_6_log}^{${est_lag_6_log}} $ &   $ ${beta_lag_6_ihs}^{${est_lag_6_ihs}} $  \\
tex  & ($ ${se_lag_6_log}$) & ($ ${se_lag_6_ihs} $) \\
tex Lag 5 years &        $ ${beta_lag_5_log}^{${est_lag_5_log}} $ &   $ ${beta_lag_5_ihs}^{${est_lag_5_ihs}} $ \\
tex  & ($ ${se_lag_5_log}$) & ($ ${se_lag_5_ihs} $) \\
tex Lag 4 years &         $ ${beta_lag_4_log}^{${est_lag_4_log}} $ &      $ ${beta_lag_4_ihs}^{${est_lag_4_ihs}} $  \\
tex  & ($ ${se_lag_4_log}$) & ($ ${se_lag_4_ihs} $) \\
tex Lag 3 years &        $ ${beta_lag_3_log}^{${est_lag_3_log}} $ &     $ ${beta_lag_3_ihs}^{${est_lag_3_ihs}} $ \\
tex  & ($ ${se_lag_3_log}$) & ($ ${se_lag_3_ihs} $) \\
tex Lag 2 years &        $ ${beta_lag_2_log}^{${est_lag_2_log}} $ &    $ ${beta_lag_2_ihs}^{${est_lag_2_ihs}} $  \\
tex  & ($ ${se_lag_2_log}$) & ($ ${se_lag_3_ihs} $) \\
tex Reform, time 0 &        $ ${beta_date_0_log}^{${est_date_0_log}} $ &     $ ${beta_date_0_ihs}^{${est_date_0_ihs}} $ \\
tex  & ($ ${se_date_0_log}$) & ($ ${se_date_0_ihs} $) \\
tex Lead 1 year &         $ ${beta_lead_1_log}^{${est_lead_1_log}} $ &       $ ${beta_lead_1_ihs}^{${est_lead_1_ihs}} $ \\
tex  & ($ ${se_lead_1_log}$) & ($ ${se_lead_1_ihs} $) \\
tex Lead 2 years &         $ ${beta_lead_2_log}^{${est_lead_2_log}} $ &      $ ${beta_lead_2_ihs}^{${est_lead_2_ihs}} $  \\
tex  & ($ ${se_lead_2_log}$) & ($ ${se_lead_2_ihs} $) \\
tex Lead 3 years &        $ ${beta_lead_3_log}^{${est_lead_3_log}} $ &     $ ${beta_lead_3_ihs}^{${est_lead_3_ihs}} $ \\
tex  & ($ ${se_lead_3_log}$) & ($ ${se_lead_3_ihs} $) \\

tex \addlinespace
tex Observations       &            ${N_log}        &     ${N_ihs}  \\
tex R-squared        &              ${r2_log}        &           ${r2_ihs}   \\
tex Mun. FEs       &     \checkmark         &  \checkmark    \\
tex Year. FEs       &     \checkmark         &  \checkmark   \\
tex State Controls$^b$   &    \checkmark      &   \checkmark    \\
tex Cohort weighted   &   \checkmark       &   \checkmark    \\
tex Lag DV &                  &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close



