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
*SET PANEL
xtset inegi year 

*========================================================================
*SET GLOBALS
*1) controls
*temporal globals with date_0
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

*temporal globals with lag_1
	global homicides2 logdefuncionespc_lag_1 logdefuncionespc_lag_2 logdefuncionespc_lag_3 logdefuncionespc_lag_4 logdefuncionespc_lag_5 logdefuncionespc_lag_6 logdefuncionespc_lag_7 logdefuncionespc_lag_8
	global executive2 alignment_executive_strong_lag_1 alignment_executive_strong_lag_2 alignment_executive_strong_lag_3 alignment_executive_strong_lag_4 alignment_executive_strong_lag_5 alignment_executive_strong_lag_6 alignment_executive_strong_lag_7 alignment_executive_strong_lag_8
	global governor2 alignment_governor_strong_lag_1 alignment_governor_strong_lag_2 alignment_governor_strong_lag_3 alignment_governor_strong_lag_4 alignment_governor_strong_lag_5 alignment_governor_strong_lag_6 alignment_governor_strong_lag_7 alignment_governor_strong_lag_8
	global margin2 winning_margin_lag_1 winning_margin_lag_2 winning_margin_lag_3 winning_margin_lag_4 winning_margin_lag_5 winning_margin_lag_6 winning_margin_lag_7 winning_margin_lag_8
	global margin_governor2 winning_margin_governor_lag_1 winning_margin_governor_lag_2 winning_margin_governor_lag_3 winning_margin_governor_lag_4 winning_margin_governor_lag_5 winning_margin_governor_lag_6 winning_margin_governor_lag_7 winning_margin_governor_lag_8
	global logpop2 logpop_lag_1 logpop_lag_2 logpop_lag_3 logpop_lag_4 logpop_lag_5 logpop_lag_6 logpop_lag_7 logpop_lag_8
	global carteles2 hayCarteles_lag_1 hayCarteles_lag_2 hayCarteles_lag_3 hayCarteles_lag_4 hayCarteles_lag_5 hayCarteles_lag_6 hayCarteles_lag_7 hayCarteles_lag_8

	global controls_time_acuerdo2  $homicides2 $executive2 $governor2 $margin2 $margin_governor2 $logpop2 $carteles2
	
	
*2) treatment
	global lagsleads   lag_7 lag_6 lag_5 lag_4 lag_3 lag_2  date_0 lead_1 lead_2 lead_3
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
    global saturated2 lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 lag_1_2015 lag_1_2016 lag_1_2017 lag_1_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
    global trim lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

*3) outcomes
	global effort logdetenidospc logdetenidos_2pc logheroina_kg logheroina_kg_2 logmetanfetamina_kg logmetanfetamina_kg_2 logcocaina_kg logcocaina_kg_2 logamapola_kghec logamapola_kghec_2 loglaboratorio loglaboratorio_2 logdrugs logdrugs_2
	global effort1 logdetenidospc  logheroina_kg  logmetanfetamina_kg  logcocaina_kg  logamapola_kghec  loglaboratorio  mariguana_kghec  logdrugs   
	global effort2  logdetenidos_2pc  logheroina_kg_2  logmetanfetamina_kg_2  logcocaina_kg_2 logamapola_kghec_2  loglaboratorio_2  mariguana_kghec_2 logdrugs_2 
*========================================================================
*AS all variables:
global var1 logdetenidos_2pc   
global var2 logheroina_kg_2  
global var3 logmetanfetamina_kg_2  
global var4 logcocaina_kg_2   
global var5 logamapola_kghec_2   
global var6 loglaboratorio_2   
global var7 mariguana_kghec_2 
global var8 logdrugs_2 

*========================================================================
*Run .ado 
do "wild_areg.do"
do "macros_tables.do"
*========================================================================
*Naive event study:
est clear
foreach i in $effort2 {
eststo: qui reghdfe `i' $lagsleads $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
macros_tables
*eststo:  wildcorrection_short3 `i'
*macros_tables4
}

esttab est*, keep($lagsleads) t(%9.3f)  star(* 0.1 ** 0.05 *** 0.01)


esttab using "../Tables/reform_effort.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N r2 controls munfe yearfe clustermun wildci aggregate se_aggregate p_aggregate, fmt(%11.2gc 3) ///
 label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "State Cluster S.E." "Wild corr." "Aggregate beta" "SE (aggregate)" ///
 "p-value(aggregate)")) ///
keep($lagsleads ) ///
mgroups("Sec. Coop. Agreement w/ Governor", ///
pattern(1 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(lag_7 "t-7" lag_6 "t-6" lag_5 "t-5" lag_4 "t-4" lag_3 "t-3" lag_2 "t-2"  date_0 "Reform t=0" ///
 lead_1 "t+1" lead_2 "t+2" lead_3 "t+3") ///
collabels(none) nonotes booktabs nomtitles  nolines

*========================================================================
*AS all variables:
******************
*1)Reforma Cons
******************
est clear
xi: reghdfe  $var1  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_logdet:  di %5.4f e(r2)
		glo N_logdet: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2018]*`b') 
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	 (_b[`i'_2018]*`b') 
	glo se_`i'_logdet: di %5.4f r(se)
			
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_logdet: di %5.4f r(se)
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
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_logdet: di %5.4f r(se)
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
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_logdet: di %5.4f r(se)
}






	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	lincom 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
	glo aggregate_logdet: di %5.4f r(estimate)
	estadd local aggregate_logdet $aggregate_logdet
	glo se_aggregate_logdet: di %5.4f r(se)
	estadd local se_aggregate_logdet $se_aggregate_logdet
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate_logdet: di %5.4f r(p)
	estadd local p_aggregate_logdet $p_aggregate_logdet
	glo est_aggregate_logdet= "" 
			if (${p_aggregate_logdet}<=0.1) global est_aggregate_logdet = "*"
			if (${p_aggregate_logdet}<=0.05) global est_aggregate_logdet = "**"
			if (${p_aggregate_logdet}<=0.01) global est_aggregate_logdet = "***"


******************
*2)Reforma Ley:
******************
xi: reghdfe  $var2  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihsdet:  di %5.4f e(r2)
		glo N_ihsdet: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2018]*`b') 
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	 (_b[`i'_2018]*`b') 
	glo se_`i'_ihsdet: di %5.4f r(se)
			
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihsdet: di %5.4f r(se)
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
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihsdet: di %5.4f r(se)
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
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_ihsdet: di %5.4f r(se)
}



	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	lincom 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
	glo aggregate_ihsdet: di %5.4f r(estimate)
	estadd local aggregate_ihsdet $aggregate_ihsdet
	glo se_aggregate_ihsdet: di %5.4f r(se)
	estadd local se_aggregate_ihsdet $se_aggregate_ihsdet
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate_ihsdet: di %5.4f r(p)
	estadd local p_aggregate_ihsdet $p_aggregate_ihsdet
	glo est_aggregate_ihsdet= "" 
			if (${p_aggregate_ihsdet}<=0.1) global est_aggregate_ihsdet = "*"
			if (${p_aggregate_ihsdet}<=0.05) global est_aggregate_ihsdet = "**"
			if (${p_aggregate_ihsdet}<=0.01) global est_aggregate_ihsdet = "***"

******************
*3)Falta recursos
******************
xi: reghdfe  $var3  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_her:  di %5.4f e(r2)
		glo N_her: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2018]*`b') 
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	 (_b[`i'_2018]*`b') 
	glo se_`i'_her: di %5.4f r(se)
			
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_her: di %5.4f r(se)
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
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_her: di %5.4f r(se)
}

foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_her: di %5.4f r(se)
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
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_her: di %5.4f r(se)
}



	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	lincom 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
	glo aggregate_her: di %5.4f r(estimate)
	estadd local aggregate_her $aggregate_her
	glo se_aggregate_her: di %5.4f r(se)
	estadd local se_aggregate_her $se_aggregate_her
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate_her: di %5.4f r(p)
	estadd local p_aggregate_her $p_aggregate_her
	glo est_aggregate_her= "" 
			if (${p_aggregate_her}<=0.1) global est_aggregate_her = "*"
			if (${p_aggregate_her}<=0.05) global est_aggregate_her = "**"
			if (${p_aggregate_her}<=0.01) global est_aggregate_her = "***"


******************
*4)Profesionalizacion
******************
xi: reghdfe  $var4  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_her_2:  di %5.4f e(r2)
		glo N_her_2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f (_b[`i'_2018]*`b') 
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"
	lincom 	 (_b[`i'_2018]*`b') 
	glo se_`i'_her_2: di %5.4f r(se)
			
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"	
	lincom 	(_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_her_2: di %5.4f r(se)
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
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_her_2: di %5.4f r(se)
}

foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_her_2: di %5.4f r(se)
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
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_her_2: di %5.4f r(se)
}



	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	lincom 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
	glo aggregate_her_2: di %5.4f r(estimate)
	estadd local aggregate_her_2 $aggregate_her_2
	glo se_aggregate_her_2: di %5.4f r(se)
	estadd local se_aggregate_her_2 $se_aggregate_her_2
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate_her_2: di %5.4f r(p)
	estadd local p_aggregate_her_2 $p_aggregate_her_2
	glo est_aggregate_her_2= "" 
			if (${p_aggregate_her_2}<=0.1) global est_aggregate_her_2 = "*"
			if (${p_aggregate_her_2}<=0.05) global est_aggregate_her_2 = "**"
			if (${p_aggregate_her_2}<=0.01) global est_aggregate_her_2 = "***"
******************
*5)Coordinacion
******************
xi: reghdfe  $var5  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_met:  di %5.4f e(r2)
		glo N_met: di %11.2gc e(N)

		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2018]*`b') 
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"
	lincom 	 (_b[`i'_2018]*`b') 
	glo se_`i'_met: di %5.4f r(se)
			
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_met: di %5.4f r(se)
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
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_met: di %5.4f r(se)
}
foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_met: di %5.4f r(se)
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
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_met: di %5.4f r(se)
}



	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	lincom 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
	glo aggregate_met: di %5.4f r(estimate)
	estadd local aggregate_met $aggregate_met
	glo se_aggregate_met: di %5.4f r(se)
	estadd local se_aggregate_met $se_aggregate_met
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate_met: di %5.4f r(p)
	estadd local p_aggregate_met $p_aggregate_met
	glo est_aggregate_met= "" 
			if (${p_aggregate_met}<=0.1) global est_aggregate_met = "*"
			if (${p_aggregate_met}<=0.05) global est_aggregate_met = "**"
			if (${p_aggregate_met}<=0.01) global est_aggregate_met = "***"
******************
*6)Crimen
******************
xi: reghdfe  $var6  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_met_2:  di %5.4f e(r2)
		glo N_met_2: di %11.2gc e(N)
foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f (_b[`i'_2018]*`b') 
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	 (_b[`i'_2018]*`b') 
	glo se_`i'_met_2: di %5.4f r(se)
			
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"	
	lincom 	(_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_met_2: di %5.4f r(se)
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
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_met_2: di %5.4f r(se)
}
foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_met_2: di %5.4f r(se)
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
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_met_2: di %5.4f r(se)
}



	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	lincom 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
	glo aggregate_met_2: di %5.4f r(estimate)
	estadd local aggregate_met_2 $aggregate_met_2
	glo se_aggregate_met_2: di %5.4f r(se)
	estadd local se_aggregate_met_2 $se_aggregate_met_2
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate_met_2: di %5.4f r(p)
	estadd local p_aggregate_met_2 $p_aggregate_met_2
	glo est_aggregate_met_2= "" 
			if (${p_aggregate_met_2}<=0.1) global est_aggregate_met_2 = "*"
			if (${p_aggregate_met_2}<=0.05) global est_aggregate_met_2 = "**"
			if (${p_aggregate_met_2}<=0.01) global est_aggregate_met_2 = "***"

******************
*7)Otros
******************
xi: reghdfe  $var7  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_lab:  di %5.4f e(r2)
		glo N_lab: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2018]*`b') 
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	 (_b[`i'_2018]*`b') 
	glo se_`i'_lab: di %5.4f r(se)
			
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_lab: di %5.4f r(se)
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
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_lab: di %5.4f r(se)
}

foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_lab: di %5.4f r(se)
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
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_lab: di %5.4f r(se)
}



	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	lincom 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
	glo aggregate_lab: di %5.4f r(estimate)
	estadd local aggregate_lab $aggregate_lab
	glo se_aggregate_lab: di %5.4f r(se)
	estadd local se_aggregate_lab $se_aggregate_lab
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate_lab: di %5.4f r(p)
	estadd local p_aggregate_lab $p_aggregate_lab
	glo est_aggregate_lab= "" 
			if (${p_aggregate_lab}<=0.1) global est_aggregate_lab = "*"
			if (${p_aggregate_lab}<=0.05) global est_aggregate_lab = "**"
			if (${p_aggregate_lab}<=0.01) global est_aggregate_lab = "***"
			
******************
*8)No sabe
******************
est clear
xi: reghdfe  $var8  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_lab_2:  di %5.4f e(r2)
		glo N_lab_2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2018]*`b')
	test (_b[`i'_2018]*`b')=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f (_b[`i'_2018]*`b') 
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	 (_b[`i'_2018]*`b') 
	glo se_`i'_lab_2: di %5.4f r(se)
			
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	test (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f (_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"	
	lincom 	(_b[`i'_2016]*`a') + (_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_lab_2: di %5.4f r(se)
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
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_lab_2: di %5.4f r(se)
}

foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	test  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f  (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_lab_2: di %5.4f r(se)
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
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_lab_2: di %5.4f r(se)
}



	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	lincom 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4
	glo aggregate_lab_2: di %5.4f r(estimate)
	estadd local aggregate_lab_2 $aggregate_lab_2
	glo se_aggregate_lab_2: di %5.4f r(se)
	estadd local se_aggregate_lab_2 $se_aggregate_lab_2
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate_lab_2: di %5.4f r(p)
	estadd local p_aggregate_lab_2 $p_aggregate_lab_2
	glo est_aggregate_lab_2= "" 
			if (${p_aggregate_lab_2}<=0.1) global est_aggregate_lab_2 = "*"
			if (${p_aggregate_lab_2}<=0.05) global est_aggregate_lab_2 = "**"
			if (${p_aggregate_lab_2}<=0.01) global est_aggregate_lab_2 = "***"				

* You need to install the following packages:
	*ssc install texdoc, replace
	*ssc install texify, replace
	*ssc install estout, replace 
	
texdoc init  "../Tables/abraham_sun_effort.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Effort Placed by Security Forces}
tex \label{tab:effort_final}
tex \scalebox{0.75}{    
tex \begin{tabular}{lcccccccc}  
tex \hline \hline       
tex \\ \multicolumn{9}{l}{Dependent variable: effort by security forces}\\
tex & Cons. reform  & Law reform & Lack resources & Professionalization & Coordination & Crime \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)}  & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)}  \\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \cmidrule(lrr){5-5} \cmidrule(lrr){6-6} \cmidrule(lrr){7-7} \cmidrule(lrr){8-8} \cmidrule(lrr){9-9}\\
tex \addlinespace

tex t-7 &     $ ${beta_lag_7_logdet}^{${est_lag_7_logdet}} $ &     $ ${beta_lag_7_ihsdet}^{${est_lag_7_ihsdet}} $ & $ ${beta_lag_7_her}^{${est_lag_7_her}} $ & $ ${beta_lag_7_her_2}^{${est_lag_7_her_2}} $  &     $ ${beta_lag_7_met}^{${est_lag_7_met}} $   &     $ ${beta_lag_7_met_2}^{${est_lag_7_met_2}} $  &     $ ${beta_lag_7_met}^{${est_lag_7_lab}} $   &     $ ${beta_lag_7_met_2}^{${est_lag_7_lab_2}} $ \\
tex &     ($${se_lag_7_logdet}$) &     ($${se_lag_7_ihsdet}$) & ($${se_lag_7_her}$)& ($ ${se_lag_7_her_2}$)  &    ($${se_lag_7_met}$)   &   ($${se_lag_7_met_2}$) &    ($${se_lag_7_lab}$)   &   ($${se_lag_7_lab_2}$)  \\
tex t-6 &     $ ${beta_lag_6_logdet}^{${est_lag_6_logdet}} $ &     $ ${beta_lag_6_ihsdet}^{${est_lag_6_ihsdet}} $ & $ ${beta_lag_6_her}^{${est_lag_6_her}} $ & $ ${beta_lag_6_her_2}^{${est_lag_6_her_2}} $  &     $ ${beta_lag_6_met}^{${est_lag_6_met}} $   &     $ ${beta_lag_6_met_2}^{${est_lag_6_met_2}} $  &     $ ${beta_lag_6_met}^{${est_lag_6_lab}} $   &     $ ${beta_lag_6_met_2}^{${est_lag_6_lab_2}} $ \\
tex &     ($${se_lag_6_logdet}$) &     ($${se_lag_6_ihsdet}$) & ($${se_lag_6_her}$)& ($ ${se_lag_6_her_2}$)  &    ($${se_lag_6_met}$)   &   ($${se_lag_6_met_2}$) &    ($${se_lag_6_lab}$)   &   ($${se_lag_6_lab_2}$)  \\
tex t-5 &     $ ${beta_lag_5_logdet}^{${est_lag_5_logdet}} $ &     $ ${beta_lag_5_ihsdet}^{${est_lag_5_ihsdet}} $ & $ ${beta_lag_5_her}^{${est_lag_5_her}} $ & $ ${beta_lag_5_her_2}^{${est_lag_5_her_2}} $  &     $ ${beta_lag_5_met}^{${est_lag_5_met}} $   &     $ ${beta_lag_5_met_2}^{${est_lag_5_met_2}} $  &     $ ${beta_lag_5_met}^{${est_lag_5_lab}} $   &     $ ${beta_lag_5_met_2}^{${est_lag_5_lab_2}} $ \\
tex &     ($${se_lag_5_logdet}$) &     ($${se_lag_5_ihsdet}$) & ($${se_lag_5_her}$)& ($ ${se_lag_5_her_2}$)  &    ($${se_lag_5_met}$)   &   ($${se_lag_5_met_2}$) &    ($${se_lag_5_lab}$)   &   ($${se_lag_5_lab_2}$)  \\
tex t-4 &     $ ${beta_lag_4_logdet}^{${est_lag_4_logdet}} $ &     $ ${beta_lag_4_ihsdet}^{${est_lag_4_ihsdet}} $ & $ ${beta_lag_4_her}^{${est_lag_4_her}} $ & $ ${beta_lag_4_her_2}^{${est_lag_4_her_2}} $  &     $ ${beta_lag_4_met}^{${est_lag_4_met}} $   &     $ ${beta_lag_4_met_2}^{${est_lag_4_met_2}} $  &     $ ${beta_lag_4_met}^{${est_lag_4_lab}} $   &     $ ${beta_lag_4_met_2}^{${est_lag_4_lab_2}} $ \\
tex &     ($${se_lag_4_logdet}$) &     ($${se_lag_4_ihsdet}$) & ($${se_lag_4_her}$)& ($ ${se_lag_4_her_2}$)  &    ($${se_lag_4_met}$)   &   ($${se_lag_4_met_2}$) &    ($${se_lag_4_lab}$)   &   ($${se_lag_4_lab_2}$)  \\
tex t-3 &     $ ${beta_lag_3_logdet}^{${est_lag_3_logdet}} $ &     $ ${beta_lag_3_ihsdet}^{${est_lag_3_ihsdet}} $ & $ ${beta_lag_3_her}^{${est_lag_3_her}} $ & $ ${beta_lag_3_her_2}^{${est_lag_3_her_2}} $  &     $ ${beta_lag_3_met}^{${est_lag_3_met}} $   &     $ ${beta_lag_3_met_2}^{${est_lag_3_met_2}} $  &     $ ${beta_lag_3_met}^{${est_lag_3_lab}} $   &     $ ${beta_lag_3_met_2}^{${est_lag_3_lab_2}} $ \\
tex &     ($${se_lag_3_logdet}$) &     ($${se_lag_3_ihsdet}$) & ($${se_lag_3_her}$)& ($ ${se_lag_3_her_2}$)  &    ($${se_lag_3_met}$)   &   ($${se_lag_3_met_2}$) &    ($${se_lag_3_lab}$)   &   ($${se_lag_3_lab_2}$)  \\
tex t-2 &     $ ${beta_lag_2_logdet}^{${est_lag_2_logdet}} $ &     $ ${beta_lag_2_ihsdet}^{${est_lag_2_ihsdet}} $ & $ ${beta_lag_2_her}^{${est_lag_2_her}} $ & $ ${beta_lag_2_her_2}^{${est_lag_2_her_2}} $  &     $ ${beta_lag_2_met}^{${est_lag_2_met}} $   &     $ ${beta_lag_2_met_2}^{${est_lag_2_met_2}} $  &     $ ${beta_lag_2_met}^{${est_lag_2_lab}} $   &     $ ${beta_lag_2_met_2}^{${est_lag_2_lab_2}} $ \\
tex &     ($${se_lag_2_logdet}$) &     ($${se_lag_2_ihsdet}$) & ($${se_lag_2_her}$)& ($ ${se_lag_2_her_2}$)  &    ($${se_lag_2_met}$)   &   ($${se_lag_2_met_2}$) &    ($${se_lag_2_lab}$)   &   ($${se_lag_2_lab_2}$)  \\
tex Reform, t=0 &     $ ${beta_date_0_logdet}^{${est_date_0_logdet}} $ &     $ ${beta_date_0_ihsdet}^{${est_date_0_ihsdet}} $ & $ ${beta_date_0_her}^{${est_date_0_her}} $ & $ ${beta_date_0_her_2}^{${est_date_0_her_2}} $  &     $ ${beta_date_0_met}^{${est_date_0_met}} $   &     $ ${beta_date_0_met_2}^{${est_date_0_met_2}} $  &     $ ${beta_date_0_met}^{${est_date_0_lab}} $   &     $ ${beta_date_0_met_2}^{${est_date_0_lab_2}} $ \\
tex &     ($${se_date_0_logdet}$) &     ($${se_date_0_ihsdet}$) & ($${se_date_0_her}$)& ($ ${se_date_0_her_2}$)  &    ($${se_date_0_met}$)   &   ($${se_date_0_met_2}$) &    ($${se_date_0_lab}$)   &   ($${se_date_0_lab_2}$)  \\
tex t+1 &     $ ${beta_lead_1_logdet}^{${est_lead_1_logdet}} $ &     $ ${beta_lead_1_ihsdet}^{${est_lead_1_ihsdet}} $ & $ ${beta_lead_1_her}^{${est_lead_1_her}} $ & $ ${beta_lead_1_her_2}^{${est_lead_1_her_2}} $  &     $ ${beta_lead_1_met}^{${est_lead_1_met}} $   &     $ ${beta_lead_1_met_2}^{${est_lead_1_met_2}} $  &     $ ${beta_lead_1_met}^{${est_lead_1_lab}} $   &     $ ${beta_lead_1_met_2}^{${est_lead_1_lab_2}} $ \\
tex &     ($${se_lead_1_logdet}$) &     ($${se_lead_1_ihsdet}$) & ($${se_lead_1_her}$)& ($ ${se_lead_1_her_2}$)  &    ($${se_lead_1_met}$)   &   ($${se_lead_1_met_2}$) &    ($${se_lead_1_lab}$)   &   ($${se_lead_1_lab_2}$)  \\
tex t+2 &     $ ${beta_lead_2_logdet}^{${est_lead_2_logdet}} $ &     $ ${beta_lead_2_ihsdet}^{${est_lead_2_ihsdet}} $ & $ ${beta_lead_2_her}^{${est_lead_2_her}} $ & $ ${beta_lead_2_her_2}^{${est_lead_2_her_2}} $  &     $ ${beta_lead_2_met}^{${est_lead_2_met}} $   &     $ ${beta_lead_2_met_2}^{${est_lead_2_met_2}} $  &     $ ${beta_lead_2_met}^{${est_lead_2_lab}} $   &     $ ${beta_lead_2_met_2}^{${est_lead_2_lab_2}} $ \\
tex &     ($${se_lead_2_logdet}$) &     ($${se_lead_2_ihsdet}$) & ($${se_lead_2_her}$)& ($ ${se_lead_2_her_2}$)  &    ($${se_lead_2_met}$)   &   ($${se_lead_2_met_2}$) &    ($${se_lead_2_lab}$)   &   ($${se_lead_2_lab_2}$)  \\
tex t+3 &     $ ${beta_lead_3_logdet}^{${est_lead_3_logdet}} $ &     $ ${beta_lead_3_ihsdet}^{${est_lead_3_ihsdet}} $ & $ ${beta_lead_3_her}^{${est_lead_3_her}} $ & $ ${beta_lead_3_her_2}^{${est_lead_3_her_2}} $  &     $ ${beta_lead_3_met}^{${est_lead_3_met}} $   &     $ ${beta_lead_3_met_2}^{${est_lead_3_met_2}} $  &     $ ${beta_lead_3_met}^{${est_lead_3_lab}} $   &     $ ${beta_lead_3_met_2}^{${est_lead_3_lab_2}} $ \\
tex &     ($${se_lead_3_logdet}$) &     ($${se_lead_3_ihsdet}$) & ($${se_lead_3_her}$)& ($ ${se_lead_3_her_2}$)  &    ($${se_lead_3_met}$)   &   ($${se_lead_3_met_2}$) &    ($${se_lead_3_lab}$)   &   ($${se_lead_3_lab_2}$)  \\

tex \\
tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}    &     ${N_her}      &     ${N_her_2}  &        ${N_met}    &        ${N_met_2}     &        ${N_lab}    &        ${N_lab_2}   \\
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet}    &    ${r2_her}       &           ${r2_her_2} &          ${r2_met} &          ${r2_met_2}    &          ${r2_lab} &          ${r2_lab_2}   \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark  &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    &     \checkmark         &  \checkmark  \\
tex Controls$^a$  &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark     &     \checkmark         &  \checkmark   \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark     &     \checkmark         &  \checkmark  \\
tex Reform aggregate effect         & $${aggregate_logdet}^{${est_aggregate_logdet}} $$      & $${aggregate_ihsdet}^{${est_aggregate_ihsdet}} $$    & $${aggregate_her}^{${est_aggregate_her}} $$      & $${aggregate_her_2}^{${est_aggregate_her_2}} $$     & $${aggregate_met}^{${est_aggregate_met}} $$      & $${aggregate_met_2}^{${est_aggregate_met_2}} $$     & $${aggregate_lab}^{${est_aggregate_lab}} $$      & $${aggregate_lab_2}^{${est_aggregate_lab_2}} $$     \\
tex SE       & (${se_aggregate_logdet})  & (${se_aggregate_ihsdet}) & (${se_aggregate_her})  & (${se_aggregate_her_2})  & (${se_aggregate_met})  & (${se_aggregate_met_2})   & (${se_aggregate_lab})  & (${se_aggregate_lab_2})   \\ \\
*tex p-value  & [${p_aggregate_logdet}]   & [${p_aggregate_ihsdet}]  & [${p_aggregate_her}]   & [${p_aggregate_her_2}]   & [${p_aggregate_met}]   & [${p_aggregate_met_2}]    \\

tex \hline \hline      
tex \multicolumn{9}{p{1.5\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Pretreatment controls include: governor winning margin; party alignment with the President;  party alignment with the Governor; municipal winning margin; logged population; logged organized crime related deaths; and Cartel presence.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close


*ONLY AGGREGATES:	
	
texdoc init  "../Tables/abraham_sun_effort_average.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Effort Placed by Security Forces}
tex \label{tab:effort_final_average}
tex \scalebox{0.75}{    
tex \begin{tabular}{lcccccccc}  
tex \hline \hline       
tex \\ \multicolumn{9}{l}{Dependent variable: effort by security forces}\\
tex & Cons. reform  & Law reform & Lack resources & Professionalization & Coordination & Crime \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)}  & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)}  \\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \cmidrule(lrr){5-5} \cmidrule(lrr){6-6} \cmidrule(lrr){7-7} \cmidrule(lrr){8-8} \cmidrule(lrr){9-9}\\
tex \addlinespace

tex Reform aggregate effect         & $${aggregate_logdet}^{${est_aggregate_logdet}} $$      & $${aggregate_ihsdet}^{${est_aggregate_ihsdet}} $$    & $${aggregate_her}^{${est_aggregate_her}} $$      & $${aggregate_her_2}^{${est_aggregate_her_2}} $$     & $${aggregate_met}^{${est_aggregate_met}} $$      & $${aggregate_met_2}^{${est_aggregate_met_2}} $$     & $${aggregate_lab}^{${est_aggregate_lab}} $$      & $${aggregate_lab_2}^{${est_aggregate_lab_2}} $$     \\
tex SE       & (${se_aggregate_logdet})  & (${se_aggregate_ihsdet}) & (${se_aggregate_her})  & (${se_aggregate_her_2})  & (${se_aggregate_met})  & (${se_aggregate_met_2})   & (${se_aggregate_lab})  & (${se_aggregate_lab_2})   \\ \\
*tex p-value  & [${p_aggregate_logdet}]   & [${p_aggregate_ihsdet}]  & [${p_aggregate_her}]   & [${p_aggregate_her_2}]   & [${p_aggregate_met}]   & [${p_aggregate_met_2}]    \\


tex \\
tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}    &     ${N_her}      &     ${N_her_2}  &        ${N_met}    &        ${N_met_2}     &        ${N_lab}    &        ${N_lab_2}   \\
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet}    &    ${r2_her}       &           ${r2_her_2} &          ${r2_met} &          ${r2_met_2}    &          ${r2_lab} &          ${r2_lab_2}   \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark  &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    &     \checkmark         &  \checkmark  \\
tex Controls$^a$  &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark     &     \checkmark         &  \checkmark   \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark     &     \checkmark         &  \checkmark  \\

tex \hline \hline      
tex \multicolumn{9}{p{1.5\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Pretreatment controls include: governor winning margin; party alignment with the President;  party alignment with the Governor; municipal winning margin; logged population; logged organized crime related deaths; and Cartel presence.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close


*========================================================================
*FIGURE WITH AGGREGATES
est clear
foreach i in $effort2{
qui xi: reghdfe `i'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	sum perc if date_0_2015==1, meanonly
	local a = r(mean)
	sum perc if date_0_2016==1, meanonly
	local b = r(mean)
	sum perc if date_0_2017==1, meanonly
	local c = r(mean)
	sum perc if date_0_2018==1, meanonly
	local d = r(mean)
	sum perc if lead_1_2015==1, meanonly
	local e = r(mean)
	sum perc if lead_1_2016==1, meanonly
	local f = r(mean)
	sum perc if lead_1_2017==1, meanonly
	local g = r(mean)
	sum perc if lead_2_2015==1, meanonly
	local h = r(mean)
	sum perc if lead_2_2016==1, meanonly
	local i = r(mean)
	sum perc if lead_3_2015==1, meanonly
	local j = r(mean)
	eststo: lincomest 	[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4

}

coefplot (est1, rename((1) = "log(Detentions per capita)")  mfcolor(blue) msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*0.5 *2 *4) color(black black black))) ///
 (est2, rename((1) = "log(Heroine) (kg)")  mfcolor(red) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *2 *4) color(black black black))) ///
 (est3, rename((1) = "log(Meth) (kg)")  mfcolor(red) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *2 *4) color(black black black))) ///
 (est4, rename((1) = "log(Cocaine) (kg)")  mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *2 *4) color(black black black))) ///
 (est5, rename((1) = "log(Poppy) (kg)")  mfcolor(red) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *2 *4) color(black black black))) ///
 (est6, rename((1) = "log(Labs)") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *2 *4) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle("Term Limit Reform Average Effect" "from t to t+3") ///
subtitle(" ") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI"  ///
4 "Municipal" 20 "Federal") rows(2)) 
graph export "../Figures/effort.png", as(png) replace
graph export "../Figures/effort.pdf", as(pdf) replace
graph export "../Figures/effort.tif", as(tif) replace
graph save "../Figures/effort.gph", replace

* (est7, rename((1) = "log(Mariguana) (kg/hec)")  mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
* (est8, rename((1) = "log(drugs) (kg)") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*0.5 *2 *4) color(black black black))) ///


/*========================================================================

*1) Corrected-TWFE (Chaisemartin and D'Haultfoeuille, forthcoming AER) -Effect of Reform on Homicides

/*Graph:
foreach outcome in logdefuncionespc  ihs_defuncionespc {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) seed(5675) ///
cluster(estado) robust_dynamic dynamic(3) p2lacebo(4) longdiff_placebo 
graph export "../Figures/chaisemartin_`outcome'.png", as(png) replace
}
*/

************
*A) detenidos:
************
foreach outcome in logdetenidospc {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta_t_`i': di %5.3f e(effect_`i')
	glo se_t_`i': di %5.3f e(se_effect_`i')
	glo t_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval_t_`i': di 2*ttail(${grados_t_`i'},abs(${t_t_`i'})) 
	glo est_t_`i'= "" 
			if (${pval_t_`i'}<=0.1) global est_t_`i' = "*"
			if (${pval_t_`i'}<=0.05) global est_t_`i' = "**"
			if (${pval_t_`i'}<=0.01) global est_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta_t_`i': di %5.3f e(placebo_`i')
	glo pse_t_`i': di %5.3f e(se_placebo_`i')
	glo pt_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval_t_`i': di 2*ttail(${pgrados_t_`i'},abs(${pt_t_`i'})) 
	glo pest_t_`i'= "" 
			if (${ppval_t_`i'}<=0.1) global pest_t_`i' = "*"
			if (${ppval_t_`i'}<=0.05) global pest_t_`i' = "**"
			if (${ppval_t_`i'}<=0.01) global pest_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	



************
*B) 
************
foreach outcome in logdetenidos_2pc {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados2_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta2_t_`i': di %5.3f e(effect_`i')
	glo se2_t_`i': di %5.3f e(se_effect_`i')
	glo t2_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval2_t_`i': di 2*ttail(${grados2_t_`i'},abs(${t2_t_`i'})) 
	glo est2_t_`i'= "" 
			if (${pval2_t_`i'}<=0.1) global est2_t_`i' = "*"
			if (${pval2_t_`i'}<=0.05) global est2_t_`i' = "**"
			if (${pval2_t_`i'}<=0.01) global est2_t_`i' = "***"
			*if (${pval2_t_`i'}=0) global est2_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados2_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta2_t_`i': di %5.3f e(placebo_`i')
	glo pse2_t_`i': di %5.3f e(se_placebo_`i')
	glo pt2_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval2_t_`i': di 2*ttail(${pgrados2_t_`i'},abs(${pt2_t_`i'})) 
	glo pest2_t_`i'= "" 
			if (${ppval2_t_`i'}<=0.1) global pest2_t_`i' = "*"
			if (${ppval2_t_`i'}<=0.05) global pest2_t_`i' = "**"
			if (${ppval2_t_`i'}<=0.01) global pest2_t_`i' = "***"
			*if (${ppval2_t_`i'}=0) global pest2_t_`i' = "***"
}

*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVdetenidos.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicides per capita)} & \multicolumn{1}{c}{IHS(homicides per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 4 years &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
tex Lag 3 years &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) \\
tex Lag 2 years &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) \\
tex Reform, time 0 &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) \\
tex Lead 1 year &         $ ${beta_t_1}^{${est_t_1}} $ &       $ ${beta2_t_1}^{${est2_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) \\
tex Lead 2 years &         $ ${beta_t_2}^{${est_t_2}} $ &      $ ${beta2_t_2}^{${est2_t_2}} $  \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) \\
tex Lead 3 years &        $ ${beta_t_3}^{${est_t_3}} $ &     $ ${beta2_t_3}^{${est2_t_3}} $ \\
tex  & ($ ${se_t_3}$) & ($ ${se2_t_3} $) \\

tex \addlinespace
tex Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Refers to the inverse hyperbolic sine transformation.  $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: detentions fall with the total number _2


************
*A) heroine:
************
foreach outcome in logheroina_kg {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta_t_`i': di %5.3f e(effect_`i')
	glo se_t_`i': di %5.3f e(se_effect_`i')
	glo t_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval_t_`i': di 2*ttail(${grados_t_`i'},abs(${t_t_`i'})) 
	glo est_t_`i'= "" 
			if (${pval_t_`i'}<=0.1) global est_t_`i' = "*"
			if (${pval_t_`i'}<=0.05) global est_t_`i' = "**"
			if (${pval_t_`i'}<=0.01) global est_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta_t_`i': di %5.3f e(placebo_`i')
	glo pse_t_`i': di %5.3f e(se_placebo_`i')
	glo pt_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval_t_`i': di 2*ttail(${pgrados_t_`i'},abs(${pt_t_`i'})) 
	glo pest_t_`i'= "" 
			if (${ppval_t_`i'}<=0.1) global pest_t_`i' = "*"
			if (${ppval_t_`i'}<=0.05) global pest_t_`i' = "**"
			if (${ppval_t_`i'}<=0.01) global pest_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	



************
*B) 
************
foreach outcome in logheroina_kg_2 {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados2_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta2_t_`i': di %5.3f e(effect_`i')
	glo se2_t_`i': di %5.3f e(se_effect_`i')
	glo t2_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval2_t_`i': di 2*ttail(${grados2_t_`i'},abs(${t2_t_`i'})) 
	glo est2_t_`i'= "" 
			if (${pval2_t_`i'}<=0.1) global est2_t_`i' = "*"
			if (${pval2_t_`i'}<=0.05) global est2_t_`i' = "**"
			if (${pval2_t_`i'}<=0.01) global est2_t_`i' = "***"
			*if (${pval2_t_`i'}=0) global est2_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados2_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta2_t_`i': di %5.3f e(placebo_`i')
	glo pse2_t_`i': di %5.3f e(se_placebo_`i')
	glo pt2_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval2_t_`i': di 2*ttail(${pgrados2_t_`i'},abs(${pt2_t_`i'})) 
	glo pest2_t_`i'= "" 
			if (${ppval2_t_`i'}<=0.1) global pest2_t_`i' = "*"
			if (${ppval2_t_`i'}<=0.05) global pest2_t_`i' = "**"
			if (${ppval2_t_`i'}<=0.01) global pest2_t_`i' = "***"
			*if (${ppval2_t_`i'}=0) global pest2_t_`i' = "***"
}

*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVheroine.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicides per capita)} & \multicolumn{1}{c}{IHS(homicides per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 4 years &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
tex Lag 3 years &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) \\
tex Lag 2 years &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) \\
tex Reform, time 0 &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) \\
tex Lead 1 year &         $ ${beta_t_1}^{${est_t_1}} $ &       $ ${beta2_t_1}^{${est2_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) \\
tex Lead 2 years &         $ ${beta_t_2}^{${est_t_2}} $ &      $ ${beta2_t_2}^{${est2_t_2}} $  \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) \\
tex Lead 3 years &        $ ${beta_t_3}^{${est_t_3}} $ &     $ ${beta2_t_3}^{${est2_t_3}} $ \\
tex  & ($ ${se_t_3}$) & ($ ${se2_t_3} $) \\

tex \addlinespace
tex Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Refers to the inverse hyperbolic sine transformation.  $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: 


************
*A) meta:
************
foreach outcome in logmetanfetamina_kg {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta_t_`i': di %5.3f e(effect_`i')
	glo se_t_`i': di %5.3f e(se_effect_`i')
	glo t_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval_t_`i': di 2*ttail(${grados_t_`i'},abs(${t_t_`i'})) 
	glo est_t_`i'= "" 
			if (${pval_t_`i'}<=0.1) global est_t_`i' = "*"
			if (${pval_t_`i'}<=0.05) global est_t_`i' = "**"
			if (${pval_t_`i'}<=0.01) global est_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta_t_`i': di %5.3f e(placebo_`i')
	glo pse_t_`i': di %5.3f e(se_placebo_`i')
	glo pt_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval_t_`i': di 2*ttail(${pgrados_t_`i'},abs(${pt_t_`i'})) 
	glo pest_t_`i'= "" 
			if (${ppval_t_`i'}<=0.1) global pest_t_`i' = "*"
			if (${ppval_t_`i'}<=0.05) global pest_t_`i' = "**"
			if (${ppval_t_`i'}<=0.01) global pest_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	



************
*B) 
************
foreach outcome in logmetanfetamina_kg_2 {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados2_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta2_t_`i': di %5.3f e(effect_`i')
	glo se2_t_`i': di %5.3f e(se_effect_`i')
	glo t2_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval2_t_`i': di 2*ttail(${grados2_t_`i'},abs(${t2_t_`i'})) 
	glo est2_t_`i'= "" 
			if (${pval2_t_`i'}<=0.1) global est2_t_`i' = "*"
			if (${pval2_t_`i'}<=0.05) global est2_t_`i' = "**"
			if (${pval2_t_`i'}<=0.01) global est2_t_`i' = "***"
			*if (${pval2_t_`i'}=0) global est2_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados2_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta2_t_`i': di %5.3f e(placebo_`i')
	glo pse2_t_`i': di %5.3f e(se_placebo_`i')
	glo pt2_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval2_t_`i': di 2*ttail(${pgrados2_t_`i'},abs(${pt2_t_`i'})) 
	glo pest2_t_`i'= "" 
			if (${ppval2_t_`i'}<=0.1) global pest2_t_`i' = "*"
			if (${ppval2_t_`i'}<=0.05) global pest2_t_`i' = "**"
			if (${ppval2_t_`i'}<=0.01) global pest2_t_`i' = "***"
			*if (${ppval2_t_`i'}=0) global pest2_t_`i' = "***"
}

*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVmeta.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicides per capita)} & \multicolumn{1}{c}{IHS(homicides per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 4 years &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
tex Lag 3 years &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) \\
tex Lag 2 years &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) \\
tex Reform, time 0 &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) \\
tex Lead 1 year &         $ ${beta_t_1}^{${est_t_1}} $ &       $ ${beta2_t_1}^{${est2_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) \\
tex Lead 2 years &         $ ${beta_t_2}^{${est_t_2}} $ &      $ ${beta2_t_2}^{${est2_t_2}} $  \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) \\
tex Lead 3 years &        $ ${beta_t_3}^{${est_t_3}} $ &     $ ${beta2_t_3}^{${est2_t_3}} $ \\
tex  & ($ ${se_t_3}$) & ($ ${se2_t_3} $) \\

tex \addlinespace
tex Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Refers to the inverse hyperbolic sine transformation.  $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: 



************
*A) laboratories:
************
foreach outcome in loglaboratorio {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta_t_`i': di %5.3f e(effect_`i')
	glo se_t_`i': di %5.3f e(se_effect_`i')
	glo t_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval_t_`i': di 2*ttail(${grados_t_`i'},abs(${t_t_`i'})) 
	glo est_t_`i'= "" 
			if (${pval_t_`i'}<=0.1) global est_t_`i' = "*"
			if (${pval_t_`i'}<=0.05) global est_t_`i' = "**"
			if (${pval_t_`i'}<=0.01) global est_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta_t_`i': di %5.3f e(placebo_`i')
	glo pse_t_`i': di %5.3f e(se_placebo_`i')
	glo pt_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval_t_`i': di 2*ttail(${pgrados_t_`i'},abs(${pt_t_`i'})) 
	glo pest_t_`i'= "" 
			if (${ppval_t_`i'}<=0.1) global pest_t_`i' = "*"
			if (${ppval_t_`i'}<=0.05) global pest_t_`i' = "**"
			if (${ppval_t_`i'}<=0.01) global pest_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	



************
*B) 
************
foreach outcome in loglaboratorio_2 {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados2_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta2_t_`i': di %5.3f e(effect_`i')
	glo se2_t_`i': di %5.3f e(se_effect_`i')
	glo t2_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval2_t_`i': di 2*ttail(${grados2_t_`i'},abs(${t2_t_`i'})) 
	glo est2_t_`i'= "" 
			if (${pval2_t_`i'}<=0.1) global est2_t_`i' = "*"
			if (${pval2_t_`i'}<=0.05) global est2_t_`i' = "**"
			if (${pval2_t_`i'}<=0.01) global est2_t_`i' = "***"
			*if (${pval2_t_`i'}=0) global est2_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados2_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta2_t_`i': di %5.3f e(placebo_`i')
	glo pse2_t_`i': di %5.3f e(se_placebo_`i')
	glo pt2_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval2_t_`i': di 2*ttail(${pgrados2_t_`i'},abs(${pt2_t_`i'})) 
	glo pest2_t_`i'= "" 
			if (${ppval2_t_`i'}<=0.1) global pest2_t_`i' = "*"
			if (${ppval2_t_`i'}<=0.05) global pest2_t_`i' = "**"
			if (${ppval2_t_`i'}<=0.01) global pest2_t_`i' = "***"
			*if (${ppval2_t_`i'}=0) global pest2_t_`i' = "***"
}

*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVlab.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicides per capita)} & \multicolumn{1}{c}{IHS(homicides per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 4 years &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
tex Lag 3 years &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) \\
tex Lag 2 years &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) \\
tex Reform, time 0 &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) \\
tex Lead 1 year &         $ ${beta_t_1}^{${est_t_1}} $ &       $ ${beta2_t_1}^{${est2_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) \\
tex Lead 2 years &         $ ${beta_t_2}^{${est_t_2}} $ &      $ ${beta2_t_2}^{${est2_t_2}} $  \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) \\
tex Lead 3 years &        $ ${beta_t_3}^{${est_t_3}} $ &     $ ${beta2_t_3}^{${est2_t_3}} $ \\
tex  & ($ ${se_t_3}$) & ($ ${se2_t_3} $) \\

tex \addlinespace
tex Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Refers to the inverse hyperbolic sine transformation.  $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: 



************
*A) cocaine:
************
foreach outcome in logcocaina_kg {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta_t_`i': di %5.3f e(effect_`i')
	glo se_t_`i': di %5.3f e(se_effect_`i')
	glo t_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval_t_`i': di 2*ttail(${grados_t_`i'},abs(${t_t_`i'})) 
	glo est_t_`i'= "" 
			if (${pval_t_`i'}<=0.1) global est_t_`i' = "*"
			if (${pval_t_`i'}<=0.05) global est_t_`i' = "**"
			if (${pval_t_`i'}<=0.01) global est_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta_t_`i': di %5.3f e(placebo_`i')
	glo pse_t_`i': di %5.3f e(se_placebo_`i')
	glo pt_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval_t_`i': di 2*ttail(${pgrados_t_`i'},abs(${pt_t_`i'})) 
	glo pest_t_`i'= "" 
			if (${ppval_t_`i'}<=0.1) global pest_t_`i' = "*"
			if (${ppval_t_`i'}<=0.05) global pest_t_`i' = "**"
			if (${ppval_t_`i'}<=0.01) global pest_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	



************
*B) 
************
foreach outcome in logcocaina_kg_2 {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados2_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta2_t_`i': di %5.3f e(effect_`i')
	glo se2_t_`i': di %5.3f e(se_effect_`i')
	glo t2_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval2_t_`i': di 2*ttail(${grados2_t_`i'},abs(${t2_t_`i'})) 
	glo est2_t_`i'= "" 
			if (${pval2_t_`i'}<=0.1) global est2_t_`i' = "*"
			if (${pval2_t_`i'}<=0.05) global est2_t_`i' = "**"
			if (${pval2_t_`i'}<=0.01) global est2_t_`i' = "***"
			*if (${pval2_t_`i'}=0) global est2_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados2_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta2_t_`i': di %5.3f e(placebo_`i')
	glo pse2_t_`i': di %5.3f e(se_placebo_`i')
	glo pt2_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval2_t_`i': di 2*ttail(${pgrados2_t_`i'},abs(${pt2_t_`i'})) 
	glo pest2_t_`i'= "" 
			if (${ppval2_t_`i'}<=0.1) global pest2_t_`i' = "*"
			if (${ppval2_t_`i'}<=0.05) global pest2_t_`i' = "**"
			if (${ppval2_t_`i'}<=0.01) global pest2_t_`i' = "***"
			*if (${ppval2_t_`i'}=0) global pest2_t_`i' = "***"
}

*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVcocaine.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicides per capita)} & \multicolumn{1}{c}{IHS(homicides per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 4 years &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
tex Lag 3 years &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) \\
tex Lag 2 years &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) \\
tex Reform, time 0 &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) \\
tex Lead 1 year &         $ ${beta_t_1}^{${est_t_1}} $ &       $ ${beta2_t_1}^{${est2_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) \\
tex Lead 2 years &         $ ${beta_t_2}^{${est_t_2}} $ &      $ ${beta2_t_2}^{${est2_t_2}} $  \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) \\
tex Lead 3 years &        $ ${beta_t_3}^{${est_t_3}} $ &     $ ${beta2_t_3}^{${est2_t_3}} $ \\
tex  & ($ ${se_t_3}$) & ($ ${se2_t_3} $) \\

tex \addlinespace
tex Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Refers to the inverse hyperbolic sine transformation.  $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: 




************
*A) Poppy:
************
foreach outcome in logamapola_kghec {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta_t_`i': di %5.3f e(effect_`i')
	glo se_t_`i': di %5.3f e(se_effect_`i')
	glo t_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval_t_`i': di 2*ttail(${grados_t_`i'},abs(${t_t_`i'})) 
	glo est_t_`i'= "" 
			if (${pval_t_`i'}<=0.1) global est_t_`i' = "*"
			if (${pval_t_`i'}<=0.05) global est_t_`i' = "**"
			if (${pval_t_`i'}<=0.01) global est_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta_t_`i': di %5.3f e(placebo_`i')
	glo pse_t_`i': di %5.3f e(se_placebo_`i')
	glo pt_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval_t_`i': di 2*ttail(${pgrados_t_`i'},abs(${pt_t_`i'})) 
	glo pest_t_`i'= "" 
			if (${ppval_t_`i'}<=0.1) global pest_t_`i' = "*"
			if (${ppval_t_`i'}<=0.05) global pest_t_`i' = "**"
			if (${ppval_t_`i'}<=0.01) global pest_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	



************
*B) 
************
foreach outcome in logamapola_kghec_2 {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados2_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta2_t_`i': di %5.3f e(effect_`i')
	glo se2_t_`i': di %5.3f e(se_effect_`i')
	glo t2_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval2_t_`i': di 2*ttail(${grados2_t_`i'},abs(${t2_t_`i'})) 
	glo est2_t_`i'= "" 
			if (${pval2_t_`i'}<=0.1) global est2_t_`i' = "*"
			if (${pval2_t_`i'}<=0.05) global est2_t_`i' = "**"
			if (${pval2_t_`i'}<=0.01) global est2_t_`i' = "***"
			*if (${pval2_t_`i'}=0) global est2_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados2_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta2_t_`i': di %5.3f e(placebo_`i')
	glo pse2_t_`i': di %5.3f e(se_placebo_`i')
	glo pt2_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval2_t_`i': di 2*ttail(${pgrados2_t_`i'},abs(${pt2_t_`i'})) 
	glo pest2_t_`i'= "" 
			if (${ppval2_t_`i'}<=0.1) global pest2_t_`i' = "*"
			if (${ppval2_t_`i'}<=0.05) global pest2_t_`i' = "**"
			if (${ppval2_t_`i'}<=0.01) global pest2_t_`i' = "***"
			*if (${ppval2_t_`i'}=0) global pest2_t_`i' = "***"
}

*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVpoppy.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicides per capita)} & \multicolumn{1}{c}{IHS(homicides per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 4 years &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
tex Lag 3 years &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) \\
tex Lag 2 years &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) \\
tex Reform, time 0 &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) \\
tex Lead 1 year &         $ ${beta_t_1}^{${est_t_1}} $ &       $ ${beta2_t_1}^{${est2_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) \\
tex Lead 2 years &         $ ${beta_t_2}^{${est_t_2}} $ &      $ ${beta2_t_2}^{${est2_t_2}} $  \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) \\
tex Lead 3 years &        $ ${beta_t_3}^{${est_t_3}} $ &     $ ${beta2_t_3}^{${est2_t_3}} $ \\
tex  & ($ ${se_t_3}$) & ($ ${se2_t_3} $) \\

tex \addlinespace
tex Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Refers to the inverse hyperbolic sine transformation.  $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: 




************
*A) Drugs:
************
foreach outcome in logdrugs {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta_t_`i': di %5.3f e(effect_`i')
	glo se_t_`i': di %5.3f e(se_effect_`i')
	glo t_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval_t_`i': di 2*ttail(${grados_t_`i'},abs(${t_t_`i'})) 
	glo est_t_`i'= "" 
			if (${pval_t_`i'}<=0.1) global est_t_`i' = "*"
			if (${pval_t_`i'}<=0.05) global est_t_`i' = "**"
			if (${pval_t_`i'}<=0.01) global est_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta_t_`i': di %5.3f e(placebo_`i')
	glo pse_t_`i': di %5.3f e(se_placebo_`i')
	glo pt_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval_t_`i': di 2*ttail(${pgrados_t_`i'},abs(${pt_t_`i'})) 
	glo pest_t_`i'= "" 
			if (${ppval_t_`i'}<=0.1) global pest_t_`i' = "*"
			if (${ppval_t_`i'}<=0.05) global pest_t_`i' = "**"
			if (${ppval_t_`i'}<=0.01) global pest_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	



************
*B) 
************
foreach outcome in logdrugs_2 {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados2_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta2_t_`i': di %5.3f e(effect_`i')
	glo se2_t_`i': di %5.3f e(se_effect_`i')
	glo t2_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval2_t_`i': di 2*ttail(${grados2_t_`i'},abs(${t2_t_`i'})) 
	glo est2_t_`i'= "" 
			if (${pval2_t_`i'}<=0.1) global est2_t_`i' = "*"
			if (${pval2_t_`i'}<=0.05) global est2_t_`i' = "**"
			if (${pval2_t_`i'}<=0.01) global est2_t_`i' = "***"
			*if (${pval2_t_`i'}=0) global est2_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4{
	glo pgrados2_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta2_t_`i': di %5.3f e(placebo_`i')
	glo pse2_t_`i': di %5.3f e(se_placebo_`i')
	glo pt2_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval2_t_`i': di 2*ttail(${pgrados2_t_`i'},abs(${pt2_t_`i'})) 
	glo pest2_t_`i'= "" 
			if (${ppval2_t_`i'}<=0.1) global pest2_t_`i' = "*"
			if (${ppval2_t_`i'}<=0.05) global pest2_t_`i' = "**"
			if (${ppval2_t_`i'}<=0.01) global pest2_t_`i' = "***"
			*if (${ppval2_t_`i'}=0) global pest2_t_`i' = "***"
}

*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVdrugsoverall.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicides per capita)} & \multicolumn{1}{c}{IHS(homicides per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} \\ 
tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3}\\
tex \addlinespace
tex Lag 4 years &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) \\
tex Lag 3 years &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) \\
tex Lag 2 years &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) \\
tex Reform, time 0 &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) \\
tex Lead 1 year &         $ ${beta_t_1}^{${est_t_1}} $ &       $ ${beta2_t_1}^{${est2_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) \\
tex Lead 2 years &         $ ${beta_t_2}^{${est_t_2}} $ &      $ ${beta2_t_2}^{${est2_t_2}} $  \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) \\
tex Lead 3 years &        $ ${beta_t_3}^{${est_t_3}} $ &     $ ${beta2_t_3}^{${est2_t_3}} $ \\
tex  & ($ ${se_t_3}$) & ($ ${se2_t_3} $) \\

tex \addlinespace
tex Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Refers to the inverse hyperbolic sine transformation.  $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: 




*========================================================================

**********************************************
*Table: Mechanisms: ARMY AND LOCAL POLICE EFFORT
**********************************************
use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*GLOBALS: treatment
global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

*GLOBALS: controls
foreach i in margin_ HHI_ effectiveparties_ NP_ golosov_ governor_alignment_{
global `i' `i'lag_8  `i'lag_7 `i'lag_6 `i'lag_5 `i'lag_4 `i'lag_3 `i'lag_2
}

global allcov2 $margin_  $governor_alignment_
xtset inegi year
global DV l.logdefuncionespc

*Estimates:
******************
*1)LOG DETENIDOS PER CAPITA:
******************
est clear
eststo: areg  logdetenidospc  $saturated $allcov2 $DV  i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_logdet:  di %5.4f e(r2)
		glo N_logdet: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_logdet: di %5.4f r(se)
			
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
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_logdet: di %5.4f r(se)
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
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_logdet: di %5.4f r(se)
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_logdet: di %5.4f r(se)
}

******************
*2)IHS DETENIDOS PER CAPITA:
******************
est clear
eststo: areg  logdetenidos_2pc  $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihsdet:  di %5.4f e(r2)
		glo N_ihsdet: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_ihsdet: di %5.4f r(se)
			
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
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet: di %5.4f r(se)
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
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet: di %5.4f r(se)
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
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_ihsdet: di %5.4f r(se)
}

******************
*3)LOG HEROINE KG:
******************
est clear
eststo: areg  logheroina_kg   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_her:  di %5.4f e(r2)
		glo N_her: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_her: di %5.4f r(se)
			
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
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_her: di %5.4f r(se)
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
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_her: di %5.4f r(se)
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
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_her: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_her: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_her: di %5.4f r(se)
}

******************
*4)LOG HEROINE KG_2:
******************
est clear
eststo: areg  logheroina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_her_2:  di %5.4f e(r2)
		glo N_her_2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_her2: di %5.4f r(se)
			
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
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_her2: di %5.4f r(se)
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
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_her2: di %5.4f r(se)
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
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_her2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_her2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_her2: di r(p)
	glo beta_`i'_her2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_her2= "" 
			if (${p_`i'_her2}<=0.1) global est_`i'_her2 = "*"
			if (${p_`i'_her2}<=0.05) global est_`i'_her2 = "**"
			if (${p_`i'_her2}<=0.01) global est_`i'_her2 = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_her2: di %5.4f r(se)
}
******************
*5)LOG METANPHETAMINE KG:
******************
est clear
eststo: areg  logmetanfetamina_kg   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_met:  di %5.4f e(r2)
		glo N_met: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_met: di %5.4f r(se)
			
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
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_met: di %5.4f r(se)
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
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_met: di %5.4f r(se)
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
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_met: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_met: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_met: di %5.4f r(se)
}

******************
*6)LOG METANPHETAMINE KG_2:
******************
est clear
eststo: areg  logmetanfetamina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_met_2:  di %5.4f e(r2)
		glo N_met_2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_met2: di %5.4f r(se)
			
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
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_met2: di %5.4f r(se)
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
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_met2: di %5.4f r(se)
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
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_met2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_met2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_met2: di r(p)
	glo beta_`i'_met2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_met2= "" 
			if (${p_`i'_met2}<=0.1) global est_`i'_met2 = "*"
			if (${p_`i'_met2}<=0.05) global est_`i'_met2 = "**"
			if (${p_`i'_met2}<=0.01) global est_`i'_met2 = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_met2: di %5.4f r(se)
}

******************
*7)LOG LABORATORIO:
******************
est clear
eststo: areg  loglaboratorio   $saturated $allcov2  $DV  i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_lab:  di %5.4f e(r2)
		glo N_lab: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_lab: di %5.4f r(se)
			
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
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_lab: di %5.4f r(se)
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
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_lab: di %5.4f r(se)
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
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_lab: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_lab: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_lab: di %5.4f r(se)
}

******************
*8)LOG LABORATORIO_2:
******************
est clear
eststo: areg  loglaboratorio_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_lab_2:  di %5.4f e(r2)
		glo N_lab_2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_lab2: di %5.4f r(se)
			
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
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"	
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_lab2: di %5.4f r(se)
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
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_lab2: di %5.4f r(se)
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
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_lab2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_lab2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_lab2: di r(p)
	glo beta_`i'_lab2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_lab2= "" 
			if (${p_`i'_lab2}<=0.1) global est_`i'_lab2 = "*"
			if (${p_`i'_lab2}<=0.05) global est_`i'_lab2 = "**"
			if (${p_`i'_lab2}<=0.01) global est_`i'_lab2 = "***"	
	lincom 	(_b[`i'_2015]*`a') 
	glo se_`i'_lab2: di %5.4f r(se)
}



* You need to install the following packages:
	*ssc install texdoc, replace
	*ssc install texify, replace
	*ssc install estout, replace 
	
texdoc init  "../Tables/abraham_sun_mechanisms.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform Security Forces Effort, cohort weighted estimates}
tex \label{tab:effort}
tex \scalebox{0.70}{    
tex \begin{tabular}{lcccccccc}  
tex \hline \hline       
tex \\ \multicolumn{9}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{Local Police} & \multicolumn{6}{c}{Military}  \\
tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-9} \\
tex & \multicolumn{2}{c}{log(detained per capita)}  & \multicolumn{2}{c}{heroine, eradicated kg}  & \multicolumn{2}{c}{methamphetamine, eradicated kg} & \multicolumn{2}{c}{laboratories destroyed}  \\
tex &  & \multicolumn{1}{c}{(relative to none)} &  & \multicolumn{1}{c}{(relative to none)} &  & \multicolumn{1}{c}{(relative to none)} & & \multicolumn{1}{c}{(relative to none)} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} \\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \cmidrule(lrr){5-5} \cmidrule(lrr){6-6} \cmidrule(lrr){7-7} \cmidrule(lrr){8-8} \cmidrule(lrr){9-9}\\
tex \addlinespace

tex Lag 7 years &     $ ${beta_lag_7_logdet}^{${est_lag_7_logdet}} $ &     $ ${beta_lag_7_ihsdet}^{${est_lag_7_ihsdet}} $ & $ ${beta_lag_7_her}^{${est_lag_7_her}} $ & $ ${beta_lag_7_her2}^{${est_lag_7_her2}} $  &     $ ${beta_lag_7_met}^{${est_lag_7_met}} $   &     $ ${beta_lag_7_met2}^{${est_lag_7_met2}} $ & $ ${beta_lag_7_lab}^{${est_lag_7_lab}} $ & $ ${beta_lag_7_lab2}^{${est_lag_7_lab2}} $   \\
tex &     ($${se_lag_7_logdet}$) &     ($${se_lag_7_ihsdet}$) & ($${se_lag_7_her}$)& ($ ${se_lag_7_her2}$)  &    ($${se_lag_7_met}$)   &   ($${se_lag_7_met2}$) & ($${se_lag_7_lab}$) & ($${se_lag_7_lab2}$)   \\
tex Lag 6 years &     $ ${beta_lag_6_logdet}^{${est_lag_6_logdet}} $ &     $ ${beta_lag_6_ihsdet}^{${est_lag_6_ihsdet}} $ &  $ ${beta_lag_6_her}^{${est_lag_6_her}} $ &  $ ${beta_lag_6_her2}^{${est_lag_6_her2}} $  &     $ ${beta_lag_6_met}^{${est_lag_6_met}} $ &     $ ${beta_lag_6_met2}^{${est_lag_6_met2}} $ &  $ ${beta_lag_6_lab}^{${est_lag_6_lab}} $ &  $ ${beta_lag_6_lab2}^{${est_lag_6_lab2}} $  \\
tex &     ($${se_lag_6_logdet}$) &     ($${se_lag_6_ihsdet}$) & ($${se_lag_6_her}$)& ($ ${se_lag_6_her2}$)  &    ($${se_lag_6_met}$)   &   ($${se_lag_6_met2}$) & ($${se_lag_6_lab}$) & ($${se_lag_6_lab2}$)   \\
tex Lag 5 years &     $ ${beta_lag_5_logdet}^{${est_lag_5_logdet}} $ &     $ ${beta_lag_5_ihsdet}^{${est_lag_5_ihsdet}} $ &  $ ${beta_lag_5_her}^{${est_lag_5_her}} $ &  $ ${beta_lag_5_her2}^{${est_lag_5_her2}} $ &     $ ${beta_lag_5_met}^{${est_lag_5_met}} $  &     $ ${beta_lag_5_met2}^{${est_lag_5_met2}} $ &  $ ${beta_lag_5_lab}^{${est_lag_5_lab}} $ &  $ ${beta_lag_5_lab2}^{${est_lag_5_lab2}} $ \\
tex &     ($${se_lag_5_logdet}$) &     ($${se_lag_5_ihsdet}$) & ($${se_lag_5_her}$)& ($ ${se_lag_5_her2}$)  &    ($${se_lag_5_met}$)   &   ($${se_lag_5_met2}$) & ($${se_lag_5_lab}$) & ($${se_lag_5_lab2}$)   \\
tex Lag 4 years &     $ ${beta_lag_4_logdet}^{${est_lag_4_logdet}} $ &     $ ${beta_lag_4_ihsdet}^{${est_lag_4_ihsdet}} $ &   $ ${beta_lag_4_her}^{${est_lag_4_her}} $ &   $ ${beta_lag_4_her2}^{${est_lag_4_her2}} $  &     $ ${beta_lag_4_met}^{${est_lag_4_met}} $ &     $ ${beta_lag_4_met2}^{${est_lag_4_met2}} $ &   $ ${beta_lag_4_lab}^{${est_lag_4_lab}} $ &   $ ${beta_lag_4_lab2}^{${est_lag_4_lab2}} $  \\
tex &     ($${se_lag_4_logdet}$) &     ($${se_lag_4_ihsdet}$) & ($${se_lag_4_her}$)& ($ ${se_lag_4_her2}$)  &    ($${se_lag_4_met}$)   &   ($${se_lag_4_met2}$) & ($${se_lag_4_lab}$) & ($${se_lag_4_lab2}$)   \\
tex Lag 3 years &     $ ${beta_lag_3_logdet}^{${est_lag_3_logdet}} $ &     $ ${beta_lag_3_ihsdet}^{${est_lag_3_ihsdet}} $ &   $ ${beta_lag_3_her}^{${est_lag_3_her}} $ &   $ ${beta_lag_3_her2}^{${est_lag_3_her2}} $  &     $ ${beta_lag_3_met}^{${est_lag_3_met}} $ &     $ ${beta_lag_3_met2}^{${est_lag_3_met2}} $ &   $ ${beta_lag_3_lab}^{${est_lag_3_lab}} $ &   $ ${beta_lag_3_lab2}^{${est_lag_3_lab2}} $  \\
tex &     ($${se_lag_3_logdet}$) &     ($${se_lag_3_ihsdet}$) & ($${se_lag_3_her}$)& ($ ${se_lag_3_her2}$)  &    ($${se_lag_3_met}$)   &   ($${se_lag_3_met2}$) & ($${se_lag_3_lab}$) & ($${se_lag_3_lab2}$)   \\
tex Lag 2 years &     $ ${beta_lag_2_logdet}^{${est_lag_2_logdet}} $ &     $ ${beta_lag_2_ihsdet}^{${est_lag_2_ihsdet}} $ &  $ ${beta_lag_2_her}^{${est_lag_2_her}} $  &  $ ${beta_lag_2_her2}^{${est_lag_2_her2}} $  &     $ ${beta_lag_2_met}^{${est_lag_2_met}} $ &     $ ${beta_lag_2_met2}^{${est_lag_2_met2}} $ &  $ ${beta_lag_2_lab}^{${est_lag_2_lab}} $  &  $ ${beta_lag_2_lab2}^{${est_lag_2_lab2}} $ \\
tex &     ($${se_lag_2_logdet}$) &     ($${se_lag_2_ihsdet}$) & ($${se_lag_2_her}$)& ($ ${se_lag_2_her2}$)  &    ($${se_lag_2_met}$)   &   ($${se_lag_2_met2}$) & ($${se_lag_2_lab}$) & ($${se_lag_2_lab2}$)   \\
tex Reform, time 0 &     $ ${beta_date_0_logdet}^{${est_date_0_logdet}} $ &     $ ${beta_date_0_ihsdet}^{${est_date_0_ihsdet}} $ &   $ ${beta_date_0_her}^{${est_date_0_her}} $   &   $ ${beta_date_0_her2}^{${est_date_0_her2}} $  &     $ ${beta_date_0_met}^{${est_date_0_met}} $ &     $ ${beta_date_0_met2}^{${est_date_0_met2}} $ &   $ ${beta_date_0_her}^{${est_date_0_lab}} $   &   $ ${beta_date_0_lab2}^{${est_date_0_lab2}} $ \\
tex &     ($${se_date_0_logdet}$) &     ($${se_date_0_ihsdet}$) & ($${se_date_0_her}$)& ($ ${se_date_0_her2}$)  &    ($${se_date_0_met}$)   &   ($${se_date_0_met2}$) & ($${se_date_0_lab}$) & ($${se_date_0_lab2}$)   \\
tex Lead 1 year &     $ ${beta_lead_1_logdet}^{${est_lead_1_logdet}} $ &     $ ${beta_lead_1_ihsdet}^{${est_lead_1_ihsdet}} $ &    $ ${beta_lead_1_her}^{${est_lead_1_her}} $ &    $ ${beta_lead_1_her2}^{${est_lead_1_her2}} $ &     $ ${beta_lead_1_met}^{${est_lead_1_met}} $ &     $ ${beta_lead_1_met2}^{${est_lead_1_met2}} $ &    $ ${beta_lead_1_lab}^{${est_lead_1_lab}} $ &    $ ${beta_lead_1_lab2}^{${est_lead_1_lab2}} $ \\
tex &     ($${se_lead_1_logdet}$) &     ($${se_lead_1_ihsdet}$) & ($${se_lead_1_her}$)& ($ ${se_lead_1_her2}$)  &    ($${se_lead_1_met}$)   &   ($${se_lead_1_met2}$) & ($${se_lead_1_lab}$) & ($${se_lead_1_lab2}$)   \\
tex Lead 2 years &     $ ${beta_lead_2_logdet}^{${est_lead_2_logdet}} $ &     $ ${beta_lead_2_ihsdet}^{${est_lead_2_ihsdet}} $ &   $ ${beta_lead_2_her}^{${est_lead_2_her}} $  &   $ ${beta_lead_2_her2}^{${est_lead_2_her2}} $ &     $ ${beta_lead_2_met}^{${est_lead_2_met}} $ &     $ ${beta_lead_2_met2}^{${est_lead_2_met2}} $ &   $ ${beta_lead_2_lab}^{${est_lead_2_lab}} $  &   $ ${beta_lead_2_lab2}^{${est_lead_2_lab2}} $ \\
tex &     ($${se_lead_2_logdet}$) &     ($${se_lead_2_ihsdet}$) & ($${se_lead_2_her}$)& ($ ${se_lead_2_her2}$)  &    ($${se_lead_2_met}$)   &   ($${se_lead_2_met2}$) & ($${se_lead_2_lab}$) & ($${se_lead_2_lab2}$)   \\
tex Lead 3 years &     $ ${beta_lead_3_logdet}^{${est_lead_3_logdet}} $ &     $ ${beta_lead_3_ihsdet}^{${est_lead_3_ihsdet}} $ &   $ ${beta_lead_3_her}^{${est_lead_3_her}} $  &   $ ${beta_lead_3_her2}^{${est_lead_3_her2}} $ &     $ ${beta_lead_3_met}^{${est_lead_3_met}} $ &     $ ${beta_lead_3_met2}^{${est_lead_3_met2}} $ &   $ ${beta_lead_3_lab}^{${est_lead_3_lab}} $  &   $ ${beta_lead_3_lab2}^{${est_lead_3_lab2}} $ \\
tex &     ($${se_lead_3_logdet}$) &     ($${se_lead_3_ihsdet}$) & ($${se_lead_3_her}$)& ($ ${se_lead_3_her2}$)  &    ($${se_lead_3_met}$)   &   ($${se_lead_3_met2}$) & ($${se_lead_3_lab}$) & ($${se_lead_3_lab2}$)   \\
tex \\
tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}    &     ${N_her}      &     ${N_her_2}  &        ${N_met}    &        ${N_met_2}    &     ${N_lab}      &     ${N_lab_2} \\
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet}    &    ${r2_her}       &           ${r2_her_2} &          ${r2_met} &          ${r2_met_2}    &           ${r2_lab}       &           ${r2_lab_2}    \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark  &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^b$  &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark     \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\
tex Lag log(homicides per capita)  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{9}{p{1.5\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Even columns with outcomes with missing values where replaced by zeros assuming no activity was registered. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close

