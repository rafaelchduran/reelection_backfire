*Regressions: Effect on Sec. Preferences
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
	global narco ap4_2_3_lag_6 ap4_2_3_lag_5 ap4_2_3_lag_4 ap4_2_3_lag_3 ap4_2_3_lag_2
	global ins ap4_2_5_lag_6 ap4_2_5_lag_5 ap4_2_5_lag_4 ap4_2_5_lag_3 ap4_2_5_lag_2
	global punishment ap4_2_11_lag_6 ap4_2_11_lag_5 ap4_2_11_lag_4 ap4_2_11_lag_3 ap4_2_11_lag_2
	global corruption ap4_2_8_lag_6 ap4_2_8_lag_5 ap4_2_8_lag_4 ap4_2_8_lag_3 ap4_2_8_lag_2
	global poverty ap4_2_1_lag_6 ap4_2_1_lag_5 ap4_2_1_lag_4 ap4_2_1_lag_3 ap4_2_1_lag_2
	global unemployment ap4_2_2_lag_6 ap4_2_2_lag_5 ap4_2_2_lag_4 ap4_2_2_lag_3 ap4_2_2_lag_2
	global inflation ap4_2_4_lag_6 ap4_2_4_lag_5 ap4_2_4_lag_4 ap4_2_4_lag_3 ap4_2_4_lag_2
	global disasters ap4_2_6_lag_6 ap4_2_6_lag_5 ap4_2_6_lag_4 ap4_2_6_lag_3 ap4_2_6_lag_2
	global water ap4_2_7_lag_6 ap4_2_7_lag_5 ap4_2_7_lag_4 ap4_2_7_lag_3 ap4_2_7_lag_2
	global educ ap4_2_9_lag_6 ap4_2_9_lag_5 ap4_2_9_lag_4 ap4_2_9_lag_3 ap4_2_9_lag_2
	global health ap4_2_10_lag_6 ap4_2_10_lag_5 ap4_2_10_lag_4 ap4_2_10_lag_3 ap4_2_10_lag_2

	global citizens 

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
	global lagsleads    lag_6 lag_5 lag_4 lag_3 lag_2  date_0 lead_1 lead_2 lead_3
	global lagsleads_services   lag_4 lag_3 lag_2  date_0 lead_1  lead_3
    global saturated lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

*3) outcomes
	global preferences ap4_2_3 ap4_2_5  ap4_2_11 ap4_2_8 ap4_2_1  ap4_2_2  ap4_2_4 ap4_2_6  ap4_2_7  ap4_2_9  ap4_2_10   
	global preferences2 ap4_2_3 ap4_2_5 ap4_2_1

	
*========================================================================
*Run .ado 
do "wild_areg.do"
do "macros_tables.do"
*========================================================================
*Final:
est clear
foreach i in $preferences {
eststo: qui reghdfe `i' $lagsleads $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
macros_tables
*eststo:  wildcorrection_short3 `i'
*macros_tables4
}

esttab est*, keep($lagsleads) t(%9.3f)  star(* 0.1 ** 0.05 *** 0.01)


esttab using "../Tables/reform_preferences.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N r2 controls munfe yearfe clustermun wildci aggregate se_aggregate, fmt(%11.2gc 3) ///
 label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "State Cluster S.E." "Wild corr." "Aggregate beta" "SE (aggregate)" ///
 "p-value(aggregate)")) ///
keep($lagsleads) ///
mgroups("Narcotraffic" "Insecurity" "Punishment" "Corruption" "Poverty" "Unemployment" "Inflation" "Disasters" ///
 "Water Scarcity" "Education" "Health", ///
pattern(1 1 1 1 1 1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel( lag_6 "t-6" lag_5 "t-5" lag_4 "t-4" lag_3 "t-3" lag_2 "t-2"  date_0 "Reform t=0" ///
 lead_1 "t+1" lead_2 "t+2" lead_3 "t+3") ///
collabels(none) nonotes booktabs nomtitles  nolines


*Figure
est clear
foreach i in $preferences {
qui reghdfe `i' $lagsleads $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
**estimate aggregate effect:
	sum perc if date_0==1, meanonly
	local a = r(mean)
	sum perc if lead_1==1, meanonly
	local b = r(mean)
	sum perc if lead_2==1, meanonly
	local c = r(mean)
	sum perc if lead_3==1, meanonly
	local d = r(mean)

eststo: lincomest [(_b[date_0]*`a')+(_b[lead_1]*`b')+(_b[lead_2]*`c') + (_b[lead_3]*`d')]/4 

}

coefplot (est1, rename((1) = "Narcotraffic")  mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Insecurity")  mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Punishment")  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est4, rename((1) = "Corruption")   msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "Poverty")  mfcolor(white)  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Unemployment")  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est7, rename((1) = "Inflation")  mfcolor(white)  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est8, rename((1) = "Disaster")  mfcolor(white)  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
  (est9, rename((1) = "Water")   mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est10, rename((1) = "Education")   mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est11, rename((1) = "Health")  mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle("Term Limit Reform Average Effect" "from t to t+3") ///
subtitle("Topic that worries the most") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures/change_preferences_twfe.png", as(png) replace
graph export "../Figures/change_preferences_twfe.pdf", as(pdf) replace
graph export "../Figures/change_preferences_twfe.tif", as(tif) replace
graph save "../Figures/change_preferences_twfe.gph", replace



*========================================================================
*Abraham and Sun (2021):
******************
*1)ap4_2_3 
******************
*est clear
xi: reghdfe  ap4_2_3  $saturated $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_logdet:  di %5.4f e(r2)
		glo N_logdet: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_6 lag_4{
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_logdet: di %5.4f r(se)
}

foreach i in lag_5{
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lag_3 lag_2 date_0{
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



**estimate aggregate effect:
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
			if (${p_aggregate_logdet}<=0.11) global est_aggregate_logdet = "*"
			if (${p_aggregate_logdet}<=0.05) global est_aggregate_logdet = "**"
			if (${p_aggregate_logdet}<=0.01) global est_aggregate_logdet = "***"	

	
******************
*2)ap4_2_5 
******************
est clear
xi: reghdfe  ap4_2_5  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihsdet:  di %5.4f e(r2)
		glo N_ihsdet: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_6 lag_4{
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihsdet: di %5.4f r(se)
}

foreach i in lag_5{
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lag_3 lag_2 date_0{
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



**estimate aggregate effect:
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
			if (${p_aggregate_ihsdet}<=0.11) global est_aggregate_ihsdet = "*"
			if (${p_aggregate_ihsdet}<=0.05) global est_aggregate_ihsdet = "**"
			if (${p_aggregate_ihsdet}<=0.01) global est_aggregate_ihsdet = "***"	

*******
*ap4_2_11
*******
est clear
xi: reghdfe  ap4_2_11  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_her:  di %5.4f e(r2)
		glo N_her: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_6 lag_4{
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_her: di %5.4f r(se)
}

foreach i in lag_5{
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_her: di %5.4f r(se)
}


foreach i in lag_3 lag_2 date_0{
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



**estimate aggregate effect:
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
			if (${p_aggregate_her}<=0.11) global est_aggregate_her = "*"
			if (${p_aggregate_her}<=0.05) global est_aggregate_her = "**"
			if (${p_aggregate_her}<=0.01) global est_aggregate_her = "***"	

*******
*ap4_2_8
*******
est clear
xi: reghdfe  ap4_2_8  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_her_2:  di %5.4f e(r2)
		glo N_her_2: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_6 lag_4{
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_her_2: di %5.4f r(se)
}

foreach i in lag_5{
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"
	lincom 	(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_her_2: di %5.4f r(se)
}


foreach i in lag_3 lag_2 date_0{
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
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
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
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_her_2: di %5.4f r(se)
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



**estimate aggregate effect:
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
			if (${p_aggregate_her_2}<=0.11) global est_aggregate_her_2 = "*"
			if (${p_aggregate_her_2}<=0.05) global est_aggregate_her_2 = "**"
			if (${p_aggregate_her_2}<=0.01) global est_aggregate_her_2 = "***"	
			
*******
*ap4_2_1
*******
est clear
xi: reghdfe  ap4_2_1  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_met:  di %5.4f e(r2)
		glo N_met: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_6 lag_4{
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_met: di %5.4f r(se)
}

foreach i in lag_5{
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"
	lincom 	(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_met: di %5.4f r(se)
}


foreach i in lag_3 lag_2 date_0{
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



**estimate aggregate effect:
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
			if (${p_aggregate_met}<=0.11) global est_aggregate_met = "*"
			if (${p_aggregate_met}<=0.05) global est_aggregate_met = "**"
			if (${p_aggregate_met}<=0.01) global est_aggregate_met = "***"	
			
*******
*ap4_2_2
*******
est clear
xi: reghdfe  ap4_2_2  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_met_2:  di %5.4f e(r2)
		glo N_met_2: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_6 lag_4{
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_met_2: di %5.4f r(se)
}

foreach i in lag_5{
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_met_2: di %5.4f r(se)
}


foreach i in lag_3 lag_2 date_0{
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
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
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
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_met_2: di %5.4f r(se)
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



**estimate aggregate effect:
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
			if (${p_aggregate_met_2}<=0.11) global est_aggregate_met_2 = "*"
			if (${p_aggregate_met_2}<=0.05) global est_aggregate_met_2 = "**"
			if (${p_aggregate_met_2}<=0.01) global est_aggregate_met_2 = "***"	
			
*******
*ap4_2_4
*******
est clear
xi: reghdfe  ap4_2_4  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_lab:  di %5.4f e(r2)
		glo N_lab: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_6 lag_4{
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_lab: di %5.4f r(se)
}

foreach i in lag_5{
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_lab: di %5.4f r(se)
}


foreach i in lag_3 lag_2 date_0{
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



**estimate aggregate effect:
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
			if (${p_aggregate_lab}<=0.11) global est_aggregate_lab = "*"
			if (${p_aggregate_lab}<=0.05) global est_aggregate_lab = "**"
			if (${p_aggregate_lab}<=0.01) global est_aggregate_lab = "***"	

*******
*ap4_2_6
*******
est clear
xi: reghdfe  ap4_2_6  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_lab_2:  di %5.4f e(r2)
		glo N_lab_2: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_6 lag_4{
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_lab_2: di %5.4f r(se)
}

foreach i in lag_5{
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_lab_2: di %5.4f r(se)
}


foreach i in lag_3 lag_2 date_0{
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
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
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
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_lab_2: di %5.4f r(se)
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



**estimate aggregate effect:
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
			if (${p_aggregate_lab_2}<=0.11) global est_aggregate_lab_2 = "*"
			if (${p_aggregate_lab_2}<=0.05) global est_aggregate_lab_2 = "**"
			if (${p_aggregate_lab_2}<=0.01) global est_aggregate_lab_2 = "***"	

*******
*ap4_2_7
*******
est clear
xi: reghdfe  ap4_2_7  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_new:  di %5.4f e(r2)
		glo N_new: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_6 lag_4{
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_new: di r(p)
	glo beta_`i'_new: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_new= "" 
			if (${p_`i'_new}<=0.1) global est_`i'_new = "*"
			if (${p_`i'_new}<=0.05) global est_`i'_new = "**"
			if (${p_`i'_new}<=0.01) global est_`i'_new = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_new: di %5.4f r(se)
}

foreach i in lag_5{
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_new: di r(p)
	glo beta_`i'_new: di %5.4f (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_new= "" 
			if (${p_`i'_new}<=0.1) global est_`i'_new = "*"
			if (${p_`i'_new}<=0.05) global est_`i'_new = "**"
			if (${p_`i'_new}<=0.01) global est_`i'_new = "***"
	lincom 	(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_new: di %5.4f r(se)
}


foreach i in lag_3 lag_2 date_0{
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
	glo p_`i'_new: di r(p)
	glo beta_`i'_new: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_new= "" 
			if (${p_`i'_new}<=0.1) global est_`i'_new = "*"
			if (${p_`i'_new}<=0.05) global est_`i'_new = "**"
			if (${p_`i'_new}<=0.01) global est_`i'_new = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_new: di %5.4f r(se)
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
	glo p_`i'_new: di r(p)
	glo beta_`i'_new: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_new= "" 
			if (${p_`i'_new}<=0.1) global est_`i'_new = "*"
			if (${p_`i'_new}<=0.05) global est_`i'_new = "**"
			if (${p_`i'_new}<=0.01) global est_`i'_new = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_new: di %5.4f r(se)
}
foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_new: di r(p)
	glo beta_`i'_new: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_new= "" 
			if (${p_`i'_new}<=0.1) global est_`i'_new = "*"
			if (${p_`i'_new}<=0.05) global est_`i'_new = "**"
			if (${p_`i'_new}<=0.01) global est_`i'_new = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_new: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_new: di r(p)
	glo beta_`i'_new: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_new= "" 
			if (${p_`i'_new}<=0.1) global est_`i'_new = "*"
			if (${p_`i'_new}<=0.05) global est_`i'_new = "**"
			if (${p_`i'_new}<=0.01) global est_`i'_new = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_new: di %5.4f r(se)
}



**estimate aggregate effect:
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
	glo aggregate_new: di %5.4f r(estimate)
	estadd local aggregate_new $aggregate_new
	glo se_aggregate_new: di %5.4f r(se)
	estadd local se_aggregate_new $se_aggregate_new
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate_new: di %5.4f r(p)
	estadd local p_aggregate_new $p_aggregate_new
	glo est_aggregate_new= "" 
			if (${p_aggregate_new}<=0.11) global est_aggregate_new = "*"
			if (${p_aggregate_new}<=0.05) global est_aggregate_new = "**"
			if (${p_aggregate_new}<=0.01) global est_aggregate_new = "***"	

*******
*ap4_2_9
*******
est clear
xi: reghdfe  ap4_2_9  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_new_2:  di %5.4f e(r2)
		glo N_new_2: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_6 lag_4{
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_new_2: di r(p)
	glo beta_`i'_new_2: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_new_2= "" 
			if (${p_`i'_new_2}<=0.1) global est_`i'_new_2 = "*"
			if (${p_`i'_new_2}<=0.05) global est_`i'_new_2 = "**"
			if (${p_`i'_new_2}<=0.01) global est_`i'_new_2 = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_new_2: di %5.4f r(se)
}

foreach i in lag_5{
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_new_2: di r(p)
	glo beta_`i'_new_2: di %5.4f (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_new_2= "" 
			if (${p_`i'_new_2}<=0.1) global est_`i'_new_2 = "*"
			if (${p_`i'_new_2}<=0.05) global est_`i'_new_2 = "**"
			if (${p_`i'_new_2}<=0.01) global est_`i'_new_2 = "***"
	lincom 	(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_new_2: di %5.4f r(se)
}


foreach i in lag_3 lag_2 date_0{
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
	glo p_`i'_new_2: di r(p)
	glo beta_`i'_new_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_new_2= "" 
			if (${p_`i'_new_2}<=0.1) global est_`i'_new_2 = "*"
			if (${p_`i'_new_2}<=0.05) global est_`i'_new_2 = "**"
			if (${p_`i'_new_2}<=0.01) global est_`i'_new_2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_new_2: di %5.4f r(se)
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
	glo p_`i'_new_2: di r(p)
	glo beta_`i'_new_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_new_2= "" 
			if (${p_`i'_new_2}<=0.1) global est_`i'_new_2 = "*"
			if (${p_`i'_new_2}<=0.05) global est_`i'_new_2 = "**"
			if (${p_`i'_new_2}<=0.01) global est_`i'_new_2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_new_2: di %5.4f r(se)
}
foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_new_2: di r(p)
	glo beta_`i'_new_2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_new_2= "" 
			if (${p_`i'_new_2}<=0.1) global est_`i'_new_2 = "*"
			if (${p_`i'_new_2}<=0.05) global est_`i'_new_2 = "**"
			if (${p_`i'_new_2}<=0.01) global est_`i'_new_2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_new_2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_new_2: di r(p)
	glo beta_`i'_new_2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_new_2= "" 
			if (${p_`i'_new_2}<=0.1) global est_`i'_new_2 = "*"
			if (${p_`i'_new_2}<=0.05) global est_`i'_new_2 = "**"
			if (${p_`i'_new_2}<=0.01) global est_`i'_new_2 = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_new_2: di %5.4f r(se)
}



**estimate aggregate effect:
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
	glo aggregate_new_2: di %5.4f r(estimate)
	estadd local aggregate_new_2 $aggregate_new_2
	glo se_aggregate_new_2: di %5.4f r(se)
	estadd local se_aggregate_new_2 $se_aggregate_new_2
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate_new_2: di %5.4f r(p)
	estadd local p_aggregate_new_2 $p_aggregate_new_2
	glo est_aggregate_new_2= "" 
			if (${p_aggregate_new_2}<=0.11) global est_aggregate_new_2 = "*"
			if (${p_aggregate_new_2}<=0.05) global est_aggregate_new_2 = "**"
			if (${p_aggregate_new_2}<=0.01) global est_aggregate_new_2 = "***"	

			
********
*ap4_2_10
********			
est clear
xi: reghdfe  ap4_2_10  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_new_3:  di %5.4f e(r2)
		glo N_new_3: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_6 lag_4{
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_new_3: di r(p)
	glo beta_`i'_new_3: di %5.4f (_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_new_3= "" 
			if (${p_`i'_new_3}<=0.1) global est_`i'_new_3 = "*"
			if (${p_`i'_new_3}<=0.05) global est_`i'_new_3 = "**"
			if (${p_`i'_new_3}<=0.01) global est_`i'_new_3 = "***"
	lincom 	(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_new_3: di %5.4f r(se)
}

foreach i in lag_5{
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	sum perc if `i'_2018==1, meanonly
	local d = r(mean)
	di (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	test (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')=0
	glo p_`i'_new_3: di r(p)
	glo beta_`i'_new_3: di %5.4f (_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_new_3= "" 
			if (${p_`i'_new_3}<=0.1) global est_`i'_new_3 = "*"
			if (${p_`i'_new_3}<=0.05) global est_`i'_new_3 = "**"
			if (${p_`i'_new_3}<=0.01) global est_`i'_new_3 = "***"
	lincom 	(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_new_3: di %5.4f r(se)
}


foreach i in lag_3 lag_2 date_0{
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
	glo p_`i'_new_3: di r(p)
	glo beta_`i'_new_3: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_new_3= "" 
			if (${p_`i'_new_3}<=0.1) global est_`i'_new_3 = "*"
			if (${p_`i'_new_3}<=0.05) global est_`i'_new_3 = "**"
			if (${p_`i'_new_3}<=0.01) global est_`i'_new_3 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_new_3: di %5.4f r(se)
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
	glo p_`i'_new_3: di r(p)
	glo beta_`i'_new_3: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_new_3= "" 
			if (${p_`i'_new_3}<=0.1) global est_`i'_new_3 = "*"
			if (${p_`i'_new_3}<=0.05) global est_`i'_new_3 = "**"
			if (${p_`i'_new_3}<=0.01) global est_`i'_new_3 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_new_3: di %5.4f r(se)
}
foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_new_3: di r(p)
	glo beta_`i'_new_3: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_new_3= "" 
			if (${p_`i'_new_3}<=0.1) global est_`i'_new_3 = "*"
			if (${p_`i'_new_3}<=0.05) global est_`i'_new_3 = "**"
			if (${p_`i'_new_3}<=0.01) global est_`i'_new_3 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_new_3: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_new_3: di r(p)
	glo beta_`i'_new_3: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_new_3= "" 
			if (${p_`i'_new_3}<=0.1) global est_`i'_new_3 = "*"
			if (${p_`i'_new_3}<=0.05) global est_`i'_new_3 = "**"
			if (${p_`i'_new_3}<=0.01) global est_`i'_new_3 = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_new_3: di %5.4f r(se)
}



**estimate aggregate effect:
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
	glo aggregate_new_3: di %5.4f r(estimate)
	estadd local aggregate_new_3 $aggregate_new_3
	glo se_aggregate_new_3: di %5.4f r(se)
	estadd local se_aggregate_new_3 $se_aggregate_new_3
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate_new_3: di %5.4f r(p)
	estadd local p_aggregate_new_3 $p_aggregate_new_3
	glo est_aggregate_new_3= "" 
			if (${p_aggregate_new_3}<=0.11) global est_aggregate_new_3 = "*"
			if (${p_aggregate_new_3}<=0.05) global est_aggregate_new_3 = "**"
			if (${p_aggregate_new_3}<=0.01) global est_aggregate_new_3 = "***"	


* You need to install the following packages:
	*ssc install texdoc, replace
	*ssc install texify, replace
	*ssc install estout, replace 
	
texdoc init  "../Tables/abraham_sun_preferences.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Citizens Preferences}
tex \label{tab:preferences}
tex \scalebox{0.60}{    
tex \begin{tabular}{lccccccccccc}  
tex \hline \hline       
tex \\ \multicolumn{12}{l}{Dependent variable: topic that worries the most}\\
tex & Narcotraffic & Insecurity & Punishment to criminals & Corruption  & Poverty & Unemployment & Inflation & Natural Disasters & Water Scarcity & Education & Health \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} & \multicolumn{1}{c}{(9)} & \multicolumn{1}{c}{(10)} & \multicolumn{1}{c}{(11)}\\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \cmidrule(lrr){5-5} \cmidrule(lrr){6-6} \cmidrule(lrr){7-7} \cmidrule(lrr){8-8} \cmidrule(lrr){9-9} \cmidrule(lrr){10-10} \cmidrule(lrr){11-11} \cmidrule(lrr){12-12}\\
tex \addlinespace

tex t-6 &     $ ${beta_lag_6_logdet}^{${est_lag_6_logdet}} $ &     $ ${beta_lag_6_ihsdet}^{${est_lag_6_ihsdet}} $ &  $ ${beta_lag_6_her}^{${est_lag_6_her}} $  &  $ ${beta_lag_6_her_2}^{${est_lag_6_her_2}} $  &     $ ${beta_lag_6_met}^{${est_lag_6_met}} $ &     $ ${beta_lag_6_met_2}^{${est_lag_6_met_2}} $ & $ ${beta_lag_6_lab}^{${est_lag_6_lab}} $ & $ ${beta_lag_6_lab_2}^{${est_lag_6_lab_2}} $ & $ ${beta_lag_6_new}^{${est_lag_6_new}} $ & $ ${beta_lag_6_new_2}^{${est_lag_6_new_2}} $ & $ ${beta_lag_6_new_3}^{${est_lag_6_new_3}} $  \\
tex &     ($${se_lag_6_logdet}$) &     ($${se_lag_6_ihsdet}$) & ($${se_lag_6_her}$)& ($ ${se_lag_6_her_2}$)  &    ($${se_lag_6_met}$)   &   ($${se_lag_6_met_2}$) &   ($${se_lag_6_lab}$) &   ($${se_lag_6_lab_2}$) &   ($${se_lag_6_new}$) &   ($${se_lag_6_new_2}$) &   ($${se_lag_6_new_3}$) \\
tex t-5 &     $ ${beta_lag_5_logdet}^{${est_lag_5_logdet}} $ &     $ ${beta_lag_5_ihsdet}^{${est_lag_5_ihsdet}} $ &  $ ${beta_lag_5_her}^{${est_lag_5_her}} $  &  $ ${beta_lag_5_her_2}^{${est_lag_5_her_2}} $  &     $ ${beta_lag_5_met}^{${est_lag_5_met}} $ &     $ ${beta_lag_5_met_2}^{${est_lag_5_met_2}} $ & $ ${beta_lag_5_lab}^{${est_lag_5_lab}} $ & $ ${beta_lag_5_lab_2}^{${est_lag_5_lab_2}} $ & $ ${beta_lag_5_new}^{${est_lag_5_new}} $ & $ ${beta_lag_5_new_2}^{${est_lag_5_new_2}} $ & $ ${beta_lag_5_new_3}^{${est_lag_5_new_3}} $  \\
tex &     ($${se_lag_5_logdet}$) &     ($${se_lag_5_ihsdet}$) & ($${se_lag_5_her}$)& ($ ${se_lag_5_her_2}$)  &    ($${se_lag_5_met}$)   &   ($${se_lag_5_met_2}$) &   ($${se_lag_5_lab}$) &   ($${se_lag_5_lab_2}$) &   ($${se_lag_5_new}$) &   ($${se_lag_5_new_2}$) &   ($${se_lag_5_new_3}$) \\
tex t-4 &     $ ${beta_lag_4_logdet}^{${est_lag_4_logdet}} $ &     $ ${beta_lag_4_ihsdet}^{${est_lag_4_ihsdet}} $ &  $ ${beta_lag_4_her}^{${est_lag_4_her}} $  &  $ ${beta_lag_4_her_2}^{${est_lag_4_her_2}} $  &     $ ${beta_lag_4_met}^{${est_lag_4_met}} $ &     $ ${beta_lag_4_met_2}^{${est_lag_4_met_2}} $ & $ ${beta_lag_4_lab}^{${est_lag_4_lab}} $ & $ ${beta_lag_4_lab_2}^{${est_lag_4_lab_2}} $ & $ ${beta_lag_4_new}^{${est_lag_4_new}} $ & $ ${beta_lag_4_new_2}^{${est_lag_4_new_2}} $ & $ ${beta_lag_4_new_3}^{${est_lag_4_new_3}} $  \\
tex &     ($${se_lag_4_logdet}$) &     ($${se_lag_4_ihsdet}$) & ($${se_lag_4_her}$)& ($ ${se_lag_4_her_2}$)  &    ($${se_lag_4_met}$)   &   ($${se_lag_4_met_2}$) &   ($${se_lag_4_lab}$) &   ($${se_lag_4_lab_2}$) &   ($${se_lag_4_new}$) &   ($${se_lag_4_new_2}$) &   ($${se_lag_4_new_3}$) \\
tex t-3 &     $ ${beta_lag_3_logdet}^{${est_lag_3_logdet}} $ &     $ ${beta_lag_3_ihsdet}^{${est_lag_3_ihsdet}} $ &  $ ${beta_lag_3_her}^{${est_lag_3_her}} $  &  $ ${beta_lag_3_her_2}^{${est_lag_3_her_2}} $  &     $ ${beta_lag_3_met}^{${est_lag_3_met}} $ &     $ ${beta_lag_3_met_2}^{${est_lag_3_met_2}} $ & $ ${beta_lag_3_lab}^{${est_lag_3_lab}} $ & $ ${beta_lag_3_lab_2}^{${est_lag_3_lab_2}} $ & $ ${beta_lag_3_new}^{${est_lag_3_new}} $ & $ ${beta_lag_3_new_2}^{${est_lag_3_new_2}} $ & $ ${beta_lag_3_new_3}^{${est_lag_3_new_3}} $  \\
tex &     ($${se_lag_3_logdet}$) &     ($${se_lag_3_ihsdet}$) & ($${se_lag_3_her}$)& ($ ${se_lag_3_her_2}$)  &    ($${se_lag_3_met}$)   &   ($${se_lag_3_met_2}$) &   ($${se_lag_3_lab}$) &   ($${se_lag_3_lab_2}$) &   ($${se_lag_3_new}$) &   ($${se_lag_3_new_2}$) &   ($${se_lag_3_new_3}$) \\
tex t-2 &     $ ${beta_lag_2_logdet}^{${est_lag_2_logdet}} $ &     $ ${beta_lag_2_ihsdet}^{${est_lag_2_ihsdet}} $ &  $ ${beta_lag_2_her}^{${est_lag_2_her}} $  &  $ ${beta_lag_2_her_2}^{${est_lag_2_her_2}} $  &     $ ${beta_lag_2_met}^{${est_lag_2_met}} $ &     $ ${beta_lag_2_met_2}^{${est_lag_2_met_2}} $ & $ ${beta_lag_2_lab}^{${est_lag_2_lab}} $ & $ ${beta_lag_2_lab_2}^{${est_lag_2_lab_2}} $ & $ ${beta_lag_2_new}^{${est_lag_2_new}} $ & $ ${beta_lag_2_new_2}^{${est_lag_2_new_2}} $ & $ ${beta_lag_2_new_3}^{${est_lag_2_new_3}} $  \\
tex &     ($${se_lag_2_logdet}$) &     ($${se_lag_2_ihsdet}$) & ($${se_lag_2_her}$)& ($ ${se_lag_2_her_2}$)  &    ($${se_lag_2_met}$)   &   ($${se_lag_2_met_2}$) &   ($${se_lag_2_lab}$) &   ($${se_lag_2_lab_2}$) &   ($${se_lag_2_new}$) &   ($${se_lag_2_new_2}$) &   ($${se_lag_2_new_3}$) \\
tex Reform, t=0 &     $ ${beta_date_0_logdet}^{${est_date_0_logdet}} $ &     $ ${beta_date_0_ihsdet}^{${est_date_0_ihsdet}} $ &  $ ${beta_date_0_her}^{${est_date_0_her}} $  &  $ ${beta_date_0_her_2}^{${est_date_0_her_2}} $  &     $ ${beta_date_0_met}^{${est_date_0_met}} $ &     $ ${beta_date_0_met_2}^{${est_date_0_met_2}} $ & $ ${beta_date_0_lab}^{${est_date_0_lab}} $ & $ ${beta_date_0_lab_2}^{${est_date_0_lab_2}} $ & $ ${beta_date_0_new}^{${est_date_0_new}} $ & $ ${beta_date_0_new_2}^{${est_date_0_new_2}} $ & $ ${beta_date_0_new_3}^{${est_date_0_new_3}} $  \\
tex &     ($${se_date_0_logdet}$) &     ($${se_date_0_ihsdet}$) & ($${se_date_0_her}$)& ($ ${se_date_0_her_2}$)  &    ($${se_date_0_met}$)   &   ($${se_date_0_met_2}$) &   ($${se_date_0_lab}$) &   ($${se_date_0_lab_2}$) &   ($${se_date_0_new}$) &   ($${se_date_0_new_2}$) &   ($${se_date_0_new_3}$) \\
tex t+1 &     $ ${beta_lead_1_logdet}^{${est_lead_1_logdet}} $ &     $ ${beta_lead_1_ihsdet}^{${est_lead_1_ihsdet}} $ &  $ ${beta_lead_1_her}^{${est_lead_1_her}} $  &  $ ${beta_lead_1_her_2}^{${est_lead_1_her_2}} $  &     $ ${beta_lead_1_met}^{${est_lead_1_met}} $ &     $ ${beta_lead_1_met_2}^{${est_lead_1_met_2}} $ & $ ${beta_lead_1_lab}^{${est_lead_1_lab}} $ & $ ${beta_lead_1_lab_2}^{${est_lead_1_lab_2}} $ & $ ${beta_lead_1_new}^{${est_lead_1_new}} $ & $ ${beta_lead_1_new_2}^{${est_lead_1_new_2}} $ & $ ${beta_lead_1_new_3}^{${est_lead_1_new_3}} $  \\
tex &     ($${se_lead_1_logdet}$) &     ($${se_lead_1_ihsdet}$) & ($${se_lead_1_her}$)& ($ ${se_lead_1_her_2}$)  &    ($${se_lead_1_met}$)   &   ($${se_lead_1_met_2}$) &   ($${se_lead_1_lab}$) &   ($${se_lead_1_lab_2}$) &   ($${se_lead_1_new}$) &   ($${se_lead_1_new_2}$) &   ($${se_lead_1_new_3}$) \\
tex t+2 &     $ ${beta_lead_2_logdet}^{${est_lead_2_logdet}} $ &     $ ${beta_lead_2_ihsdet}^{${est_lead_2_ihsdet}} $ &  $ ${beta_lead_2_her}^{${est_lead_2_her}} $  &  $ ${beta_lead_2_her_2}^{${est_lead_2_her_2}} $  &     $ ${beta_lead_2_met}^{${est_lead_2_met}} $ &     $ ${beta_lead_2_met_2}^{${est_lead_2_met_2}} $ & $ ${beta_lead_2_lab}^{${est_lead_2_lab}} $ & $ ${beta_lead_2_lab_2}^{${est_lead_2_lab_2}} $ & $ ${beta_lead_2_new}^{${est_lead_2_new}} $ & $ ${beta_lead_2_new_2}^{${est_lead_2_new_2}} $ & $ ${beta_lead_2_new_3}^{${est_lead_2_new_3}} $  \\
tex &     ($${se_lead_2_logdet}$) &     ($${se_lead_2_ihsdet}$) & ($${se_lead_2_her}$)& ($ ${se_lead_2_her_2}$)  &    ($${se_lead_2_met}$)   &   ($${se_lead_2_met_2}$) &   ($${se_lead_2_lab}$) &   ($${se_lead_2_lab_2}$) &   ($${se_lead_2_new}$) &   ($${se_lead_2_new_2}$) &   ($${se_lead_2_new_3}$) \\
tex t+3 &     $ ${beta_lead_3_logdet}^{${est_lead_3_logdet}} $ &     $ ${beta_lead_3_ihsdet}^{${est_lead_3_ihsdet}} $ &  $ ${beta_lead_3_her}^{${est_lead_3_her}} $  &  $ ${beta_lead_3_her_2}^{${est_lead_3_her_2}} $  &     $ ${beta_lead_3_met}^{${est_lead_3_met}} $ &     $ ${beta_lead_3_met_2}^{${est_lead_3_met_2}} $ & $ ${beta_lead_3_lab}^{${est_lead_3_lab}} $ & $ ${beta_lead_3_lab_2}^{${est_lead_3_lab_2}} $ & $ ${beta_lead_3_new}^{${est_lead_3_new}} $ & $ ${beta_lead_3_new_2}^{${est_lead_3_new_2}} $ & $ ${beta_lead_3_new_3}^{${est_lead_3_new_3}} $  \\
tex &     ($${se_lead_3_logdet}$) &     ($${se_lead_3_ihsdet}$) & ($${se_lead_3_her}$)& ($ ${se_lead_3_her_2}$)  &    ($${se_lead_3_met}$)   &   ($${se_lead_3_met_2}$) &   ($${se_lead_3_lab}$) &   ($${se_lead_3_lab_2}$) &   ($${se_lead_3_new}$) &   ($${se_lead_3_new_2}$) &   ($${se_lead_3_new_3}$) \\

tex \\
tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}    &     ${N_her}      &     ${N_her_2}  &        ${N_met}    &        ${N_met_2}  &        ${N_lab}    &        ${N_lab_2}  &        ${N_new} &        ${N_new_2}  &        ${N_new_3}  \\
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet}    &    ${r2_her}       &           ${r2_her_2} &          ${r2_met} &          ${r2_met_2}     &        ${r2_lab}    &        ${r2_lab_2}  &        ${r2_new}  &        ${r2_new_2}  &        ${r2_new_3}  \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark  &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    &  \checkmark   &     \checkmark         &  \checkmark \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   &  \checkmark   &     \checkmark         &  \checkmark  \\
tex Controls$^b$  &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark     &  \checkmark   &     \checkmark         &  \checkmark  \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark     &  \checkmark   &     \checkmark         &  \checkmark \\
tex Reform average effect         & $${aggregate_logdet}^{${est_aggregate_logdet}} $$      & $${aggregate_ihsdet}^{${est_aggregate_ihsdet}} $$     & $${aggregate_her}^{${est_aggregate_her}} $$        & $${aggregate_her_2}^{${est_aggregate_her_2}} $$       & $${aggregate_met}^{${est_aggregate_met}} $$        & $${aggregate_met_2}^{${est_aggregate_met_2}} $$    & $${aggregate_lab}^{${est_aggregate_lab}} $$      & $${aggregate_lab_2}^{${est_aggregate_lab_2}} $$    & $${aggregate_new}^{${est_aggregate_new}} $$   & $${aggregate_new_2}^{${est_aggregate_new_2}} $$   & $${aggregate_lab_3}^{${est_aggregate_lab_3}} $$  \\
tex SE (average effect)      & (${se_aggregate_logdet})  & (${se_aggregate_ihsdet}) & (${se_aggregate_her})  & (${se_aggregate_her_2})  & (${se_aggregate_met})  & (${se_aggregate_met_2})    & (${se_aggregate_lab})  & (${se_aggregate_lab_2})   & (${se_aggregate_new})   & (${se_aggregate_new_2})   & (${se_aggregate_new_3})  \\

tex \hline \hline      
tex \multicolumn{12}{p{1.7\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020} except for the specification that trims periods prior to t-4. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close


*ONLY AGGREGATES:	
texdoc init  "../Tables/abraham_sun_preferences_average.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Citizens Preferences}
tex \label{tab:preferences_average}
tex \scalebox{0.60}{    
tex \begin{tabular}{lccccccccccc}  
tex \hline \hline       
tex \\ \multicolumn{12}{l}{Dependent variable: topic that worries the most}\\
tex & Narcotraffic & Insecurity & Punishment to criminals & Corruption  & Poverty & Unemployment & Inflation & Natural Disasters & Water Scarcity & Education & Health \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} & \multicolumn{1}{c}{(9)} & \multicolumn{1}{c}{(10)} & \multicolumn{1}{c}{(11)}\\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \cmidrule(lrr){5-5} \cmidrule(lrr){6-6} \cmidrule(lrr){7-7} \cmidrule(lrr){8-8} \cmidrule(lrr){9-9} \cmidrule(lrr){10-10} \cmidrule(lrr){11-11} \cmidrule(lrr){12-12}\\
tex \addlinespace

tex Reform average effect         & $${aggregate_logdet}^{${est_aggregate_logdet}} $$      & $${aggregate_ihsdet}^{${est_aggregate_ihsdet}} $$     & $${aggregate_her}^{${est_aggregate_her}} $$        & $${aggregate_her_2}^{${est_aggregate_her_2}} $$       & $${aggregate_met}^{${est_aggregate_met}} $$        & $${aggregate_met_2}^{${est_aggregate_met_2}} $$    & $${aggregate_lab}^{${est_aggregate_lab}} $$      & $${aggregate_lab_2}^{${est_aggregate_lab_2}} $$    & $${aggregate_new}^{${est_aggregate_new}} $$   & $${aggregate_new_2}^{${est_aggregate_new_2}} $$   & $${aggregate_lab_3}^{${est_aggregate_lab_3}} $$  \\
tex SE (average effect)      & (${se_aggregate_logdet})  & (${se_aggregate_ihsdet}) & (${se_aggregate_her})  & (${se_aggregate_her_2})  & (${se_aggregate_met})  & (${se_aggregate_met_2})    & (${se_aggregate_lab})  & (${se_aggregate_lab_2})   & (${se_aggregate_new})   & (${se_aggregate_new_2})   & (${se_aggregate_new_3})  \\

tex \\
tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}    &     ${N_her}      &     ${N_her_2}  &        ${N_met}    &        ${N_met_2}  &        ${N_lab}    &        ${N_lab_2}  &        ${N_new} &        ${N_new_2}  &        ${N_new_3}  \\
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet}    &    ${r2_her}       &           ${r2_her_2} &          ${r2_met} &          ${r2_met_2}     &        ${r2_lab}    &        ${r2_lab_2}  &        ${r2_new}  &        ${r2_new_2}  &        ${r2_new_3}  \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark  &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    &  \checkmark   &     \checkmark         &  \checkmark \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   &  \checkmark   &     \checkmark         &  \checkmark  \\
tex Controls$^b$  &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark     &  \checkmark   &     \checkmark         &  \checkmark  \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark     &  \checkmark   &     \checkmark         &  \checkmark \\

tex \hline \hline      
tex \multicolumn{12}{p{1.7\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020} except for the specification that trims periods prior to t-4. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close
 

*========================================================================
*FIGURE WITH AGGREGATES
*est clear
foreach i in $preferences{
	qui xi: reghdfe  `i'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
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

coefplot  (est12, rename((1) = "Narcotraffic") mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est13, rename((1) = "Insecurity") mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est14, rename((1) = "Punishment")  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est15, rename((1) = "Corruption")   msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est16, rename((1) = "Poverty")  mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est17, rename((1) = "Unemployment")  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est18, rename((1) = "Inflation")  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est19, rename((1) = "Disaster")  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est20, rename((1) = "Water") mfcolor(white)  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est21, rename((1) = "Education")   msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est22, rename((1) = "Health") mfcolor(white) msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle("Term Limit Reform Average Effect" "from t to t+3") ///
subtitle("Topic that worries the most") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI") rows(1)) 
graph export "../Figures/change_preferences_as.png", as(png) replace
graph export "../Figures/change_preferences_as.pdf", as(pdf) replace
graph export "../Figures/change_preferences_as.tif", as(tif) replace
graph save "../Figures/change_preferences_as.gph", replace


coefplot (est1, rename((1) = "Narcotraffic") mfcolor(white) msize(medium) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est2, rename((1) = "Insecurity") mfcolor(white) msize(medium) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est3, rename((1) = "Punishment")  msize(medium) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est4, rename((1) = "Corruption")   msize(medium) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est5, rename((1) = "Poverty")  mfcolor(white) msize(medium) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est6, rename((1) = "Unemployment")  msize(medium) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est7, rename((1) = "Inflation") mfcolor(white) msize(medium) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est8, rename((1) = "Disaster")mfcolor(white)  msize(medium) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
  (est9, rename((1) = "Water")  mfcolor(white) msize(medium) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est10, rename((1) = "Education") mfcolor(white)  msize(medium) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est11, rename((1) = "Health") mfcolor(white) msize(medium) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est12, rename((1) = "Narcotraffic") msize(medium) msymbol(S) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est13, rename((1) = "Insecurity") msize(medium) msymbol(S) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est14, rename((1) = "Punishment")  msize(medium) msymbol(S) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est15, rename((1) = "Corruption")   msize(medium) msymbol(S) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est16, rename((1) = "Poverty")   msize(medium) msymbol(S) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est17, rename((1) = "Unemployment")  msize(medium) msymbol(S) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est18, rename((1) = "Inflation") mfcolor(white) msize(medium) msymbol(S) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est19, rename((1) = "Disaster")  msize(medium) msymbol(S) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est20, rename((1) = "Water") mfcolor(white)  msize(medium) msymbol(S) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est21, rename((1) = "Education")   msize(medium) msymbol(S) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 (est22, rename((1) = "Health") mfcolor(white) msize(medium) msymbol(S) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *4) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle("Term Limit Reform Average Effect" "from t to t+3") ///
subtitle("Topic that worries the most") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" 20 "TWFE" 80 "CATTs") rows(2)) 
graph export "../Figures/change_preferences_comparison.png", as(png) replace
graph export "../Figures/change_preferences_comparison.pdf", as(pdf) replace
graph export "../Figures/change_preferences_comparison.tif", as(tif) replace
graph save "../Figures/change_preferences_comparison.gph", replace


*========================================================================
*========================================================================
*========================================================================
