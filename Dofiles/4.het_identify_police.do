*Regressions: HET EFFECTS SECURITY CITIZEN TASTES (STATE LEVEL)
*Paper: Reelection Backfire
*Author: Rafael Ch (rafael.ch@nyu.edu)
*========================================================================
/*NOTES
ENVIPE variables: state-year level
ap4_2_3  = tema que más me preocupa - narcotráfico 
ap4_2_5  = tema que más me preocupa - inseguridad 
ap4_2_11 = tema que más me preocupa - falta castigo a delincuentes 
ap4_12b = ¿Cuánto gastaron en total por esas medidas durante 2018? 
ap5_3_2_b  = ¿Cuánta confianza le inspira la (el) (AUTORIDAD)? policia preventiva municipal
ap5_3_8_b = ¿Cuánta confianza le inspira la (el) (AUTORIDAD)? ejercito
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

	global controls_time_acuerdo  $homicides $margin $margin_governor $logpop $carteles

	
*2) treatment
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

*3) outcomes
	global outcome acuerdo_estcom

	
*========================================================================
*Run .ado 
do "wild_areg.do"
do "macros_tables.do"

*========================================================================
*Interaction effects:
************
***A: ap5_3_1_mean
************
cap drop inter_*
global split_variable ap5_3_1_mean
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated $split_variable inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_logdet:  di %5.4f e(r2)
		glo N_logdet: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_logdet: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
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
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_logdet: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_logdet: di r(p)
	glo beta_`i'_logdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_logdet= "" 
			if (${p_`i'_logdet}<=0.1) global est_`i'_logdet = "*"
			if (${p_`i'_logdet}<=0.05) global est_`i'_logdet = "**"
			if (${p_`i'_logdet}<=0.01) global est_`i'_logdet = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_logdet: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
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
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_logdet: di %5.4f r(estimate)
	estadd local aggregate_logdet $aggregate_logdet
	glo se_aggregate_logdet: di %5.4f r(se)
	estadd local se_aggregate_logdet $se_aggregate_logdet
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_logdet: di %5.4f r(p)
	estadd local p_aggregate_logdet $p_aggregate_logdet
					glo est_aggregate_logdet= "" 
			if (${p_aggregate_logdet}<=0.11) global est_aggregate_logdet = "*"
			if (${p_aggregate_logdet}<=0.05) global est_aggregate_logdet = "**"
			if (${p_aggregate_logdet}<=0.01) global est_aggregate_logdet = "***"	

	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
************
***B: ap5_3_2_mean
************
cap drop inter_*
global split_variable ap5_3_2_mean
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated $split_variable inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_ihsdet:  di %5.4f e(r2)
		glo N_ihsdet: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_ihsdet: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
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
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_ihsdet: di r(p)
	glo beta_`i'_ihsdet: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_ihsdet= "" 
			if (${p_`i'_ihsdet}<=0.1) global est_`i'_ihsdet = "*"
			if (${p_`i'_ihsdet}<=0.05) global est_`i'_ihsdet = "**"
			if (${p_`i'_ihsdet}<=0.01) global est_`i'_ihsdet = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_ihsdet: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
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
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_ihsdet: di %5.4f r(estimate)
	estadd local aggregate_ihsdet $aggregate_ihsdet
	glo se_aggregate_ihsdet: di %5.4f r(se)
	estadd local se_aggregate_ihsdet $se_aggregate_ihsdet
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_ihsdet: di %5.4f r(p)
	estadd local p_aggregate_ihsdet $p_aggregate_ihsdet
					glo est_aggregate_ihsdet= "" 
			if (${p_aggregate_ihsdet}<=0.11) global est_aggregate_ihsdet = "*"
			if (${p_aggregate_ihsdet}<=0.05) global est_aggregate_ihsdet = "**"
			if (${p_aggregate_ihsdet}<=0.01) global est_aggregate_ihsdet = "***"	
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
************
***C: ap5_3_3_mean
************
cap drop inter_*
global split_variable ap5_3_3_mean
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated $split_variable inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_her:  di %5.4f e(r2)
		glo N_her: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_her: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
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
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_her: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_her: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_her: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_her: di r(p)
	glo beta_`i'_her: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_her= "" 
			if (${p_`i'_her}<=0.1) global est_`i'_her = "*"
			if (${p_`i'_her}<=0.05) global est_`i'_her = "**"
			if (${p_`i'_her}<=0.01) global est_`i'_her = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_her: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
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
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_her: di %5.4f r(estimate)
	estadd local aggregate_her $aggregate_her
	glo se_aggregate_her: di %5.4f r(se)
	estadd local se_aggregate_her $se_aggregate_her
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_her: di %5.4f r(p)
	estadd local p_aggregate_her $p_aggregate_her
					glo est_aggregate_her= "" 
			if (${p_aggregate_her}<=0.11) global est_aggregate_her = "*"
			if (${p_aggregate_her}<=0.05) global est_aggregate_her = "**"
			if (${p_aggregate_her}<=0.01) global est_aggregate_her = "***"	
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
			
************
***D: ap5_3_6_mean
************
cap drop inter_*
global split_variable ap5_3_6_mean
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}
 
areg   $outcome $saturated $split_variable inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_her_2:  di %5.4f e(r2)
		glo N_her_2: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_her_2: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
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
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_her_2: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_her_2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_her_2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_her_2: di r(p)
	glo beta_`i'_her_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_her_2= "" 
			if (${p_`i'_her_2}<=0.1) global est_`i'_her_2 = "*"
			if (${p_`i'_her_2}<=0.05) global est_`i'_her_2 = "**"
			if (${p_`i'_her_2}<=0.01) global est_`i'_her_2 = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_her_2: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
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
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_her_2: di %5.4f r(estimate)
	estadd local aggregate_her_2 $aggregate_her_2
	glo se_aggregate_her_2: di %5.4f r(se)
	estadd local se_aggregate_her_2 $se_aggregate_her_2
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_her_2: di %5.4f r(p)
	estadd local p_aggregate_her_2 $p_aggregate_her_2
					glo est_aggregate_her_2= "" 
			if (${p_aggregate_her_2}<=0.11) global est_aggregate_her_2 = "*"
			if (${p_aggregate_her_2}<=0.05) global est_aggregate_her_2 = "**"
			if (${p_aggregate_her_2}<=0.01) global est_aggregate_her_2 = "***"	
			
			sum $split_variable, meanonly
	local median_$split_variable= r(mean)
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4	
************
***E: ap5_3_4_mean
************
cap drop inter_*
global split_variable ap5_3_4_mean
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated $split_variable inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_met:  di %5.4f e(r2)
		glo N_met: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_met: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
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
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_met: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_met: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_met: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_met: di r(p)
	glo beta_`i'_met: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_met= "" 
			if (${p_`i'_met}<=0.1) global est_`i'_met = "*"
			if (${p_`i'_met}<=0.05) global est_`i'_met = "**"
			if (${p_`i'_met}<=0.01) global est_`i'_met = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_met: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
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
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_met: di %5.4f r(estimate)
	estadd local aggregate_met $aggregate_met
	glo se_aggregate_met: di %5.4f r(se)
	estadd local se_aggregate_met $se_aggregate_met
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_met: di %5.4f r(p)
	estadd local p_aggregate_met $p_aggregate_met
					glo est_aggregate_met= "" 
			if (${p_aggregate_met}<=0.11) global est_aggregate_met = "*"
			if (${p_aggregate_met}<=0.05) global est_aggregate_met = "**"
			if (${p_aggregate_met}<=0.01) global est_aggregate_met = "***"	

	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
************
***F: ap5_3_5_mean
************
cap drop inter_*
global split_variable ap5_3_5_mean
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated $split_variable inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_met_2:  di %5.4f e(r2)
		glo N_met_2: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_met_2: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
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
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_met_2: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_met_2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_met_2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_met_2: di %5.4f r(se)
}


**estimate average effect of total interaction effect:
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
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_met_2: di %5.4f r(estimate)
	estadd local aggregate_met_2 $aggregate_met_2
	glo se_aggregate_met_2: di %5.4f r(se)
	estadd local se_aggregate_met_2 $se_aggregate_met_2
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_met_2: di %5.4f r(p)
	estadd local p_aggregate_met_2 $p_aggregate_met_2
					glo est_aggregate_met_2= "" 
			if (${p_aggregate_met_2}<=0.11) global est_aggregate_met_2 = "*"
			if (${p_aggregate_met_2}<=0.05) global est_aggregate_met_2 = "**"
			if (${p_aggregate_met_2}<=0.01) global est_aggregate_met_2 = "***"	
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
************
***H: ap5_3_8_mean
************
cap drop inter_*
global split_variable ap5_3_8_mean
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated $split_variable inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_lab:  di %5.4f e(r2)
		glo N_lab: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_lab: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
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
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_lab: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_lab: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_lab: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_lab: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
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
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_lab: di %5.4f r(estimate)
	estadd local aggregate_lab $aggregate_lab
	glo se_aggregate_lab: di %5.4f r(se)
	estadd local se_aggregate_lab $se_aggregate_lab
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_lab: di %5.4f r(p)
	estadd local p_aggregate_lab $p_aggregate_lab
					glo est_aggregate_lab= "" 
			if (${p_aggregate_lab}<=0.11) global est_aggregate_lab = "*"
			if (${p_aggregate_lab}<=0.05) global est_aggregate_lab = "**"
			if (${p_aggregate_lab}<=0.01) global est_aggregate_lab = "***"	

	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4

************
***H: ap5_3_9_mean
************
cap drop inter_*
global split_variable ap5_3_9_mean
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated $split_variable inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_lab_2:  di %5.4f e(r2)
		glo N_lab_2: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_lab_2: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
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
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_lab_2: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_lab_2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_lab_2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_lab_2: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
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
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_lab_2: di %5.4f r(estimate)
	estadd local aggregate_lab_2 $aggregate_lab_2
	glo se_aggregate_lab_2: di %5.4f r(se)
	estadd local se_aggregate_lab_2 $se_aggregate_lab_2
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_lab_2: di %5.4f r(p)
	estadd local p_aggregate_lab_2 $p_aggregate_lab_2
					glo est_aggregate_lab_2= "" 
			if (${p_aggregate_lab_2}<=0.11) global est_aggregate_lab_2 = "*"
			if (${p_aggregate_lab_2}<=0.05) global est_aggregate_lab_2 = "**"
			if (${p_aggregate_lab_2}<=0.01) global est_aggregate_lab_2 = "***"	
			
					eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
			
*Table			
texdoc init  "../Tables/interaction_identify.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Reform interaction with citizens' being able to identify a Police Force}
tex \label{tab:interaction_identify}
tex \scalebox{0.70}{    
tex \begin{tabular}{lcccccccc}  
tex \hline \hline       
tex \\ \multicolumn{9}{l}{Dependent variable: Signing Security Cooperation Agreement w/ Governor}\\
tex Jurisdiction: & \multicolumn{2}{c}{Municipal} & \multicolumn{2}{c}{State} & \multicolumn{4}{c}{Federal} \\
tex Identify Policy Force: & Traffic  & Preventive  & State Police & State Attorney Police & Federal Police & Ministerial Police & Army & Marines \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} \\ 


tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \cmidrule(lrr){5-5} \cmidrule(lrr){6-6} \cmidrule(lrr){7-7} \cmidrule(lrr){8-8} \cmidrule(lrr){9-9} \\
tex \addlinespace

tex t-7 &     $ ${beta_lag_7_logdet}^{${est_lag_7_logdet}} $ &     $ ${beta_lag_7_ihsdet}^{${est_lag_7_ihsdet}} $ &  $ ${beta_lag_7_her}^{${est_lag_7_her}} $  &  $ ${beta_lag_7_her_2}^{${est_lag_7_her_2}} $  &     $ ${beta_lag_7_met}^{${est_lag_7_met}} $ &     $ ${beta_lag_7_met_2}^{${est_lag_7_met_2}} $ & $ ${beta_lag_7_lab}^{${est_lag_7_lab}} $ & $ ${beta_lag_7_lab_2}^{${est_lag_7_lab_2}} $   \\
tex &     ($${se_lag_7_logdet}$) &     ($${se_lag_7_ihsdet}$) & ($${se_lag_7_her}$)& ($ ${se_lag_7_her_2}$)  &    ($${se_lag_7_met}$)   &   ($${se_lag_7_met_2}$) &    ($${se_lag_7_lab}$)   &   ($${se_lag_7_lab_2}$) \\
tex t-6 &     $ ${beta_lag_6_logdet}^{${est_lag_6_logdet}} $ &     $ ${beta_lag_6_ihsdet}^{${est_lag_6_ihsdet}} $ &  $ ${beta_lag_6_her}^{${est_lag_6_her}} $  &  $ ${beta_lag_6_her_2}^{${est_lag_6_her_2}} $  &     $ ${beta_lag_6_met}^{${est_lag_6_met}} $ &     $ ${beta_lag_6_met_2}^{${est_lag_6_met_2}} $ & $ ${beta_lag_6_lab}^{${est_lag_6_lab}} $ & $ ${beta_lag_6_lab_2}^{${est_lag_6_lab_2}} $   \\
tex &     ($${se_lag_6_logdet}$) &     ($${se_lag_6_ihsdet}$) & ($${se_lag_6_her}$)& ($ ${se_lag_6_her_2}$)  &    ($${se_lag_6_met}$)   &   ($${se_lag_6_met_2}$) &    ($${se_lag_6_lab}$)   &   ($${se_lag_6_lab_2}$) \\
tex t-5 &     $ ${beta_lag_5_logdet}^{${est_lag_5_logdet}} $ &     $ ${beta_lag_5_ihsdet}^{${est_lag_5_ihsdet}} $ &  $ ${beta_lag_5_her}^{${est_lag_5_her}} $  &  $ ${beta_lag_5_her_2}^{${est_lag_5_her_2}} $  &     $ ${beta_lag_5_met}^{${est_lag_5_met}} $ &     $ ${beta_lag_5_met_2}^{${est_lag_5_met_2}} $ & $ ${beta_lag_5_lab}^{${est_lag_5_lab}} $ & $ ${beta_lag_5_lab_2}^{${est_lag_5_lab_2}} $   \\
tex &     ($${se_lag_5_logdet}$) &     ($${se_lag_5_ihsdet}$) & ($${se_lag_5_her}$)& ($ ${se_lag_5_her_2}$)  &    ($${se_lag_5_met}$)   &   ($${se_lag_5_met_2}$) &    ($${se_lag_5_lab}$)   &   ($${se_lag_5_lab_2}$) \\
tex t-4 &     $ ${beta_lag_4_logdet}^{${est_lag_4_logdet}} $ &     $ ${beta_lag_4_ihsdet}^{${est_lag_4_ihsdet}} $ &  $ ${beta_lag_4_her}^{${est_lag_4_her}} $  &  $ ${beta_lag_4_her_2}^{${est_lag_4_her_2}} $  &     $ ${beta_lag_4_met}^{${est_lag_4_met}} $ &     $ ${beta_lag_4_met_2}^{${est_lag_4_met_2}} $ & $ ${beta_lag_4_lab}^{${est_lag_4_lab}} $ & $ ${beta_lag_4_lab_2}^{${est_lag_4_lab_2}} $   \\
tex &     ($${se_lag_4_logdet}$) &     ($${se_lag_4_ihsdet}$) & ($${se_lag_4_her}$)& ($ ${se_lag_4_her_2}$)  &    ($${se_lag_4_met}$)   &   ($${se_lag_4_met_2}$) &    ($${se_lag_4_lab}$)   &   ($${se_lag_4_lab_2}$) \\
tex t-3 &     $ ${beta_lag_3_logdet}^{${est_lag_3_logdet}} $ &     $ ${beta_lag_3_ihsdet}^{${est_lag_3_ihsdet}} $ &  $ ${beta_lag_3_her}^{${est_lag_3_her}} $  &  $ ${beta_lag_3_her_2}^{${est_lag_3_her_2}} $  &     $ ${beta_lag_3_met}^{${est_lag_3_met}} $ &     $ ${beta_lag_3_met_2}^{${est_lag_3_met_2}} $ & $ ${beta_lag_3_lab}^{${est_lag_3_lab}} $ & $ ${beta_lag_3_lab_2}^{${est_lag_3_lab_2}} $   \\
tex &     ($${se_lag_3_logdet}$) &     ($${se_lag_3_ihsdet}$) & ($${se_lag_3_her}$)& ($ ${se_lag_3_her_2}$)  &    ($${se_lag_3_met}$)   &   ($${se_lag_3_met_2}$) &    ($${se_lag_3_lab}$)   &   ($${se_lag_3_lab_2}$) \\
tex t-2 &     $ ${beta_lag_2_logdet}^{${est_lag_2_logdet}} $ &     $ ${beta_lag_2_ihsdet}^{${est_lag_2_ihsdet}} $ &  $ ${beta_lag_2_her}^{${est_lag_2_her}} $  &  $ ${beta_lag_2_her_2}^{${est_lag_2_her_2}} $  &     $ ${beta_lag_2_met}^{${est_lag_2_met}} $ &     $ ${beta_lag_2_met_2}^{${est_lag_2_met_2}} $ & $ ${beta_lag_2_lab}^{${est_lag_2_lab}} $ & $ ${beta_lag_2_lab_2}^{${est_lag_2_lab_2}} $   \\
tex &     ($${se_lag_2_logdet}$) &     ($${se_lag_2_ihsdet}$) & ($${se_lag_2_her}$)& ($ ${se_lag_2_her_2}$)  &    ($${se_lag_2_met}$)   &   ($${se_lag_2_met_2}$) &    ($${se_lag_2_lab}$)   &   ($${se_lag_2_lab_2}$) \\
tex Reform (t=0) &     $ ${beta_date_0_logdet}^{${est_date_0_logdet}} $ &     $ ${beta_date_0_ihsdet}^{${est_date_0_ihsdet}} $ &   $ ${beta_date_0_her}^{${est_date_0_her}} $   &   $ ${beta_date_0_her_2}^{${est_date_0_her_2}} $  &     $ ${beta_date_0_met}^{${est_date_0_met}} $ &     $ ${beta_date_0_met_2}^{${est_date_0_met_2}} $ & $ ${beta_date_0_lab}^{${est_date_0_lab}} $ & $ ${beta_date_0_lab_2}^{${est_date_0_lab_2}} $   \\
tex &     ($${se_date_0_logdet}$) &     ($${se_date_0_ihsdet}$) & ($${se_date_0_her}$)& ($ ${se_date_0_her_2}$)  &    ($${se_date_0_met}$)   &   ($${se_date_0_met_2}$) &    ($${se_date_0_lab}$)   &   ($${se_date_0_lab_2}$) \\
tex t+1 &     $ ${beta_lead_1_logdet}^{${est_lead_1_logdet}} $ &     $ ${beta_lead_1_ihsdet}^{${est_lead_1_ihsdet}} $ &    $ ${beta_lead_1_her}^{${est_lead_1_her}} $ &    $ ${beta_lead_1_her_2}^{${est_lead_1_her_2}} $ &     $ ${beta_lead_1_met}^{${est_lead_1_met}} $ &     $ ${beta_lead_1_met_2}^{${est_lead_1_met_2}} $  & $ ${beta_lead_1_lab}^{${est_lead_1_lab}} $ & $ ${beta_lead_1_lab_2}^{${est_lead_1_lab_2}} $   \\
tex &     ($${se_lead_1_logdet}$) &     ($${se_lead_1_ihsdet}$) & ($${se_lead_1_her}$)& ($ ${se_lead_1_her_2}$)  &    ($${se_lead_1_met}$)   &   ($${se_lead_1_met_2}$) &    ($${se_lead_1_lab}$)   &   ($${se_lead_1_lab_2}$) \\
tex t+2 &     $ ${beta_lead_2_logdet}^{${est_lead_2_logdet}} $ &     $ ${beta_lead_2_ihsdet}^{${est_lead_2_ihsdet}} $ &    $ ${beta_lead_2_her}^{${est_lead_2_her}} $ &    $ ${beta_lead_2_her_2}^{${est_lead_2_her_2}} $ &     $ ${beta_lead_2_met}^{${est_lead_2_met}} $ &     $ ${beta_lead_2_met_2}^{${est_lead_2_met_2}} $  & $ ${beta_lead_2_lab}^{${est_lead_2_lab}} $ & $ ${beta_lead_2_lab_2}^{${est_lead_2_lab_2}} $   \\
tex &     ($${se_lead_2_logdet}$) &     ($${se_lead_2_ihsdet}$) & ($${se_lead_2_her}$)& ($ ${se_lead_2_her_2}$)  &    ($${se_lead_2_met}$)   &   ($${se_lead_2_met_2}$) &    ($${se_lead_2_lab}$)   &   ($${se_lead_2_lab_2}$) \\
tex t+3 &     $ ${beta_lead_3_logdet}^{${est_lead_3_logdet}} $ &     $ ${beta_lead_3_ihsdet}^{${est_lead_3_ihsdet}} $ &    $ ${beta_lead_3_her}^{${est_lead_3_her}} $ &    $ ${beta_lead_3_her_2}^{${est_lead_3_her_2}} $ &     $ ${beta_lead_3_met}^{${est_lead_3_met}} $ &     $ ${beta_lead_3_met_2}^{${est_lead_3_met_2}} $  & $ ${beta_lead_3_lab}^{${est_lead_3_lab}} $ & $ ${beta_lead_3_lab_2}^{${est_lead_3_lab_2}} $   \\
tex &     ($${se_lead_3_logdet}$) &     ($${se_lead_3_ihsdet}$) & ($${se_lead_3_her}$)& ($ ${se_lead_3_her_2}$)  &    ($${se_lead_3_met}$)   &   ($${se_lead_3_met_2}$) &    ($${se_lead_3_lab}$)   &   ($${se_lead_3_lab_2}$) \\

tex \\
tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}    &     ${N_her}      &     ${N_her_2}  &        ${N_met}    &        ${N_met_2}  &        ${N_lab}    &        ${N_lab_2}   \\
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet}    &    ${r2_her}       &           ${r2_her_2} &          ${r2_met} &          ${r2_met_2}     &        ${r2_lab}    &        ${r2_lab_2}   \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark  &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex Controls$^b$  &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark     \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\
tex Reform average effect         & $${aggregate_logdet}^{${est_aggregate_logdet}} $$      & $${aggregate_ihsdet}^{${est_aggregate_ihsdet}} $$     & $${aggregate_her}^{${est_aggregate_her}} $$        & $${aggregate_her_2}^{${est_aggregate_her_2}} $$       & $${aggregate_met}^{${est_aggregate_met}} $$        & $${aggregate_met_2}^{${est_aggregate_met_2}} $$    & $${aggregate_lab}^{${est_aggregate_lab}} $$      & $${aggregate_lab_2}^{${est_aggregate_lab_2}} $$     \\
tex SE (average effect)      & (${se_aggregate_logdet})  & (${se_aggregate_ihsdet}) & (${se_aggregate_her})  & (${se_aggregate_her_2})  & (${se_aggregate_met})  & (${se_aggregate_met_2})    & (${se_aggregate_lab})  & (${se_aggregate_lab_2})   \\
*tex p-value  & [${p_aggregate_logdet}]   & [${p_aggregate_ihsdet}]  & [${p_aggregate_her}]   & [${p_aggregate_her_2}]   & [${p_aggregate_met}]   & [${p_aggregate_met_2}]     & [${p_aggregate_lab}]   & [${p_aggregate_lab_2}]    \\

tex \hline \hline      
tex \multicolumn{9}{p{1.6\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to security cooperation agreements signed with the Governor. $^b$ Pretreatment controls include: governor winning margin; party alignment with the President;  party alignment with the Governor; municipal winning margin; logged population; logged organized crime related deaths; and Cartel presence.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close

*Table average effect		
texdoc init  "../Tables/interaction_identify_average.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Reform interaction with citizens' being able to identify a Police Force}
tex \label{tab:interaction_identify_average}
tex \scalebox{0.70}{    
tex \begin{tabular}{lcccccccc}  
tex \hline \hline       
tex \\ \multicolumn{9}{l}{Dependent variable: Signing Security Cooperation Agreement w/ Governor}\\
tex Jurisdiction: & \multicolumn{2}{c}{Municipal} & \multicolumn{2}{c}{State} & \multicolumn{4}{c}{Federal} \\
tex Identify Policy Force: & Traffic  & Preventive  & State Police & State Attorney Police & Federal Police & Ministerial Police & Army & Marines \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} \\ 


tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \cmidrule(lrr){5-5} \cmidrule(lrr){6-6} \cmidrule(lrr){7-7} \cmidrule(lrr){8-8} \cmidrule(lrr){9-9} \\
tex \addlinespace

tex Reform average effect         & $${aggregate_logdet}^{${est_aggregate_logdet}} $$      & $${aggregate_ihsdet}^{${est_aggregate_ihsdet}} $$     & $${aggregate_her}^{${est_aggregate_her}} $$        & $${aggregate_her_2}^{${est_aggregate_her_2}} $$       & $${aggregate_met}^{${est_aggregate_met}} $$        & $${aggregate_met_2}^{${est_aggregate_met_2}} $$    & $${aggregate_lab}^{${est_aggregate_lab}} $$      & $${aggregate_lab_2}^{${est_aggregate_lab_2}} $$     \\
tex SE (average effect)      & (${se_aggregate_logdet})  & (${se_aggregate_ihsdet}) & (${se_aggregate_her})  & (${se_aggregate_her_2})  & (${se_aggregate_met})  & (${se_aggregate_met_2})    & (${se_aggregate_lab})  & (${se_aggregate_lab_2})   \\
*tex p-value  & [${p_aggregate_logdet}]   & [${p_aggregate_ihsdet}]  & [${p_aggregate_her}]   & [${p_aggregate_her_2}]   & [${p_aggregate_met}]   & [${p_aggregate_met_2}]     & [${p_aggregate_lab}]   & [${p_aggregate_lab_2}]    \\

tex \\
tex \addlinespace
tex Observations       &        ${N_logdet}    &        ${N_ihsdet}    &     ${N_her}      &     ${N_her_2}  &        ${N_met}    &        ${N_met_2}  &        ${N_lab}    &        ${N_lab_2}   \\
tex R-squared        &          ${r2_logdet} &          ${r2_ihsdet}    &    ${r2_her}       &           ${r2_her_2} &          ${r2_met} &          ${r2_met_2}     &        ${r2_lab}    &        ${r2_lab_2}   \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark  &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex Controls$^b$  &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark &    \checkmark     &       \checkmark  &    \checkmark      &   \checkmark     \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{9}{p{1.6\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to security cooperation agreements signed with the Governor. $^b$ Pretreatment controls include: governor winning margin; party alignment with the President;  party alignment with the Governor; municipal winning margin; logged population; logged organized crime related deaths; and Cartel presence.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close
*========================================================================
*Figure
coefplot (est1, rename((1) = "Municipal Traffic") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Municipal Preventive") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "State Police") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est4, rename((1) = "State Attorney Police")  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "Federal Police")  mfcolor(white) msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Federal Ministerial Police")   msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est7, rename((1) = "Army")  msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est8, rename((1) = "Marines")   msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle("Term Limit Reform Average Effect" "from t to t+3") ///
subtitle(" ") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" 4 "Municipal" ///
 12 "State" 20 "Federal") rows(2)) 
graph export "../Figures/identify_policeforces.png", as(png) replace
graph export "../Figures/identify_policeforces.pdf", as(pdf) replace
graph export "../Figures/identify_policeforces.tif", as(tif) replace
graph save "../Figures/identify_policeforces.gph", replace
*========================================================================
*Figure: comparing aggregate municipal to state to federal
egen municipal_police=rowmean(ap5_3_1_mean ap5_3_2_mean)
egen state_police=rowmean(ap5_3_3_mean ap5_3_6_mean)
egen federal_police=rowmean(ap5_3_4_mean ap5_3_5_mean ap5_3_8 ap5_3_9_mean)

est clear
************
***A: municipal_police
************
cap drop inter_*
global split_variable municipal_police
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated $split_variable inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_met_2:  di %5.4f e(r2)
		glo N_met_2: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_met_2: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
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
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_met_2: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_met_2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_met_2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_met_2: di r(p)
	glo beta_`i'_met_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_met_2= "" 
			if (${p_`i'_met_2}<=0.1) global est_`i'_met_2 = "*"
			if (${p_`i'_met_2}<=0.05) global est_`i'_met_2 = "**"
			if (${p_`i'_met_2}<=0.01) global est_`i'_met_2 = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_met_2: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
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
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_met_2: di %5.4f r(estimate)
	estadd local aggregate_met_2 $aggregate_met_2
	glo se_aggregate_met_2: di %5.4f r(se)
	estadd local se_aggregate_met_2 $se_aggregate_met_2
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_met_2: di %5.4f r(p)
	estadd local p_aggregate_met_2 $p_aggregate_met_2
					glo est_aggregate_met_2= "" 
			if (${p_aggregate_met_2}<=0.11) global est_aggregate_met_2 = "*"
			if (${p_aggregate_met_2}<=0.05) global est_aggregate_met_2 = "**"
			if (${p_aggregate_met_2}<=0.01) global est_aggregate_met_2 = "***"	
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
************
***B: state_police
************
cap drop inter_*
global split_variable state_police
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated $split_variable inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_lab:  di %5.4f e(r2)
		glo N_lab: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_lab: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
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
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_lab: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_lab: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_lab: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_lab: di r(p)
	glo beta_`i'_lab: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_lab= "" 
			if (${p_`i'_lab}<=0.1) global est_`i'_lab = "*"
			if (${p_`i'_lab}<=0.05) global est_`i'_lab = "**"
			if (${p_`i'_lab}<=0.01) global est_`i'_lab = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_lab: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
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
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_lab: di %5.4f r(estimate)
	estadd local aggregate_lab $aggregate_lab
	glo se_aggregate_lab: di %5.4f r(se)
	estadd local se_aggregate_lab $se_aggregate_lab
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_lab: di %5.4f r(p)
	estadd local p_aggregate_lab $p_aggregate_lab
					glo est_aggregate_lab= "" 
			if (${p_aggregate_lab}<=0.11) global est_aggregate_lab = "*"
			if (${p_aggregate_lab}<=0.05) global est_aggregate_lab = "**"
			if (${p_aggregate_lab}<=0.01) global est_aggregate_lab = "***"	

	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
		eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4

************
***C: federal_police
************
cap drop inter_*
global split_variable federal_police
foreach i in $split_variable{
foreach j in $saturated{
gen inter_`j'=`i'*`j'
}
}

areg   $outcome $saturated $split_variable inter_* $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_lab_2:  di %5.4f e(r2)
		glo N_lab_2: di %11.2gc e(N)
		
*total interaction effects: leads and lags

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	test [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	[(_b[`i'_2017]+_b[inter_`i'_2017])*`a'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`b']
	glo se_`i'_lab_2: di %5.4f r(se)
}
	
foreach i in lag_6{
	sum perc if `i'_2016==1, meanonly
	local a = r(mean)
	sum perc if `i'_2017==1, meanonly
	local b = r(mean)
	sum perc if `i'_2018==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	test [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	[(_b[`i'_2016]+_b[inter_`i'_2016])*`a'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`b'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`c']
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
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c'] + [(_b[`i'_2018]+_b[inter_`i'_2018])*`d']
	glo se_`i'_lab_2: di %5.4f r(se)
}


foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] + [(_b[`i'_2017]+_b[inter_`i'_2017])*`c']
	glo se_`i'_lab_2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] =0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a'] + [(_b[`i'_2016]+_b[inter_`i'_2016])*`b'] 
	glo se_`i'_lab_2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	test [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']=0
	glo p_`i'_lab_2: di r(p)
	glo beta_`i'_lab_2: di %5.4f [(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo est_`i'_lab_2= "" 
			if (${p_`i'_lab_2}<=0.1) global est_`i'_lab_2 = "*"
			if (${p_`i'_lab_2}<=0.05) global est_`i'_lab_2 = "**"
			if (${p_`i'_lab_2}<=0.01) global est_`i'_lab_2 = "***"	
	lincom 	[(_b[`i'_2015]+_b[inter_`i'_2015])*`a']
	glo se_`i'_lab_2: di %5.4f r(se)
}








**estimate average effect of total interaction effect:
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
	
	*evaluate at the median
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	lincom 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
	glo aggregate_lab_2: di %5.4f r(estimate)
	estadd local aggregate_lab_2 $aggregate_lab_2
	glo se_aggregate_lab_2: di %5.4f r(se)
	estadd local se_aggregate_lab_2 $se_aggregate_lab_2
	sum $split_variable, meanonly
	local median_$split_variable= r(mean)
	test 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4=0
	glo p_aggregate_lab_2: di %5.4f r(p)
	estadd local p_aggregate_lab_2 $p_aggregate_lab_2
					glo est_aggregate_lab_2= "" 
			if (${p_aggregate_lab_2}<=0.11) global est_aggregate_lab_2 = "*"
			if (${p_aggregate_lab_2}<=0.05) global est_aggregate_lab_2 = "**"
			if (${p_aggregate_lab_2}<=0.01) global est_aggregate_lab_2 = "***"	
			
					eststo:lincomest 	(`median_$split_variable'*[(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j') ///
		+ (_b[inter_date_0_2015]*`a')+(_b[inter_date_0_2016]*`b')+(_b[inter_date_0_2017]*`c') + (_b[inter_date_0_2018]*`d') ///
		+ (_b[inter_lead_1_2015]*`e')+(_b[inter_lead_1_2016]*`f')+(_b[inter_lead_1_2017]*`g') ///
		+ (_b[inter_lead_2_2015]*`h')+(_b[inter_lead_2_2016]*`i') ///
		+ (_b[inter_lead_3_2015]*`j')])/ 4
			

coefplot (est1, rename((1) = "Municipal Forces") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "State Forces") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Federal Forces") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle("Term Limit Reform Average Effect" "from t to t+3") ///
subtitle("Identify Police Forces") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" 4 "Municipal" ///
 8 "State" 12 "Federal") rows(2)) 
graph export "../Figures/identify_policeforces2.png", as(png) replace
graph export "../Figures/identify_policeforces2.pdf", as(pdf) replace
graph export "../Figures/identify_policeforces2.tif", as(tif) replace
graph save "../Figures/identify_policeforces2.gph", replace

*========================================================================
*Combine with trust
grc1leg "../Figures/trust_policeforces2.gph" "../Figures/identify_policeforces2.gph" , ///
scheme(s1color)  imargin(vsmall) ycommon  col(3) l1(" ") b1(" ")
graph export "../Figures/trust&preferences.png", as(png) replace
graph export "../Figures/trust&preferences.pdf", as(pdf) replace
graph export "../Figures/trust&preferences.tif", as(tif) replace



*========================================================================
*========================================================================
*========================================================================
*========================================================================


