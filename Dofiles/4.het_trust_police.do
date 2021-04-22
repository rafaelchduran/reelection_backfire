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
ap5_4_2_b  = ¿Cuánta confianza le inspira la (el) (AUTORIDAD)? policia preventiva municipal
ap5_4_8_b = ¿Cuánta confianza le inspira la (el) (AUTORIDAD)? ejercito
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
***A: ap5_4_1_b_mean
************
cap drop inter_*
global split_variable ap5_4_1_b_mean
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
***B: ap5_4_2_b_mean
************
cap drop inter_*
global split_variable ap5_4_2_b_mean
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
***C: ap5_4_3_b_mean
************
cap drop inter_*
global split_variable ap5_4_3_b_mean
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
***D: ap5_4_6_b_mean
************
cap drop inter_*
global split_variable ap5_4_6_b_mean
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
***E: ap5_4_4_b_mean
************
cap drop inter_*
global split_variable ap5_4_4_b_mean
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
***F: ap5_4_5_b_mean
************
cap drop inter_*
global split_variable ap5_4_5_b_mean
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
***H: ap5_4_8_b_mean
************
cap drop inter_*
global split_variable ap5_4_8_b_mean
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
***H: ap5_4_9_b_mean
************
cap drop inter_*
global split_variable ap5_4_9_b_mean
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
texdoc init  "../Tables/interaction_trust.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Services Delegated to the Governor}
tex \label{tab:interaction_trust}
tex \scalebox{0.70}{    
tex \begin{tabular}{lcccccccc}  
tex \hline \hline       
tex \\ \multicolumn{9}{l}{Dependent variable: Signing Security Cooperation Agreement w/ Governor}\\
tex Jurisdiction: & \multicolumn{2}{c}{Municipal} & \multicolumn{2}{c}{State} & \multicolumn{4}{c}{Federal} \\
tex Police force: & Traffic  & Preventive  & State Police & State Attorney Police & Federal Police & Ministerial Police & Army & Marines \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} \\ 


tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \cmidrule(lrr){5-5} \cmidrule(lrr){6-6} \cmidrule(lrr){7-7} \cmidrule(lrr){8-8} \cmidrule(lrr){9-9} \\
tex \addlinespace

tex t-7 &     $ ${beta_lag_7_logdet}^{${est_lag_7_logdet}} $ &     $ ${beta_lag_7_ihsdet}^{${est_lag_7_ihsdet}} $ &  $ ${beta_lag_7_her}^{${est_lag_7_her}} $  &  $ ${beta_lag_7_her_2}^{${est_lag_7_her_2}} $  &     $ ${beta_lag_7_met}^{${est_lag_7_met}} $ &     $ ${beta_lag_7_met_2}^{${est_lag_7_met_2}} $ & $ ${beta_lag_7_lab}^{${est_lag_7_lab}} $ & $ ${beta_lag_7_lab_2}^{${est_lag_7_lab_2}} $   \\
tex &     ($${se_lag_7_logdet}$) &     ($${se_lag_7_ihsdet}$) & ($${se_lag_7_her}$)& ($ ${se_lag_7_her_2}$)  &    ($${se_lag_7_met}$)   &   ($${se_lag_7_met_2}$) \\
tex t-6 &     $ ${beta_lag_6_logdet}^{${est_lag_6_logdet}} $ &     $ ${beta_lag_6_ihsdet}^{${est_lag_6_ihsdet}} $ &  $ ${beta_lag_6_her}^{${est_lag_6_her}} $  &  $ ${beta_lag_6_her_2}^{${est_lag_6_her_2}} $  &     $ ${beta_lag_6_met}^{${est_lag_6_met}} $ &     $ ${beta_lag_6_met_2}^{${est_lag_6_met_2}} $ & $ ${beta_lag_6_lab}^{${est_lag_6_lab}} $ & $ ${beta_lag_6_lab_2}^{${est_lag_6_lab_2}} $   \\
tex &     ($${se_lag_6_logdet}$) &     ($${se_lag_6_ihsdet}$) & ($${se_lag_6_her}$)& ($ ${se_lag_6_her_2}$)  &    ($${se_lag_6_met}$)   &   ($${se_lag_6_met_2}$) \\
tex t-5 &     $ ${beta_lag_5_logdet}^{${est_lag_5_logdet}} $ &     $ ${beta_lag_5_ihsdet}^{${est_lag_5_ihsdet}} $ &  $ ${beta_lag_5_her}^{${est_lag_5_her}} $  &  $ ${beta_lag_5_her_2}^{${est_lag_5_her_2}} $  &     $ ${beta_lag_5_met}^{${est_lag_5_met}} $ &     $ ${beta_lag_5_met_2}^{${est_lag_5_met_2}} $ & $ ${beta_lag_5_lab}^{${est_lag_5_lab}} $ & $ ${beta_lag_5_lab_2}^{${est_lag_5_lab_2}} $   \\
tex &     ($${se_lag_5_logdet}$) &     ($${se_lag_5_ihsdet}$) & ($${se_lag_5_her}$)& ($ ${se_lag_5_her_2}$)  &    ($${se_lag_5_met}$)   &   ($${se_lag_5_met_2}$) \\
tex t-4 &     $ ${beta_lag_4_logdet}^{${est_lag_4_logdet}} $ &     $ ${beta_lag_4_ihsdet}^{${est_lag_4_ihsdet}} $ &  $ ${beta_lag_4_her}^{${est_lag_4_her}} $  &  $ ${beta_lag_4_her_2}^{${est_lag_4_her_2}} $  &     $ ${beta_lag_4_met}^{${est_lag_4_met}} $ &     $ ${beta_lag_4_met_2}^{${est_lag_4_met_2}} $ & $ ${beta_lag_4_lab}^{${est_lag_4_lab}} $ & $ ${beta_lag_4_lab_2}^{${est_lag_4_lab_2}} $   \\
tex &     ($${se_lag_4_logdet}$) &     ($${se_lag_4_ihsdet}$) & ($${se_lag_4_her}$)& ($ ${se_lag_4_her_2}$)  &    ($${se_lag_4_met}$)   &   ($${se_lag_4_met_2}$) \\
tex t-3 &     $ ${beta_lag_3_logdet}^{${est_lag_3_logdet}} $ &     $ ${beta_lag_3_ihsdet}^{${est_lag_3_ihsdet}} $ &  $ ${beta_lag_3_her}^{${est_lag_3_her}} $  &  $ ${beta_lag_3_her_2}^{${est_lag_3_her_2}} $  &     $ ${beta_lag_3_met}^{${est_lag_3_met}} $ &     $ ${beta_lag_3_met_2}^{${est_lag_3_met_2}} $ & $ ${beta_lag_3_lab}^{${est_lag_3_lab}} $ & $ ${beta_lag_3_lab_2}^{${est_lag_3_lab_2}} $   \\
tex &     ($${se_lag_3_logdet}$) &     ($${se_lag_3_ihsdet}$) & ($${se_lag_3_her}$)& ($ ${se_lag_3_her_2}$)  &    ($${se_lag_3_met}$)   &   ($${se_lag_3_met_2}$) \\
tex t-2 &     $ ${beta_lag_2_logdet}^{${est_lag_2_logdet}} $ &     $ ${beta_lag_2_ihsdet}^{${est_lag_2_ihsdet}} $ &  $ ${beta_lag_2_her}^{${est_lag_2_her}} $  &  $ ${beta_lag_2_her_2}^{${est_lag_2_her_2}} $  &     $ ${beta_lag_2_met}^{${est_lag_2_met}} $ &     $ ${beta_lag_2_met_2}^{${est_lag_2_met_2}} $ & $ ${beta_lag_2_lab}^{${est_lag_2_lab}} $ & $ ${beta_lag_2_lab_2}^{${est_lag_2_lab_2}} $   \\
tex &     ($${se_lag_2_logdet}$) &     ($${se_lag_2_ihsdet}$) & ($${se_lag_2_her}$)& ($ ${se_lag_2_her_2}$)  &    ($${se_lag_2_met}$)   &   ($${se_lag_2_met_2}$) \\
tex Reform (t=0) &     $ ${beta_date_0_logdet}^{${est_date_0_logdet}} $ &     $ ${beta_date_0_ihsdet}^{${est_date_0_ihsdet}} $ &   $ ${beta_date_0_her}^{${est_date_0_her}} $   &   $ ${beta_date_0_her_2}^{${est_date_0_her_2}} $  &     $ ${beta_date_0_met}^{${est_date_0_met}} $ &     $ ${beta_date_0_met_2}^{${est_date_0_met_2}} $ & $ ${beta_date_0_lab}^{${est_date_0_lab}} $ & $ ${beta_date_0_lab_2}^{${est_date_0_lab_2}} $   \\
tex &     ($${se_date_0_logdet}$) &     ($${se_date_0_ihsdet}$) & ($${se_date_0_her}$)& ($ ${se_date_0_her_2}$)  &    ($${se_date_0_met}$)   &   ($${se_date_0_met_2}$) \\
tex t+1 &     $ ${beta_lead_1_logdet}^{${est_lead_1_logdet}} $ &     $ ${beta_lead_1_ihsdet}^{${est_lead_1_ihsdet}} $ &    $ ${beta_lead_1_her}^{${est_lead_1_her}} $ &    $ ${beta_lead_1_her_2}^{${est_lead_1_her_2}} $ &     $ ${beta_lead_1_met}^{${est_lead_1_met}} $ &     $ ${beta_lead_1_met_2}^{${est_lead_1_met_2}} $  & $ ${beta_lead_1_lab}^{${est_lead_1_lab}} $ & $ ${beta_lead_1_lab_2}^{${est_lead_1_lab_2}} $   \\
tex &     ($${se_lead_1_logdet}$) &     ($${se_lead_1_ihsdet}$) & ($${se_lead_1_her}$)& ($ ${se_lead_1_her_2}$)  &    ($${se_lead_1_met}$)   &   ($${se_lead_1_met_2}$) \\
tex t+2 &     $ ${beta_lead_2_logdet}^{${est_lead_2_logdet}} $ &     $ ${beta_lead_2_ihsdet}^{${est_lead_2_ihsdet}} $  \\
tex &     ($${se_lead_2_logdet}$) &     ($${se_lead_2_ihsdet}$)  \\
tex t+3 &     $ ${beta_lead_3_logdet}^{${est_lead_3_logdet}} $ &     $ ${beta_lead_3_ihsdet}^{${est_lead_3_ihsdet}} $  \\
tex &     ($${se_lead_3_logdet}$) &     ($${se_lead_3_ihsdet}$)  \\
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
tex \multicolumn{9}{p{1.5\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Even columns with outcomes with missing values where replaced by zeros assuming no activity was registered. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close

*Table average effect		
texdoc init  "../Tables/interaction_trust_average.tex", replace force
tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Services Delegated to the Governor}
tex \label{tab:interaction_trust_average}
tex \scalebox{0.70}{    
tex \begin{tabular}{lcccccccc}  
tex \hline \hline       
tex \\ \multicolumn{9}{l}{Dependent variable: Signing Security Cooperation Agreement w/ Governor}\\
tex Jurisdiction: & \multicolumn{2}{c}{Municipal} & \multicolumn{2}{c}{State} & \multicolumn{4}{c}{Federal} \\
tex Police force: & Traffic  & Preventive  & State Police & State Attorney Police & Federal Police & Ministerial Police & Army & Marines \\
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
tex \multicolumn{9}{p{1.5\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Even columns with outcomes with missing values where replaced by zeros assuming no activity was registered. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
tex \end{landscape}
texdoc close
*========================================================================
*Figure
preserve
label variable reform " "
coefplot (est1, rename((1) = "Municipal Traffic") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "Municipal Preventive") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "State Police") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est4, rename((1) = "State Attorney Police")  msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est5, rename((1) = "Federal Police")   msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est6, rename((1) = "Federal Ministerial Police")   msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est7, rename((1) = "Army")  msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est8, rename((1) = "Marines")   msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle("Term Limit Reform Average Effect" "from t to t+3") ///
subtitle(" ") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" 4 "Municipal" ///
 12 "State" 20 "Federal") rows(2)) 
graph export "../Figures/trust_policeforces.png", as(png) replace
graph export "../Figures/trust_policeforces.pdf", as(pdf) replace
graph export "../Figures/trust_policeforces.tif", as(tif) replace
graph save "../Figures/trust_policeforces.gph", replace
restore
*========================================================================
*Figure: comparing aggregate municipal to state to federal
egen municipal_police=rowmean(ap5_4_1_b_mean ap5_4_2_b_mean)
egen state_police=rowmean(ap5_4_3_b_mean ap5_4_6_b_mean)
egen federal_police=rowmean(ap5_4_4_b_mean ap5_4_5_b_mean ap5_4_8 ap5_4_9_b_mean)

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
			

preserve
label variable reform " "
coefplot (est1, rename((1) = "Municipal Forces") msize(large) mcolor(green) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est2, rename((1) = "State Forces") msize(large) mcolor(red) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 (est3, rename((1) = "Federal Forces") msize(large) mcolor(blue) levels(99 95 90) ciopts(lwidth(*1 *3 *5) color(black black black))) ///
 , ///
 horizontal scheme(s1color)  xline(0)    ///
ytitle(" ")  xtitle("Term Limit Reform Average Effect" "from t to t+3") ///
subtitle(" ") legend(order(1 "99% CI" 2 "95% CI" 3 "90% CI" 4 "Municipal" ///
 8 "State" 12 "Federal") rows(2)) 
graph export "../Figures/trust_policeforces2.png", as(png) replace
graph export "../Figures/trust_policeforces2.pdf", as(pdf) replace
graph export "../Figures/trust_policeforces2.tif", as(tif) replace
graph save "../Figures/trust_policeforces2.gph", replace
restore

*========================================================================
*========================================================================
*========================================================================
*========================================================================
*========================================================================

/*========================================================================
*1) AS(2021) split TRUST
global split_variable ap5_4_1_mean  // ap5_4_1_mean ap5_4_2_mean ap5_4_3_mean ap5_4_6_mean

preserve
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'

************
***A: BELOW MEAN 
************	
est clear 
*eststo: qui wildcorrection_as_new2 $outcome
xi: reghdfe  $outcome  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols 
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)


***estimate linear combination by lead/lag:
cap foreach i in lag_7{
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
	
cap foreach i in lag_6{
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

cap foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
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


cap foreach i in lead_1{
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


cap foreach i in lead_2{
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


cap foreach i in lead_3{
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
	glo aggregate: di %5.4f r(estimate)
	estadd local aggregate $aggregate
	glo se_aggregate: di %5.4f r(se)
	estadd local se_aggregate $se_aggregate
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate: di %5.4f r(p)
	estadd local p_aggregate $p_aggregate

restore
************
***B: ABOVE MEAN
************
preserve
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'

*eststo: qui wildcorrection_as_new2 $outcome
xi: reghdfe  $outcome2  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
cap foreach i in lag_7{
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
	
cap foreach i in lag_6{
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

cap foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
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


cap foreach i in lead_1{
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


cap foreach i in lead_2{
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


cap foreach i in lead_3{
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
		+ (_b[lead_3_2015]*`j')] / 4
	glo aggregate2: di %5.4f r(estimate)
	estadd local aggregate2 $aggregate2
	glo se_aggregate2: di %5.4f r(se)
	estadd local se_aggregate2 $se_aggregate2
	test [(_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d') ///
		+ (_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g') ///
		+ (_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i') ///
		+ (_b[lead_3_2015]*`j')]/4	=0
	glo p_aggregate2: di %5.4f r(p)
	estadd local p_aggregate2 $p_aggregate2
restore 

*Table
texdoc init  "../Tables/abraham_sun_estimates_DVmandounico_trust_police.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements, \citet{chaisemarting_etal_2019} correction}
tex \label{tab:chaisemartin_agreements}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} \\
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
tex Controls$^b$   &      \checkmark       &      \checkmark    \\
tex Cohort weighted   &   \checkmark       &   \checkmark    \\
tex WILD CI   &   \checkmark       &   \checkmark    \\
tex Aggregate effect        &              ${aggregate}        &           ${aggregate2}   \\
tex SE (aggregate eff.)        &              ${se_aggregate}        &           ${se_aggregate2}   \\
tex p-value(aggregate eff.)       &              ${p_aggregate}        &           ${p_aggregate2}   \\

tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*========================================================================
**2) Robustness: TRUST. For Chaisemartin I can only do this by subsetting the data
global outcome acuerdo_estcom
************
*A) POLICIA TRANSITO below mean:
***********
global split_variable ap5_4_1_mean

preserve
foreach outcome in $outcome {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

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
foreach i in 1 2 3 4 5{
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

	


restore

************
*B) POLICIA TRANSITO above mean:
************
preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

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
foreach i in 1 2 3 4 5{
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


restore

************
*A) POLICIA PREVENTIVA below mean:
***********
global split_variable ap5_4_2_mean

preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados3_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta3_t_`i': di %5.3f e(effect_`i')
	glo se3_t_`i': di %5.3f e(se_effect_`i')
	glo t3_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval3_t_`i': di 2*ttail(${grados3_t_`i'},abs(${t3_t_`i'})) 
	glo est3_t_`i'= "" 
			if (${pval3_t_`i'}<=0.1) global est3_t_`i' = "*"
			if (${pval3_t_`i'}<=0.05) global est3_t_`i' = "**"
			if (${pval3_t_`i'}<=0.01) global est3_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados3_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta3_t_`i': di %5.3f e(placebo_`i')
	glo pse3_t_`i': di %5.3f e(se_placebo_`i')
	glo pt3_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval3_t_`i': di 2*ttail(${pgrados3_t_`i'},abs(${pt3_t_`i'})) 
	glo pest3_t_`i'= "" 
			if (${ppval3_t_`i'}<=0.1) global pest3_t_`i' = "*"
			if (${ppval3_t_`i'}<=0.05) global pest3_t_`i' = "**"
			if (${ppval3_t_`i'}<=0.01) global pest3_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	


restore

************
*B) POLICIA PREVENTIVA above mean:
************
preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados4_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta4_t_`i': di %5.3f e(effect_`i')
	glo se4_t_`i': di %5.3f e(se_effect_`i')
	glo t4_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval4_t_`i': di 2*ttail(${grados4_t_`i'},abs(${t4_t_`i'})) 
	glo est4_t_`i'= "" 
			if (${pval4_t_`i'}<=0.1) global est4_t_`i' = "*"
			if (${pval4_t_`i'}<=0.05) global est4_t_`i' = "**"
			if (${pval4_t_`i'}<=0.01) global est4_t_`i' = "***"
			*if (${pval4_t_`i'}=0) global est4_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados4_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta4_t_`i': di %5.3f e(placebo_`i')
	glo pse4_t_`i': di %5.3f e(se_placebo_`i')
	glo pt4_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval4_t_`i': di 2*ttail(${pgrados4_t_`i'},abs(${pt4_t_`i'})) 
	glo pest4_t_`i'= "" 
			if (${ppval4_t_`i'}<=0.1) global pest4_t_`i' = "*"
			if (${ppval4_t_`i'}<=0.05) global pest4_t_`i' = "**"
			if (${ppval4_t_`i'}<=0.01) global pest4_t_`i' = "***"
			*if (${ppval4_t_`i'}=0) global pest4_t_`i' = "***"
}


restore

************
*A) POLICIA ESTATAL below mean:
***********
global split_variable ap5_4_3_mean

preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados5_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta5_t_`i': di %5.3f e(effect_`i')
	glo se5_t_`i': di %5.3f e(se_effect_`i')
	glo t5_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval5_t_`i': di 2*ttail(${grados5_t_`i'},abs(${t5_t_`i'})) 
	glo est5_t_`i'= "" 
			if (${pval5_t_`i'}<=0.1) global est5_t_`i' = "*"
			if (${pval5_t_`i'}<=0.05) global est5_t_`i' = "**"
			if (${pval5_t_`i'}<=0.01) global est5_t_`i' = "***"
			*if (${pval5_t_`i'}=0) global est5_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados5_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta5_t_`i': di %5.3f e(placebo_`i')
	glo pse5_t_`i': di %5.3f e(se_placebo_`i')
	glo pt5_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval5_t_`i': di 2*ttail(${pgrados5_t_`i'},abs(${pt5_t_`i'})) 
	glo pest5_t_`i'= "" 
			if (${ppval5_t_`i'}<=0.1) global pest5_t_`i' = "*"
			if (${ppval5_t_`i'}<=0.05) global pest5_t_`i' = "**"
			if (${ppval5_t_`i'}<=0.01) global pest5_t_`i' = "***"
			*if (${ppval5_t_`i'}=0) global pest5_t_`i' = "***"
}

	


restore

************
*B) POLICIA ESTATAL above mean:
************
preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados6_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta6_t_`i': di %5.3f e(effect_`i')
	glo se6_t_`i': di %5.3f e(se_effect_`i')
	glo t6_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval6_t_`i': di 2*ttail(${grados6_t_`i'},abs(${t6_t_`i'})) 
	glo est6_t_`i'= "" 
			if (${pval6_t_`i'}<=0.1) global est6_t_`i' = "*"
			if (${pval6_t_`i'}<=0.05) global est6_t_`i' = "**"
			if (${pval6_t_`i'}<=0.01) global est6_t_`i' = "***"
			*if (${pval6_t_`i'}=0) global est6_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados6_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta6_t_`i': di %5.3f e(placebo_`i')
	glo pse6_t_`i': di %5.3f e(se_placebo_`i')
	glo pt6_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval6_t_`i': di 2*ttail(${pgrados6_t_`i'},abs(${pt6_t_`i'})) 
	glo pest6_t_`i'= "" 
			if (${ppval6_t_`i'}<=0.1) global pest6_t_`i' = "*"
			if (${ppval6_t_`i'}<=0.05) global pest6_t_`i' = "**"
			if (${ppval6_t_`i'}<=0.01) global pest6_t_`i' = "***"
			*if (${ppval6_t_`i'}=0) global pest6_t_`i' = "***"
}


restore

************
*A) POLICIA FEDERAL below mean:
***********
global split_variable ap5_4_6_mean

preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados7_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta7_t_`i': di %5.3f e(effect_`i')
	glo se7_t_`i': di %5.3f e(se_effect_`i')
	glo t7_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval7_t_`i': di 2*ttail(${grados7_t_`i'},abs(${t7_t_`i'})) 
	glo est7_t_`i'= "" 
			if (${pval7_t_`i'}<=0.1) global est7_t_`i' = "*"
			if (${pval7_t_`i'}<=0.05) global est7_t_`i' = "**"
			if (${pval7_t_`i'}<=0.01) global est7_t_`i' = "***"
			*if (${pval7_t_`i'}=0) global est7_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados7_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta7_t_`i': di %5.3f e(placebo_`i')
	glo pse7_t_`i': di %5.3f e(se_placebo_`i')
	glo pt7_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval7_t_`i': di 2*ttail(${pgrados7_t_`i'},abs(${pt7_t_`i'})) 
	glo pest7_t_`i'= "" 
			if (${ppval7_t_`i'}<=0.1) global pest7_t_`i' = "*"
			if (${ppval7_t_`i'}<=0.05) global pest7_t_`i' = "**"
			if (${ppval7_t_`i'}<=0.01) global pest7_t_`i' = "***"
			*if (${ppval7_t_`i'}=0) global pest7_t_`i' = "***"
}

	


restore

************
*B) POLICIA FEDERAL above mean:
************
preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados8_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta8_t_`i': di %5.3f e(effect_`i')
	glo se8_t_`i': di %5.3f e(se_effect_`i')
	glo t8_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval8_t_`i': di 2*ttail(${grados8_t_`i'},abs(${t8_t_`i'})) 
	glo est8_t_`i'= "" 
			if (${pval8_t_`i'}<=0.1) global est8_t_`i' = "*"
			if (${pval8_t_`i'}<=0.05) global est8_t_`i' = "**"
			if (${pval8_t_`i'}<=0.01) global est8_t_`i' = "***"
			*if (${pval8_t_`i'}=0) global est8_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados8_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta8_t_`i': di %5.3f e(placebo_`i')
	glo pse8_t_`i': di %5.3f e(se_placebo_`i')
	glo pt8_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval8_t_`i': di 2*ttail(${pgrados8_t_`i'},abs(${pt8_t_`i'})) 
	glo pest8_t_`i'= "" 
			if (${ppval8_t_`i'}<=0.1) global pest8_t_`i' = "*"
			if (${ppval8_t_`i'}<=0.05) global pest8_t_`i' = "**"
			if (${ppval8_t_`i'}<=0.01) global pest8_t_`i' = "***"
			*if (${ppval8_t_`i'}=0) global pest8_t_`i' = "***"
}


restore



*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVmandounico_by_trust_police.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Centralization, by security preferences}
tex \label{tab:chaisemartin_sec_preferences}
tex \scalebox{0.7}{    
tex \begin{tabular}{lcccccccc}  
tex \hline \hline       
tex \\ \multicolumn{9}{l}{Dependent variable: Signing Security Cooperation Agreement dummy}\\
tex \addlinespace
tex & \multicolumn{2}{c}{Worried about Narco} & \multicolumn{2}{c}{Worried about insecurity} & \multicolumn{2}{c}{Worried about low punishment} & \multicolumn{2}{c}{Home security expenses}
tex & & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} \\ 
tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5} \cmidrule(lrr){6-7} \cmidrule(lrr){8-9}  \\
tex \addlinespace
tex t-5 &        $ ${pbeta_t_4}^{${pest_t_4}} $ &     $ ${pbeta2_t_4}^{${pest2_t_4}} $ &     $ ${pbeta3_t_4}^{${pest3_t_4}} $ &     $ ${pbeta4_t_4}^{${pest4_t_4}} $ &    $ ${pbeta5_t_4}^{${pest5_t_4}} $ &     $ ${pbeta6_t_4}^{${pest6_t_4}} $ &     $ ${pbeta7_t_4}^{${pest7_t_4}} $ &     $ ${pbeta8_t_4}^{${pest8_t_4}} $ \\
tex  & ($ ${pse_t_4}$) & ($ ${pse2_t_4} $) & ($ ${pse3_t_4}$) & ($ ${pse4_t_4} $)  & ($ ${pse5_t_4}$) & ($ ${pse6_t_4} $) & ($ ${pse7_t_4}$) & ($ ${pse8_t_4} $) \\
tex t-4 &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ &     $ ${pbeta3_t_3}^{${pest3_t_3}} $ &     $ ${pbeta4_t_3}^{${pest4_t_3}} $ &    $ ${pbeta5_t_3}^{${pest5_t_3}} $ &     $ ${pbeta6_t_3}^{${pest6_t_3}} $ &     $ ${pbeta7_t_3}^{${pest7_t_3}} $ &     $ ${pbeta8_t_3}^{${pest8_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) & ($ ${pse3_t_3}$) & ($ ${pse4_t_3} $)  & ($ ${pse5_t_3}$) & ($ ${pse6_t_3} $) & ($ ${pse7_t_3}$) & ($ ${pse8_t_3} $) \\
tex t-3 &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ &     $ ${pbeta3_t_2}^{${pest3_t_2}} $ &     $ ${pbeta4_t_2}^{${pest4_t_2}} $ &    $ ${pbeta5_t_2}^{${pest5_t_2}} $ &     $ ${pbeta6_t_2}^{${pest6_t_2}} $ &     $ ${pbeta7_t_2}^{${pest7_t_2}} $ &     $ ${pbeta8_t_2}^{${pest8_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) & ($ ${pse3_t_2}$) & ($ ${pse4_t_2} $)  & ($ ${pse5_t_2}$) & ($ ${pse6_t_2} $) & ($ ${pse7_t_2}$) & ($ ${pse8_t_2} $) \\
tex t-2 &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ &     $ ${pbeta3_t_1}^{${pest3_t_1}} $ &     $ ${pbeta4_t_1}^{${pest4_t_1}} $ &    $ ${pbeta5_t_1}^{${pest5_t_1}} $ &     $ ${pbeta6_t_1}^{${pest6_t_1}} $ &     $ ${pbeta7_t_1}^{${pest7_t_1}} $ &     $ ${pbeta8_t_1}^{${pest8_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) & ($ ${pse3_t_1}$) & ($ ${pse4_t_1} $)  & ($ ${pse5_t_1}$) & ($ ${pse6_t_1} $) & ($ ${pse7_t_1}$) & ($ ${pse8_t_1} $) \\
tex t=0 (Reform) &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ &     $ ${beta3_t_0}^{${est3_t_0}} $ &     $ ${beta4_t_0}^{${est4_t_0}} $ &    $ ${beta5_t_0}^{${est5_t_0}} $ &     $ ${beta6_t_0}^{${est6_t_0}} $ &     $ ${beta7_t_0}^{${est7_t_0}} $ &     $ ${beta8_t_0}^{${est8_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) & ($ ${se3_t_0}$) & ($ ${se4_t_0} $)  & ($ ${se5_t_0}$) & ($ ${se6_t_0} $) & ($ ${se7_t_0}$) & ($ ${se8_t_0} $) \\
tex t+1 &        $ ${beta_t_1}^{${est_t_1}} $ &     $ ${beta2_t_1}^{${est2_t_1}} $ &     $ ${beta3_t_1}^{${est3_t_1}} $ &     $ ${beta4_t_1}^{${est4_t_1}} $ &    $ ${beta5_t_1}^{${est5_t_1}} $ &     $ ${beta6_t_1}^{${est6_t_1}} $ &     $ ${beta7_t_1}^{${est7_t_1}} $ &     $ ${beta8_t_1}^{${est8_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) & ($ ${se3_t_1}$) & ($ ${se4_t_1} $)  & ($ ${se5_t_1}$) & ($ ${se6_t_1} $) & ($ ${se7_t_1}$) & ($ ${se8_t_1} $) \\
tex t+2 &        $ ${beta_t_2}^{${est_t_2}} $ &     $ ${beta2_t_2}^{${est2_t_2}} $ &     $ ${beta3_t_2}^{${est3_t_2}} $ &     $ ${beta4_t_2}^{${est4_t_2}} $ &    $ ${beta5_t_2}^{${est5_t_2}} $ &     $ ${beta6_t_2}^{${est6_t_2}} $ &     $ ${beta7_t_2}^{${est7_t_2}} $ &     $ ${beta8_t_2}^{${est8_t_2}} $ \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) & ($ ${se3_t_2}$) & ($ ${se4_t_2} $)  & ($ ${se5_t_2}$) & ($ ${se6_t_2} $) & ($ ${se7_t_2}$) & ($ ${se8_t_2} $) \\


tex \addlinespace
tex Controls$^a$   &    \checkmark      &   \checkmark  &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark   \\
tex 95\% Diff. in CI  &    \checkmark      &   \checkmark  &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark   \\
tex \hline \hline      
tex \multicolumn{9}{p{1.5\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Pre-treatment controls include the following: governor winning margin, mayor winning margin, governor and president party alignment dummies, and logged homicides per capita.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

*========================================================================
**2) TRUST 2
global outcome acuerdo_estcom
************
*A) EJERCITO below mean:
***********
global split_variable ap5_4_8_mean

preserve
foreach outcome in $outcome {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

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
foreach i in 1 2 3 4 5{
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

	


restore

************
*B) EJERCITO above mean:
************
preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

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
foreach i in 1 2 3 4 5{
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


restore

************
*A) MARINA below mean:
***********
global split_variable ap5_4_9_mean

preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados3_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta3_t_`i': di %5.3f e(effect_`i')
	glo se3_t_`i': di %5.3f e(se_effect_`i')
	glo t3_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval3_t_`i': di 2*ttail(${grados3_t_`i'},abs(${t3_t_`i'})) 
	glo est3_t_`i'= "" 
			if (${pval3_t_`i'}<=0.1) global est3_t_`i' = "*"
			if (${pval3_t_`i'}<=0.05) global est3_t_`i' = "**"
			if (${pval3_t_`i'}<=0.01) global est3_t_`i' = "***"
			*if (${pval_t_`i'}=0) global est_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados3_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta3_t_`i': di %5.3f e(placebo_`i')
	glo pse3_t_`i': di %5.3f e(se_placebo_`i')
	glo pt3_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval3_t_`i': di 2*ttail(${pgrados3_t_`i'},abs(${pt3_t_`i'})) 
	glo pest3_t_`i'= "" 
			if (${ppval3_t_`i'}<=0.1) global pest3_t_`i' = "*"
			if (${ppval3_t_`i'}<=0.05) global pest3_t_`i' = "**"
			if (${ppval3_t_`i'}<=0.01) global pest3_t_`i' = "***"
			*if (${ppval_t_`i'}=0) global pest_t_`i' = "***"
}

	


restore

************
*B) MARINA above mean:
************
preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados4_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta4_t_`i': di %5.3f e(effect_`i')
	glo se4_t_`i': di %5.3f e(se_effect_`i')
	glo t4_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval4_t_`i': di 2*ttail(${grados4_t_`i'},abs(${t4_t_`i'})) 
	glo est4_t_`i'= "" 
			if (${pval4_t_`i'}<=0.1) global est4_t_`i' = "*"
			if (${pval4_t_`i'}<=0.05) global est4_t_`i' = "**"
			if (${pval4_t_`i'}<=0.01) global est4_t_`i' = "***"
			*if (${pval4_t_`i'}=0) global est4_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados4_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta4_t_`i': di %5.3f e(placebo_`i')
	glo pse4_t_`i': di %5.3f e(se_placebo_`i')
	glo pt4_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval4_t_`i': di 2*ttail(${pgrados4_t_`i'},abs(${pt4_t_`i'})) 
	glo pest4_t_`i'= "" 
			if (${ppval4_t_`i'}<=0.1) global pest4_t_`i' = "*"
			if (${ppval4_t_`i'}<=0.05) global pest4_t_`i' = "**"
			if (${ppval4_t_`i'}<=0.01) global pest4_t_`i' = "***"
			*if (${ppval4_t_`i'}=0) global pest4_t_`i' = "***"
}


restore

************
*A) PROCU. ESTATAL below mean:
***********
global split_variable ap5_4_6_mean

preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados5_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta5_t_`i': di %5.3f e(effect_`i')
	glo se5_t_`i': di %5.3f e(se_effect_`i')
	glo t5_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval5_t_`i': di 2*ttail(${grados5_t_`i'},abs(${t5_t_`i'})) 
	glo est5_t_`i'= "" 
			if (${pval5_t_`i'}<=0.1) global est5_t_`i' = "*"
			if (${pval5_t_`i'}<=0.05) global est5_t_`i' = "**"
			if (${pval5_t_`i'}<=0.01) global est5_t_`i' = "***"
			*if (${pval5_t_`i'}=0) global est5_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados5_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta5_t_`i': di %5.3f e(placebo_`i')
	glo pse5_t_`i': di %5.3f e(se_placebo_`i')
	glo pt5_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval5_t_`i': di 2*ttail(${pgrados5_t_`i'},abs(${pt5_t_`i'})) 
	glo pest5_t_`i'= "" 
			if (${ppval5_t_`i'}<=0.1) global pest5_t_`i' = "*"
			if (${ppval5_t_`i'}<=0.05) global pest5_t_`i' = "**"
			if (${ppval5_t_`i'}<=0.01) global pest5_t_`i' = "***"
			*if (${ppval5_t_`i'}=0) global pest5_t_`i' = "***"
}

	


restore

************
*B) PROCU. ESTATAL above mean:
************
preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados6_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta6_t_`i': di %5.3f e(effect_`i')
	glo se6_t_`i': di %5.3f e(se_effect_`i')
	glo t6_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval6_t_`i': di 2*ttail(${grados6_t_`i'},abs(${t6_t_`i'})) 
	glo est6_t_`i'= "" 
			if (${pval6_t_`i'}<=0.1) global est6_t_`i' = "*"
			if (${pval6_t_`i'}<=0.05) global est6_t_`i' = "**"
			if (${pval6_t_`i'}<=0.01) global est6_t_`i' = "***"
			*if (${pval6_t_`i'}=0) global est6_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados6_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta6_t_`i': di %5.3f e(placebo_`i')
	glo pse6_t_`i': di %5.3f e(se_placebo_`i')
	glo pt6_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval6_t_`i': di 2*ttail(${pgrados6_t_`i'},abs(${pt6_t_`i'})) 
	glo pest6_t_`i'= "" 
			if (${ppval6_t_`i'}<=0.1) global pest6_t_`i' = "*"
			if (${ppval6_t_`i'}<=0.05) global pest6_t_`i' = "**"
			if (${ppval6_t_`i'}<=0.01) global pest6_t_`i' = "***"
			*if (${ppval6_t_`i'}=0) global pest6_t_`i' = "***"
}


restore

************
*A) PGR below mean:
***********
global split_variable ap5_4_7_mean

preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable < `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100)  placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados7_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta7_t_`i': di %5.3f e(effect_`i')
	glo se7_t_`i': di %5.3f e(se_effect_`i')
	glo t7_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval7_t_`i': di 2*ttail(${grados7_t_`i'},abs(${t7_t_`i'})) 
	glo est7_t_`i'= "" 
			if (${pval7_t_`i'}<=0.1) global est7_t_`i' = "*"
			if (${pval7_t_`i'}<=0.05) global est7_t_`i' = "**"
			if (${pval7_t_`i'}<=0.01) global est7_t_`i' = "***"
			*if (${pval7_t_`i'}=0) global est7_t_`i' = "***"
}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados7_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta7_t_`i': di %5.3f e(placebo_`i')
	glo pse7_t_`i': di %5.3f e(se_placebo_`i')
	glo pt7_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval7_t_`i': di 2*ttail(${pgrados7_t_`i'},abs(${pt7_t_`i'})) 
	glo pest7_t_`i'= "" 
			if (${ppval7_t_`i'}<=0.1) global pest7_t_`i' = "*"
			if (${ppval7_t_`i'}<=0.05) global pest7_t_`i' = "**"
			if (${ppval7_t_`i'}<=0.01) global pest7_t_`i' = "***"
			*if (${ppval7_t_`i'}=0) global pest7_t_`i' = "***"
}

	


restore

************
*B) PGR above mean:
************
preserve
foreach outcome in $outcome  {
sum $split_variable
return list
local outcome_mean = r(mean)
keep if $split_variable >= `outcome_mean'
did_multiplegt `outcome' group year reform, breps(100) placebo(4) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
save_results("../../Data/chaisemartin_hayCarteles1_`outcome'.dta")

}

ereturn list
return list

*save results to globals:
foreach i in 0 1 2 3{
	glo grados8_t_`i': di %5.3f e(N_effect_`i')-3
	glo beta8_t_`i': di %5.3f e(effect_`i')
	glo se8_t_`i': di %5.3f e(se_effect_`i')
	glo t8_t_`i': di e(effect_`i')/e(se_effect_`i')
	glo pval8_t_`i': di 2*ttail(${grados8_t_`i'},abs(${t8_t_`i'})) 
	glo est8_t_`i'= "" 
			if (${pval8_t_`i'}<=0.1) global est8_t_`i' = "*"
			if (${pval8_t_`i'}<=0.05) global est8_t_`i' = "**"
			if (${pval8_t_`i'}<=0.01) global est8_t_`i' = "***"
			*if (${pval8_t_`i'}=0) global est8_t_`i' = "***"

}

*Placebos: 
foreach i in 1 2 3 4 5{
	glo pgrados8_t_`i': di %5.3f e(N_placebo_`i')-3
	glo pbeta8_t_`i': di %5.3f e(placebo_`i')
	glo pse8_t_`i': di %5.3f e(se_placebo_`i')
	glo pt8_t_`i': di e(placebo_`i')/e(se_placebo_`i')
	glo ppval8_t_`i': di 2*ttail(${pgrados8_t_`i'},abs(${pt8_t_`i'})) 
	glo pest8_t_`i'= "" 
			if (${ppval8_t_`i'}<=0.1) global pest8_t_`i' = "*"
			if (${ppval8_t_`i'}<=0.05) global pest8_t_`i' = "**"
			if (${ppval8_t_`i'}<=0.01) global pest8_t_`i' = "***"
			*if (${ppval8_t_`i'}=0) global pest8_t_`i' = "***"
}


restore



*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVmandounico_by_trust_police2.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Centralization, by security preferences}
tex \label{tab:chaisemartin_sec_preferences}
tex \scalebox{0.7}{    
tex \begin{tabular}{lcccccccc}  
tex \hline \hline       
tex \\ \multicolumn{9}{l}{Dependent variable: Signing Security Cooperation Agreement dummy}\\
tex \addlinespace
tex & \multicolumn{2}{c}{Worried about Narco} & \multicolumn{2}{c}{Worried about insecurity} & \multicolumn{2}{c}{Worried about low punishment} & \multicolumn{2}{c}{Home security expenses}
tex & & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} & \multicolumn{1}{c}{Below mean} & \multicolumn{1}{c}{Above mean} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} & \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} \\ 
tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5} \cmidrule(lrr){6-7} \cmidrule(lrr){8-9}  \\
tex \addlinespace
tex t-5 &        $ ${pbeta_t_4}^{${pest_t_4}} $ &     $ ${pbeta2_t_4}^{${pest2_t_4}} $ &     $ ${pbeta3_t_4}^{${pest3_t_4}} $ &     $ ${pbeta4_t_4}^{${pest4_t_4}} $ &    $ ${pbeta5_t_4}^{${pest5_t_4}} $ &     $ ${pbeta6_t_4}^{${pest6_t_4}} $ &     $ ${pbeta7_t_4}^{${pest7_t_4}} $ &     $ ${pbeta8_t_4}^{${pest8_t_4}} $ \\
tex  & ($ ${pse_t_4}$) & ($ ${pse2_t_4} $) & ($ ${pse3_t_4}$) & ($ ${pse4_t_4} $)  & ($ ${pse5_t_4}$) & ($ ${pse6_t_4} $) & ($ ${pse7_t_4}$) & ($ ${pse8_t_4} $) \\
tex t-4 &        $ ${pbeta_t_3}^{${pest_t_3}} $ &     $ ${pbeta2_t_3}^{${pest2_t_3}} $ &     $ ${pbeta3_t_3}^{${pest3_t_3}} $ &     $ ${pbeta4_t_3}^{${pest4_t_3}} $ &    $ ${pbeta5_t_3}^{${pest5_t_3}} $ &     $ ${pbeta6_t_3}^{${pest6_t_3}} $ &     $ ${pbeta7_t_3}^{${pest7_t_3}} $ &     $ ${pbeta8_t_3}^{${pest8_t_3}} $ \\
tex  & ($ ${pse_t_3}$) & ($ ${pse2_t_3} $) & ($ ${pse3_t_3}$) & ($ ${pse4_t_3} $)  & ($ ${pse5_t_3}$) & ($ ${pse6_t_3} $) & ($ ${pse7_t_3}$) & ($ ${pse8_t_3} $) \\
tex t-3 &        $ ${pbeta_t_2}^{${pest_t_2}} $ &     $ ${pbeta2_t_2}^{${pest2_t_2}} $ &     $ ${pbeta3_t_2}^{${pest3_t_2}} $ &     $ ${pbeta4_t_2}^{${pest4_t_2}} $ &    $ ${pbeta5_t_2}^{${pest5_t_2}} $ &     $ ${pbeta6_t_2}^{${pest6_t_2}} $ &     $ ${pbeta7_t_2}^{${pest7_t_2}} $ &     $ ${pbeta8_t_2}^{${pest8_t_2}} $ \\
tex  & ($ ${pse_t_2}$) & ($ ${pse2_t_2} $) & ($ ${pse3_t_2}$) & ($ ${pse4_t_2} $)  & ($ ${pse5_t_2}$) & ($ ${pse6_t_2} $) & ($ ${pse7_t_2}$) & ($ ${pse8_t_2} $) \\
tex t-2 &        $ ${pbeta_t_1}^{${pest_t_1}} $ &     $ ${pbeta2_t_1}^{${pest2_t_1}} $ &     $ ${pbeta3_t_1}^{${pest3_t_1}} $ &     $ ${pbeta4_t_1}^{${pest4_t_1}} $ &    $ ${pbeta5_t_1}^{${pest5_t_1}} $ &     $ ${pbeta6_t_1}^{${pest6_t_1}} $ &     $ ${pbeta7_t_1}^{${pest7_t_1}} $ &     $ ${pbeta8_t_1}^{${pest8_t_1}} $ \\
tex  & ($ ${pse_t_1}$) & ($ ${pse2_t_1} $) & ($ ${pse3_t_1}$) & ($ ${pse4_t_1} $)  & ($ ${pse5_t_1}$) & ($ ${pse6_t_1} $) & ($ ${pse7_t_1}$) & ($ ${pse8_t_1} $) \\
tex t=0 (Reform) &        $ ${beta_t_0}^{${est_t_0}} $ &     $ ${beta2_t_0}^{${est2_t_0}} $ &     $ ${beta3_t_0}^{${est3_t_0}} $ &     $ ${beta4_t_0}^{${est4_t_0}} $ &    $ ${beta5_t_0}^{${est5_t_0}} $ &     $ ${beta6_t_0}^{${est6_t_0}} $ &     $ ${beta7_t_0}^{${est7_t_0}} $ &     $ ${beta8_t_0}^{${est8_t_0}} $ \\
tex  & ($ ${se_t_0}$) & ($ ${se2_t_0} $) & ($ ${se3_t_0}$) & ($ ${se4_t_0} $)  & ($ ${se5_t_0}$) & ($ ${se6_t_0} $) & ($ ${se7_t_0}$) & ($ ${se8_t_0} $) \\
tex t+1 &        $ ${beta_t_1}^{${est_t_1}} $ &     $ ${beta2_t_1}^{${est2_t_1}} $ &     $ ${beta3_t_1}^{${est3_t_1}} $ &     $ ${beta4_t_1}^{${est4_t_1}} $ &    $ ${beta5_t_1}^{${est5_t_1}} $ &     $ ${beta6_t_1}^{${est6_t_1}} $ &     $ ${beta7_t_1}^{${est7_t_1}} $ &     $ ${beta8_t_1}^{${est8_t_1}} $ \\
tex  & ($ ${se_t_1}$) & ($ ${se2_t_1} $) & ($ ${se3_t_1}$) & ($ ${se4_t_1} $)  & ($ ${se5_t_1}$) & ($ ${se6_t_1} $) & ($ ${se7_t_1}$) & ($ ${se8_t_1} $) \\
tex t+2 &        $ ${beta_t_2}^{${est_t_2}} $ &     $ ${beta2_t_2}^{${est2_t_2}} $ &     $ ${beta3_t_2}^{${est3_t_2}} $ &     $ ${beta4_t_2}^{${est4_t_2}} $ &    $ ${beta5_t_2}^{${est5_t_2}} $ &     $ ${beta6_t_2}^{${est6_t_2}} $ &     $ ${beta7_t_2}^{${est7_t_2}} $ &     $ ${beta8_t_2}^{${est8_t_2}} $ \\
tex  & ($ ${se_t_2}$) & ($ ${se2_t_2} $) & ($ ${se3_t_2}$) & ($ ${se4_t_2} $)  & ($ ${se5_t_2}$) & ($ ${se6_t_2} $) & ($ ${se7_t_2}$) & ($ ${se8_t_2} $) \\


tex \addlinespace
tex Controls$^a$   &    \checkmark      &   \checkmark  &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark   \\
tex 95\% Diff. in CI  &    \checkmark      &   \checkmark  &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark   \\
tex \hline \hline      
tex \multicolumn{9}{p{1.5\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Pre-treatment controls include the following: governor winning margin, mayor winning margin, governor and president party alignment dummies, and logged homicides per capita.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*========================================================================
***3) Interactions

*Set globals: 
	global outcome acuerdo_estcom

************
***A: acuerdo w/o covariates // GOVERNOR ALIGNMENT
************
est clear
foreach i in ap5_4_1_mean{

areg   $outcome c.$saturated##c.`i' $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'1:  di %5.4f e(r2)
		glo N_`i'1: di %11.2gc e(N)	
		

lincom _b[1.lead_3_2015] + _b[1.lead_3_2015#c.`i'] 
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
***B: acuerdo2 w/o covariates // GOVERNOR ALIGNMENT
************
est clear
foreach i in ap5_4_2_mean{

areg   $outcome c.${saturated}##c.`i' $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
		glo r2_`i'2:  di %5.4f e(r2)
		glo N_`i'2: di %11.2gc e(N)	

lincom _b[1.lead_3_2015] + _b[1.lead_3_2015#c.`i'] 
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
***C: acuerdo w/o covariates // FEDERAL ALIGNMENT
************
est clear
foreach i in ap5_4_3_mean{

areg   $outcome c.${saturated}##c.`i' $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
		glo r2_`i'3:  di %5.4f e(r2)
		glo N_`i'3: di %11.2gc e(N)	

lincom _b[1.lead_3_2015] + _b[1.lead_3_2015#c.`i'] 
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
***D: acuerdo2 w/o covariates // FEDERAL ALIGNMENT
************
est clear
foreach i in ap5_4_6_mean{

areg   $outcome c.${saturated}##c.`i' $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
		glo r2_`i'4:  di %5.4f e(r2)
		glo N_`i'4: di %11.2gc e(N)	
		

lincom _b[1.lead_3_2015] + _b[1.lead_3_2015#c.`i'] 
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




texdoc init  "../Tables/abraham_sun_estimates_heteffects_carteles.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect$^a$: Difference by Cartel Presence}
tex \label{tab:abraham_sun_heteffects}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{Agreement A} & \multicolumn{2}{c}{Agreement B$^{b}$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace

tex Reform (t+3)*Dummy Cartel presence &     $ ${beta_hayCarteles1}^{${est_hayCarteles1}} $ &  &  $ ${beta_hayCarteles2}^{${est_hayCarteles2}} $  & \\
tex  &     ($ ${se_hayCarteles1} $) &  & ($ ${se_hayCarteles2} $)  & \\

tex Reform (t+3)*Num. Carteles  &    &   $ ${beta_nCarteles3}^{${est_nCarteles3}} $ &  &  $ ${beta_nCarteles4}^{${est_nCarteles4}} $ \\
tex  &    &     ($ ${se_nCarteles3} $) & & ($ ${se_nCarteles4} $)  \\


tex \\
tex \addlinespace
tex Observations       &        ${N_hayCarteles1}    &        ${N_hayCarteles2}    &     ${N_nCarteles3}      &     ${N_nCarteles4}  \\
tex R-squared       &        ${r2_hayCarteles1}    &        ${r2_hayCarteles2}    &     ${r2_nCarteles3}      &     ${r2_nCarteles4}  \\


tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^c$  &  \checkmark       &       \checkmark  &  \checkmark        &   \checkmark    \\
tex Cohort weighted$^d$  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{5}{p{1\textwidth}}{\footnotesize{Notes:$^a$ Total interaction effect tests the linear hypothesis of the estimated coefficient of alignment with Federal Government indicator (winning margin) + estimated coefficient of the interaction of alignment (winning margin)*lead t=3. This is a post-estimation test using the same specification as that of Table \ref{tab:abraham_sun_lagdv} column (1). Other leads and the indicator at time t=0 when reform came to effect are omitted due to collinearity. Standard errors of linear hypothesis test in parentheses with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided test with the null hypothesis equal to 0. Main regression with standard errors clustered at the state-level. $^b$ Refers to the inverse hyperbolic sine transformation. $^c$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. I also include the lag of the outcome, i.e. logged homicides per capita as control. $^d$ Estimates weighted by each cohort's relative share of the sample following \citet{abraham_sun_2020}.}} \\
tex \end{tabular}
tex } 
tex \end{table} 
texdoc close

*MESSAGE: differential effect with cartel presence



*=========================================================================
*=========================================================================