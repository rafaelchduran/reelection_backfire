*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*Matching  
*****************************************************
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
/*NOTES
IN R:
https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html

Regressions
on A)
-running regression without covariates gives no significance and their is an Ashfelter's dip 

*/
************************************************************************************
****************************MATCHING BY RESHAPING***********************************
************************************************************************************
/* STEPS
1.Use reshape to create a wide format dataset 
2.Format the pretreatment variables to use them in the matching procedure
	**can average variables or keep multiple ones across different years
	**goal is one dataset with one observation per individual 
3.Using this dataset, perform the matching procedure with psmatch2
4.Merge the info from the matched cases with the original dataset. 
	**drop cases that are not matched

*/
*1. RESHAPE TO WIDE
*========================================================================
*LOAD DATA
use "../../Data/ConstructionDatabase/data_final.dta", clear


preserve
/*
/*global control_matching winning_margin_governor governor_alignment ///
		 effectiveparties  distEntradasPrinc  hayCarteles nCarteles  defunciones pop logdefuncionespc ///
			arma_corta_2 arma_larga_2 cartuchos_2 cocaina_kg_2 heroina_kg_2 mariguana_kg_2 metanfetamina_kg_2 amapola_kghec_2 mariguana_kghec_2 granadas_2 laboratorio_2 pistas_2 vehiculo_aereo_2 vehiculo_lacustre_2 cocaina_kgperkm2_2 heroina_kgperkm2_2 drugs_2
*/
global control_matching winning_margin_governor governor_alignment ///
		 effectiveparties  distEntradasPrinc  hayCarteles nCarteles  defunciones pop logdefuncionespc 
			*arma_corta_2 arma_larga_2 cartuchos_2 cocaina_kg_2 heroina_kg_2 mariguana_kg_2 metanfetamina_kg_2 amapola_kghec_2 mariguana_kghec_2 granadas_2 laboratorio_2 pistas_2 vehiculo_aereo_2 vehiculo_lacustre_2 cocaina_kgperkm2_2 heroina_kgperkm2_2 drugs_2

*keep year rel_year estado nombre_estado municipio nombre_municipio inegi $control_matching
keep  rel_year reform estado nombre_estado municipio nombre_municipio inegi $control_matching

drop if rel_year==.
replace rel_year=1 if rel_year==-8
replace rel_year=2 if rel_year==-7
replace rel_year=3 if rel_year==-6
replace rel_year=4 if rel_year==-5
replace rel_year=5 if rel_year==-4
replace rel_year=6 if rel_year==-3
replace rel_year=7 if rel_year==-2
replace rel_year=8 if rel_year==-1
replace rel_year=9 if rel_year==0
replace rel_year=10 if rel_year==1
replace rel_year=11 if rel_year==2
replace rel_year=12 if rel_year==3

reshape wide reform winning_margin_governor governor_alignment ///
		 effectiveparties  distEntradasPrinc  hayCarteles nCarteles  defunciones pop logdefuncionespc ///
, i(inegi) j(rel_year)

/*reshape wide reform winning_margin_governor governor_alignment ///
		 effectiveparties  distEntradasPrinc  hayCarteles nCarteles  defunciones pop logdefuncionespc ///
			arma_corta_2 arma_larga_2 cartuchos_2 cocaina_kg_2 heroina_kg_2 mariguana_kg_2 metanfetamina_kg_2 amapola_kghec_2 mariguana_kghec_2 granadas_2 laboratorio_2 pistas_2 vehiculo_aereo_2 vehiculo_lacustre_2 cocaina_kgperkm2_2 heroina_kgperkm2_2 drugs_2 ///
, i(inegi) j(rel_year)
*/

*2) transform variables 
foreach i in $control_matching{
egen p_`i'=rowmean(`i'4  `i'5  `i'6  `i'7)  

}

order inegi reform4 reform5 reform6 reform7 reform8 reform9 reform10 reform11 reform12
*/		
*3) propensity score matching 

*a) estimate the propensity score model
*global controls4 winning_margin_governor4 effectiveparties4 governor_alignment4 distEntradasPrinc4 hayCarteles4 nCarteles4 pop4 arma_corta_24 arma_larga_24 cartuchos_24 cocaina_kg_24 heroina_kg_24 mariguana_kg_24 metanfetamina_kg_24 amapola_kghec_24 mariguana_kghec_24 granadas_24 laboratorio_24 pistas_24 vehiculo_aereo_24 vehiculo_lacustre_24 cocaina_kgperkm2_24 logdefuncionespc4
*global controls5 winning_margin_governor5 effectiveparties5 governor_alignment5 distEntradasPrinc5 hayCarteles5 nCarteles5 pop5 arma_corta_25 arma_larga_25 cartuchos_25 cocaina_kg_25 heroina_kg_25 mariguana_kg_25 metanfetamina_kg_25 amapola_kghec_25 mariguana_kghec_25 granadas_25 laboratorio_25 pistas_25 vehiculo_aereo_25 vehiculo_lacustre_25 cocaina_kgperkm2_25 logdefuncionespc5
*global controls6 winning_margin_governor6 effectiveparties6 governor_alignment6 distEntradasPrinc6 hayCarteles6 nCarteles6 pop6 arma_corta_26 arma_larga_26 cartuchos_26 cocaina_kg_26 heroina_kg_26 mariguana_kg_26 metanfetamina_kg_26 amapola_kghec_26 mariguana_kghec_26 granadas_26 laboratorio_26 pistas_26 vehiculo_aereo_26 vehiculo_lacustre_26 cocaina_kgperkm2_26 logdefuncionespc6
*global controls7 winning_margin_governor7 effectiveparties7 governor_alignment7 distEntradasPrinc7 hayCarteles7 nCarteles7 pop7 arma_corta_27 arma_larga_27 cartuchos_27 cocaina_kg_27 heroina_kg_27 mariguana_kg_27 metanfetamina_kg_27 amapola_kghec_27 mariguana_kghec_27 granadas_27 laboratorio_27 pistas_27 vehiculo_aereo_27 vehiculo_lacustre_27 cocaina_kgperkm2_27 logdefuncionespc7
*global pre p_winning_margin_governor p_governor_alignment p_effectiveparties p_distEntradasPrinc p_hayCarteles p_nCarteles p_defunciones p_pop p_logdefuncionespc p_arma_corta_2 p_arma_larga_2 p_cartuchos_2 p_cocaina_kg_2 p_heroina_kg_2 p_mariguana_kg_2 p_metanfetamina_kg_2 p_amapola_kghec_2 p_mariguana_kghec_2 p_granadas_2 p_laboratorio_2 p_pistas_2 p_vehiculo_aereo_2 p_vehiculo_lacustre_2 p_cocaina_kgperkm2_2 p_heroina_kgperkm2_2 p_drugs_2
*global pre p_winning_margin_governor p_governor_alignment p_effectiveparties p_distEntradasPrinc p_hayCarteles p_nCarteles p_defunciones p_pop p_logdefuncionespc 
*p_arma_corta_2 p_arma_larga_2 p_cartuchos_2 p_cocaina_kg_2 p_heroina_kg_2 p_mariguana_kg_2 p_metanfetamina_kg_2 p_amapola_kghec_2 p_mariguana_kghec_2 p_granadas_2 p_laboratorio_2 p_pistas_2 p_vehiculo_aereo_2 p_vehiculo_lacustre_2 p_cocaina_kgperkm2_2 p_heroina_kgperkm2_2 p_drugs_2
global controls_mean logdefuncionespc_mean winning_margin_governor_mean  ///
	alignment_governor_strong_mean winning_margin_mean ///
	pan_mayor2_mean pri_mayor2_mean hayCarteles_mean acuerdo5_mean
	
	
*logit reform12  $pre
logit reform  $controls_mean

*b) estimate the propensity 
predict pscore1, pr

*c) implement propensity score matching with psmatch2
**nearest neighbor with caliper, replacement and common support
**caliper is set to 0.25 standard deviations of the ps
**so, we first need to estimate the standard deviation of the ps

sum pscore1
scalar cal=r(sd)*0.2 // caliper = 1/5 standard deviations of the ps

*d) psm using psmatch2
*global outcome logdefuncionespc12
global outcome acuerdo_estcom

	*i) naive psm
	*psmatch2 reform12, pscore(pscore1) outcome($outcome) common caliper(`=scalar(cal)')
	psmatch2 reform, pscore(pscore1) outcome($outcome) common caliper(`=scalar(cal)')

	/*ii) within psm
	gen weight=.
	gen att=.

	sum pscore1
	scalar cal=r(sd)*0.2 // caliper = 1/5 standard deviations of the ps

	egen c=group(estado)
	levels c, local(cluster)

	quietly foreach j of local cluster{
	psmatch2 reform12 if c==`j', pscore(pscore1) outcome($outcome) caliper (`=scalar(cal)')
	replace weight=_weight if c==`j'
	replace att = r(att) if c==`j'
	}
	*/

	*keep inegi p_winning_margin_governor-_pdif
	keep inegi _weight

save "../../Data/ConstructionDatabase/weights_psmatching.dta", replace
restore 

drop _merge
merge m:m inegi using "../../Data/ConstructionDatabase/weights_psmatching.dta"
drop if _merge==2
drop _merge

*5) ESTIMATE WITHING PSM: Model based ATT with SE adjusted for clustering
	global outcome acuerdo_estcom
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

areg  $outcome  $saturated   i.year [fweight=_weight] if _weight!=., a(inegi) vce(cluster estado)

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
	lincom 	((_b[`i'_2017]*`a') + (_b[`i'_2018]*`b'))
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
	lincom 	((_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b'))
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




 

************************************************************************************
************************************************************************************
************************************************************************************
*A) Pretreatment controls to "control for heterogeneity"
use "../../Data/ConstructionDatabase/data_final.dta", clear
	global outcome logdefuncionespc
	*global outcome ihs_defuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	xtset inegi year 
	
	global narco2 ap4_2_3
	global punishment2 ap4_2_11
	global money2 ap4_12b
	global police2 ap5_4_2_b
	global army2 ap5_4_8_b
	global citizens2  $narco2 $punishment2 $money2 $police2 $army2
	
*generate pretreatment violence
foreach i in logdefuncionespc winning_margin_governor winning_margin governor_alignment $citizens2{  
bysort inegi: egen p_`i'=mean(`i') if rel_year<0
bysort inegi: replace p_`i'=p_`i'[_n-1] if p_`i'==.
sort inegi rel_year
bysort inegi: replace p_`i'=p_`i'[_n-1] if p_`i'==.
}


foreach var in p_logdefuncionespc p_winning_margin_governor p_winning_margin p_governor_alignment p_ap4_2_3 p_ap4_2_11 p_ap4_12b p_ap5_4_2_b p_ap5_4_8_b{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

	global controls p_logdefuncionespc p_winning_margin_governor  p_winning_margin p_governor_alignment p_ap4_2_3 p_ap4_2_11
	*global controls p_logdefuncionespc
	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3	 logdefuncionespc_lag_2
	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 
	/*global controls p_ap4_2_3_lag_8 p_ap4_2_3_lag_7 p_ap4_2_3_lag_6 p_ap4_2_3_lag_5 p_ap4_2_3_lag_4 p_ap4_2_3_lag_3 p_ap4_2_3_lag_2 p_ap4_2_3_date0 p_ap4_2_3_lead_1 p_ap4_2_3_lead_2 p_ap4_2_3_lead_3 ///
	p_ap4_2_11_lag_8 p_ap4_2_11_lag_7 p_ap4_2_11_lag_6 p_ap4_2_11_lag_5 p_ap4_2_11_lag_4 p_ap4_2_11_lag_3 p_ap4_2_11_lag_2 p_ap4_2_11_date0 p_ap4_2_11_lead_1 p_ap4_2_11_lead_2 p_ap4_2_11_lead_3 ///
	p_ap4_12b_lag_8 p_ap4_12b_lag_7 p_ap4_12b_lag_6 p_ap4_12b_lag_5 p_ap4_12b_lag_4 p_ap4_12b_lag_3 p_ap4_12b_lag_2 p_ap4_12b_date0 p_ap4_12b_lead_1 p_ap4_12b_lead_2 p_ap4_12b_lead_3 ///
	p_ap5_4_2_b_lag_8 p_ap5_4_2_b_lag_7 p_ap5_4_2_b_lag_6 p_ap5_4_2_b_lag_5 p_ap5_4_2_b_lag_4 p_ap5_4_2_b_lag_3 p_ap5_4_2_b_lag_2 p_ap5_4_2_b_date0 p_ap5_4_2_b_lead_1 p_ap5_4_2_b_lead_2 p_ap5_4_2_b_lead_3 ///
	p_ap5_4_8_b_lag_8 p_ap5_4_8_b_lag_7 p_ap5_4_8_b_lag_6 p_ap5_4_8_b_lag_5 p_ap5_4_8_b_lag_4 p_ap5_4_8_b_lag_3 p_ap5_4_8_b_lag_2 p_ap5_4_8_b_date0 p_ap5_4_8_b_lead_1 p_ap5_4_8_b_lead_2 p_ap5_4_8_b_lead_3 ///
	p_logdefuncionespc_lag_8 p_logdefuncionespc_lag_7 p_logdefuncionespc_lag_6 p_logdefuncionespc_lag_5 p_logdefuncionespc_lag_4 p_logdefuncionespc_lag_3 p_logdefuncionespc_lag_2 p_logdefuncionespc_date0 p_logdefuncionespc_lead_1 p_logdefuncionespc_lead_2 p_logdefuncionespc_lead_3 ///
	p_winning_margin_governor_lag_8 p_winning_margin_governor_lag_7 p_winning_margin_governor_lag_6 p_winning_margin_governor_lag_5 p_winning_margin_governor_lag_4 p_winning_margin_governor_lag_3 p_winning_margin_governor_lag_2 p_winning_margin_governor_date0 p_winning_margin_governor_lead_1 p_winning_margin_governor_lead_2 p_winning_margin_governor_lead_3 ///
	p_winning_margin_lag_8 p_winning_margin_lag_7 p_winning_margin_lag_6 p_winning_margin_lag_5 p_winning_margin_lag_4 p_winning_margin_lag_3 p_winning_margin_lag_2 p_winning_margin_date0 p_winning_margin_lead_1 p_winning_margin_lead_2 p_winning_margin_lead_3 ///
	p_governor_alignment_lag_8 p_governor_alignment_lag_7 p_governor_alignment_lag_6 p_governor_alignment_lag_5 p_governor_alignment_lag_4 p_governor_alignment_lag_3 p_governor_alignment_lag_2 p_governor_alignment_date0 p_governor_alignment_lead_1 p_governor_alignment_lead_2 p_governor_alignment_lead_3

	global controls p_ap4_2_3_lag_8 p_ap4_2_3_lag_7 p_ap4_2_3_lag_6 p_ap4_2_3_lag_5 p_ap4_2_3_lag_4 p_ap4_2_3_lag_3 p_ap4_2_3_lag_2 ///
	p_ap4_2_11_lag_8 p_ap4_2_11_lag_7 p_ap4_2_11_lag_6 p_ap4_2_11_lag_5 p_ap4_2_11_lag_4 p_ap4_2_11_lag_3 p_ap4_2_11_lag_2 ///
	p_ap4_12b_lag_8 p_ap4_12b_lag_7 p_ap4_12b_lag_6 p_ap4_12b_lag_5 p_ap4_12b_lag_4 p_ap4_12b_lag_3 p_ap4_12b_lag_2  ///
	p_ap5_4_2_b_lag_8 p_ap5_4_2_b_lag_7 p_ap5_4_2_b_lag_6 p_ap5_4_2_b_lag_5 p_ap5_4_2_b_lag_4 p_ap5_4_2_b_lag_3 p_ap5_4_2_b_lag_2  ///
	p_ap5_4_8_b_lag_8 p_ap5_4_8_b_lag_7 p_ap5_4_8_b_lag_6 p_ap5_4_8_b_lag_5 p_ap5_4_8_b_lag_4 p_ap5_4_8_b_lag_3 p_ap5_4_8_b_lag_2  ///
	p_logdefuncionespc_lag_8 p_logdefuncionespc_lag_7 p_logdefuncionespc_lag_6 p_logdefuncionespc_lag_5 p_logdefuncionespc_lag_4 p_logdefuncionespc_lag_3 p_logdefuncionespc_lag_2  ///
	p_winning_margin_governor_lag_8 p_winning_margin_governor_lag_7 p_winning_margin_governor_lag_6 p_winning_margin_governor_lag_5 p_winning_margin_governor_lag_4 p_winning_margin_governor_lag_3 p_winning_margin_governor_lag_2   ///
	p_winning_margin_lag_8 p_winning_margin_lag_7 p_winning_margin_lag_6 p_winning_margin_lag_5 p_winning_margin_lag_4 p_winning_margin_lag_3 p_winning_margin_lag_2   ///
	p_governor_alignment_lag_8 p_governor_alignment_lag_7 p_governor_alignment_lag_6 p_governor_alignment_lag_5 p_governor_alignment_lag_4 p_governor_alignment_lag_3 p_governor_alignment_lag_2 
	*/
	
est clear
*areg  $outcome  $saturated i.year i.year#i.estado, a(inegi) vce(cluster inegi)

*areg  $outcome  $saturated $controls i.year i.year#i.estado, a(inegi) vce(cluster estado)
*areg  $outcome  $saturated i.year i.year#i.estado , a(inegi) vce(cluster estado)
areg  $outcome  $saturated i.year i.year##c.$controls i.year#i.estado , a(inegi) vce(cluster estado)

	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)
		
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
	lincom 	((_b[`i'_2017]*`a') + (_b[`i'_2018]*`b'))
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
	lincom 	((_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b'))
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

		
		
		
		
		

		
		
******************************************		
******************************************		
******************************************		

***estimate linear combination by lead/lag:

foreach i in lag_7{
*areg  $outcome  $saturated $controls i.year , a(inegi) vce(cluster estado)
areg  $outcome  $saturated i.year i.year#c.$controls , a(inegi) vce(cluster estado)

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
	lincom 	((_b[`i'_2017]*`a') + (_b[`i'_2018]*`b'))
	glo se_`i'_log: di %5.4f r(se)
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')

}
	
foreach i in lag_6{
*areg  $outcome  $saturated $controls i.year , a(inegi) vce(cluster estado)
areg  $outcome  $saturated i.year i.year#c.$controls , a(inegi) vce(cluster estado)

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
	lincom 	((_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c'))
	glo se_`i'_log: di %5.4f r(se)
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')

}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
*areg  $outcome  $saturated $controls i.year , a(inegi) vce(cluster estado)
areg  $outcome  $saturated i.year i.year#c.$controls , a(inegi) vce(cluster estado)

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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d'))
	glo se_`i'_log: di %5.4f r(se)
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')

}


foreach i in lead_1{
*areg  $outcome  $saturated $controls i.year , a(inegi) vce(cluster estado)
areg  $outcome  $saturated i.year i.year#c.$controls , a(inegi) vce(cluster estado)

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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c'))
	glo se_`i'_log: di %5.4f r(se)
	   eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')

}


foreach i in lead_2{
*areg  $outcome  $saturated $controls i.year , a(inegi) vce(cluster estado)
areg  $outcome  $saturated i.year i.year#c.$controls , a(inegi) vce(cluster estado)

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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b'))
	glo se_`i'_log: di %5.4f r(se)
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')

}


foreach i in lead_3{
*areg  $outcome  $saturated $controls i.year , a(inegi) vce(cluster estado)
areg  $outcome  $saturated i.year i.year#c.$controls , a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')

}

*Figure
			   
coefplot (est1, rename((1) = t-7) mcolor(red) ciopts(color(black))) ///
 (est2, rename((1) = t-6) mcolor(red) ciopts(color(black))) ///
 (est3, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
 (est4, rename((1) = t-4) mcolor(red) ciopts(color(black))) ///
 (est5, rename((1) = t-3) mcolor(red) ciopts(color(black))) ///
 (est6, rename((1) = t-2) mcolor(red) ciopts(color(black))) ///
 (est7, rename((1) = Reform (t=0)) mcolor(green) ciopts(color(black))) ///
 (est8, rename((1) = t+1) mcolor(green) ciopts(color(black))) ///
 (est9, rename((1) = t+2) mcolor(green) ciopts(color(black))) ///
 (est10, rename((1) = t+3) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color) legend(off) yline(0)    ///
ytitle("estimate: log(homicides per capita)") 


name(loghomicides)

graph export "../Figures/event_study_log.png", as(png) replace
graph export "../Figures/event_study_log.pdf", as(pdf) replace
graph export "../Figures/event_study_log.tif", as(tif) replace
graph save "../Figures/event_study_log.gph", replace


*RECOVER THE WHOLE EFFECT>
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
	lincom 	(((_b[date_0_2015]*`a')+(_b[date_0_2016]*`b')+(_b[date_0_2017]*`c') + (_b[date_0_2018]*`d'))+ ///
			((_b[lead_1_2015]*`e')+(_b[lead_1_2016]*`f')+(_b[lead_1_2017]*`g')) + ///
			((_b[lead_2_2015]*`h')+(_b[lead_2_2016]*`i')) + ///
			((_b[lead_3_2015]*`j')))/4
	glo se_all_log: di %5.4f r(se)



*B) Matching with psmatch2 (pstest2 psgraph)
/*
psmatch2 makes it easy by creating a _weight variable automatically. For observations in the treated group, 
_weight is 1. For observations in the control group it is the number of observations from the treated group 
for which the observation is a match. If the observation is not a match, _weight is missing. _weight thus acts 
as a frequency weight (fweight) and can be used with Stata's standard weighting syntax.

Observations with a missing value for _weight are omitted from the regression, so it is automatically limited 
to the matched sample. Again, keep in mind that the standard errors given by the reg command are incorrect because 
they do not take into account the matching stage.

*/
use "../../Data/ConstructionDatabase/data_final.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 	
	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 	logdefuncionespc_lag_8 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3	
	*global controls_matching winning_margin_governor logdefuncionespc governor_alignment
	global narco2 ap4_2_3
	global punishment2 ap4_2_11
	global money2 ap4_12b
	global police2 ap5_4_2_b
	global army2 ap5_4_8_b
	global citizens2  $narco2 $punishment2 $money2 $police2 $army2  
	
	global controls_matching logdefuncionespc
	*global controls_matching  winning_margin_governor governor_alignment 
		*global controls_matching   effectiveparties  winning_margin_governor ///
		  *distEntradasPrinc  hayCarteles nCarteles  defunciones pop  
		 *arma_corta arma_larga cartuchos cocaina_kg heroina_kg mariguana_kg metanfetamina_kg amapola_kghec mariguana_kghec granadas laboratorio vehiculo_lacustre vehiculo_aereo
		 *arma_corta_2 arma_larga_2 cartuchos_2 cocaina_kg_2 heroina_kg_2  mariguana_kg_2 metanfetamina_kg_2 amapola_kghec_2 mariguana_kghec_2 granadas_2 laboratorio_2 pistas_2 vehiculo_aereo_2 vehiculo_lacustre_2

	global outcome logdefuncionespc
		*global outcome ihs_defuncionespc

    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc
	xtset inegi year 
	
	global controls_matching $controls_mean
	
	global controls logdefuncionespc_mean_y_1 logdefuncionespc_mean_y_2 logdefuncionespc_mean_y_3 logdefuncionespc_mean_y_4 logdefuncionespc_mean_y_5 logdefuncionespc_mean_y_6 logdefuncionespc_mean_y_7 logdefuncionespc_mean_y_8 ///
	align_gov_y_1 align_gov_y_2 align_gov_y_3 align_gov_y_4 align_gov_y_5 align_gov_y_6 align_gov_y_7 align_gov_y_8 ///
	margin_gov_y_1 margin_gov_y_2 margin_gov_y_3 margin_gov_y_4 margin_gov_y_5 margin_gov_y_6 margin_gov_y_7 margin_gov_y_8 ///
	hayCarteles_y_1 hayCarteles_y_2 hayCarteles_y_3 hayCarteles_y_4 hayCarteles_y_5 hayCarteles_y_6 hayCarteles_y_7 hayCarteles_y_8 ///
	pri_mayor2_y_1 pri_mayor2_y_2 pri_mayor2_y_3 pri_mayor2_y_4 pri_mayor2_y_5 pri_mayor2_y_6 pri_mayor2_y_7 pri_mayor2_y_8 ///
	pan_mayor2_y_1 pan_mayor2_y_2 pan_mayor2_y_3 pan_mayor2_y_4 pan_mayor2_y_5 pan_mayor2_y_6 pan_mayor2_y_7 pan_mayor2_y_8 ///
	winning_margin_mean_y_1 winning_margin_mean_y_2 winning_margin_mean_y_3 winning_margin_mean_y_4 winning_margin_mean_y_5 winning_margin_mean_y_6 winning_margin_mean_y_7 winning_margin_mean_y_8 


************************************
**NAIVE PSM: DON'T CONSIDER CLUSTERING
************************************
	
*1) estimate the propensity score model
logit reform  $controls_matching 

*2) estimate the propensity 
predict pscore1, pr

*3) implement propensity score matching with psmatch2
**nearest neighbor with caliper, replacement and common support
**caliper is set to 0.25 standard deviations of the ps
**so, we first need to estimate the standard deviation of the ps

sum pscore1
scalar cal=r(sd)*0.2 // caliper = 1/4 standard deviastion of the ps

*4) psm using psmatch2
psmatch2 reform, pscore(pscore1) outcome($outcome) common caliper(`=scalar(cal)')

*5) model based ATT estimate (as above) and cluster 
est clear
areg  $outcome  $saturated $controls  i.year [fweight=_weight], a(inegi) vce(cluster estado)
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
	lincom 	((_b[`i'_2017]*`a') + (_b[`i'_2018]*`b'))
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
	lincom 	((_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b'))
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

*6) balance check
pstest $controls_matching,sum both
pstest $controls_matching, both graph

************************************
**WITHIN PSM: TO ADJUST CLUSTERING
************************************

gen weight=.
gen att=.

egen c=group(estado)
levels c, local(cluster)
global cluster_num 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
quietly foreach j in $cluster_num{
cap psmatch2 reform if c==`j', pscore(pscore1) outcome($outcome) caliper (`=scalar(cal)')
	replace weight=_weight if c==`j'
	replace att = r(att) if c==`j'
}

*2) estimate ATT (ignoring clustering)
sum att

*3) Model based ATT with SE adjusted for clusteringest clear
areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)
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
	lincom 	((_b[`i'_2017]*`a') + (_b[`i'_2018]*`b'))
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
	lincom 	((_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b'))
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


*4) balance before matching
pstest $controls_matching, treated(reform) raw graph

*5) Balance after matching
pstest $controls_matching if weight!=., treated(reform) mweight(weight) raw graph
pstest $controls_matching if weight!=.,sum both
pstest $controls_matching, both graph


*6) number of proportion of unmatched by cluster
gen unmatched=(weight==.)

table estado if reform==1, c(rawsum unmatched n reform mean unmatched)

/*
----------------------------------------------------------------
    State | rawsum(unmatc~d)         N(reform)    mean(unmatc~d)
----------+-----------------------------------------------------
        1 |                0                33                 0
        2 |                6                15                .4
        3 |                1                20               .05
        4 |                6                40               .15
        5 |               14                76          .1842105
        6 |                5                40              .125
        7 |               28               436          .0642202
        8 |               19               201          .0945274
        9 |               16                16                 1
       10 |               27               117          .2307692
       11 |               14               184           .076087
       12 |               17               320           .053125
       14 |                7               500              .014
       15 |               40               500               .08
       16 |               54               452           .119469
       17 |                8               132          .0606061
       19 |               27               204          .1323529
       20 |             1144             1,587           .720857
       21 |               21               213          .0985916
       22 |                1                72          .0138889
       23 |                4                24          .1666667
       24 |                1               232          .0043103
       25 |                6                54          .1111111
       26 |                9                69          .1304348
       27 |               14                68          .2058824
       28 |               21               129          .1627907
       31 |                5               344          .0145349
       32 |                7               174          .0402299




*/

************************************
**PREFERENTIAL WITHIN PSM: TO ADJUST CLUSTERING, BUT ALLOW TO IDENTIFY MATCHES IN OTHER CLUSTERS
************************************
*1) balance before matching
pstest $controls_matching, treated(reform) raw graph

gen weight_pw=.

*egen c=group(estado)
levels c, local(cluster)

foreach j in  3 4 5 6 7 8  11 12  14 15 16 17  19  21 22 23 24  26 27 28   31 32{
psmatch2 reform if c==`j', pscore(pscore1) outcome($outcome) caliper (`=scalar(cal)')
	replace weight_pw=_weight if c==`j'
}

*2) new matching

psmatch2 reform if ((weight_pw==. & reform==1) | (reform==0)), pscore(pscore1) outcome($outcome) caliper (`=scalar(cal)')
replace weight_pw=weight_pw + _weight if (weight_pw!=. & _weight!=.) // for conrol units that were alredy used in the within
replace weight_pw = _weight if weight_pw==. // both treated and control taht were unmatched in within

*3) Model based ATT with SE adjusted for clustering
est clear
reghdfe  $outcome  $saturated i.year [fweight=weight_pw] if weight_pw!=., a(inegi) vce(cluster estado)
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
	lincom 	((_b[`i'_2017]*`a') + (_b[`i'_2018]*`b'))
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
	lincom 	((_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b'))
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

*4) balance before matching
pstest $controls_matching, treated(reform) raw graph

*5) Balance after matching
pstest $controls_matching if weight_pw!=., treated(reform) mweight(weight_pw) raw graph

*6) number of proportion of unmatched by cluster
gen unmatched_pw=(weight_pw==.)

table estado if reform==1, c(rawsum unmatched_pw n reform mean unmatched_pw)

/*
----------------------------------------------------------------
    State | rawsum(unmatc~w)         N(reform)    mean(unmatc~w)
----------+-----------------------------------------------------
        1 |                0                33                 0
        2 |                6                15                .4
        3 |                1                20               .05
        4 |                6                40               .15
        5 |               14                76          .1842105
        6 |                5                40              .125
        7 |               28               436          .0642202
        8 |               19               201          .0945274
        9 |               16                16                 1
       10 |               27               117          .2307692
       11 |               14               184           .076087
       12 |               17               320           .053125
       14 |                7               500              .014
       15 |               40               500               .08
       16 |               54               452           .119469
       17 |                8               132          .0606061
       19 |               27               204          .1323529
       20 |             1144             1,587           .720857
       21 |               21               213          .0985916
       22 |                1                72          .0138889
       23 |                4                24          .1666667
       24 |                1               232          .0043103
       25 |                6                54          .1111111
       26 |                9                69          .1304348
       27 |               14                68          .2058824
       28 |               21               129          .1627907
       31 |                5               344          .0145349
       32 |                7               174          .0402299
----------------------------------------------------------------


*/

************* THE SAME AS ABOVE BUT USING OPTION TIES **************************

gen weight_pw2 = . 

* egen c = group(schid)       
levels c, local(cluster)

 quietly foreach j of local cluster {
	psmatch2 reform if  c==`j', pscore(pscore1) outcome($outcome) caliper(`=scalar(cal)') ties
		replace weight_pw2 = _weight if  c==`j'
 }

psmatch2 reform if ((weight_pw2==. & reform==1) | (reform==0)), pscore(pscore1) outcome($outcome) caliper(`=scalar(cal)') ties
* ties uses all matched controls with the same value of the ps

replace weight_pw2 = weight_pw2 + _weight if (weight_pw2!=. & _weight!=.) // for control units that were already used in the within
replace weight_pw2 = _weight if weight_pw2 ==. // both treated and control that were unmatched in within
 
  
* Model based ATT with se adjusted for clustering
gen inverse_weight=1/weight_pw2
est clear
reghdfe  $outcome  $saturated  [pweight=inverse_weight] if weight_pw2!=., a(inegi year) vce(cluster estado)
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
	lincom 	((_b[`i'_2017]*`a') + (_b[`i'_2018]*`b'))
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
	lincom 	((_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c'))
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
	lincom 	((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b'))
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


* with ties weight may be noninteger

* Balance after matching
pstest $controls_matching if weight_pw2!=., treated(reform) mweight(weight_pw2) raw graph
	*Note: mweight is need to get appropriate estimates in case some obs are used more than once
	* raw here actually refers to the matched dataset (weight2!=.)

* number and proportion of unmatched by cluster
gen unmatched_pw2 =(weight_pw2==.)

table estado if reform==1, c(rawsum unmatched_pw2 n reform mean unmatched_pw2)
* number unmatched / number of treated / % unmatched	





**********************************************
*Table: Mechanisms: INCUMBENCY ADVANTAGE WITH POLYNOMIALS
**********************************************
use "../../Data/ConstructionDatabase/data_wleads&lags_incumbency_weights.dta", clear

*GLOBALS: treatment
capture global saturated date_0_2015 date_0_2016 date_0_2017 date_0_2018 lag_2_2016 lag_3_2015 lag_3_2016 lag_3_2018 lag_4_2015 lag_4_2017 lag_5_2015 lag_5_2018 lag_6_2016 lag_6_2018 lag_8_2018 lead_1_2015 lead_1_2016
global outcome incumbent_yesterday_w_tomorrow2
*GLOBALS: controls
global controls_matching logdefuncionespc winning_margin_governor governor_alignment

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


*1) estimate the propensity score model
logit reform  $controls_matching

*** HERE IS WHERE I NEED TO THINK HOW TO CONTROL FOR PRE

*2) estimate the propensity 
predict pscore1, pr

*3) implement propensity score matching with psmatch2
**nearest neighbor with caliper, replacement and common support
**caliper is set to 0.25 standard deviations of the ps
**so, we first need to estimate the standard deviation of the ps

sum pscore1
scalar cal=r(sd)*0.25 // caliper = 1/4 standard deviastion of the ps

*4) psm using psmatch2
rename incumbent_yesterday_w_tomorrow2 incumbent_psm
global outcome incumbent_psm

psmatch2 reform, pscore(pscore1) outcome($outcome) common caliper (`=scalar(cal)')

*5) model based ATT estimate (as above) and cluster **WITHIN PSM: TO ADJUST CLUSTERING

gen weight=.
gen att=.

egen c=group(estado)
levels c, local(cluster)

quietly foreach j of local cluster{
psmatch2 reform if c==`j', pscore(pscore1) outcome($outcome) caliper (`=scalar(cal)')
	replace weight=_weight if c==`j'
	replace att = r(att) if c==`j'
}

*2) estimate ATT (ignoring clustering)
sum att

*3) Model based ATT with SE adjusted for clusteringest clear

foreach j in $outcome {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 

est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2   i.year [fweight=weight] if mv_incparty<${optimal} & mv_incparty>-${optimal} & weight!=., a(inegi) vce(cluster estado)

		glo r2_ihsdet_`pol':  di %5.4f e(r2)
		glo N_ihsdet_`pol': di %11.2gc e(N)

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

*get the partisan and personal effect
/*****
test on partisan incumbency advantage
_b[reform] = difference between reelection and term limited elections: partisan(A) + personal(B) = A+B = .0714323 (sig 10%)
_b[_cons] = inc. advantage for term limited elections: partisan inc. advantage = A =  .1156307   (sig 5%)
_b[reform]-b[_cons]  = A+B-A=B=personal incumbency advantage =  -.0441984  (no sig but close to 10%)
*****/
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
	*personal incumbency advantage
	lincom 	(((_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d'))/2) -(_b[_cons])/2 
	*partisan incumbency advantage
	lincom (_b[_cons])/2
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


*4) balance before matching
pstest $controls_matching, treated(reform) raw graph

*5) Balance after matching
pstest $controls_matching if weight!=., treated(reform) mweight(weight) raw graph

*6) number of proportion of unmatched by cluster
gen unmatched=(weight==.)

table estado if reform==1, c(rawsum unmatched n reform mean unmatched)

/*
			number unmatched.  number of treated.  %unmatched
----------------------------------------------------------------
    State | rawsum(unmatc~d)         N(reform)    mean(unmatc~d)
----------+-----------------------------------------------------
        1 |               15                22          .6818182
        2 |                5                10                .5
        3 |                7                10                .7
        4 |               12                20                .6
        5 |               53                76          .6973684
        6 |               11                20               .55
        7 |              139               214          .6495327
        8 |               75               134          .5597015
        9 |               16                16                 1
       10 |               48                78          .6153846
       11 |               50                92          .5434783
       12 |               90               161          .5590062
       14 |              151               250              .604
       15 |              139               250              .556
       16 |              132               221          .5972851
       17 |               35                66           .530303
       19 |               69               101          .6831683
       20 |              217               303          .7161716
       21 |              211               214          .9859813
       22 |               23                36          .6388889
       23 |                9                16             .5625
       24 |               78               116          .6724138
       25 |               18                36                .5
       26 |               72                72                 1
       27 |               16                33          .4848485
       28 |               48                86          .5581396
       31 |              192               212          .9056604
       32 |               70               115          .6086956
----------------------------------------------------------------

*/
