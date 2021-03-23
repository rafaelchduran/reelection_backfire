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
	*global controls winning_margin_governor  governor_alignment pri_mayor2 morena_mayor2 
	global controls_pre winning_margin_governor_pre governor_alignment_pre logdefuncionespc_pre  winning_margin_pre alignment_executive_strong_pre $citizens3

*NOTE: similar results using pre-treatment controls

*========================================================================
*SET MATSIZE
set matsize 11000 

*========================================================================
***0) TWFE model - Effect of decentralization in public good provision
sort inegi year

est clear
foreach outcome in logdetenidospc logdetenidos_2pc logheroina_kg logheroina_kg_2 logmetanfetamina_kg logmetanfetamina_kg_2 loglaboratorio loglaboratorio_2{
foreach treatment in  acuerdo2{
eststo: quietly xi: areg `outcome' `treatment' $controls_pre i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l.`treatment' $controls_pre i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l2.`treatment' $controls_pre i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}

esttab est*, keep( acuerdo2) t star(* 0.1 ** 0.05 *** 0.01)

*MESSAGE: there are more local police forces detentions. The military doesn't do much.
*MESSAGE2: the effect is contemporary


*========================================================================
***1) TWFE model -Effect of Sec. Coop Agreement on Homicide Related Deaths
sort inegi year

est clear
foreach outcome in logdefuncionespc ihs_defuncionespc{
foreach treatment in acuerdo  acuerdo2{
eststo: quietly xi: areg `outcome' `treatment'  $controls_pre i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l.`treatment' $controls_pre i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l2.`treatment' $controls_pre i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}

esttab est*, keep(acuerdo acuerdo2) t star(* 0.1 ** 0.05 *** 0.01)
esttab est*, keep(acuerdo L.acuerdo L2.acuerdo acuerdo2 L.acuerdo2 L2.acuerdo2) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/twfe_coop_agreements_homicides.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E.")) ///
keep(acuerdo L.acuerdo L2.acuerdo acuerdo2 L.acuerdo2 L2.acuerdo2) ///
mgroups("DV: log(homicides per capita)" "DV: IHS(homicides per capita)", ///
pattern(1 0 0  1 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(acuerdo "SCA A (contemporary)" L.acuerdo "lag" L2.acuerdo "lag 2" ///
 acuerdo2 "SCA B (contemporary)" L.acuerdo2 "lag" L2.acuerdo2 "lag 2") ///
collabels(none) nonotes booktabs nomtitles  nolines
	
*MESSAGE: security cooperation agreements decrease homicides

*========================================================================
**EFFECT IS NOT MEDIATED BY THE REFORM
est clear
foreach outcome in logdefuncionespc ihs_defuncionespc{
foreach treatment in acuerdo  acuerdo2{
eststo: quietly xi: areg `outcome' `treatment' reform $controls_pre i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' L.`treatment' reform  $controls_pre i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' L2.`treatment' reform  $controls_pre i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}

esttab est*, keep(acuerdo acuerdo2 reform) t star(* 0.1 ** 0.05 *** 0.01)

*let's see the interaction
est clear
foreach outcome in logdefuncionespc {
foreach treatment in   acuerdo2{
eststo: quietly xi: areg `outcome' c.`treatment'##c.reform $controls i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	}
	}
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' cL.`treatment'##cL.reform  $controls i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' cL2.`treatment'##cL.reform  $controls i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}

esttab est*, keep(acuerdo2 reform c.acuerdo2#c.reform) t star(* 0.1 ** 0.05 *** 0.01)
lincom _b[acuerdo2] + _b[c.acuerdo2#reform]

*THIS KIND OF DOESN'T MAKE SENSE BECAUSE ACUERDO2 IS AFFECTED BY REFORM



*========================================================================
***2A) TWFE model -Effect of Reform on Sec. Coop Agreement
sort inegi year

est clear
foreach outcome in acuerdo2 {
foreach treatment in reform{
eststo: quietly xi: areg `outcome' `treatment' $controls_pre i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l.`treatment' $controls_pre i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l2.`treatment' $controls_pre i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l3.`treatment' $controls_pre i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}

esttab est*, keep(reform L.reform L2.reform L3.reform) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/twfe_reform_coop_agreements.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E.")) ///
keep(reform L.reform L2.reform L3.reform) ///
mgroups("DV: Security Cooperation Agreement A" "DV: Security Cooperation Agreement B", ///
pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(reform "Term Limit Reform"  L.reform "Lag" L2.reform "Lag 2" L3.reform "Lag 3") ///
collabels(none) nonotes booktabs nomtitles  nolines
	
*MESSAGE: 
**reform decreased the likelihood of signing security cooperation agreements
**but results are biased due to heterogeneous treatment effects (staggered treatment)
**same results clustering at the municipality level but should cluster at the estado level

*========================================================================
***2B) TWFE model - demanding -Effect of Reform on Sec. Coop Agreement

est clear
foreach outcome in acuerdo acuerdo2 {
foreach treatment in reform{
eststo: quietly xi: areg `outcome' `treatment' c.`treatment'#c.year_* $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
/*eststo: quietly xi: areg `outcome' l.`treatment' c.l.`treatment'#c.year_* $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l2.`treatment' c.l2.`treatment'#c.year_* $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l3.`treatment' c.l3.`treatment'#c.year_* $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	*/
}
}
esttab est*, keep(reform c.reform#c.year_1  c.reform#c.year_2  c.reform#c.year_3  c.reform#c.year_4 ///
 c.reform#c.year_5  c.reform#c.year_6  c.reform#c.year_7  c.reform#c.year_8  c.reform#c.year_9) t star(* 0.1 ** 0.05 *** 0.01)


*========================================================================
***3) Matching model -Effect of Reform on Sec. Coop Agreement
*in R
*MESSAGE: Reform decreases the likelihood of security cooperation agreements 

*========================================================================
***4) TWFE model -Effect of Reform on Sec. Coop Agreement Type
 
est clear
foreach outcome in tipoacuerdo_noaplica tipoacuerdo_convenio tipoacuerdo_contrato tipoacuerdo_acuerdo tipoacuerdo_otro{
foreach treatment in reform{
eststo: quietly xi: areg `outcome' `treatment' $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
/*eststo: quietly xi: areg `outcome' l.`treatment' $controls i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l2.`treatment' $controls i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l3.`treatment' $controls i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	*/
}
}

*esttab est*, keep(reform L.reform L2.reform L3.reform) t star(* 0.1 ** 0.05 *** 0.01)
esttab est*, keep(reform) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/twfe_reform_coop_agreements.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "Cluster S.E.")) ///
keep(reform L.reform L2.reform L3.reform) ///
mgroups("DV: Security Cooperation Agreement A" "DV: Security Cooperation Agreement B", ///
pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(reform "Term Limit Reform" ) ///
*coeflabel(reform "Term Limit Reform"  L.reform "Lag" L2.reform "Lag 2" L3.reform "Lag 3") ///
collabels(none) nonotes booktabs nomtitles  nolines
	
	
*MESSAGE: ... 


*========================================================================
***5) Saturated Event study design (Abraham and Sun, 2020) -Effect of Reform on Sec. Coop Agreement

*Set globals: 
	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 logdefuncionespc_lag_8 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3	 logdefuncionespc_lag_2
	global outcome acuerdo
	global outcome2 acuerdo2
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

************
***A: ACUERDO W/O COVARIATES
************	

est clear
quietly	areg  $outcome  $saturated   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols 
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
***B: ACUERDO2 W/O COVARIATES
************	
quietly	areg  $outcome2  $saturated i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols
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
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:abraham_sun_lagdv}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{Security Cooperation Agreement A} & \multicolumn{1}{c}{Security Cooperation Agreement B$^{a}$} \\
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
tex Controls$^b$   &          &       \\
tex Cohort weighted   &   \checkmark       &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: Reform decreases the likelihood of signing security cooperation agreements.
/*NOTE: cannot adjust for covariates as this is not adjusted in the weighting. 
However, the matching exercise shows similar results.  
Also, if include controls, include all such as past homicides, winning margin and governor alignment. Things look relatively well but weights are wrong.
*/

*========================================================================
***6) Corrected-TWFE (Chaisemartin and D'Haultfoeuille, forthcoming AER) -Effect of Reform on Sec. Coop Agreement

*global controls winning_margin_governor_pre governor_alignment_pre logdefuncionespc_prereform $citizens2 alignment_executive_strong_pre winning_margin_pre

*Graph:
foreach outcome in acuerdo  acuerdo2 {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) seed(5675) ///
cluster(estado) robust_dynamic dynamic(3) placebo(4) longdiff_placebo 
graph export "../Figures/chaisemartin_`outcome'.png", as(png) replace
}

************
*A) Acuerdo:
************
foreach outcome in acuerdo {
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
*B) Acuerdo2:
************
foreach outcome in acuerdo2 {
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVmandounico.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on the likelihood of signing Security Cooperation Agreements}
tex \label{tab:chaisemartin}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{Security Cooperation Agreement A} & \multicolumn{1}{c}{Security Cooperation Agreement B$^{a}$} \\
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
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Secondary version of security cooperation agreements. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: Reform decreases the likelihood of signing security cooperation agreements.

*========================================================================
***7) Wild bootstrap -Effect of Reform on Sec. Coop Agreement. To adjust for the small number of clusters.
*set matsize
set matsize 11000 

sort inegi year
foreach i in reform{
*gen `i'_pre=L.`i'
gen `i'_pre2=L2.`i'
}

/*WILD:
est clear
eststo: clustse reg acuerdo reform_pre2 year_* $controls, fe(inegi)  cluster(estado) method(wild) reps(1000)
	estadd local comcontrols \checkmark
	estadd local departmentfixed \checkmark
	estadd local clusterdepartment \checkmark
	estadd local ci "[-0.315, -0.052]"
	estadd local ci "[-0.417, -0.083]" // with L2.

eststo: clustse reg acuerdo2 reform_pre2 year_* $controls, fe(inegi)  cluster(estado) method(wild) reps(1000)
	estadd local comcontrols \checkmark
	estadd local departmentfixed \checkmark
	estadd local clusterdepartment \checkmark
	estadd local ci "[-0.237, 0.019]"
	estadd local ci "[-0.365, -0.019]" // with L2.

	*/
	

*TWFE
est clear
eststo: quietly xi: areg acuerdo L.reform $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	estadd local ci "[-0.315, -0.052]"

eststo: quietly xi: areg acuerdo L2.reform $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	estadd local ci "[-0.417, -0.083]" // with L2.

eststo: quietly xi: areg acuerdo2 L.reform $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	estadd local ci "[-0.237, 0.019]"

eststo: quietly xi: areg acuerdo2 L2.reform $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	estadd local ci "[-0.365, -0.019]" // with L2.
	
esttab est*, keep(L.reform L2.reform) t star(* 0.1 ** 0.05 *** 0.01)

esttab using "../Tables/wild_reform_sec_agreement.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun ci, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "Depto. Cluster S.E." "Wild CI")) ///
keep(L.reform L2.reform) ///
mgroups("DV: Security Cooperation Agreement A" "DV: Security Cooperation Agreement B", ///
pattern(1 1 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L.reform "Reform (t-1)" L2.reform "Reform (t-2)") ///
collabels(none) nonotes booktabs nomtitles  nolines


*========================================================================
*MESSAGE TILL HERE: CENTRALIZATION DECREASES HOMICIDES AND INCREASES PUBLIC GOOD PROVISION; REFORM DECRESED CENTRALIZATION, AND THUS INCREASED HOMICIDES AND PUBLIC GOOD PROVISION
**SO REELECTION INCENTIVES LED TO DECENTRALIZATION, PUBLIC GOOD UNDERPROVISION AND VIOLENCE
*========================================================================

*========================================================================
*1) WINNING MARGIN
***do margins

est clear
foreach outcome in acuerdo2 {
foreach treatment in reform{
eststo: quietly xi: areg `outcome' `treatment' $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
/*eststo: quietly xi: areg `outcome' l.`treatment' $controls i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	*/
}
}

margins, at(winning_margin_pre=(0(.1)1))
marginsplot, scheme(s1color)

esttab est*, keep(reform L.reform) t star(* 0.1 ** 0.05 *** 0.01)



