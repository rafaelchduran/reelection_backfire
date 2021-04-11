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

global controls *_y_*
global controls logdefuncionespc_mean_y_1 logdefuncionespc_mean_y_2 logdefuncionespc_mean_y_3 logdefuncionespc_mean_y_4 logdefuncionespc_mean_y_5 logdefuncionespc_mean_y_6 logdefuncionespc_mean_y_7 logdefuncionespc_mean_y_8   ///
 align_gov_y_1 align_gov_y_2 align_gov_y_3 align_gov_y_4 align_gov_y_5 align_gov_y_6 align_gov_y_7 align_gov_y_8  ///
 margin_gov_y_1 margin_gov_y_2 margin_gov_y_3 margin_gov_y_4 margin_gov_y_5 margin_gov_y_6 margin_gov_y_7 margin_gov_y_8  ///
 hayCarteles_y_1 hayCarteles_y_2 hayCarteles_y_3 hayCarteles_y_4 hayCarteles_y_5 hayCarteles_y_6 hayCarteles_y_7 hayCarteles_y_8  ///
ap4_2_3_mean_y_1 ap4_2_3_mean_y_2 ap4_2_3_mean_y_3 ap4_2_3_mean_y_4 ap4_2_3_mean_y_5 ap4_2_3_mean_y_6 ap4_2_3_mean_y_7 ap4_2_3_mean_y_8 

/*
align_pres_y_1 align_pres_y_2 align_pres_y_3 align_pres_y_4 align_pres_y_5 align_pres_y_6 align_pres_y_7 align_pres_y_8 align_pres_y_9 ///
winning_margin_mean_y_1 winning_margin_mean_y_2 winning_margin_mean_y_3 winning_margin_mean_y_4 winning_margin_mean_y_5 winning_margin_mean_y_6 winning_margin_mean_y_7 winning_margin_mean_y_8 winning_margin_mean_y_9 ///
pri_mayor2_y_1 pri_mayor2_y_2 pri_mayor2_y_3 pri_mayor2_y_4 pri_mayor2_y_5 pri_mayor2_y_6 pri_mayor2_y_7 pri_mayor2_y_8 pri_mayor2_y_9 ///
ap4_12b_mean_y_1 ap4_12b_mean_y_2 ap4_12b_mean_y_3 ap4_12b_mean_y_4 ap4_12b_mean_y_5 ap4_12b_mean_y_6 ap4_12b_mean_y_7 ap4_12b_mean_y_8 ap4_12b_mean_y_9 ///
ap4_2_11_mean_y_1 ap4_2_11_mean_y_2 ap4_2_11_mean_y_3 ap4_2_11_mean_y_4 ap4_2_11_mean_y_5 ap4_2_11_mean_y_6 ap4_2_11_mean_y_7 ap4_2_11_mean_y_8 ap4_2_11_mean_y_9 ///
ap4_2_5_mean_y_1 ap4_2_5_mean_y_2 ap4_2_5_mean_y_3 ap4_2_5_mean_y_4 ap4_2_5_mean_y_5 ap4_2_5_mean_y_6 ap4_2_5_mean_y_7 ap4_2_5_mean_y_8 ap4_2_5_mean_y_9
*/

*========================================================================
*1) Naive Event study design: Cluster vs Wild corrected errors
sort inegi year

center logdefuncionespc ihs_defuncionespc loghomicidecombinedpc ihs_homicidecombinedpc, inplace standardize

preserve
est clear
*quietly  xi: areg acuerdo3 $lagsleads_short  $controls  i.year, a(inegi) vce(cluster estado)
*keep if e(sample)==1 
eststo: quietly  xi: areg logdefuncionespc $lagsleads  $controls  i.year, a(inegi) vce(cluster estado)
macros_tables
eststo: qui wildcorrection logdefuncionespc
macros_tables
eststo: quietly  xi: areg ihs_defuncionespc $lagsleads  $controls  i.year, a(inegi) vce(cluster estado)
macros_tables
eststo: qui wildcorrection ihs_defuncionespc
macros_tables
eststo: quietly  xi: areg loghomicidecombinedpc $lagsleads  $controls  i.year, a(inegi) vce(cluster estado)
macros_tables
eststo: qui wildcorrection loghomicidecombinedpc
macros_tables
eststo: quietly  xi: areg ihs_homicidecombinedpc $lagsleads  $controls  i.year, a(inegi) vce(cluster estado)
macros_tables
eststo: qui wildcorrection ihs_homicidecombinedpc
macros_tables

esttab est*, keep($lagsleads) t(%9.3f)  star(* 0.1 ** 0.05 *** 0.01)
restore

esttab using "../Tables/event_study_reform_coop_agreements_forpres.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N r2 controls munfe yearfe clustermun aggregate se_aggregate p_aggregate, fmt(%11.2gc 3) ///
 label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "State Cluster S.E." "Aggregate beta" "SE (aggregate)" ///
 "p-value(aggregate)")) ///
keep($lagsleads ) ///
mgroups("Agreement old" "total" "Mando" "Mando 2", ///
pattern(1 0 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(lag_7 "t-7" lag_6 "t-6" lag_5 "t-5" lag_4 "t-4" lag_3 "t-3" lag_2 "t-2"  date_0 "Reform t=0" ///
 lead_1 "t+1" lead_2 "t+2" lead_3 "t+3") ///
collabels(none) nonotes booktabs nomtitles  nolines

*CONCLUSION 1: MANDO UNICO DECREASES AND ITS SIGNIFICANT; SO IS THE AGGREGATE EFFECT
*CONCLUSION 2: MANDO UNICO SERVICE DECREASES AND ITS SIGNIFICANT; SO IS THE AGGREGATE EFFECT
*CONCLUION 3: GOB. FEDERAL IS POSITIVE AND NON-SIGNIFICANT


*========================================================================
***0) TWFE model -Effect of Sec. Coop Agreement on Homicide Related Deaths
sort inegi year

est clear
preserve
center logdefuncionespc ihs_defuncionespc, inplace standardize
foreach outcome in logdefuncionespc {
foreach treatment in acuerdo  acuerdo2{
eststo: quietly xi: areg `outcome' `treatment' $controls  i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
eststo: quietly xi: areg `outcome' l.`treatment' $controls   i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
eststo: quietly xi: areg `outcome' l2.`treatment' $controls   i.year, a(inegi) vce(cluster inegi)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark

}
}

esttab est*, keep(L.acuerdo L.acuerdo2) t star(* 0.1 ** 0.05 *** 0.01)
esttab est*, keep(acuerdo L.acuerdo L2.acuerdo acuerdo2 L.acuerdo2 L2.acuerdo2) t star(* 0.1 ** 0.05 *** 0.01)

restore

esttab using "../Tables/twfe_coop_agreements_homicides.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "Mun. Cluster S.E.")) ///
keep( L.acuerdo L.acuerdo2 ) ///
mgroups("DV: log(homicides per capita)" "DV: IHS(homicides per capita)", ///
pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(L.acuerdo "Agreement A in t-1" L.acuerdo2 "Agreement B in t-1") ///
collabels(none) nonotes booktabs nomtitles  nolines
	
*MESSAGE: security cooperation agreements decrease homicides


*========================================================================
***1) TWFE model -Effect of Reform on Homicides
sort inegi year


est clear  
foreach outcome in logdefuncionespc ihs_defuncionespc  {
*foreach outcome in logdefuncionespc ihs_defuncionespc  loghomicidepc loghomicide_oldpc loghomicidecombinedpc{
foreach treatment in reform L.reform L2.reform{
eststo: quietly xi: areg `outcome' `treatment' $controls  i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}

esttab est*, keep(reform L.reform L2.reform) t star(* 0.1 ** 0.05 *** 0.01)


est clear  
foreach outcome in logdefuncionespc  logdefuncionespc_post logdefuncionespc_post2 logdefuncionespc_post3  {
foreach treatment in reform {
eststo: quietly xi: areg `outcome' `treatment' $controls  i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
}
}
qui boottest reform, bootcluster(estado) nograph 

esttab est*, keep(reform) t star(* 0.1 ** 0.05 *** 0.01)


*for presentation:
esttab using "../Tables/reform_violence.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "Mun. Cluster S.E.")) ///
keep(acuerdo L.acuerdo ) ///
mgroups("log(homicides per capita)" "IHS(homicides per capita)", ///
pattern(1 0 1 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(acuerdo "Sec. Coop. Agreement (Centralization)" ///
L.acuerdo "Sec. Coop. Agreement (Centralization) in t-1") ///
collabels(none) nonotes booktabs nomtitles  nolines
	
*MESSAGE: REFORM HAS A POSITIVE EFFECT ON HOMICIDES BUT ITS NOT SIGNIFICANT 

**********************************************
*2) Wild CI
set seed 5675
est clear
eststo: clustse reg logdefuncionespc reform year_* $controls, fe(inegi)  cluster(estado) method(wild) reps(100)
	estadd local ci "[.00636851   .25057945]"
eststo: clustse reg logdefuncionespc_post reform year_* $controls, fe(inegi)  cluster(estado) method(wild) reps(100)
	estadd local ci "[-.0700435   .16782346]"
eststo: clustse reg logdefuncionespc_post2 reform year_* $controls, fe(inegi)  cluster(estado) method(wild) reps(100)
	estadd local ci "[-.09404895   .23374517]"
eststo: clustse reg logdefuncionespc_post3 reform year_* $controls, fe(inegi)  cluster(estado) method(wild) reps(100)
	estadd local ci "[-.12583297   .28164688]"	
	
	

*MESSAGE: ALL POSITIVE BUT NON SIGNIFICANT. 
**********************************************
*3) Simple event study design
**use 2014 as reference period

est clear  
foreach outcome in logdefuncionespc ihs_defuncionespc{
foreach treatment in reform {
eststo:  xi: areg `outcome'  c.`treatment'#c.year_1 c.`treatment'#c.year_2 c.`treatment'#c.year_3 ///
c.`treatment'#c.year_4 c.`treatment'#c.year_6 c.`treatment'#c.year_7 c.`treatment'#c.year_8 c.`treatment'#c.year_9 ///
 $controls  i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark


esttab est*, keep(c.`treatment'#c.year_1 c.`treatment'#c.year_2 c.`treatment'#c.year_3 ///
c.`treatment'#c.year_4 c.`treatment'#c.year_6 c.`treatment'#c.year_7 c.`treatment'#c.year_8 c.`treatment'#c.year_9) ///
 t star(* 0.1 ** 0.05 *** 0.01)


*for presentation:
esttab using "../Tables/reform_violence.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun, fmt(%11.2gc 3) label("Observations" "R2" "Controls" "Mun. FE" "Year FE" "Mun. Cluster S.E.")) ///
keep(c.`treatment'#c.year_6 c.`treatment'#c.year_7 c.`treatment'#c.year_8 c.`treatment'#c.year_9) ///
mgroups("log(homicides per capita)" "IHS(homicides per capita)", ///
pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(c.`treatment'#c.year_6 "Reform X t=0" c.`treatment'#c.year_7 "Reform X t+1" ///
 c.`treatment'#c.year_8 "Reform X t+2" c.`treatment'#c.year_9 "Reform X t+3") ///
collabels(none) nonotes booktabs nomtitles  nolines
}
}

**********************************************
*4) WILD for Simple event study design
foreach i in 1 2 3 4 6 7 8 9{
gen inter_`i'=reform*year_`i'
}

/*est clear
eststo: clustse reg logdefuncionespc inter_1 inter_2 inter_3 inter_4 inter_6 inter_7 inter_8 inter_9 ///
 year_* $controls, fe(inegi)  cluster(estado) method(wild) reps(100)
	estadd local ci0 "[-.11284504   .12806472]"
	estadd local ci1 "[-.06956541    .2018265]"
	estadd local ci2 "[-.13241531   .17744428]"
	estadd local ci3 "[.02354645   .42384848]"
eststo: clustse reg ihs_defuncionespc inter_1 inter_2 inter_3 inter_4 inter_6 inter_7 inter_8 inter_9 ///
 year_* $controls, fe(inegi)  cluster(estado) method(wild) reps(100)
	estadd local ci0 "[-.38302872   .65980321]"
	estadd local ci1 "[-.18646193   .76731455]"
	estadd local ci2 "[-.32873836   .82095933]"
	estadd local ci3 "[-.20161788   .78158647]"
*/

est clear  
foreach treatment in reform {
eststo:  xi: areg logdefuncionespc  c.`treatment'#c.year_1 c.`treatment'#c.year_2 c.`treatment'#c.year_3 ///
c.`treatment'#c.year_4 c.`treatment'#c.year_6 c.`treatment'#c.year_7 c.`treatment'#c.year_8 c.`treatment'#c.year_9 ///
 $controls  i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	estadd local ci0 "[-.11284504   .12806472]"
	estadd local ci1 "[-.06956541    .2018265]"
	estadd local ci2 "[-.13241531   .17744428]"
	estadd local ci3 "[.02354645   .42384848]"
	
eststo:  xi: areg ihs_defuncionespc  c.`treatment'#c.year_1 c.`treatment'#c.year_2 c.`treatment'#c.year_3 ///
c.`treatment'#c.year_4 c.`treatment'#c.year_6 c.`treatment'#c.year_7 c.`treatment'#c.year_8 c.`treatment'#c.year_9 ///
 $controls  i.year, a(inegi) vce(cluster estado)
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	estadd local ci0 "[-.38302872   .65980321]"
	estadd local ci1 "[-.18646193   .76731455]"
	estadd local ci2 "[-.32873836   .82095933]"
	estadd local ci3 "[-.20161788   .78158647]"

*for presentation:
esttab using "../Tables/reform_violence_wild.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N R2 controls munfe yearfe clustermun ci0 ci1 ci2 ci3, fmt(%11.2gc 3) label("Observations" "R2" "Controls" ///
 "Mun. FE" "Year FE" "Mun. Cluster S.E." "Wild CI t=0" "Wild CI t+1" "Wild CI t+2" "Wild CI t+3")) ///
keep(c.`treatment'#c.year_6 c.`treatment'#c.year_7 c.`treatment'#c.year_8 c.`treatment'#c.year_9) ///
mgroups("log(homicides per capita)" "IHS(homicides per capita)", ///
pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(c.`treatment'#c.year_6 "Reform X t=0" c.`treatment'#c.year_7 "Reform X t+1" ///
 c.`treatment'#c.year_8 "Reform X t+2" c.`treatment'#c.year_9 "Reform X t+3") ///
collabels(none) nonotes booktabs nomtitles  nolines
}

boottest c.reform#c.year_8, bootcluster(estado) 

quietly xi: areg logdefuncionespc_post reform $controls  i.year, a(inegi) vce(cluster estado)
boottest reform, bootcluster(estado)  
matrix a = r(CI)
svmat a
display a[1,1] a[1,2]
	
**********************************************
*5) ABRAHAM AND SUN (2021)

xtset inegi year

*Set globals: 
global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015


************
***A: w/o covariates
************	

est clear
preserve
quietly	areg  logdefuncionespc  $saturated $controls  i.year, a(inegi) vce(cluster estado)
keep if e(sample)==1
quietly	areg  logdefuncionespc  $saturated  i.year, a(inegi) vce(cluster estado)
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



restore
************
***B: with covariates
************	
quietly	areg  logdefuncionespc  $saturated $controls  i.year, a(inegi) vce(cluster estado)
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
	
texdoc init  "../Tables/abraham_sun_estimates_lagDV.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence}
tex \label{tab:abraham_sun_lagdv}
tex \scalebox{1}{    
tex \begin{tabular}{lcc}  
tex \hline \hline       
tex \\ \multicolumn{3}{l}{Dependent variable:}\\
tex & \multicolumn{1}{c}{log(homicide per capita)} & \multicolumn{1}{c}{log(homicide per capita)$^{a}$} \\
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
tex Controls$^b$   &          &   \checkmark     \\
tex Cohort weighted   &   \checkmark       &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close

*To get quantities of interest:

preserve
*Set globals: 
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015

	xtset inegi year 
	
center logdefuncionespc , inplace

eststo: areg  logdefuncionespc  $saturated  $controls i.year, a(inegi) vce(cluster estado)
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





restore

*MESSAGE: reform increased homicides per capita. 
	
*========================================================================
*6) Chaisemartin and D'Haultfoeuille correction
*Graph:
foreach outcome in  logdefuncionespc ihs_defuncionespc{
did_multiplegt `outcome' group year reform, breps(100) controls($controls)  seed(5675) ///
cluster(estado) robust_dynamic dynamic(2) placebo(2) longdiff_placebo 
graph export "../Figures/chaisemartin_`outcome'.png", as(png) replace
}

foreach outcome in  logdefunciones ihs_defunciones{
did_multiplegt `outcome' group year reform, breps(100) controls($controls logpop)  seed(5675) ///
cluster(estado) robust_dynamic dynamic(2) placebo(2) longdiff_placebo 
graph export "../Figures/chaisemartin_`outcome'.png", as(png) replace
}

************
*A) Acuerdo:
************
foreach outcome in logdefuncionespc {
did_multiplegt `outcome' group year reform, breps(100)  placebo(5) seed(5675) ///
cluster(estado) robust_dynamic dynamic(2)  average_effect ///
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

	


	

************
*B) Acuerdo2:
************
foreach outcome in ihs_defuncionespc {
did_multiplegt `outcome' group year reform, breps(100)  placebo(5) seed(5675) ///
cluster(estado) robust_dynamic  dynamic(2) ///
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

*Table
	
texdoc init  "../Tables/chaisemarting_estimates_DVreform_violence.tex", replace force
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
tex Lag 5 years &        $ ${pbeta_t_4}^{${pest_t_4}} $ &     $ ${pbeta2_t_4}^{${pest2_t_4}} $ \\
tex  & ($ ${pse_t_4}$) & ($ ${pse2_t_4} $) \\
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

tex \addlinespace
tex Controls$^b$   &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{3}{p{0.8\textwidth}}{\footnotesize{Notes: Coefficients show corrected estimators following \citet{chaisemarting_etal_2019}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%.$^a$ Secondary version of security cooperation agreements. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*MESSAGE: CANNOT ESTIMATE PARALLEL TRENDS; RESULTS SEEM NEGATIVE. 


*========================================================================
*2) 2SLS: effect of decentralization on homicides, instrumented by reform

est clear

eststo:  ivreg2 logdefuncionespc (acuerdo=reform) $controls year_* inegi_*, cluster(estado) partial(year_* inegi_*)  first
eststo:  ivreg2 logdefuncionespc (L.acuerdo=L.reform) $controls  year_* inegi_*, cluster(estado) partial(year_* inegi_*)  first
eststo:  ivreg2 logdefuncionespc (acuerdo2=L2.reform) $controls_pre year_* inegi_*, cluster(estado) partial(year_* inegi_*)  first
eststo:  ivreg2 logdefuncionespc (acuerdo2=L3.reform) $controls_pre year_* inegi_*, cluster(estado) partial(year_* inegi_*)  first

**the conclusion could be there is no effect on homicides

*========================================================================
*3) Corrected-TWFE (Chaisemartin and D'Haultfoeuille, forthcoming AER) -Effect of Reform on Homicides

/*Graph:
foreach outcome in logdefuncionespc  ihs_defuncionespc {
did_multiplegt `outcome' group year reform, breps(1000) controls($controls_pre) seed(5675) ///
cluster(estado) robust_dynamic dynamic(3) p2lacebo(4) longdiff_placebo 
graph export "../Figures/chaisemartin_`outcome'.png", as(png) replace
}
*/

************
*A) logdefuncionespc:
************
eststo:  xi: areg ihs_defunciones reform  i.year logpop, a(inegi) vce(cluster estado)

foreach outcome in ihs_defunciones {
did_multiplegt `outcome' group year reform, breps(1000) controls(logpop)  placebo(4) seed(5675) ///
cluster(inegi) robust_dynamic  dynamic(3) ///
save_results("../../Data/chaisemartin_`outcome'.dta")

}


foreach outcome in logdefuncionespc {
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
*B) ihs_defuncionespc:
************
foreach outcome in ihs_defuncionespc {
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
	
texdoc init  "../Tables/chaisemarting_estimates_DVhomicides.tex", replace force
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


 
**********************************************
*Table: Het effects, PRI and Public Security Effort
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	xtset inegi year 
	global DV l.logdefuncionespc
	
************
***A: cocaine with covariates // PRI MAYOR
************
quietly	areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster inegi)
keep if e(sample)==1

est clear
foreach i in pri_mayor{

areg   logcocaina_kg c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster inegi)
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
***B: heroine with covariates // PRI MAYOR
************


est clear
foreach i in pri_mayor{

areg   logheroina_kg c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster inegi)
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
***C: cocaine with covariates // PAN MAYOR
************
est clear
foreach i in pan_mayor{

areg   logcocaina_kg c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster inegi)
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
***D: heroine with covariates // MORENA MAYOR
************
est clear
foreach i in pan_mayor{

areg   logheroina_kg c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster inegi)
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

*similar sign with laboratorio, mariguana, methanmphetamine

texdoc init  "../Tables/abraham_sun_estimates_pri_effort.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect of Partisanship on Public Security Effort$^a$}
tex \label{tab:abraham_sun_pri_effort}
tex \scalebox{1}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(cocaine)$^b$} & \multicolumn{2}{c}{log(heroine)$^b$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace
tex Reform (t+3)*PRI &     $ ${beta_pri_mayor1}^{${est_pri_mayor1}} $ &  &  $ ${beta_pri_mayor2}^{${est_pri_mayor2}} $  & \\
tex  &     ($ ${se_pri_mayor1} $) &  & ($ ${se_pri_mayor2} $)  & \\

tex Reform (t+3)*PAN &    &   $ ${beta_pan_mayor3}^{${est_pan_mayor3}} $ &  &  $ ${beta_pan_mayor4}^{${est_pan_mayor4}} $ \\
tex  &    &     ($ ${se_pan_mayor3} $) & & ($ ${se_pan_mayor4} $)  \\
tex \\
tex \addlinespace
tex Observations       &        ${N_pri_mayor1}    &        ${N_pan_mayor3}    &     ${N_pri_mayor2}      &     ${N_pan_mayor4}  \\
tex R-squared       &        ${r2_pri_mayor1}    &        ${r2_pan_mayor3}    &     ${r2_pri_mayor2}      &     ${r2_pan_mayor4}  \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^c$  &  \checkmark       &       \checkmark  &  \checkmark        &   \checkmark    \\
tex Cohort weighted$^d$  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\

tex \hline \hline  
tex \multicolumn{5}{p{0.65\textwidth}}{\footnotesize{Notes:$^a$ Total interaction effect tests the linear hypothesis of the estimated coefficient of PRI mayor (PAN mayor) + estimated coefficient of the interaction of PRI (PAN)*lead t=3. This is a post-estimation test using the same specification as that of Table \ref{tab:abraham_sun_lagdv} column (1). Other leads and the indicator at time t=0 when reform came to effect are omitted due to collinearity. Standard errors of linear hypothesis test in parentheses with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided test with the null hypothesis equal to 0. Main regression with standard errors clustered at the state-level. $^b$ Refers to the inverse hyperbolic sine transformation. $^c$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. I also include the lag of the outcome, i.e. logged homicides per capita as control. $^d$ Estimates weighted by each cohort's relative share of the sample following \citet{abraham_sun_2020}.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close



**********************************************
*Table: Mechanisms: INCUMBENCY QUALITY WITH POLYNOMIALS
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

preserve
foreach i in incumbent_yesterday_w_tomorrow2{
rdbwselect  `i' mv_incparty, c(0) p(2) kernel(tri) bwselect(CCT) 
global optimal2 = e(h_CCT)

quietly areg  `i'  $saturated pol2 $interacted_pol2 $allcov $DV2  i.year if mv_incparty<${optimal2} & mv_incparty>-${optimal2}, a(inegi) vce(cluster inegi)
keep if e(sample)==1
}

*------------------
*A)SAMPLE INC. AT T-1 WINS AT T+1 :
*------------------
est clear
foreach j in incumbent_quality {
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
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)		
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
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
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
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
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
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)		
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
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
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
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
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
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)		
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
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
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
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
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
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)		
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
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
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
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)	
			}


}


}
restore


*------------------
*B)WITH HOMICIDES CONTROL
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
foreach j in incumbent_quality{
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
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)		
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
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
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
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
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
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)		
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
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
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
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
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
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)		
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
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
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
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
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
			
	lincom (_b[`i'_2016]*`b'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)		
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
	lincom (_b[`i'_2015]*`a'+_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
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
	lincom (_b[`i'_2015]*`a'+(_b[`i'_2017]*`c')  ) 
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')  
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
	lincom (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d')  
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)	
			}


}
}

restore


texdoc init  "../Tables/abraham_sun_quality_wpolynomials_short.tex", replace force
*tex \begin{landscape}
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Event-in-Discontinuity in close elections model: Effect of 2014 Term Limit Reform on Incumbent's Quality }
tex \label{tab:incumbency_quality}
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

tex Lag 6 years &     $ ${beta_lag_6_ihsdet_1}^{${est_lag_6_ihsdet_1}} $ &     $ ${beta_lag_6_ihsdet2_1}^{${est_lag_6_ihsdet2_1}} $  \\
tex  &     ($${se_lag_6_ihsdet_1}$) &    ($${se_lag_6_ihsdet2_1}$)  \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_1}^{${est_lag_5_ihsdet_1}} $ &       $ ${beta_lag_5_ihsdet2_1}^{${est_lag_5_ihsdet2_1}} $  \\
tex  &     ($${se_lag_5_ihsdet_1}$) &    ($${se_lag_5_ihsdet2_1}$)  \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_1}^{${est_lag_4_ihsdet_1}} $ &       $ ${beta_lag_4_ihsdet2_1}^{${est_lag_4_ihsdet2_1}} $\\  
tex  &     ($${se_lag_4_ihsdet_1}$) &    ($${se_lag_4_ihsdet2_1}$)  \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_1}^{${est_lag_3_ihsdet_1}} $  &     $ ${beta_lag_3_ihsdet2_1}^{${est_lag_3_ihsdet2_1}} $ \\   
tex  &     ($${se_lag_3_ihsdet_1}$) &    ($${se_lag_3_ihsdet2_1}$)  \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_1}^{${est_date_0_ihsdet_1}} $ &     $ ${beta_date_0_ihsdet2_1}^{${est_date_0_ihsdet2_1}} $\\   
tex  &     ($${se_date_0_ihsdet_1}$) &    ($${se_date_0_ihsdet2_1}$)  \\
tex \\
tex Observations        &        ${N_ihsdet_1}    &        ${N_ihsdet2_1} \\ 
tex R-squared        &          ${r2_ihsdet_1}  &          ${r2_ihsdet2_1} \\  
tex\\
tex & \multicolumn{2}{c}{quadratic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 6 years &     $ ${beta_lag_6_ihsdet_2}^{${est_lag_6_ihsdet_2}} $ &     $ ${beta_lag_6_ihsdet2_2}^{${est_lag_6_ihsdet2_2}} $  \\
tex  &     ($${se_lag_6_ihsdet_2}$) &    ($${se_lag_6_ihsdet2_2}$)  \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_2}^{${est_lag_5_ihsdet_2}} $ &       $ ${beta_lag_5_ihsdet2_2}^{${est_lag_5_ihsdet2_2}} $  \\
tex  &     ($${se_lag_5_ihsdet_2}$) &    ($${se_lag_5_ihsdet2_2}$)  \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_2}^{${est_lag_4_ihsdet_2}} $ &       $ ${beta_lag_4_ihsdet2_2}^{${est_lag_4_ihsdet2_2}} $\\  
tex  &     ($${se_lag_4_ihsdet_2}$) &    ($${se_lag_4_ihsdet2_2}$)  \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_2}^{${est_lag_3_ihsdet_2}} $  &     $ ${beta_lag_3_ihsdet2_2}^{${est_lag_3_ihsdet2_2}} $ \\   
tex  &     ($${se_lag_3_ihsdet_2}$) &    ($${se_lag_3_ihsdet2_2}$)  \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_2}^{${est_date_0_ihsdet_2}} $ &     $ ${beta_date_0_ihsdet2_2}^{${est_date_0_ihsdet2_2}} $\\   
tex  &     ($${se_date_0_ihsdet_2}$) &    ($${se_date_0_ihsdet2_2}$)  \\
tex \\
tex Observations        &        ${N_ihsdet_2}    &        ${N_ihsdet2_2} \\ 
tex R-squared        &          ${r2_ihsdet_2}  &          ${r2_ihsdet2_2} \\  
tex \\
tex & \multicolumn{2}{c}{cubic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 6 years &     $ ${beta_lag_6_ihsdet_3}^{${est_lag_6_ihsdet_3}} $ &     $ ${beta_lag_6_ihsdet2_3}^{${est_lag_6_ihsdet2_3}} $  \\
tex  &     ($${se_lag_6_ihsdet_3}$) &    ($${se_lag_6_ihsdet2_3}$)  \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_3}^{${est_lag_5_ihsdet_3}} $ &       $ ${beta_lag_5_ihsdet2_3}^{${est_lag_5_ihsdet2_3}} $  \\
tex  &     ($${se_lag_5_ihsdet_3}$) &    ($${se_lag_5_ihsdet2_3}$)  \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_3}^{${est_lag_4_ihsdet_3}} $ &       $ ${beta_lag_4_ihsdet2_3}^{${est_lag_4_ihsdet2_3}} $\\  
tex  &     ($${se_lag_4_ihsdet_3}$) &    ($${se_lag_4_ihsdet2_3}$)  \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_3}^{${est_lag_3_ihsdet_3}} $  &     $ ${beta_lag_3_ihsdet2_3}^{${est_lag_3_ihsdet2_3}} $ \\   
tex  &     ($${se_lag_3_ihsdet_3}$) &    ($${se_lag_3_ihsdet2_3}$)  \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_3}^{${est_date_0_ihsdet_3}} $ &     $ ${beta_date_0_ihsdet2_3}^{${est_date_0_ihsdet2_3}} $\\   
tex  &     ($${se_date_0_ihsdet_3}$) &    ($${se_date_0_ihsdet2_3}$)  \\
tex \\
tex Observations        &        ${N_ihsdet_3}    &        ${N_ihsdet2_3} \\ 
tex R-squared        &          ${r2_ihsdet_3}  &          ${r2_ihsdet2_3} \\  
tex \\
tex & \multicolumn{2}{c}{quartic polynomial} \\
tex \cmidrule(lrr){2-3} \\
tex Lag 6 years &     $ ${beta_lag_6_ihsdet_4}^{${est_lag_6_ihsdet_4}} $ &     $ ${beta_lag_6_ihsdet2_4}^{${est_lag_6_ihsdet2_4}} $  \\
tex  &     ($${se_lag_6_ihsdet_4}$) &    ($${se_lag_6_ihsdet2_4}$)  \\
tex Lag 5 years &     $ ${beta_lag_5_ihsdet_4}^{${est_lag_5_ihsdet_4}} $ &       $ ${beta_lag_5_ihsdet2_4}^{${est_lag_5_ihsdet2_4}} $  \\
tex  &     ($${se_lag_5_ihsdet_4}$) &    ($${se_lag_5_ihsdet2_4}$)  \\
tex Lag 4 years &     $ ${beta_lag_4_ihsdet_4}^{${est_lag_4_ihsdet_4}} $ &       $ ${beta_lag_4_ihsdet2_4}^{${est_lag_4_ihsdet2_4}} $\\  
tex  &     ($${se_lag_4_ihsdet_4}$) &    ($${se_lag_4_ihsdet2_4}$)  \\
tex Lag 3 years &     $ ${beta_lag_3_ihsdet_4}^{${est_lag_3_ihsdet_4}} $  &     $ ${beta_lag_3_ihsdet2_4}^{${est_lag_3_ihsdet2_4}} $ \\   
tex  &     ($${se_lag_3_ihsdet_4}$) &    ($${se_lag_3_ihsdet2_4}$)  \\
tex Reform, time 0 &    $ ${beta_date_0_ihsdet_4}^{${est_date_0_ihsdet_4}} $ &     $ ${beta_date_0_ihsdet2_4}^{${est_date_0_ihsdet2_4}} $\\   
tex  &     ($${se_date_0_ihsdet_4}$) &    ($${se_date_0_ihsdet2_4}$)  \\
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
tex \multicolumn{3}{p{0.75\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 6 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020} or because they are collinear or inexistent like lag 2. Standard errors in parentheses are clustered at the state level for estimates in saturaded model. Significance-levels: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Even columns with outcomes where missing values where replaced by zeros assuming no activity was registered. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. $^c$ Municipal controls include logged homicides per capita interacted with cohort fixed effects pre-treatment.}} \\
tex \end{tabular}
tex } 
tex \end{table}
*tex \end{landscape}
texdoc close




**********************************************
*Table: WITH LAGGED DV. & citizen demands as controlss
**********************************************
/*
ap4_2_3 - effect but too big; citizens that see narcotraffick as important
ap4_2_5 - effect but too big; citizens that see insecurity as important
ap4_2_11 - effect but too big; citizens that demand punishmnet
ap4_3_1 - effect but too big; citizens that see insecurity in their neighborhood
ap4_3_2 - effect but too big; citizens that see insecurity in their municipality
ap4_3_3 - effect but too big; citizens that see insecurity in their state
***effect of these last three diminishes strongly when moving away from home, i.e. externalities of war
ap4_7_1 - one year after will security better in neighborhood
ap4_7_2 - one year after will security better in municipality
ap4_7_3 - one year after will security better in state
ap4_7_1_b - dummy
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

**create citizen demands as controls:
global insecurityperception ap4_2_3 ap4_2_5 ap4_2_11 ap4_3_1 ap4_3_2 ap4_3_3 ap4_7_1 ap4_7_2 ap4_7_3 ap4_12b ap5_1_6 ap5_1_7 ap5_1_8 ap5_1_10 ap5_1_12 ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_9 ap4_7_1_b ap4_7_2_b ap4_7_3_b ap5_4_2_b ap5_4_3_b ap5_4_4_b ap5_4_5_b ap5_4_6_b ap5_4_7_b ap5_4_8_b ap5_4_9_b
foreach var in $insecurityperception{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}


*Set globals: 
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

est clear
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)
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
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 i.year, a(inegi) vce(cluster estado)
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



	
texdoc init  "../Tables/abraham_sun_estimates_lagDV&citizendemands.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence, controlling for citizens security perception}
tex \label{tab:abraham_sun_citizensdemands}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{a}$} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace
tex Lag 7 years &     $ ${beta_lag_7_log_nocov}^{${est_lag_7_log_nocov}} $ &      & $ ${beta_lag_7_ihs_nocov}^{${est_lag_7_ihs_nocov}} $ &   \\
tex  &     ($${se_lag_7_log_nocov}$) &    & ($${se_lag_7_ihs_nocov}$) & \\
tex Lag 6 years &     $ ${beta_lag_6_log_nocov}^{${est_lag_6_log_nocov}} $ &  $ ${beta_lag_6_log}^{${est_lag_6_log}} $    &  $ ${beta_lag_6_ihs_nocov}^{${est_lag_6_ihs_nocov}} $ &  $ ${beta_lag_6_ihs}^{${est_lag_6_ihs}} $  \\
tex  &     ($${se_lag_6_log_nocov}$) &   ($${se_lag_6_log}$) & ($${se_lag_6_ihs_nocov}$) & ($${se_lag_6_ihs}$) \\
tex Lag 5 years &     $ ${beta_lag_5_log_nocov}^{${est_lag_5_log_nocov}} $ &   $ ${beta_lag_5_log}^{${est_lag_5_log}} $   &  $ ${beta_lag_5_ihs_nocov}^{${est_lag_5_ihs_nocov}} $ &  $ ${beta_lag_5_ihs}^{${est_lag_5_ihs}} $ \\
tex  &     ($${se_lag_5_log_nocov}$) &   ($${se_lag_5_log}$) & ($${se_lag_5_ihs_nocov}$) & ($${se_lag_5_ihs}$) \\
tex Lag 4 years &     $ ${beta_lag_4_log_nocov}^{${est_lag_4_log_nocov}} $ &   $ ${beta_lag_4_log}^{${est_lag_4_log}} $   &   $ ${beta_lag_4_ihs_nocov}^{${est_lag_4_ihs_nocov}} $ &   $ ${beta_lag_4_ihs}^{${est_lag_4_ihs}} $  \\
tex  &     ($${se_lag_4_log_nocov}$) &   ($${se_lag_4_log}$) & ($${se_lag_4_ihs_nocov}$) & ($${se_lag_4_ihs}$) \\
tex Lag 3 years &     $ ${beta_lag_3_log_nocov}^{${est_lag_3_log_nocov}} $ &   $ ${beta_lag_3_log}^{${est_lag_3_log}} $   &   $ ${beta_lag_3_ihs_nocov}^{${est_lag_3_ihs_nocov}} $ &   $ ${beta_lag_3_ihs}^{${est_lag_3_ihs}} $ \\
tex  &     ($${se_lag_3_log_nocov}$) &   ($${se_lag_3_log}$) & ($${se_lag_3_ihs_nocov}$) & ($${se_lag_3_ihs}$) \\
tex Lag 2 years &     $ ${beta_lag_2_log_nocov}^{${est_lag_2_log_nocov}} $ &     $ ${beta_lag_2_log}^{${est_lag_2_log}} $ &  $ ${beta_lag_2_ihs_nocov}^{${est_lag_2_ihs_nocov}} $  &  $ ${beta_lag_2_ihs}^{${est_lag_2_ihs}} $  \\
tex  &     ($${se_lag_2_log_nocov}$) &   ($${se_lag_2_log}$) & ($${se_lag_2_ihs_nocov}$) & ($${se_lag_2_ihs}$) \\
tex Reform, time 0 &     $ ${beta_date_0_log_nocov}^{${est_date_0_log_nocov}} $ &     $ ${beta_date_0_log}^{${est_date_0_log}} $ &   $ ${beta_date_0_ihs_nocov}^{${est_date_0_ihs_nocov}} $   &   $ ${beta_date_0_ihs}^{${est_date_0_ihs}} $ \\
tex  &     ($${se_date_0_log_nocov}$) &   ($${se_date_0_log}$) & ($${se_date_0_ihs_nocov}$) & ($${se_date_0_ihs}$) \\
tex Lead 1 year &     $ ${beta_lead_1_log_nocov}^{${est_lead_1_log_nocov}} $ &     $ ${beta_lead_1_log}^{${est_lead_1_log}} $ &    $ ${beta_lead_1_ihs_nocov}^{${est_lead_1_ihs_nocov}} $ &    $ ${beta_lead_1_ihs}^{${est_lead_1_ihs}} $ \\
tex  &     ($${se_lead_1_log_nocov}$) &   ($${se_lead_1_log}$) & ($${se_lead_1_ihs_nocov}$) & ($${se_lead_1_ihs}$) \\
tex Lead 2 years &     $ ${beta_lead_2_log_nocov}^{${est_lead_2_log_nocov}} $ &     $ ${beta_lead_2_log}^{${est_lead_2_log}} $ &   $ ${beta_lead_2_ihs_nocov}^{${est_lead_2_ihs_nocov}} $  &   $ ${beta_lead_2_ihs}^{${est_lead_2_ihs}} $  \\
tex  &     ($${se_lead_2_log_nocov}$) &   ($${se_lead_2_log}$) & ($${se_lead_2_ihs_nocov}$) & ($${se_lead_2_ihs}$) \\
tex Lead 3 years &     $ ${beta_lead_3_log_nocov}^{${est_lead_3_log_nocov}} $ &     $ ${beta_lead_3_log}^{${est_lead_3_log}} $ &   $ ${beta_lead_3_ihs_nocov}^{${est_lead_3_ihs_nocov}} $ &   $ ${beta_lead_3_ihs}^{${est_lead_3_ihs}} $ \\
tex  &     ($${se_lead_3_log_nocov}$) &   ($${se_lead_3_log}$) & ($${se_lead_3_ihs_nocov}$) & ($${se_lead_3_ihs}$) \\
tex \\

tex \addlinespace
tex Observations       &        ${N_log_nocov}    &        ${N_log}    &     ${N_ihs_nocov}      &     ${N_ihs}  \\
tex R-squared        &          ${r2_log_nocov} &          ${r2_log}    &           ${r2_ihs_nocov}       &           ${r2_ihs}   \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^b$  &    \checkmark      &       \checkmark  &    \checkmark      &   \checkmark    \\
tex Cohort weighted  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\
tex Lag DV &  \checkmark      &       \checkmark  &    \checkmark      &   \checkmark    \\
tex Citizens' Security Perception$^c$ &    &   \checkmark  &         &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{5}{p{0.9\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}; lag 7 is removed in columns (2) and (4) due to collinearity. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. $^c$ Citizens' Security Perception are state-level covariates that include the percentage of citizens who see narcotraffick as the most worrisome issue in the country, the percentage of citizens who see a lack of punishment of criminals as the most worrisome public issue, the amount of money (in thousands) spend to protect from crime, and citizens trust on local police forces and the army.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


**********************************************
*Table: Abraham-Sun (2020) DTOs het effects
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

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc

	xtset inegi year 


/************
***A: log(homicides per capita) with covariates 
************
est clear
foreach i in hayCarteles{

eststo: areg   logdefuncionespc c.${saturated}##c.`i'##c.distEntradasPrinc $controls $lagDV    i.year & acuerdo2==0, a(inegi) vce(cluster inegi)

	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local statecontrols \checkmark
	estadd local cohortweights \checkmark
	estadd local lagdv \checkmark

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] + _b[distEntradasPrinc] + _b[1.lead_3_2015#c.distEntradasPrinc] + _b[1.lead_3_2015#c.`i'#c.distEntradasPrinc] + _b[c.`i'#c.distEntradasPrinc] 
	glo beta_`i'3: di %5.4f r(estimate)
	glo se_`i'3: di %5.4f r(se)
	glo g_`i'3: di r(df)
	glo t_`i'3: di  %5.4f ${beta_`i'3}/${se_`i'3}
	glo p_`i'3: di 2*ttail(${g_`i'3},abs(${t_`i'3})) 
	glo est_`i'3= "" 
			if (${p_`i'3}<=0.1) global est_`i'3  = "*"
			if (${p_`i'3}<=0.05) global est_`i'3 = "**"
			if (${p_`i'3}<=0.01) global est_`i'3 = "***"		
			
    estadd local fstat_`i'3 $${beta_`i'3}^{${est_`i'3}}
	estadd local error_`i'3 (${se_`i'3})



************
***B: ihs(homicides per capita) with covariates 
************

eststo: areg ihs_defuncionespc c.${saturated}##c.`i'##c.distEntradasPrinc $controls $lagDV2    i.year & acuerdo2==0, a(inegi) vce(cluster inegi)

	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local statecontrols \checkmark
	estadd local cohortweights \checkmark
	estadd local lagdv \checkmark

lincom _b[`i'] + _b[1.lead_3_2015#c.`i'] + _b[distEntradasPrinc] + _b[1.lead_3_2015#c.distEntradasPrinc] + _b[1.lead_3_2015#c.`i'#c.distEntradasPrinc] + _b[c.`i'#c.distEntradasPrinc] 
	glo beta_`i'3: di %5.4f r(estimate)
	glo se_`i'3: di %5.4f r(se)
	glo g_`i'3: di r(df)
	glo t_`i'3: di  %5.4f ${beta_`i'3}/${se_`i'3}
	glo p_`i'3: di 2*ttail(${g_`i'3},abs(${t_`i'3})) 
	glo est_`i'3= "" 
			if (${p_`i'3}<=0.1) global est_`i'3  = "*"
			if (${p_`i'3}<=0.05) global est_`i'3 = "**"
			if (${p_`i'3}<=0.01) global est_`i'3 = "***"		
			
    estadd local fstat_`i'3 $${beta_`i'3}^{${est_`i'3}}
	estadd local error_`i'3 (${se_`i'3})


esttab using "../Tables/abraham_sun_estimates_dtos_het_results.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///d
s(N r2 munfe yearfe statecontrols cohortweights lagdv fstat_`i'3 error_`i'3, fmt(0 3) label("Observations" "R-squared"  ///
"Mun. FEs" "Year FEs" "State Controls$^c$" "Cohort weighted$^d$" ///
"Lag DV" "Tot. int. effect$^a$" "Tot. int. effect, SE")) /// 
 keep( 1.lead_3_2015#c.`i' 1.lead_3_2015#c.distEntradasPrinc 1.lead_3_2015#c.`i'#c.distEntradasPrinc ) ///
coeflabel( ///
`i'  "Cartel presence" ///
1.lead_3_2015#c.`i' "Reform (t+3) X Cartel presence" ///
1.lead_3_2015#c.distEntradasPrinc "Reform (t+3) X Proximity to U.S." ///
c.`i'#c.distEntradasPrinc "Cartel presence X Proximity to U.S." ///
1.lead_3_2015#c.`i'#c.distEntradasPrinc "Reform (t+3) X Cartel presence X Proximity to U.S.") ///
mgroups("log(homicide per capita)" "ihs(homicide per capita)$^b$", ///
 pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
collabels(none) nonotes booktabs nomtitles  nolines

}


 */
 
************
***A: log(homicides per capita) with covariates 
quietly	areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster inegi)
keep if e(sample)==1

est clear
foreach i in distEntradasPrinc{

areg   logdefuncionespc c.${saturated}##c.`i' $controls $lagDV   i.year, a(inegi) vce(cluster inegi)
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
foreach i in distEntradasPrinc{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls $lagDV2   i.year, a(inegi) vce(cluster inegi)
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
foreach i in hayCarteles{

areg   logdefuncionespc c.${saturated}##c.`i' $controls $lagDV   i.year, a(inegi) vce(cluster inegi)
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
foreach i in hayCarteles{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls $lagDV2  i.year, a(inegi) vce(cluster inegi)
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




texdoc init  "../Tables/abraham_sun_estimates_dtos_het.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect of Term Limit Reform and Drug Trafficking Organization Presence on Violence$^a$}
tex \label{tab:abraham_sun_dto_het}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{b}$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace
tex Reform (t+3) X Proximity to U.S. &     $ ${beta_distEntradasPrinc1}^{${est_distEntradasPrinc1}} $ &  &  $ ${beta_distEntradasPrinc2}^{${est_distEntradasPrinc2}} $  & \\
tex  &     ($ ${se_distEntradasPrinc1} $) &  & ($ ${se_distEntradasPrinc2} $)  & \\

tex Reform (t+3) X Cartel presence (indicator) &    &   $ ${beta_hayCarteles3}^{${est_hayCarteles3}} $ &  &  $ ${beta_hayCarteles4}^{${est_hayCarteles4}} $ \\
tex  &    &     ($ ${se_hayCarteles3} $) & & ($ ${se_hayCarteles4} $)  \\

tex \addlinespace
tex Observations       &        ${N_distEntradasPrinc1}    &        ${N_distEntradasPrinc2}    &     ${N_hayCarteles3}      &     ${N_hayCarteles4}  \\
tex R-squared       &        ${r2_distEntradasPrinc1}    &        ${r2_distEntradasPrinc2}    &     ${r2_hayCarteles3}      &     ${r2_hayCarteles4}  \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^c$  &  \checkmark       &       \checkmark  &  \checkmark        &   \checkmark    \\
tex Cohort weighted$^d$  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\
tex Lag DV &  \checkmark      &       \checkmark  &    \checkmark      &   \checkmark    \\
tex \hline \hline      
tex \multicolumn{5}{p{1.1\textwidth}}{\footnotesize{Notes:$^a$ Total interaction effect tests the linear hypothesis of the estimated coefficient of proximity to the US (Cartel presence indicator) + estimated coefficient of the interaction of proximity (cartel presence)*lead t=3. This is a post-estimation test using the same specification as that of Table \ref{tab:abraham_sun_lagdv}. Other leads and the indicator at time t=0 when reform came to effect are omitted due to collinearity. Standard errors of linear hypothesis test in parentheses with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided test with the null hypothesis equal to 0. $^b$ Refers to the inverse hyperbolic sine transformation. $^c$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. $^d$ Estimates weighted by each cohort's relative share of the sample following \citet{abraham_sun_2020}.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


**********************************************
*Table: EFFECT OF REFORM ON HOMICIDES: CONTROLLING FOR MANDO UNICO
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 acuerdo2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc
	xtset inegi year 
	

************
***A: log(homicides per capita) with covariates
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
	
texdoc init  "../Tables/abraham_sun_estimates_lagDV_mandounico.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence, controlling for security cooperation agreements}
tex \label{tab:abraham_sun_lagdv_mandounico}
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
tex Lag DV &          \checkmark         &   \checkmark    \\
tex Security Coop. Agreement &          \checkmark         &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{3}{p{0.9\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


**********************************************
*Table: EFFECT OF REFORM ON HOMICIDES: SPLITTING SAMPLE FOLLOWING MANDO UNICO STATUS
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc
	xtset inegi year 
	
	gen acuerdo2_pre=0
	replace acuerdo2_pre=1 if acuerdo2==1 & year<=2015
		gen acuerdo_pre=0
	replace acuerdo_pre=1 if acuerdo==1 & year<=2015
	global mandounico  acuerdo2_pre
************
***A: Acuerdo2 =0
************	

est clear
quietly	areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==0, a(inegi) vce(cluster estado)
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
***B: Acuerdo2 =1 
************	
quietly	areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==1, a(inegi) vce(cluster estado)
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



************
***C: ihs Acuerdo2 =0
************	

est clear
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year if $mandounico==0, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log2:  di %5.4f e(r2)
		glo N_log2: di %11.2gc e(N)

***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log2: di %5.4f r(se)
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
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log2: di %5.4f r(se)
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
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log2: di %5.4f r(se)
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
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_log2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_log2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_log2: di r(p)
	glo beta_`i'_log2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_log2= "" 
			if (${p_`i'_log2}<=0.1) global est_`i'_log2 = "*"
			if (${p_`i'_log2}<=0.05) global est_`i'_log2 = "**"
			if (${p_`i'_log2}<=0.01) global est_`i'_log2 = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_log2: di %5.4f r(se)
}


************
***D: IHS Acuerdo2 =1 
************	
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year if $mandounico==1, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs2:  di %5.4f e(r2)
		glo N_ihs2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:

foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"
	lincom 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs2: di %5.4f r(se)
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
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"
	lincom 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs2: di %5.4f r(se)
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
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs2: di %5.4f r(se)
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
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs2: di %5.4f r(se)
}


foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')=0
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"	
	lincom 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs2: di %5.4f r(se)
}


foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a')=0
	glo p_`i'_ihs2: di r(p)
	glo beta_`i'_ihs2: di %5.4f (_b[`i'_2015]*`a')
	glo est_`i'_ihs2= "" 
			if (${p_`i'_ihs2}<=0.1) global est_`i'_ihs2 = "*"
			if (${p_`i'_ihs2}<=0.05) global est_`i'_ihs2 = "**"
			if (${p_`i'_ihs2}<=0.01) global est_`i'_ihs2 = "***"	
	lincom 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs2: di %5.4f r(se)
}





*Table 
	
texdoc init  "../Tables/abraham_sun_estimates_lagDV_splitmandounico.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Effect of 2014 Term Limit Reform on Violence, comparing municipalities with security cooperation agreements with other levels of government}
tex \label{tab:abraham_sun_lagdv_splitmandounico}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^a$}\\
tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5} \\
tex & \multicolumn{1}{c}{w/o Security} & \multicolumn{1}{c}{w/ Security} & \multicolumn{1}{c}{w/o Security} & \multicolumn{1}{c}{w/ Security} \\ 
tex & \multicolumn{1}{c}{Coop. Agreement} & \multicolumn{1}{c}{Coop. Agreement} & \multicolumn{1}{c}{Coop. Agreement} & \multicolumn{1}{c}{Coop. Agreement}\\ 
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)}  & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)}\\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4} \cmidrule(lrr){5-5}\\
tex \addlinespace
tex Lag 7 years &      $ ${beta_lag_7_log}^{${est_lag_7_log}} $ &  $ ${beta_lag_7_ihs}^{${est_lag_7_ihs}} $   &      $ ${beta_lag_7_log2}^{${est_lag_7_log2}} $ &  $ ${beta_lag_7_ihs2}^{${est_lag_7_ihs2}} $ \\
tex  & ($ ${se_lag_7_log}$) & ($ ${se_lag_7_ihs} $)  & ($ ${se_lag_7_log2}$) & ($ ${se_lag_7_ihs2} $) \\
tex Lag 6 years &      $ ${beta_lag_6_log}^{${est_lag_6_log}} $ &  $ ${beta_lag_6_ihs}^{${est_lag_6_ihs}} $   &      $ ${beta_lag_6_log2}^{${est_lag_6_log2}} $ &  $ ${beta_lag_6_ihs2}^{${est_lag_6_ihs2}} $ \\
tex  & ($ ${se_lag_6_log}$) & ($ ${se_lag_6_ihs} $)  & ($ ${se_lag_6_log2}$) & ($ ${se_lag_6_ihs2} $) \\
tex Lag 5 years &      $ ${beta_lag_5_log}^{${est_lag_5_log}} $ &  $ ${beta_lag_5_ihs}^{${est_lag_5_ihs}} $   &      $ ${beta_lag_5_log2}^{${est_lag_5_log2}} $ &  $ ${beta_lag_5_ihs2}^{${est_lag_5_ihs2}} $ \\
tex  & ($ ${se_lag_5_log}$) & ($ ${se_lag_5_ihs} $)  & ($ ${se_lag_5_log2}$) & ($ ${se_lag_5_ihs2} $) \\
tex Lag 4 years &      $ ${beta_lag_4_log}^{${est_lag_4_log}} $ &  $ ${beta_lag_4_ihs}^{${est_lag_4_ihs}} $   &      $ ${beta_lag_4_log2}^{${est_lag_4_log2}} $ &  $ ${beta_lag_4_ihs2}^{${est_lag_4_ihs2}} $ \\
tex  & ($ ${se_lag_4_log}$) & ($ ${se_lag_4_ihs} $)  & ($ ${se_lag_4_log2}$) & ($ ${se_lag_4_ihs2} $) \\
tex Lag 3 years &      $ ${beta_lag_3_log}^{${est_lag_3_log}} $ &  $ ${beta_lag_3_ihs}^{${est_lag_3_ihs}} $   &      $ ${beta_lag_3_log2}^{${est_lag_3_log2}} $ &  $ ${beta_lag_3_ihs2}^{${est_lag_3_ihs2}} $ \\
tex  & ($ ${se_lag_3_log}$) & ($ ${se_lag_3_ihs} $)  & ($ ${se_lag_3_log2}$) & ($ ${se_lag_3_ihs2} $) \\
tex Lag 2 years &      $ ${beta_lag_2_log}^{${est_lag_2_log}} $ &  $ ${beta_lag_2_ihs}^{${est_lag_2_ihs}} $   &      $ ${beta_lag_2_log2}^{${est_lag_2_log2}} $ &  $ ${beta_lag_2_ihs2}^{${est_lag_2_ihs2}} $ \\
tex  & ($ ${se_lag_2_log}$) & ($ ${se_lag_2_ihs} $)  & ($ ${se_lag_2_log2}$) & ($ ${se_lag_2_ihs2} $) \\
tex Reform, time 0 &      $ ${beta_date_0_log}^{${est_date_0_log}} $ &  $ ${beta_date_0_ihs}^{${est_date_0_ihs}} $   &      $ ${beta_date_0_log2}^{${est_date_0_log2}} $ &  $ ${beta_date_0_ihs2}^{${est_date_0_ihs2}} $ \\
tex  & ($ ${se_date_0_log}$) & ($ ${se_date_0_ihs} $)  & ($ ${se_date_0_log2}$) & ($ ${se_date_0_ihs2} $) \\
tex Lead 1 year &      $ ${beta_lead_1_log}^{${est_lead_1_log}} $ &  $ ${beta_lead_1_ihs}^{${est_lead_1_ihs}} $   &      $ ${beta_lead_1_log2}^{${est_lead_1_log2}} $ &  $ ${beta_lead_1_ihs2}^{${est_lead_1_ihs2}} $ \\
tex  & ($ ${se_lead_1_log}$) & ($ ${se_lead_1_ihs} $)  & ($ ${se_lead_1_log2}$) & ($ ${se_lead_1_ihs2} $) \\
tex Lead 2 years &      $ ${beta_lead_2_log}^{${est_lead_2_log}} $ &  $ ${beta_lead_2_ihs}^{${est_lead_2_ihs}} $   &      $ ${beta_lead_2_log2}^{${est_lead_2_log2}} $ &  $ ${beta_lead_2_ihs2}^{${est_lead_2_ihs2}} $ \\
tex  & ($ ${se_lead_2_log}$) & ($ ${se_lead_2_ihs} $)  & ($ ${se_lead_2_log2}$) & ($ ${se_lead_2_ihs2} $) \\
tex Lead 3 years &      $ ${beta_lead_3_log}^{${est_lead_3_log}} $ &  $ ${beta_lead_3_ihs}^{${est_lead_3_ihs}} $   &      $ ${beta_lead_3_log2}^{${est_lead_3_log2}} $ &  $ ${beta_lead_3_ihs2}^{${est_lead_3_ihs2}} $ \\
tex  & ($ ${se_lead_3_log}$) & ($ ${se_lead_3_ihs} $)  & ($ ${se_lead_3_log2}$) & ($ ${se_lead_3_ihs2} $) \\

tex \addlinespace
tex Observations       &            ${N_log}        &     ${N_ihs} &            ${N_log2}        &     ${N_ihs2}  \\
tex R-squared        &              ${r2_log}        &           ${r2_ihs}  &              ${r2_log2}        &           ${r2_ihs2}  \\
tex Mun. FEs       &     \checkmark         &  \checkmark  &     \checkmark         &  \checkmark  \\
tex Year. FEs       &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark \\
tex State Controls$^b$   &    \checkmark      &   \checkmark &    \checkmark      &   \checkmark   \\
tex Cohort weighted   &   \checkmark       &   \checkmark &   \checkmark       &   \checkmark    \\
tex Lag DV &          \checkmark         &   \checkmark &          \checkmark         &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{5}{p{1.1\textwidth}}{\footnotesize{Notes: Coefficients show IW estimators following \citet{abraham_sun_2020}. Two relative time periods (lag 8 and 1) are removed to avoid collinearity problems noted by \citet{abraham_sun_2020}. Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. $^a$ Refers to the inverse hyperbolic sine transformation. $^b$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party.}} \\
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


**********************************************
*Table: HET EFFECTS: MANDO UNICO
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	xtset inegi year
	global DV l.logdefuncionespc
	

************
***A: log(homicides per capita) with covariates // ACUERDO
************
quietly	areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster inegi)
keep if e(sample)==1

est clear
foreach i in acuerdo{

areg   logdefuncionespc c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster estado)
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
***B: ihs(homicides per capita) with covariates // ACUERDO
************


est clear
foreach i in acuerdo{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls $DV i.year, a(inegi) vce(cluster estado)
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
***C: log(homicides per capita) with covariates // ACUERDO2
************
est clear
foreach i in acuerdo2{

areg   logdefuncionespc c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster estado)
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
***D: ihs(homicides per capita) with covariates // WINNING MARGIN
************
est clear
foreach i in acuerdo2{

areg   ihs_defuncionespc c.${saturated}##c.`i' $controls $DV  i.year, a(inegi) vce(cluster estado)
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



texdoc init  "../Tables/abraham_sun_estimates_hetmando_unico.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Total Interaction Effect$^a$: the role of security cooperation agreements}
tex \label{tab:abraham_sun_mando_unico}
tex \scalebox{0.8}{    
tex \begin{tabular}{lcccc}  
tex \hline \hline       
tex \\ \multicolumn{5}{l}{Dependent variable:}\\
tex & \multicolumn{2}{c}{log(homicide per capita)} & \multicolumn{2}{c}{ihs(homicide per capita)$^{b}$} \\  
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} \\ 

tex \cmidrule(lrr){2-3}  \cmidrule(lrr){4-5}\\
tex \addlinespace

tex Reform (t+3)*Coop. Agreement &     $ ${beta_acuerdo1}^{${est_acuerdo1}} $ &  &  $ ${beta_acuerdo2}^{${est_acuerdo2}} $  & \\
tex  &     ($ ${se_acuerdo1} $) &  & ($ ${se_acuerdo2} $)  & \\

tex Reform (t+3)*Coop. Agreement (other measure)$^e$ &    &   $ ${beta_acuerdo23}^{${est_acuerdo23}} $ &  &  $ ${beta_acuerdo24}^{${est_acuerdo24}} $ \\
tex  &    &     ($ ${se_acuerdo23} $) & & ($ ${se_acuerdo24} $)  \\

tex \\
tex \addlinespace
tex Observations       &        ${N_acuerdo1}    &        ${N_acuerdo2}    &     ${N_acuerdo23}      &     ${N_acuerdo24}  \\
tex R-squared       &        ${r2_acuerdo1}    &        ${r2_acuerdo2}    &     ${r2_acuerdo23}      &     ${r2_acuerdo24}  \\
tex Mun. FEs      &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark    \\
tex Year. FEs    &     \checkmark         &  \checkmark   &     \checkmark         &  \checkmark   \\
tex State Controls$^c$  &  \checkmark       &       \checkmark  &  \checkmark        &   \checkmark    \\
tex Cohort weighted$^d$  &   \checkmark      &       \checkmark  &   \checkmark       &   \checkmark    \\

tex \hline \hline      
tex \multicolumn{5}{p{1.2\textwidth}}{\footnotesize{Notes:$^a$ Total interaction effect tests the linear hypothesis of the estimated coefficient of alignment with Federal Government indicator (winning margin) + estimated coefficient of the interaction of alignment (winning margin)*lead t=3. This is a post-estimation test using the same specification as that of Table \ref{tab:abraham_sun_lagdv} column (1). Other leads and the indicator at time t=0 when reform came to effect are omitted due to collinearity. Standard errors of linear hypothesis test in parentheses with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided test with the null hypothesis equal to 0. Main regression with standard errors clustered at the state-level. $^b$ Refers to the inverse hyperbolic sine transformation. $^c$ State-level controls include governor winning margin in last pre-treatment election and an indicator of whether the governor's party is the same as the federal incumbent party. I also include the lag of the outcome, i.e. logged homicides per capita as control. $^d$ Estimates weighted by each cohort's relative share of the sample following \citet{abraham_sun_2020}.} $^e$ Measure taken from reasons to modify police structure questionnaire from the National Census of Municipal Governments from INEGI.} \\
tex \end{tabular}
tex } 
tex \end{table} 
texdoc close


