*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*Main Figures     
*****************************************************

clear all
set more off  
set varabbrev off  

*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Dofiles"


**********************************************
*Figure: EFFECT OF REFORM ON HOMICIDES, WITH LAGGED DV & COHORT WEIGHTS
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
	
graph drop _all

************
***A: log(homicides per capita) with covariates
************	

est clear
***estimate linear combination by lead/lag:

foreach i in lag_7{
	areg  logdefuncionespc  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)
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
	glo b_`i'_log: di %5.4f r(estimate)
	glo se_`i'_log: di %5.4f r(se)
	glo lb_`i'_log: di %5.4f r(lb)
	glo ub_`i'_log: di %5.4f r(ub)
	*plotbeta ${b_`i'_log}, level(90)
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	*est store `i'

}
foreach i in lag_6{
areg  logdefuncionespc  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')

}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  logdefuncionespc  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')

}


foreach i in lead_1{
areg  logdefuncionespc  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)

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
    eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')

}


foreach i in lead_2{
areg  logdefuncionespc  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')

}


foreach i in lead_3{
areg  logdefuncionespc  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)
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
*ssc install coefplot
			   
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
ytitle("estimate: log(homicides per capita)") name(loghomicides)
graph export "../Figures/event_study_log.png", as(png) replace
graph export "../Figures/event_study_log.pdf", as(pdf) replace
graph export "../Figures/event_study_log.tif", as(tif) replace
graph save "../Figures/event_study_log.gph", replace

************
***B: ihs(homicides per capita) with covariates
************	

est clear
***estimate linear combination by lead/lag:

foreach i in lag_7{
	areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)
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
	glo b_`i'_log: di %5.4f r(estimate)
	glo se_`i'_log: di %5.4f r(se)
	glo lb_`i'_log: di %5.4f r(lb)
	glo ub_`i'_log: di %5.4f r(ub)
	*plotbeta ${b_`i'_log}, level(90)
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	*est store `i'

}
foreach i in lag_6{
areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')

}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')

}


foreach i in lead_1{
areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)

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
    eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')

}


foreach i in lead_2{
areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')

}


foreach i in lead_3{
areg  ihs_defuncionespc  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)
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
*ssc install coefplot
			   
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
ytitle("estimate: IHS(homicides per capita)") name(ihshomicides)
graph export "../Figures/event_study_ihs.png", as(png) replace
graph export "../Figures/event_study_ihs.pdf", as(pdf) replace
graph export "../Figures/event_study_ihs.tif", as(tif) replace
graph save "../Figures/event_study_ihs.gph", replace


*Combine
graph combine  "../Figures/event_study_log.gph" "../Figures/event_study_ihs.gph", ///
 subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) ycommon col(1) b1(Timing) l1(homicides per capita)
graph export "../Figures/event_study_log_ihs.png", as(png) replace
graph export "../Figures/event_study_log_ihs.pdf", as(pdf) replace
graph export "../Figures/event_study_log_ihs.tif", as(tif) replace



******************************
*Figure. RDD plot
******************************
use "../Data/municipal_elections_incumbent_mexico_1989_present_v2.dta", clear

*Merge with Dube, Garcia-Ponce and Thoms (2016): data from 1990 to 2010
gen muncode=inegi

merge 1:1 muncode year using "../Mexico/Data/Dube, Garcia-Ponce and Thom (2016)/jeea12172-sup-0002-replication-data/Dube_Garcia_ Thom_ReplicationData/MaizeToHaze_JEEA_ReplicationData.dta"
drop if _merge==2
drop _merge


gen dv2= incumbent_yesterday_w_tomorrow2

*RD plot
graph drop _all
foreach pol in 1 2 3 4{
rdbwselect  dv2 mv_incparty if reform==0 & year<2015, c(0) p(`pol') kernel(tri) bwselect(CCT)
global optimal = e(h_CCT)

rdplot dv2 mv_incparty if reform==0 & year<2015, c(0) p(`pol') lowerend(-${optimal}) upperend(${optimal}) ///
name(prereform) ///
graph_options(legend(off) title("(a) Prior Term-Limit Reform")  scheme(s1color))
graph export "../Figures/prior_reform_pol`pol'.png", as(png) replace
graph save "../Figures/prior_reform`var'_pol`pol'.gph", replace

rdbwselect  dv2 mv_incparty if reform==1, c(0) p(`pol') kernel(tri) bwselect(CCT)
global optimal = e(h_CCT)

rdplot dv2 mv_incparty if reform==1, c(0) p(`pol') lowerend(-${optimal}) upperend(${optimal}) ///
name(postreform) ///
graph_options(legend(off) title("(b) Post Term-Limit Reform") scheme(s1color))
graph export "../Figures/post_reform_pol`pol'.png", as(png) replace
graph save "../Figures/post_reform`var'_pol`pol'.gph", replace

*All
graph combine  "../Figures/prior_reform.gph" "../Figures/post_reform.gph", ///
 subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) xcommon col(1) l1(Probability of victory at t+1) b1(Incumbent party vote margin at t (municipal elections))
graph export "../Figures/RDD_incumbency_pol`pol'.png", as(png) replace
graph export "../Figures/RDD_incumbency_pol`pol'.pdf", as(pdf) replace
graph export "../Figures/RDD_incumbency_pol`pol'.tif", as(tif) replace

}

******************************
*Figure. McCrary test
******************************

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

*polynomial globals
capture global interacted_pol1 date_0_2015_pol1  date_0_2016_pol1  date_0_2017_pol1  date_0_2018_pol1  lag_5_2018_pol1  lag_4_2015_pol1  lag_3_2015_pol1  lag_3_2016_pol1  lag_3_2018_pol1
capture global interacted_pol2 date_0_2015_pol2  date_0_2016_pol2  date_0_2017_pol2  date_0_2018_pol2  lag_5_2018_pol2  lag_4_2015_pol2  lag_3_2015_pol2  lag_3_2016_pol2  lag_3_2018_pol2
capture global interacted_pol3 date_0_2015_pol3  date_0_2016_pol3  date_0_2017_pol3  date_0_2018_pol3  lag_5_2018_pol3  lag_4_2015_pol3  lag_3_2015_pol3  lag_3_2016_pol3  lag_3_2018_pol3
capture global interacted_pol4 date_0_2015_pol4  date_0_2016_pol4  date_0_2017_pol4  date_0_2018_pol4  lag_5_2018_pol4  lag_4_2015_pol4  lag_3_2015_pol4  lag_3_2016_pol4  lag_3_2018_pol4



foreach j in incumbent_yesterday_w_tomorrow2{
foreach pol in 2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

keep if e(sample)==1

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT)
global optimal = e(h_CCT)

rddensity mv_incparty, c(0) p(`pol') h(${optimal}) plot vce(jackknife) ///
graph_options(legend(off) title("") xtitle("Incumbent party vote margin at t (municipal elections)") ytitle("Density") scheme(s1color))
graph export "../Figures/mccrary_test_pol`pol'.png", as(png) replace
graph save "../Figures/mccrary_test_pol`pol'.gph", replace
}
}

foreach j in incumbent_yesterday_w_tomorrow2{
foreach pol in 3{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


est clear
eststo: areg  `j'  $saturated pol`pol' $interacted_pol3 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

keep if e(sample)==1

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT)
global optimal = e(h_CCT)

rddensity mv_incparty, c(0) p(`pol') h(${optimal}) plot vce(jackknife) ///
graph_options(legend(off) title("") xtitle("Incumbent party vote margin at t (municipal elections)") ytitle("Density") scheme(s1color))
graph export "../Figures/mccrary_test_pol`pol'.png", as(png) replace
graph save "../Figures/mccrary_test_pol`pol'.gph", replace
}
}

******************************
*Figure. Falsification test on Electoral Reform Effect on Violence, RANDOM STATES
******************************
*Program:
capture program drop randomstates
program define randomstates, rclass
	drop _all
	use "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v2.dta"
	*1) create random treatment allocation
	**https://blog.stata.com/2012/08/03/using-statas-random-number-generators-part-2-drawing-without-replacement/
	*generate ui = floor((b–a+1)*runiform() + a) \\ generate integer random numbers between a and b
	preserve
	collapse municipio, by(estado)
	generate estado_fake = floor(32*runiform() + 1)
	tempfile state_randomization
	save `state_randomization'
	restore

	merge m:m estado using `state_randomization'
	drop _merge
	*generate reform_fake treatment variable:
	gen reform_fake=.

	*first treatment year
	foreach i in 3 4 6 7 11 12 14 15 16 17 19 22 24 27 31{
	replace reform_fake=1 if year==2015 & estado_fake==`i'
	replace reform_fake=1 if year==2016 & estado_fake==`i'
	replace reform_fake=1 if year==2017 & estado_fake==`i'
	replace reform_fake=1 if year==2018 & estado_fake==`i'
	replace reform_fake=1 if year==2019 & estado_fake==`i'
	}

	*second treatment year
	foreach i in 1 2 8 10 20 23 25 28 32{
	replace reform_fake=1 if year==2016 & estado_fake==`i'
	replace reform_fake=1 if year==2017 & estado_fake==`i'
	replace reform_fake=1 if year==2018 & estado_fake==`i'
	replace reform_fake=1 if year==2019 & estado_fake==`i'
	}

	*third treatment year 
	foreach i in 5{
	replace reform_fake=1 if year==2017 & estado_fake==`i'
	replace reform_fake=1 if year==2018 & estado_fake==`i'
	replace reform_fake=1 if year==2019 & estado_fake==`i'
	}

	*fourth treatment year
	foreach i in 9 21 26{
	replace reform_fake=1 if year==2018 & estado_fake==`i'
	replace reform_fake=1 if year==2019 & estado_fake==`i'
	}

	replace reform_fake=0 if reform_fake==.
	label variable reform_fake "Dummy=1 if treated Electoral Reform (fake); o otherwise"


	*2) create lead and lags:
	*Create adoption year variable:
	preserve
	collapse (mean)reform_fake (firstnm)nombre_estado, by(estado year)
	xtset estado year
	gen adopt=.
	replace adopt=1 if reform_fake>0 & l.reform_fake==0
	replace adopt=0 if adopt==.
	gen adopt_year=year if adopt==1 //the non-treated states do not have leads or lags. 
		tempfile adopt_year_fake
		save `adopt_year_fake'
	restore	

	merge m:m estado year using `adopt_year_fake'
	xtset inegi year
	xfill adopt_year, i(inegi)

	*Create lead/lag indicators
	order year adopt_year
	gen rel_year=year-adopt_year
	order year adopt_year rel_year

	*turn lead/lags to indicator variables
	tab rel_year, gen(rel_year_) // recall rel_year_9 is the year_zero
	rename rel_year_1 lag_8
	rename rel_year_2 lag_7
	rename rel_year_3 lag_6
	rename rel_year_4 lag_5
	rename rel_year_5 lag_4
	rename rel_year_6 lag_3
	rename rel_year_7 lag_2
	rename rel_year_8 lag_1
	rename rel_year_9 date_0
	rename rel_year_10 lead_1
	rename rel_year_11 lead_2
	rename rel_year_12 lead_3
	rename rel_year_13 lead_4
	
	*3) change leads and lags to long format: 
	gen whichlead="" 
	replace whichlead="lag_8" if lag_8==1
	replace whichlead="lag_7" if lag_7==1
	replace whichlead="lag_6" if lag_6==1
	replace whichlead="lag_5" if lag_5==1
	replace whichlead="lag_4" if lag_4==1
	replace whichlead="lag_3" if lag_3==1
	replace whichlead="lag_2" if lag_2==1
	*replace whichlead="lag_1" if lag_1==1
	replace whichlead="date_0" if date_0==1
	replace whichlead="lead_1" if lead_1==1
	replace whichlead="lead_2" if lead_2==1
	replace whichlead="lead_3" if lead_3==1
	*replace whichlead="lead_4" if lead_4==1
	encode whichlead, gen(whichlead_num)


	**drop missing values of dep. variable: this is the effective sample
	drop if logdefuncionespc==.

	*4) get weights and saturated indicators
	preserve
	**a) get counts; recall that four states don't have lead and lags (the non-treated)
	foreach i in adopt_year{
	**b) Get n: n is the count of observations by adoption year and lead/lag:
	****group observations by adoption year and type of lead/lag
	order `i' whichlead
	egen group_`i'_leadlag=group(`i' whichlead)
	****count the number of times each group appears
	bysort group_`i'_leadlag: egen n=count(group_`i'_leadlag)
	**c) Get percentage: n / total
	****group observations by type of lead/lag
	egen group_leadlag=group(whichlead)
	****
	bysort group_leadlag: egen total=count(n) //total is the total number of leads/lags in the effective sample 
	bysort group_leadlag: gen perc= n/total
	keep whichlead `i' perc
	drop if whichlead==""
	sort whichlead `i'
	collapse (mean)perc, by(`i' whichlead)
	order whichlead `i' perc
	sort whichlead `i' perc

	**d) make variable name to merge in for indicators; we want only the effective indicators that we need for estimation
	 tostring `i', generate(`i'_s)
	gen indic=whichlead+"_"+`i'_s
		tempfile weights_fake
		save `weights_fake'
	restore
	
	***e) merge with main file:
	rename _merge _mergeold
	merge m:m whichlead `i' using `weights_fake' //we do not merge the lag_8 and lag_1
	gen indic_name = strtoname(indic)
	}

	levelsof indic_name, local(names)
	foreach n of local names {
		gen byte `n' = (indic_name == "`n'")
	}
		
	*5) create final covariates:	
	foreach i in 1 2 3 4 5 6 7 8{
	capture gen margin_lag_`i'=lag_`i'*winning_margin_governor
	capture gen margin_lead_`i'=lead_`i'*winning_margin_governor
	capture gen margin_date0=date_0*winning_margin_governor
	}

	foreach var in governor_alignment{
	foreach i in 1 2 3 4 5 6 7 8{
	capture gen `var'_lag_`i'=lag_`i'*`var'
	capture gen `var'_lead_`i'=lead_`i'*`var'
	capture gen `var'_date0=date_0*`var'
	}
	}
	foreach i in margin_ governor_alignment_{
	global `i' `i'lag_8  `i'lag_7 `i'lag_6 `i'lag_5 `i'lag_4 `i'lag_3 `i'lag_2
	}
	
	*6) create final globals	
	xtset inegi year
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc

	*keep $outcome $saturated $controls $other

	*7) run regression:	
	areg  $outcome  $saturated $controls $lagDV  i.year, a(inegi) vce(cluster estado)
	
	
	*8) add weights and estimate final estimates
	foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo beta_`i': di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	return scalar beta_`i' = ${beta_`i'}
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
	glo beta_`i': di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	return scalar beta_`i' = ${beta_`i'}
	}	

	foreach i in lag_5{
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
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	return scalar beta_`i' = ${beta_`i'}
	}
	
	foreach i in lag_4{
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
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	return scalar beta_`i' = ${beta_`i'}
	}
	
	foreach i in lag_3{
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
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	return scalar beta_`i' = ${beta_`i'}
	}
	
	foreach i in lag_2{
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
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	return scalar beta_`i' = ${beta_`i'}
	}
	
	foreach i in date_0{
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
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	return scalar beta_`i' = ${beta_`i'}
	}
	
	foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') =0
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') 
	return scalar beta_`i' = ${beta_`i'}
	}	
	
	foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') =0
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	return scalar beta_`i' = ${beta_`i'}
	}		

	foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a') =0
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')
	return scalar beta_`i' = ${beta_`i'}
	}	
end

*Simulation:
set seed 2
simulate lag_7=r(beta_lag_7) lag_6=r(beta_lag_6) lag_5=r(beta_lag_5) lag_4=r(beta_lag_4) lag_3=r(beta_lag_3) ///
lag_2=r(beta_lag_2) date_0=r(beta_date_0) lead_1=r(beta_lead_1) lead_2=r(beta_lead_2) lead_3=r(beta_lead_3), ///
 reps(2000): randomstates

save "../Data/ConstructionDatabase/simulations_2000_wlagDV.dta", replace


*Create figure:
*use "../Data/ConstructionDatabase/simulations_1000.dta", clear
*use "../Data/ConstructionDatabase/simulations_2000.dta", clear
use "../Data/ConstructionDatabase/simulations_2000_wlagDV.dta", clear

drop if lag_7==.
generate insample = _n <= 1000
keep if insample==1

preserve
	drop _all
	use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta"
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform perc
	global lagDV l.logdefuncionespc
		xtset inegi year

	keep $outcome $saturated $controls $other
	areg  $outcome  $saturated $controls $lagDV  i.year, a(inegi) vce(cluster estado)
		foreach i in lag_7{
	sum perc if `i'_2017==1, meanonly
	local a = r(mean)
	sum perc if `i'_2018==1, meanonly
	local b = r(mean)
	di (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	test (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')=0
	glo beta_`i': di %5.4f (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	local beta_`i' = ${beta_`i'}
	di ${beta_`i'}
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
	glo beta_`i': di %5.4f (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	local beta_`i' = ${beta_`i'}
	di ${beta_`i'}
	}	

	foreach i in lag_5{
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
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	local beta_`i' = ${beta_`i'}
	di ${beta_`i'}
	}
	
	foreach i in lag_4{
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
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	local beta_`i' = ${beta_`i'}
	di ${beta_`i'}
	}
	
	foreach i in lag_3{
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
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	local beta_`i' = ${beta_`i'}
	di ${beta_`i'}
	}
	
	foreach i in lag_2{
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
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	local beta_`i' = ${beta_`i'}
	di ${beta_`i'}

	}
	
	foreach i in date_0{
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
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	local beta_`i' = ${beta_`i'}
	di ${beta_`i'}
	}
	
	foreach i in lead_1{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	sum perc if `i'_2017==1, meanonly
	local c = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') 
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') =0
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') 
	local beta_`i' = ${beta_`i'}
	di ${beta_`i'}
	}	
	
	foreach i in lead_2{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	sum perc if `i'_2016==1, meanonly
	local b = r(mean)
	di (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	test (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') =0
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	local beta_`i' = ${beta_`i'}
	di ${beta_`i'}
	}		

	foreach i in lead_3{
	sum perc if `i'_2015==1, meanonly
	local a = r(mean)
	di (_b[`i'_2015]*`a')
	test (_b[`i'_2015]*`a') =0
	glo beta_`i': di %5.4f (_b[`i'_2015]*`a')
	local beta_`i' = ${beta_`i'}
	di ${beta_`i'}
	}		
restore

*Create histogram:
label variable lag_7 "Lag 7"
label variable lag_6 "Lag 6"
label variable lag_5 "Lag 5"
label variable lag_4 "Lag 4"
label variable lag_3 "Lag 3"
label variable lag_2 "Lag 2"
label variable date_0 "Reform, time=0"
label variable lead_1 "Lead 1"
label variable lead_2 "Lead 2"
label variable lead_3 "Lead 3"

foreach var in lag_7 {
sum `var', meanonly
local a = r(mean)
di ${a}
graph drop _all
twoway hist `var' if `var'<1 & `var'>-1, percent xscale(range(-0.1 0.5))  bin(100) ///
title(" ", size(medium)) ///
name(`var') ///
ytitle(" ") ///
xtitle("Lag 7") ///
xline(`a',lpattern(dash))  xline(${beta_`var'}, lcolor(blue)) ///
scheme(s1color)

/* 
*ytitle("percent") ///
xtitle("estimated (weighted) beta")
|| scatteri 0 1, msymbol(i) xaxis(2) /// 
       xtitle("",axis(2))                 /// 
       xscale(range(-0.1 0.5) axis(2))       /// 
       legend(off)                      /// 
       xlab(`a' "mean simulation" ${beta_`var'} "Electoral Reform", axis(2) labsize(vsmall))	
	*/
graph export "../Figures/falsification_`var'.png", as(png) replace
graph export "../Figures/falsification_`var'.pdf", as(pdf) replace
graph export "../Figures/falsification_`var'.tif", as(tif) replace
graph save "../Figures/falsification_`var'.gph", replace
}


foreach var in lag_6 {
sum `var', meanonly
local a = r(mean)
di ${a}
graph drop _all
twoway hist `var' if `var'<1 & `var'>-1, percent xscale(range(-0.1 0.5))  bin(100) ///
title(" ", size(medium)) ///
name(`var') ///
ytitle(" ") ///
xtitle("Lag 6") ///
xline(`a',lpattern(dash))  xline(${beta_`var'}, lcolor(blue)) ///
scheme(s1color)

/* 
*ytitle("percent") ///
xtitle("estimated (weighted) beta")
|| scatteri 0 1, msymbol(i) xaxis(2) /// 
       xtitle("",axis(2))                 /// 
       xscale(range(-0.1 0.5) axis(2))       /// 
       legend(off)                      /// 
       xlab(`a' "mean simulation" ${beta_`var'} "Electoral Reform", axis(2) labsize(vsmall))	
	*/
graph export "../Figures/falsification_`var'.png", as(png) replace
graph export "../Figures/falsification_`var'.pdf", as(pdf) replace
graph export "../Figures/falsification_`var'.tif", as(tif) replace
graph save "../Figures/falsification_`var'.gph", replace


}
foreach var in lag_5 {
sum `var', meanonly
local a = r(mean)
di ${a}
graph drop _all
twoway hist `var' if `var'<1 & `var'>-1, percent xscale(range(-0.1 0.5))  bin(100) ///
title(" ", size(medium)) ///
name(`var') ///
ytitle(" ") ///
xtitle("Lag 5") ///
xline(`a',lpattern(dash))  xline(${beta_`var'}, lcolor(blue)) ///
scheme(s1color)

/* 
*ytitle("percent") ///
xtitle("estimated (weighted) beta")
|| scatteri 0 1, msymbol(i) xaxis(2) /// 
       xtitle("",axis(2))                 /// 
       xscale(range(-0.1 0.5) axis(2))       /// 
       legend(off)                      /// 
       xlab(`a' "mean simulation" ${beta_`var'} "Electoral Reform", axis(2) labsize(vsmall))	
	*/
graph export "../Figures/falsification_`var'.png", as(png) replace
graph export "../Figures/falsification_`var'.pdf", as(pdf) replace
graph export "../Figures/falsification_`var'.tif", as(tif) replace
graph save "../Figures/falsification_`var'.gph", replace

}
foreach var in lag_4 {
sum `var', meanonly
local a = r(mean)
di ${a}
graph drop _all
twoway hist `var' if `var'<1 & `var'>-1, percent xscale(range(-0.1 0.5))  bin(100) ///
title(" ", size(medium)) ///
name(`var') ///
ytitle(" ") ///
xtitle("Lag 4") ///
xline(`a',lpattern(dash))  xline(${beta_`var'}, lcolor(blue)) ///
scheme(s1color)

/* 
*ytitle("percent") ///
xtitle("estimated (weighted) beta")
|| scatteri 0 1, msymbol(i) xaxis(2) /// 
       xtitle("",axis(2))                 /// 
       xscale(range(-0.1 0.5) axis(2))       /// 
       legend(off)                      /// 
       xlab(`a' "mean simulation" ${beta_`var'} "Electoral Reform", axis(2) labsize(vsmall))	
	*/
graph export "../Figures/falsification_`var'.png", as(png) replace
graph export "../Figures/falsification_`var'.pdf", as(pdf) replace
graph export "../Figures/falsification_`var'.tif", as(tif) replace
graph save "../Figures/falsification_`var'.gph", replace

}
foreach var in lag_3 {
sum `var', meanonly
local a = r(mean)
di ${a}
graph drop _all
twoway hist `var' if `var'<1 & `var'>-1, percent xscale(range(-0.1 0.5))  bin(100) ///
title(" ", size(medium)) ///
name(`var') ///
ytitle(" ") ///
xtitle("Lag 3") ///
xline(`a',lpattern(dash))  xline(${beta_`var'}, lcolor(blue)) ///
scheme(s1color)

/* 
*ytitle("percent") ///
xtitle("estimated (weighted) beta")
|| scatteri 0 1, msymbol(i) xaxis(2) /// 
       xtitle("",axis(2))                 /// 
       xscale(range(-0.1 0.5) axis(2))       /// 
       legend(off)                      /// 
       xlab(`a' "mean simulation" ${beta_`var'} "Electoral Reform", axis(2) labsize(vsmall))	
	*/
graph export "../Figures/falsification_`var'.png", as(png) replace
graph export "../Figures/falsification_`var'.pdf", as(pdf) replace
graph export "../Figures/falsification_`var'.tif", as(tif) replace
graph save "../Figures/falsification_`var'.gph", replace

}
foreach var in lag_2 {
sum `var', meanonly
local a = r(mean)
di ${a}
graph drop _all
twoway hist `var' if `var'<1 & `var'>-1, percent xscale(range(-0.1 0.5))  bin(100) ///
title(" ", size(medium)) ///
name(`var') ///
ytitle(" ") ///
xtitle("Lag 2") ///
xline(`a',lpattern(dash))  xline(${beta_`var'}, lcolor(blue)) ///
scheme(s1color)

/* 
*ytitle("percent") ///
xtitle("estimated (weighted) beta")
|| scatteri 0 1, msymbol(i) xaxis(2) /// 
       xtitle("",axis(2))                 /// 
       xscale(range(-0.1 0.5) axis(2))       /// 
       legend(off)                      /// 
       xlab(`a' "mean simulation" ${beta_`var'} "Electoral Reform", axis(2) labsize(vsmall))	
	*/
graph export "../Figures/falsification_`var'.png", as(png) replace
graph export "../Figures/falsification_`var'.pdf", as(pdf) replace
graph export "../Figures/falsification_`var'.tif", as(tif) replace
graph save "../Figures/falsification_`var'.gph", replace

}
foreach var in date_0 {
sum `var', meanonly
local a = r(mean)
di ${a}
graph drop _all
twoway hist `var' if `var'<1 & `var'>-1, percent xscale(range(-0.1 0.5))  bin(100) ///
title(" ", size(medium)) ///
name(`var') ///
ytitle(" ") ///
xtitle("Reform, time=0") ///
xline(`a',lpattern(dash))  xline(${beta_`var'}, lcolor(blue)) ///
scheme(s1color)

/* 
*ytitle("percent") ///
xtitle("estimated (weighted) beta")
|| scatteri 0 1, msymbol(i) xaxis(2) /// 
       xtitle("",axis(2))                 /// 
       xscale(range(-0.1 0.5) axis(2))       /// 
       legend(off)                      /// 
       xlab(`a' "mean simulation" ${beta_`var'} "Electoral Reform", axis(2) labsize(vsmall))	
	*/
graph export "../Figures/falsification_`var'.png", as(png) replace
graph export "../Figures/falsification_`var'.pdf", as(pdf) replace
graph export "../Figures/falsification_`var'.tif", as(tif) replace
graph save "../Figures/falsification_`var'.gph", replace

}
foreach var in lead_1 {
sum `var', meanonly
local a = r(mean)
di ${a}
graph drop _all
twoway hist `var' if `var'<1 & `var'>-1, percent xscale(range(-0.1 0.5))  bin(100) ///
title(" ", size(medium)) ///
name(`var') ///
ytitle(" ") ///
xtitle("Lead 1") ///
xline(`a',lpattern(dash))  xline(${beta_`var'}, lcolor(blue)) ///
scheme(s1color)

/* 
*ytitle("percent") ///
xtitle("estimated (weighted) beta")
|| scatteri 0 1, msymbol(i) xaxis(2) /// 
       xtitle("",axis(2))                 /// 
       xscale(range(-0.1 0.5) axis(2))       /// 
       legend(off)                      /// 
       xlab(`a' "mean simulation" ${beta_`var'} "Electoral Reform", axis(2) labsize(vsmall))	
	*/
graph export "../Figures/falsification_`var'.png", as(png) replace
graph export "../Figures/falsification_`var'.pdf", as(pdf) replace
graph export "../Figures/falsification_`var'.tif", as(tif) replace
graph save "../Figures/falsification_`var'.gph", replace

}
foreach var in lead_2 {
sum `var', meanonly
local a = r(mean)
di ${a}
graph drop _all
twoway hist `var' if `var'<1 & `var'>-1, percent xscale(range(-0.1 0.5))  bin(100) ///
title(" ", size(medium)) ///
name(`var') ///
ytitle(" ") ///
xtitle("Lead 2") ///
xline(`a',lpattern(dash))  xline(${beta_`var'}, lcolor(blue)) ///
scheme(s1color)

/* 
*ytitle("percent") ///
xtitle("estimated (weighted) beta")
|| scatteri 0 1, msymbol(i) xaxis(2) /// 
       xtitle("",axis(2))                 /// 
       xscale(range(-0.1 0.5) axis(2))       /// 
       legend(off)                      /// 
       xlab(`a' "mean simulation" ${beta_`var'} "Electoral Reform", axis(2) labsize(vsmall))	
	*/
graph export "../Figures/falsification_`var'.png", as(png) replace
graph export "../Figures/falsification_`var'.pdf", as(pdf) replace
graph export "../Figures/falsification_`var'.tif", as(tif) replace
graph save "../Figures/falsification_`var'.gph", replace

}
foreach var in lead_3 {
sum `var', meanonly
local a = r(mean)
di ${a}
graph drop _all
twoway hist `var' if `var'<1 & `var'>-1, percent xscale(range(-0.1 0.5))  bin(100) ///
title(" ", size(medium)) ///
name(`var') ///
ytitle(" ") ///
xtitle("Lead 3") ///
xline(`a',lpattern(dash))  xline(${beta_`var'}, lcolor(blue)) ///
scheme(s1color)

/* 
*ytitle("percent") ///
xtitle("estimated (weighted) beta")
|| scatteri 0 1, msymbol(i) xaxis(2) /// 
       xtitle("",axis(2))                 /// 
       xscale(range(-0.1 0.5) axis(2))       /// 
       legend(off)                      /// 
       xlab(`a' "mean simulation" ${beta_`var'} "Electoral Reform", axis(2) labsize(vsmall))	
	*/
graph export "../Figures/falsification_`var'.png", as(png) replace
graph export "../Figures/falsification_`var'.pdf", as(pdf) replace
graph export "../Figures/falsification_`var'.tif", as(tif) replace
graph save "../Figures/falsification_`var'.gph", replace

}




*Combine 
*Pre treatment:
graph combine  "../Figures/falsification_lag_7.gph" ///
 "../Figures/falsification_lag_6.gph" "../Figures/falsification_lag_5.gph" "../Figures/falsification_lag_4.gph" ///
 "../Figures/falsification_lag_3.gph" "../Figures/falsification_lag_2.gph", subtitle(" ")   ///
scheme(s1color)  imargin(vsmall) xcommon col(3) l1(percent) b1(estimated beta (cohort weighted))
graph export "../Figures/falsification_pre2.png", as(png) replace
graph export "../Figures/falsification_pre2.pdf", as(pdf) replace
graph export "../Figures/falsification_pre2.tif", as(tif) replace

*Post treatment:
graph combine "../Figures/falsification_date_0.gph" "../Figures/falsification_lead_1.gph" ///
 "../Figures/falsification_lead_2.gph" "../Figures/falsification_lead_3.gph", subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) xcommon col(2) l1(percent) b1(estimated beta (cohort weighted))
graph export "../Figures/falsification_post.png", as(png) replace
graph export "../Figures/falsification_post.pdf", as(pdf) replace
graph export "../Figures/falsification_post.tif", as(tif) replace

*All
graph combine  "../Figures/falsification_lag_7.gph" ///
 "../Figures/falsification_lag_6.gph" "../Figures/falsification_lag_5.gph" "../Figures/falsification_lag_4.gph" ///
 "../Figures/falsification_lag_3.gph" "../Figures/falsification_lag_2.gph" "../Figures/falsification_date_0.gph" ///
 "../Figures/falsification_lead_1.gph" "../Figures/falsification_lead_2.gph" "../Figures/falsification_lead_3.gph", ///
 subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) xcommon col(3) l1(percent) b1(estimated (cohort weighted) beta)
graph export "../Figures/falsification_all.png", as(png) replace
graph export "../Figures/falsification_all.pdf", as(pdf) replace
graph export "../Figures/falsification_all.tif", as(tif) replace


**********************************************
*Figure: EFFECT OF REFORM ON HOMICIDES, WITH MANDO UNICO SPLIT
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
	
graph drop _all

	gen acuerdo_pre=.
	replace acuerdo_pre=1 if acuerdo==1 & year==2014
	replace acuerdo_pre=0 if acuerdo==0 & year==2014
	xfill acuerdo_pre, i(inegi)

	gen acuerdo2_pre=.
	replace acuerdo2_pre=1 if acuerdo2==1 & year==2014
	replace acuerdo2_pre=0 if acuerdo2==0 & year==2014
	xfill acuerdo2_pre, i(inegi)

	*global mandounico  acuerdo
	*global mandounico  acuerdo2
	*global mandounico  acuerdo_pre
	global mandounico  acuerdo2_pre

************
***A: log(homicides per capita) with covariates
************	

est clear

***estimate linear combination by lead/lag:
foreach i in lag_7{
	areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==1, a(inegi) vce(cluster estado)
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
	glo b_`i'_log: di %5.4f r(estimate)
	glo se_`i'_log: di %5.4f r(se)
	glo lb_`i'_log: di %5.4f r(lb)
	glo ub_`i'_log: di %5.4f r(ub)
	lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	est sto `i'
}
foreach i in lag_6{
	areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==1, a(inegi) vce(cluster estado)
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
	lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	est sto `i'
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==1, a(inegi) vce(cluster estado)

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
	lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	est sto `i'

}


foreach i in lead_1{
	areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==1, a(inegi) vce(cluster estado)

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
    lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	est sto `i'

}


foreach i in lead_2{
	areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==1, a(inegi) vce(cluster estado)
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
	lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	est sto `i'

}


foreach i in lead_3{
	areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==1, a(inegi) vce(cluster estado)
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
	lincomest (_b[`i'_2015]*`a')
	est sto `i'

}


*Figure
*ssc install coefplot
			   
coefplot (lag_7, rename((1) = t-7) mcolor(red) ciopts(color(black))) ///
 (lag_6, rename((1) = t-6) mcolor(red) ciopts(color(black))) ///
 (lag_5, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
 (lag_4, rename((1) = t-4) mcolor(red) ciopts(color(black))) ///
 (lag_3, rename((1) = t-3) mcolor(red) ciopts(color(black))) ///
 (lag_2, rename((1) = t-2) mcolor(red) ciopts(color(black))) ///
 (date_0, rename((1) = Reform (t=0)) mcolor(green) ciopts(color(black))) ///
 (lead_1, rename((1) = t+1) mcolor(green) ciopts(color(black))) ///
 (lead_2, rename((1) = t+2) mcolor(green) ciopts(color(black))) ///
 (lead_3, rename((1) = t+3) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color) legend(off) yline(0)    ///
ytitle("") ///
subtitle("(a) w/ Security Coop. Agreement")
graph export "../Figures/event_study_log_wacuerdo2.png", as(png) replace
graph export "../Figures/event_study_log_wacuerdo2.pdf", as(pdf) replace
graph export "../Figures/event_study_log_wacuerdo2.tif", as(tif) replace
graph save "../Figures/event_study_log_wacuerdo2.gph", replace
************
***B: ihs(homicides per capita) with covariates
************	

*est clear
***estimate linear combination by lead/lag:

foreach i in lag_7{
areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==0, a(inegi) vce(cluster estado)
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
	glo b_`i'_log: di %5.4f r(estimate)
	glo se_`i'_log: di %5.4f r(se)
	glo lb_`i'_log: di %5.4f r(lb)
	glo ub_`i'_log: di %5.4f r(ub)
	lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	est sto `i'_2

}
foreach i in lag_6{
areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==0, a(inegi) vce(cluster estado)
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
	 lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	est sto `i'_2

}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==0, a(inegi) vce(cluster estado)

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
	lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	est sto `i'_2

}


foreach i in lead_1{
areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==0, a(inegi) vce(cluster estado)

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
     lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	est sto `i'_2

}


foreach i in lead_2{
areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==0, a(inegi) vce(cluster estado)
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
	lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	est sto `i'_2

}


foreach i in lead_3{
areg  logdefuncionespc  $saturated $lagDV  $controls i.year if $mandounico==0, a(inegi) vce(cluster estado)
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
	lincomest (_b[`i'_2015]*`a')
	est sto `i'_2

}

*Figure
*ssc install coefplot
			   
coefplot (lag_7_2, rename((1) = t-7) mcolor(red) ciopts(color(black))) ///
 (lag_6_2, rename((1) = t-6) mcolor(red) ciopts(color(black))) ///
 (lag_5_2, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
 (lag_4_2, rename((1) = t-4) mcolor(red) ciopts(color(black))) ///
 (lag_3_2, rename((1) = t-3) mcolor(red) ciopts(color(black))) ///
 (lag_2_2, rename((1) = t-2) mcolor(red) ciopts(color(black))) ///
 (date_0_2, rename((1) = Reform (t=0)) mcolor(green) ciopts(color(black))) ///
 (lead_1_2, rename((1) = t+1) mcolor(green) ciopts(color(black))) ///
 (lead_2_2, rename((1) = t+2) mcolor(green) ciopts(color(black))) ///
 (lead_3_2, rename((1) = t+3) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color) legend(off) yline(0)    ///
ytitle("") name(ihshomicides) ///
subtitle("(b) w/o Security Coop. Agreement")
graph export "../Figures/event_study_log_wacuerdo2placebo.png", as(png) replace
graph export "../Figures/event_study_log_wacuerdo2placebo.pdf", as(pdf) replace
graph export "../Figures/event_study_log_wacuerdo2placebo.tif", as(tif) replace
graph save "../Figures/event_study_log_wacuerdo2placebo.gph", replace
restore

*Combine
graph combine  "../Figures/event_study_log_wacuerdo2.gph" "../Figures/event_study_log_wacuerdo2placebo.gph", ///
 subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) ycommon col(1) b1(timing) l1(estimate: log(homicides per capita))
graph export "../Figures/event_study_wacuerdo2_placebo.png", as(png) replace
graph export "../Figures/event_study_wacuerdo2_placebo.pdf", as(pdf) replace
graph export "../Figures/event_study_wacuerdo2_placebo.tif", as(tif) replace


*COMBINED
use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV l.logdefuncionespc
	global lagDV2 l.ihs_defuncionespc
	xtset inegi year 
	
graph drop _all

************
***A: log(homicides per capita) with covariates
************	

foreach var in logdefuncionespc{
global lagDV l.`var'
est clear
foreach i in lag_7{
	areg  `var'  $saturated $lagDV  $controls i.year if $mandounico==1, a(inegi) vce(cluster estado)
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
	glo b_`i'_log: di %5.4f r(estimate)
	glo se_`i'_log: di %5.4f r(se)
	glo lb_`i'_log: di %5.4f r(lb)
	glo ub_`i'_log: di %5.4f r(ub)
	*plotbeta ${b_`i'_log}, level(90)
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	*est store `i'

}
foreach i in lag_6{
areg  `var'  $saturated $lagDV  $controls i.year if mandounico==1, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')

}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  `var'  $saturated $lagDV  $controls i.year if mandounico==1, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')

}


foreach i in lead_1{
areg  `var'  $saturated $lagDV  $controls i.year if mandounico==1, a(inegi) vce(cluster estado)

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
    eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')

}


foreach i in lead_2{
areg  `var'  $saturated $lagDV  $controls i.year if mandounico==1, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')

}


foreach i in lead_3{
areg  `var'  $saturated $lagDV  $controls i.year if mandounico==1, a(inegi) vce(cluster estado)
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






************
***B: ihs(homicides per capita) with covariates
************	
foreach i in lag_7{
areg  `var'  $saturated $lagDV  $controls i.year if mandounico==0, a(inegi) vce(cluster estado)
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
	glo b_`i'_log: di %5.4f r(estimate)
	glo se_`i'_log: di %5.4f r(se)
	glo lb_`i'_log: di %5.4f r(lb)
	glo ub_`i'_log: di %5.4f r(ub)
	*plotbeta ${b_`i'_log}, level(90)
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	*est store `i'

}
foreach i in lag_6{
areg  `var'  $saturated $lagDV  $controls i.year if mandounico==0, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')

}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  `var'  $saturated $lagDV  $controls i.year if mandounico==0, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')

}


foreach i in lead_1{
areg  `var'  $saturated $lagDV  $controls i.year if mandounico==0, a(inegi) vce(cluster estado)

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
    eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')

}


foreach i in lead_2{
areg  `var'  $saturated $lagDV  $controls i.year if mandounico==0, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')

}


foreach i in lead_3{
areg  `var'  $saturated $lagDV  $controls i.year if mandounico==0, a(inegi) vce(cluster estado)
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







}
coefplot (est1, rename((1) = t-7) msize(large) mcolor(red) ciopts(color(black))) ///
 (est11, rename((1) = t-7) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est2, rename((1) = t-6) msize(large) mcolor(red) ciopts(color(black))) ///
  (est12, rename((1) = t-6) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est3, rename((1) = t-5) msize(large) mcolor(red) ciopts(color(black))) ///
  (est13, rename((1) = t-5) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est4, rename((1) = t-4) msize(large) mcolor(red) ciopts(color(black))) ///
  (est14, rename((1) = t-4) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est5, rename((1) = t-3) msize(large) mcolor(red) ciopts(color(black))) ///
  (est15, rename((1) = t-3) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est6, rename((1) = t-2) msize(large) mcolor(red) ciopts(color(black))) ///
  (est16, rename((1) = t-2) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est7, rename((1) = Reform (t=0)) msize(large) mcolor(green) ciopts(color(black))) ///
  (est17, rename((1) = Reform (t=0)) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est8, rename((1) = t+1) msize(large) mcolor(green) ciopts(color(black) )) ///
  (est18, rename((1) = t+1) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est9, rename((1) = t+2) msize(large) mcolor(green) ciopts(color(black))) ///
  (est19, rename((1) = t+2) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est10, rename((1) = t+3) msize(large) mcolor(green) ciopts(color(black))) ///
  (est20, rename((1) = t+3) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle("estimate: log(homicides per capita)")  xtitle("timing") ///
legend(order(1 "w/ Security Coop. Agreement" 11 "w/o Security Coop. Agreement")  )

graph export "../Figures/event_study_w&wo_placebo.png", as(png) replace
graph export "../Figures/event_study_w&wo_placebo.pdf", as(pdf) replace
graph export "../Figures/event_study_w&wo_placebo.tif", as(tif) replace
graph save "../Figures/event_study_w&wo_placebo.gph", replace

**********************************************
*Figure: EFFECT OF REFORM ON HOMICIDES, WITH LAGGED DV & COHORT WEIGHTS
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
*A)Linear 
*------------------
preserve
est clear

foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


***estimate linear combination by lead/lag:


foreach i in lag_5 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


}
}

foreach j in inc_party_won{
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


quietly areg  incumbent_yesterday_w_tomorrow2  $saturated pol`pol' $interacted_pol2 $allcov2 logdefuncionespc  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
keep if e(sample)==1



		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_5 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in date_0 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
			}


}
}


*Figure
*ssc install coefplot
			   
coefplot (est1, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
 (est2, rename((1) = t-4) mcolor(red) ciopts(color(black))) ///
 (est3, rename((1) = t-3) mcolor(red) ciopts(color(black))) ///
 (est4, rename((1) = Reform (t=0)) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color) yline(0)    ///
ytitle("") ///
subtitle("linear polynomial") ///
legend(off) ///
legend(order(13 "Incumbent at t-1 won at t+1" 3 "Incumbent at t won at t+1")  )

graph export "../Figures/incumbency_advantage_linear.png", as(png) replace
graph export "../Figures/incumbency_advantage_linear.pdf", as(pdf) replace
graph export "../Figures/incumbency_advantage_linear.tif", as(tif) replace
graph save "../Figures/incumbency_advantage_linear.gph", replace
restore

*------------------
*B)Quadratic 
*------------------
preserve
est clear

foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 2 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


***estimate linear combination by lead/lag:


foreach i in lag_5 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


}
}

foreach j in inc_party_won{
foreach pol in 2 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


quietly areg  incumbent_yesterday_w_tomorrow2  $saturated pol`pol' $interacted_pol2 $allcov2 logdefuncionespc  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
keep if e(sample)==1



		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_5 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in date_0 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
			}


}
}


*Figure
*ssc install coefplot
			   
coefplot (est1, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
 (est2, rename((1) = t-4) mcolor(red) ciopts(color(black))) ///
 (est3, rename((1) = t-3) mcolor(red) ciopts(color(black))) ///
 (est4, rename((1) = Reform (t=0)) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color) yline(0)    ///
ytitle("") ///
subtitle("quadratic polynomial") ///
legend(off) ///
legend(order(13 "Incumbent at t-1 won at t+1" 3 "Incumbent at t won at t+1")  )

graph export "../Figures/incumbency_advantage_quadratic.png", as(png) replace
graph export "../Figures/incumbency_advantage_quadratic.pdf", as(pdf) replace
graph export "../Figures/incumbency_advantage_quadratic.tif", as(tif) replace
graph save "../Figures/incumbency_advantage_quadratic.gph", replace
restore

*------------------
*C. Cubic polynomial
*------------------
preserve
est clear

foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 3 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


***estimate linear combination by lead/lag:


foreach i in lag_5 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


}
}

foreach j in inc_party_won{
foreach pol in 3 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


quietly areg  incumbent_yesterday_w_tomorrow2  $saturated pol`pol' $interacted_pol2 $allcov2 logdefuncionespc  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
keep if e(sample)==1



		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_5 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in date_0 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
			}


}
}


*Figure
*ssc install coefplot
			   
coefplot (est1, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
 (est2, rename((1) = t-4) mcolor(red) ciopts(color(black))) ///
 (est3, rename((1) = t-3) mcolor(red) ciopts(color(black))) ///
 (est4, rename((1) = Reform (t=0)) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color) yline(0)    ///
ytitle("") ///
subtitle("cubic polynomial") ///
legend(off) ///
legend(order(13 "Incumbent at t-1 won at t+1" 3 "Incumbent at t won at t+1")  )

graph export "../Figures/incumbency_advantage_cubic.png", as(png) replace
graph export "../Figures/incumbency_advantage_cubic.pdf", as(pdf) replace
graph export "../Figures/incumbency_advantage_cubic.tif", as(tif) replace
graph save "../Figures/incumbency_advantage_cubic.gph", replace
restore

*------------------
*D. Cubic polynomial
*------------------
preserve
est clear

foreach j in incumbent_yesterday_w_tomorrow2 {
foreach pol in 4{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


***estimate linear combination by lead/lag:


foreach i in lag_5 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


}
}

foreach j in inc_party_won{
foreach pol in 4 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


quietly areg  incumbent_yesterday_w_tomorrow2  $saturated pol`pol' $interacted_pol2 $allcov2 logdefuncionespc  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
keep if e(sample)==1



		glo r2_ihsdet2_`pol':  di %5.4f e(r2)
		glo N_ihsdet2_`pol': di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_5 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
}

foreach i in date_0 {
quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet2_`pol': di %5.4f r(se)
			}


}
}


*Figure
*ssc install coefplot
			   
coefplot (est1, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
 (est2, rename((1) = t-4) mcolor(red) ciopts(color(black))) ///
 (est3, rename((1) = t-3) mcolor(red) ciopts(color(black))) ///
 (est4, rename((1) = Reform (t=0)) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color) yline(0)    ///
ytitle("") ///
subtitle("quartic polynomial") ///
legend(off) ///
legend(order(13 "Incumbent at t-1 won at t+1" 3 "Incumbent at t won at t+1")  )

graph export "../Figures/incumbency_advantage_quartic.png", as(png) replace
graph export "../Figures/incumbency_advantage_quartic.pdf", as(pdf) replace
graph export "../Figures/incumbency_advantage_quartic.tif", as(tif) replace
graph save "../Figures/incumbency_advantage_quartic.gph", replace
restore


*Combine
graph combine "../Figures/incumbency_advantage_linear.gph" "../Figures/incumbency_advantage_quadratic.gph" "../Figures/incumbency_advantage_cubic.gph" "../Figures/incumbency_advantage_quartic.gph", ///
 subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) ycommon col(2) l1(estimate: probability of victory at t+1)
graph export "../Figures/incumbency_advantage_234.png", as(png) replace
graph export "../Figures/incumbency_advantage_234.pdf", as(pdf) replace
graph export "../Figures/incumbency_advantage_234.tif", as(tif) replace

/*grc1leg2 "../Figures/incumbency_advantage_linear.gph" "../Figures/incumbency_advantage_quadratic.gph" "../Figures/incumbency_advantage_cubic.gph" "../Figures/incumbency_advantage_quartic.gph", ///
 subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) ycommon col(2)  l1(estimate: probability of victory at t+1)
graph export "../Figures/incumbency_advantage_234.png", as(png) replace
graph export "../Figures/incumbency_advantage_234.pdf", as(pdf) replace
graph export "../Figures/incumbency_advantage_234.tif", as(tif) replace
*/

**********************************************
*Figure: Mechanisms: ARMY AND LOCAL POLICE EFFORT
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
areg  logdetenidospc  $saturated $allcov2 $DV  i.year, a(inegi) vce(cluster inegi)
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
*est clear
areg  logdetenidos_2pc  $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihsdet:  di %5.4f e(r2)
		glo N_ihsdet: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
areg  logdetenidos_2pc  $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_ihsdet: di %5.4f r(se)
			
}
	
foreach i in lag_6{
areg  logdetenidos_2pc  $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  logdetenidos_2pc  $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_1{
areg  logdetenidos_2pc  $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_2{
areg  logdetenidos_2pc  $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_ihsdet: di %5.4f r(se)
}


foreach i in lead_3{
areg  logdetenidos_2pc  $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a') 
	glo se_`i'_ihsdet: di %5.4f r(se)
}

******************
*3)LOG HEROINE KG:
******************
*est clear
areg  logheroina_kg   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
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
*est clear
areg  logheroina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_her_2:  di %5.4f e(r2)
		glo N_her_2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
areg  logheroina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_her2: di %5.4f r(se)
			
}
	
foreach i in lag_6{
areg  logheroina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_her2: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  logheroina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_her2: di %5.4f r(se)
}


foreach i in lead_1{
areg  logheroina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_her2: di %5.4f r(se)
}


foreach i in lead_2{
areg  logheroina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_her2: di %5.4f r(se)
}


foreach i in lead_3{
areg  logheroina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a') 
	glo se_`i'_her2: di %5.4f r(se)
}
******************
*5)LOG METANPHETAMINE KG:
******************
*est clear
areg  logmetanfetamina_kg   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
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
*est clear
areg  logmetanfetamina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_met_2:  di %5.4f e(r2)
		glo N_met_2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
areg  logmetanfetamina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_met2: di %5.4f r(se)
			
}
	
foreach i in lag_6{
areg  logmetanfetamina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_met2: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  logmetanfetamina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_met2: di %5.4f r(se)
}


foreach i in lead_1{
areg  logmetanfetamina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_met2: di %5.4f r(se)
}


foreach i in lead_2{
areg  logmetanfetamina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_met2: di %5.4f r(se)
}


foreach i in lead_3{
areg  logmetanfetamina_kg_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a') 
	glo se_`i'_met2: di %5.4f r(se)
}

******************
*7)LOG LABORATORIO:
******************
*est clear
areg  loglaboratorio   $saturated $allcov2  $DV  i.year, a(inegi) vce(cluster inegi)
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
*est clear
areg  loglaboratorio_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_lab_2:  di %5.4f e(r2)
		glo N_lab_2: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
areg  loglaboratorio_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b') 
	glo se_`i'_lab2: di %5.4f r(se)
			
}
	
foreach i in lag_6{
areg  loglaboratorio_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c') 
	glo se_`i'_lab2: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  loglaboratorio_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d') 
	glo se_`i'_lab2: di %5.4f r(se)
}


foreach i in lead_1{
areg  loglaboratorio_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  
	glo se_`i'_lab2: di %5.4f r(se)
}


foreach i in lead_2{
areg  loglaboratorio_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') 
	glo se_`i'_lab2: di %5.4f r(se)
}


foreach i in lead_3{
areg  loglaboratorio_2   $saturated $allcov2 $DV   i.year, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a') 
	glo se_`i'_lab2: di %5.4f r(se)
}











coefplot (est1, rename((1) = t-7) msize(large) mcolor(red) ciopts(color(black))) ///
 (est2, rename((1) = t-6) msize(large) mcolor(red) ciopts(color(black))) ///
 (est3, rename((1) = t-5) msize(large) mcolor(red) ciopts(color(black))) ///
 (est4, rename((1) = t-4) msize(large) mcolor(red) ciopts(color(black))) ///
 (est5, rename((1) = t-3) msize(large) mcolor(red) ciopts(color(black))) ///
 (est6, rename((1) = t-2) msize(large) mcolor(red) ciopts(color(black))) ///
 (est7, rename((1) = Reform (t=0)) msize(large) mcolor(green) ciopts(color(black))) ///
 (est8, rename((1) = t+1) msize(large) mcolor(green) ciopts(color(black) )) ///
 (est9, rename((1) = t+2) msize(large) mcolor(green) ciopts(color(black))) ///
 (est10, rename((1) = t+3) msize(large) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle("estimate")  xtitle("") ///
subtitle("log(detained per capita)") legend(off)
graph export "../Figures/detained.png", as(png) replace
graph export "../Figures/detained.pdf", as(pdf) replace
graph export "../Figures/detained.tif", as(tif) replace
graph save "../Figures/detained.gph", replace

coefplot (est11, rename((1) = t-7) msize(large) mcolor(red) ciopts(color(black))) ///
 (est12, rename((1) = t-6) msize(large) mcolor(red) ciopts(color(black))) ///
 (est13, rename((1) = t-5) msize(large) mcolor(red) ciopts(color(black))) ///
 (est14, rename((1) = t-4) msize(large) mcolor(red) ciopts(color(black))) ///
 (est15, rename((1) = t-3) msize(large) mcolor(red) ciopts(color(black))) ///
 (est16, rename((1) = t-2) msize(large) mcolor(red) ciopts(color(black))) ///
 (est17, rename((1) = Reform (t=0)) msize(large) mcolor(green) ciopts(color(black))) ///
 (est18, rename((1) = t+1) msize(large) mcolor(green) ciopts(color(black) )) ///
 (est19, rename((1) = t+2) msize(large) mcolor(green) ciopts(color(black))) ///
 (est20, rename((1) = t+3) msize(large) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle("")  xtitle("") ///
subtitle("heroine erradicated (kg)") legend(off)
graph export "../Figures/heroine.png", as(png) replace
graph export "../Figures/heroine.pdf", as(pdf) replace
graph export "../Figures/heroine.tif", as(tif) replace
graph save "../Figures/heroine.gph", replace

coefplot (est21, rename((1) = t-7) msize(large) mcolor(red) ciopts(color(black))) ///
 (est22, rename((1) = t-6) msize(large) mcolor(red) ciopts(color(black))) ///
 (est23, rename((1) = t-5) msize(large) mcolor(red) ciopts(color(black))) ///
 (est24, rename((1) = t-4) msize(large) mcolor(red) ciopts(color(black))) ///
 (est25, rename((1) = t-3) msize(large) mcolor(red) ciopts(color(black))) ///
 (est26, rename((1) = t-2) msize(large) mcolor(red) ciopts(color(black))) ///
 (est27, rename((1) = Reform (t=0)) msize(large) mcolor(green) ciopts(color(black))) ///
 (est28, rename((1) = t+1) msize(large) mcolor(green) ciopts(color(black) )) ///
 (est29, rename((1) = t+2) msize(large) mcolor(green) ciopts(color(black))) ///
 (est30, rename((1) = t+3) msize(large) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle("estimate")  xtitle("") ///
subtitle("methamphetamine erradicated (kg)") legend(off)
graph export "../Figures/methamphetamine.png", as(png) replace
graph export "../Figures/methamphetamine.pdf", as(pdf) replace
graph export "../Figures/methamphetamine.tif", as(tif) replace
graph save "../Figures/methamphetamine.gph", replace

coefplot (est31, rename((1) = t-7) msize(large) mcolor(red) ciopts(color(black))) ///
 (est32, rename((1) = t-6) msize(large) mcolor(red) ciopts(color(black))) ///
 (est33, rename((1) = t-5) msize(large) mcolor(red) ciopts(color(black))) ///
 (est34, rename((1) = t-4) msize(large) mcolor(red) ciopts(color(black))) ///
 (est35, rename((1) = t-3) msize(large) mcolor(red) ciopts(color(black))) ///
 (est36, rename((1) = t-2) msize(large) mcolor(red) ciopts(color(black))) ///
 (est37, rename((1) = Reform (t=0)) msize(large) mcolor(green) ciopts(color(black))) ///
 (est38, rename((1) = t+1) msize(large) mcolor(green) ciopts(color(black) )) ///
 (est39, rename((1) = t+2) msize(large) mcolor(green) ciopts(color(black))) ///
 (est40, rename((1) = t+3) msize(large) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle("")  xtitle("") ///
subtitle("laboratories destroyed") legend(off)
graph export "../Figures/laboratories.png", as(png) replace
graph export "../Figures/laboratories.pdf", as(pdf) replace
graph export "../Figures/laboratories.tif", as(tif) replace
graph save "../Figures/laboratories.gph", replace


*Combine
graph combine  "../Figures/detained.gph"  "../Figures/heroine.gph" "../Figures/methamphetamine.gph" "../Figures/laboratories.gph", ///
 subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) col(2)  
graph export "../Figures/effort_security.png", as(png) replace
graph export "../Figures/effort_security.pdf", as(pdf) replace
graph export "../Figures/effort_security.tif", as(tif) replace


**********************************************
*Figure: EFFECT OF REFORM ON POPULATION
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

gen pop_k=pop/1000

foreach j in incumbent_yesterday_w_tomorrow2{
foreach pol in 2 {

quietly areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
keep if e(sample)==1
}
}

*------------------
*A)Linear 
*------------------
preserve
est clear

foreach j in pop_k {
foreach pol in 1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


***estimate linear combination by lead/lag:


foreach i in lag_5 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


}
}

*Figure
*ssc install coefplot
			   
coefplot (est1, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
 (est2, rename((1) = t-4) mcolor(red) ciopts(color(black))) ///
 (est3, rename((1) = t-3) mcolor(red) ciopts(color(black))) ///
 (est4, rename((1) = Reform (t=0)) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color) yline(0)    ///
ytitle("") ///
subtitle("linear polynomial") ///
legend(off) ///
legend(order(13 "Incumbent at t-1 won at t+1" 3 "Incumbent at t won at t+1")  )

graph export "../Figures/pop_linear.png", as(png) replace
graph export "../Figures/pop_linear.pdf", as(pdf) replace
graph export "../Figures/pop_linear.tif", as(tif) replace
graph save "../Figures/pop_linear.gph", replace
restore

*------------------
*B)Quadratic 
*------------------
preserve
est clear

foreach j in pop_k {
foreach pol in 2 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


***estimate linear combination by lead/lag:


foreach i in lag_5 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


}
}


*Figure
*ssc install coefplot
			   
coefplot (est1, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
 (est2, rename((1) = t-4) mcolor(red) ciopts(color(black))) ///
 (est3, rename((1) = t-3) mcolor(red) ciopts(color(black))) ///
 (est4, rename((1) = Reform (t=0)) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color) yline(0)    ///
ytitle("") ///
subtitle("quadratic polynomial") ///
legend(off) ///
legend(order(13 "Incumbent at t-1 won at t+1" 3 "Incumbent at t won at t+1")  )

graph export "../Figures/pop_quadratic.png", as(png) replace
graph export "../Figures/pop_quadratic.pdf", as(pdf) replace
graph export "../Figures/pop_quadratic.tif", as(tif) replace
graph save "../Figures/pop_quadratic.gph", replace
restore

*------------------
*C. Cubic polynomial
*------------------
preserve
est clear

foreach j in pop_k {
foreach pol in 3 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


***estimate linear combination by lead/lag:


foreach i in lag_5 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


}
}


*Figure
*ssc install coefplot
			   
coefplot (est1, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
 (est2, rename((1) = t-4) mcolor(red) ciopts(color(black))) ///
 (est3, rename((1) = t-3) mcolor(red) ciopts(color(black))) ///
 (est4, rename((1) = Reform (t=0)) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color) yline(0)    ///
ytitle("") ///
subtitle("cubic polynomial") ///
legend(off) ///
legend(order(13 "Incumbent at t-1 won at t+1" 3 "Incumbent at t won at t+1")  )

graph export "../Figures/pop_cubic.png", as(png) replace
graph export "../Figures/pop_cubic.pdf", as(pdf) replace
graph export "../Figures/pop_cubic.tif", as(tif) replace
graph save "../Figures/pop_cubic.gph", replace
restore

*------------------
*D. Cubic polynomial
*------------------
preserve
est clear

foreach j in pop_k {
foreach pol in 4{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT) 


***estimate linear combination by lead/lag:


foreach i in lag_5 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2018]*`c') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_4 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)
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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in lag_3 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b') +(_b[`i'_2018]*`c')
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
}

foreach i in date_0 {
areg  `j'  $saturated pol`pol' $interacted_pol2 $allcov2 $DV  i.year if mv_incparty<${optimal} & mv_incparty>-${optimal}, a(inegi) vce(cluster inegi)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')  +(_b[`i'_2018]*`d') 
	glo se_`i'_ihsdet_`pol': di %5.4f r(se)
			}


}
}




*Figure
*ssc install coefplot
			   
coefplot (est1, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
 (est2, rename((1) = t-4) mcolor(red) ciopts(color(black))) ///
 (est3, rename((1) = t-3) mcolor(red) ciopts(color(black))) ///
 (est4, rename((1) = Reform (t=0)) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color) yline(0)    ///
ytitle("") ///
subtitle("quartic polynomial") ///
legend(off) ///
legend(order(13 "Incumbent at t-1 won at t+1" 3 "Incumbent at t won at t+1")  )

graph export "../Figures/pop_quartic.png", as(png) replace
graph export "../Figures/pop_quartic.pdf", as(pdf) replace
graph export "../Figures/pop_quartic.tif", as(tif) replace
graph save "../Figures/pop_quartic.gph", replace
restore


*Combine
graph combine "../Figures/pop_linear.gph" "../Figures/pop_quadratic.gph" "../Figures/pop_cubic.gph" "../Figures/pop_quartic.gph", ///
 subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) ycommon col(2) l1(estimate: population('000))
graph export "../Figures/pop.png", as(png) replace
graph export "../Figures/pop.pdf", as(pdf) replace
graph export "../Figures/pop.tif", as(tif) replace

**********************************************
*Figure: Citizens preferences
**********************************************
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
areg  logdefuncionespc  $saturated $lagDV  $controls   i.year, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log_nocov: di %5.4f r(se)
}
	
foreach i in lag_6{
areg  logdefuncionespc  $saturated $lagDV  $controls   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  logdefuncionespc  $saturated $lagDV  $controls   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


foreach i in lead_1{
areg  logdefuncionespc  $saturated $lagDV  $controls   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


foreach i in lead_2{
areg  logdefuncionespc  $saturated $lagDV  $controls   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


foreach i in lead_3{
areg  logdefuncionespc  $saturated $lagDV  $controls   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}




************
***B: ihs(homicides per capita) without covariates
************	
	areg  ihs_defuncionespc  $saturated  $controls $lagDV2   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs_nocov:  di %5.4f e(r2)
		glo N_ihs_nocov: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_7{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
}
	
foreach i in lag_6{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


foreach i in lead_1{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


foreach i in lead_2{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


foreach i in lead_3{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}



************
***C: log(homicides per capita) with covariates
************	

	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)

***estimate linear combination by lead/lag:
foreach i in lag_7{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log: di %5.4f r(se)
}
	
foreach i in lag_6{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log: di %5.4f r(se)
			}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
			}


foreach i in lead_1{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_log: di %5.4f r(se)
			}


foreach i in lead_2{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_log: di %5.4f r(se)
			}


foreach i in lead_3{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')
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
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
			}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
			}


foreach i in lead_1{
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
			}


foreach i in lead_2{
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
			}


foreach i in lead_3{
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
			}






			
coefplot (est1, rename((1) = t-7) msize(large) mcolor(red) ciopts(color(black))) ///
 (est21, rename((1) = t-7) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est2, rename((1) = t-6) msize(large) mcolor(red) ciopts(color(black))) ///
  (est22, rename((1) = t-6) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est3, rename((1) = t-5) msize(large) mcolor(red) ciopts(color(black))) ///
  (est23, rename((1) = t-5) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est4, rename((1) = t-4) msize(large) mcolor(red) ciopts(color(black))) ///
  (est24, rename((1) = t-4) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est5, rename((1) = t-3) msize(large) mcolor(red) ciopts(color(black))) ///
  (est25, rename((1) = t-3) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est6, rename((1) = t-2) msize(large) mcolor(red) ciopts(color(black))) ///
  (est26, rename((1) = t-2) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est7, rename((1) = Reform (t=0)) msize(large) mcolor(green) ciopts(color(black))) ///
  (est27, rename((1) = Reform (t=0)) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est8, rename((1) = t+1) msize(large) mcolor(green) ciopts(color(black) )) ///
  (est28, rename((1) = t+1) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est9, rename((1) = t+2) msize(large) mcolor(green) ciopts(color(black))) ///
  (est29, rename((1) = t+2) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est10, rename((1) = t+3) msize(large) mcolor(green) ciopts(color(black))) ///
  (est30, rename((1) = t+3) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle("estimate: log(homicides per capita)")  xtitle("timing") ///
legend(order(1 "w/o Citizens' sec. pref." 11 "w/ Citizens' sec. pref.")  )

graph export "../Figures/citizens_preferences_log.png", as(png) replace
graph export "../Figures/citizens_preferences_log.pdf", as(pdf) replace
graph export "../Figures/citizens_preferences_log.tif", as(tif) replace
graph save "../Figures/citizens_preferences_log.gph", replace

**ONLY CITIZENS PREFERENCES BEGINNING 

coefplot  (est21, rename((1) = t-7) msize(large) mcolor(red) ciopts(color(black))) ///
  (est22, rename((1) = t-6) msize(large) mcolor(red) ciopts(color(black))) ///
  (est23, rename((1) = t-5) msize(large) mcolor(red) ciopts(color(black))) ///
  (est24, rename((1) = t-4) msize(large) mcolor(red) ciopts(color(black))) ///
  (est25, rename((1) = t-3)  msize(large) mcolor(red) ciopts(color(black))) ///
  (est26, rename((1) = t-2) msize(large) mcolor(red) ciopts(color(black))) ///
  (est27, rename((1) = Reform (t=0)) msize(large) mcolor(green) ciopts(color(black))) ///
  (est28, rename((1) = t+1) msize(large) mcolor(green) ciopts(color(black))) ///
  (est29, rename((1) = t+2) msize(large) mcolor(green) ciopts(color(black))) ///
  (est30, rename((1) = t+3) msize(large) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle("estimate: log(homicides per capita)")  xtitle("timing") ///
legend(off) 

graph export "../Figures/citizens_preferences_only.png", as(png) replace
graph export "../Figures/citizens_preferences_only.pdf", as(pdf) replace
graph export "../Figures/citizens_preferences_only.tif", as(tif) replace
graph save "../Figures/citizens_preferences_only.gph", replace

***ONLY CITIZENS PREFERENES END 

coefplot (est11, rename((1) = t-7) msize(large) mcolor(red) ciopts(color(black))) ///
 (est31, rename((1) = t-7) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est12, rename((1) = t-6) msize(large) mcolor(red) ciopts(color(black))) ///
  (est32, rename((1) = t-6) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est13, rename((1) = t-5) msize(large) mcolor(red) ciopts(color(black))) ///
  (est33, rename((1) = t-5) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est14, rename((1) = t-4) msize(large) mcolor(red) ciopts(color(black))) ///
  (est34, rename((1) = t-4) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est15, rename((1) = t-3) msize(large) mcolor(red) ciopts(color(black))) ///
  (est35, rename((1) = t-3) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est16, rename((1) = t-2) msize(large) mcolor(red) ciopts(color(black))) ///
  (est36, rename((1) = t-2) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est17, rename((1) = Reform (t=0)) msize(large) mcolor(green) ciopts(color(black))) ///
  (est37, rename((1) = Reform (t=0)) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est18, rename((1) = t+1) msize(large) mcolor(green) ciopts(color(black) )) ///
  (est38, rename((1) = t+1) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est19, rename((1) = t+2) msize(large) mcolor(green) ciopts(color(black))) ///
  (est39, rename((1) = t+2) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est20, rename((1) = t+3) msize(large) mcolor(green) ciopts(color(black))) ///
  (est40, rename((1) = t+3) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle("estimate: ihs(homicides per capita)")  xtitle("timing") ///
legend(order(1 "w/o Citizens' sec. pref." 11 "w/ Citizens' sec. pref.")  )

graph export "../Figures/citizens_preferences_ihs.png", as(png) replace
graph export "../Figures/citizens_preferences_ihs.pdf", as(pdf) replace
graph export "../Figures/citizens_preferences_ihs.tif", as(tif) replace
graph save "../Figures/citizens_preferences_ihs.gph", replace


**********************************************
*FIGURE: EFFECT OF REFORM ON MANDO UNICO
**********************************************
/*
with acuerdo2 there are no pretrends and no effects
with acuerdo there are pretrends and positive effect but increasing

*/

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 logdefuncionespc_lag_8     	 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 logdefuncionespc_lag_8 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3	 logdefuncionespc_lag_2
	global outcome logdefuncionespc
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	global lagDV  
	global lagDV2  l.acuerdo2
	xtset inegi year 
	

************
***A: log(homicides per capita) with covariates
************	

est clear

***estimate linear combination by lead/lag:

foreach i in lag_7{
quietly	areg  acuerdo2  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log: di %5.4f r(se)
}
	
foreach i in lag_6{
quietly	areg  acuerdo2  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
quietly	areg  acuerdo2  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
}


foreach i in lead_1{
quietly	areg  acuerdo2  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_log: di %5.4f r(se)
}


foreach i in lead_2{
quietly	areg  acuerdo2  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_log: di %5.4f r(se)
}


foreach i in lead_3{
quietly	areg  acuerdo2  $saturated $lagDV  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_log: di %5.4f r(se)
}


************
***B: ihs(homicides per capita) with covariates
************	
	
***estimate linear combination by lead/lag:

foreach i in lag_7{
quietly	areg  acuerdo2  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
quietly	areg  acuerdo2  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
quietly	areg  acuerdo2  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_1{
quietly	areg  acuerdo2  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_2{
quietly	areg  acuerdo2  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}


foreach i in lead_3{
quietly	areg  acuerdo2  $saturated $lagDV2  $controls i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest 	(_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
}






coefplot (est1, rename((1) = t-7) msize(large) mcolor(red) ciopts(color(black))) ///
 (est2, rename((1) = t-6) msize(large) mcolor(red) ciopts(color(black))) ///
 (est3, rename((1) = t-5) msize(large) mcolor(red) ciopts(color(black))) ///
 (est4, rename((1) = t-4) msize(large) mcolor(red) ciopts(color(black))) ///
 (est5, rename((1) = t-3) msize(large) mcolor(red) ciopts(color(black))) ///
 (est6, rename((1) = t-2) msize(large) mcolor(red) ciopts(color(black))) ///
 (est7, rename((1) = Reform (t=0)) msize(large) mcolor(green) ciopts(color(black))) ///
 (est8, rename((1) = t+1) msize(large) mcolor(green) ciopts(color(black) )) ///
 (est9, rename((1) = t+2) msize(large) mcolor(green) ciopts(color(black))) ///
 (est10, rename((1) = t+3) msize(large) mcolor(green) ciopts(color(black))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle("prob(security coop. agreement)")  xtitle("") ///
subtitle(" ") legend(off)
graph export "../Figures/mando_unico.png", as(png) replace
graph export "../Figures/mando_unico.pdf", as(pdf) replace
graph export "../Figures/mando_unico.tif", as(tif) replace
graph save "../Figures/mando_unico.gph", replace


**********************************************
*Figure: CONTROLLING FOR MANDO UNICO
**********************************************
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
	global mandounico  l.acuerdo2
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
areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log_nocov:  di %5.4f e(r2)
		glo N_log_nocov: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log_nocov: di %5.4f r(se)
}
	
foreach i in lag_6{
areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


foreach i in lead_1{
areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


foreach i in lead_2{
areg  logdefuncionespc  $saturated $lagDV  $controls  $citizens2  i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}


foreach i in lead_3{
areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2  i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')
	glo se_`i'_log_nocov: di %5.4f r(se)
			}




************
***B: ihs(homicides per capita) without covariates
************	
	areg  ihs_defuncionespc  $saturated  $controls $lagDV2  $citizens2   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs_nocov:  di %5.4f e(r2)
		glo N_ihs_nocov: di %11.2gc e(N)
***estimate linear combination by lead/lag:
foreach i in lag_7{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2 $citizens2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
}
	
foreach i in lag_6{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2  $citizens2  i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2 $citizens2   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


foreach i in lead_1{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2 $citizens2  i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


foreach i in lead_2{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2 $citizens2  i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}


foreach i in lead_3{
quietly	areg  ihs_defuncionespc  $saturated  $controls $lagDV2 $citizens2  i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')
	glo se_`i'_ihs_nocov: di %5.4f r(se)
			}



************
***C: log(homicides per capita) with covariates
************	

	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2 $mandounico   i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_log:  di %5.4f e(r2)
		glo N_log: di %11.2gc e(N)

***estimate linear combination by lead/lag:
foreach i in lag_7{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2 $mandounico   i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_log: di %5.4f r(se)
}
	
foreach i in lag_6{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2 $mandounico  i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_log: di %5.4f r(se)
			}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2 $mandounico  i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_log: di %5.4f r(se)
			}


foreach i in lead_1{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2 $mandounico  i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_log: di %5.4f r(se)
			}


foreach i in lead_2{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2 $mandounico  i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_log: di %5.4f r(se)
			}


foreach i in lead_3{
	areg  logdefuncionespc  $saturated $lagDV  $controls $citizens2  $mandounico i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')
	glo se_`i'_log: di %5.4f r(se)
			}


************
***D: ihs(homicides per capita) with covariates
************	
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 $mandounico i.year, a(inegi) vce(cluster estado)
	estadd local depcontrols \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	
		glo r2_ihs:  di %5.4f e(r2)
		glo N_ihs: di %11.2gc e(N)
		
***estimate linear combination by lead/lag:
foreach i in lag_7{
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 $mandounico i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
}
	
foreach i in lag_6{
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 $mandounico i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
			}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 $mandounico i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')
	glo se_`i'_ihs: di %5.4f r(se)
			}


foreach i in lead_1{
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 $mandounico i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')
	glo se_`i'_ihs: di %5.4f r(se)
			}


foreach i in lead_2{
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 $mandounico i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')
	glo se_`i'_ihs: di %5.4f r(se)
			}


foreach i in lead_3{
quietly	areg  ihs_defuncionespc  $saturated $lagDV2  $controls $citizens2 $mandounico i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')
	glo se_`i'_ihs: di %5.4f r(se)
			}






			
coefplot (est1, rename((1) = t-7) msize(large) mcolor(red) ciopts(color(black))) ///
 (est21, rename((1) = t-7) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est2, rename((1) = t-6) msize(large) mcolor(red) ciopts(color(black))) ///
  (est22, rename((1) = t-6) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est3, rename((1) = t-5) msize(large) mcolor(red) ciopts(color(black))) ///
  (est23, rename((1) = t-5) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est4, rename((1) = t-4) msize(large) mcolor(red) ciopts(color(black))) ///
  (est24, rename((1) = t-4) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est5, rename((1) = t-3) msize(large) mcolor(red) ciopts(color(black))) ///
  (est25, rename((1) = t-3) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est6, rename((1) = t-2) msize(large) mcolor(red) ciopts(color(black))) ///
  (est26, rename((1) = t-2) msize(large) mfcolor(white) msymbol(S) mcolor(red) ciopts(color(black) lpattern( dash))) ///
 (est7, rename((1) = Reform (t=0)) msize(large) mcolor(green) ciopts(color(black))) ///
  (est27, rename((1) = Reform (t=0)) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est8, rename((1) = t+1) msize(large) mcolor(green) ciopts(color(black) )) ///
  (est28, rename((1) = t+1) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est9, rename((1) = t+2) msize(large) mcolor(green) ciopts(color(black))) ///
  (est29, rename((1) = t+2) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 (est10, rename((1) = t+3) msize(large) mcolor(green) ciopts(color(black))) ///
  (est30, rename((1) = t+3) msize(large) mfcolor(white) msymbol(S) mcolor(green) ciopts(color(black) lpattern( dash))) ///
 , ///
 vertical scheme(s1color)  yline(0)    ///
ytitle("estimate: log(homicides per capita)")  xtitle("timing") ///
legend(order(1 "w/o Sec. Coop. Agreement" 11 "w/ Sec. Coop. Agreement")  )

graph export "../Figures/controlling_mando_unico_log.png", as(png) replace
graph export "../Figures/controlling_mando_unico_log.pdf", as(pdf) replace
graph export "../Figures/controlling_mando_unico_log.tif", as(tif) replace
graph save "../Figures/controlling_mando_unico_log.gph", replace



**********************************************
*Figure: EFFECT OF REFORM ON HOMICIDES, MATCHING ON PRETREATMENT OUTCOME
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
	global controls_matching logdefuncionespc

graph drop _all

************************************
**WITHIN PSM: TO ADJUST CLUSTERING
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
areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)


************
***A: log(homicides per capita) with covariates
************	

est clear
***estimate linear combination by lead/lag:

foreach i in lag_7{
areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)
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
	glo b_`i'_log: di %5.4f r(estimate)
	glo se_`i'_log: di %5.4f r(se)
	glo lb_`i'_log: di %5.4f r(lb)
	glo ub_`i'_log: di %5.4f r(ub)
	*plotbeta ${b_`i'_log}, level(90)
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	*est store `i'

}
foreach i in lag_6{
areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')

}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')

}


foreach i in lead_1{
areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)

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
    eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')

}


foreach i in lead_2{
areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')

}


foreach i in lead_3{
areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)
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
*ssc install coefplot

/*coefplot (est1, rename((1) = t-7) mcolor(red) ciopts(color(black))) ///
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
ytitle("estimate: log(homicides per capita)") name(psm_clustering_loghomicides)
*/
			   
coefplot  (est3, rename((1) = t-5) mcolor(red) ciopts(color(black))) ///
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
*name(psm_clustering_loghomicides)
graph export "../Figures/event_study_psm_clustering.png", as(png) replace
graph export "../Figures/event_study_psm_clustering.pdf", as(pdf) replace
graph export "../Figures/event_study_psm_clustering.tif", as(tif) replace
graph save "../Figures/event_study_psm_clustering.gph", replace


**********************************************
*Table: EFFECT OF REFORM ON MANDO UNICO
**********************************************

use "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", clear

*Set globals: 
	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 
	global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 logdefuncionespc_lag_8 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3	 logdefuncionespc_lag_2
	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 logdefuncionespc_lag_8 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3	
	global outcome acuerdo2
    global saturated lag_7_2017 lag_7_2018 lag_6_2016 lag_6_2017 lag_6_2018 lag_5_2015 lag_5_2016 lag_5_2017 lag_5_2018 lag_4_2015 lag_4_2016 lag_4_2017 lag_4_2018 lag_3_2015 lag_3_2016 lag_3_2017 lag_3_2018 lag_2_2015 lag_2_2016 lag_2_2017 lag_2_2018 date_0_2015 date_0_2016 date_0_2017 date_0_2018 lead_1_2015 lead_1_2016 lead_1_2017 lead_2_2015 lead_2_2016 lead_3_2015
	global other inegi estado year reform
	*global lagDV  
	*global lagDV2  l.acuerdo2
	xtset inegi year 
	global controls_matching logdefuncionespc acuerdo2


graph drop _all


***estimate linear combination by lead/lag:

foreach i in lag_7{
 areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster estado)
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
	glo b_`i'_log: di %5.4f r(estimate)
	glo se_`i'_log: di %5.4f r(se)
	glo lb_`i'_log: di %5.4f r(lb)
	glo ub_`i'_log: di %5.4f r(ub)
	*plotbeta ${b_`i'_log}, level(90)
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	*est store `i'

}
foreach i in lag_6{
 areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')

}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
 areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')

}


foreach i in lead_1{
 areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster estado)

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
    eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')

}


foreach i in lead_2{
 areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')

}


foreach i in lead_3{
 areg  $outcome  $saturated $controls  i.year, a(inegi) vce(cluster estado)
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
*ssc install coefplot

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
ytitle("estimate: Pr(sign security cooperation agreement)") 
graph export "../Figures/mando_unico_new.png", as(png) replace
graph export "../Figures/mando_unico_new.pdf", as(pdf) replace
graph export "../Figures/mando_unico_new.tif", as(tif) replace
graph save "../Figures/mando_unico_new.gph", replace







************************************
**WITHIN PSM: TO ADJUST CLUSTERING
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
 areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)

est clear
***estimate linear combination by lead/lag:

foreach i in lag_7{
 areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)
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
	glo b_`i'_log: di %5.4f r(estimate)
	glo se_`i'_log: di %5.4f r(se)
	glo lb_`i'_log: di %5.4f r(lb)
	glo ub_`i'_log: di %5.4f r(ub)
	*plotbeta ${b_`i'_log}, level(90)
	eststo: lincomest (_b[`i'_2017]*`a') + (_b[`i'_2018]*`b')
	*est store `i'

}
foreach i in lag_6{
 areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2016]*`a')+(_b[`i'_2017]*`b') + (_b[`i'_2018]*`c')

}

foreach i in lag_5 lag_4 lag_3 lag_2 date_0{
 areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)

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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c') + (_b[`i'_2018]*`d')

}


foreach i in lead_1{
 areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)

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
    eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')+(_b[`i'_2017]*`c')

}


foreach i in lead_2{
 areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)
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
	eststo: lincomest (_b[`i'_2015]*`a')+(_b[`i'_2016]*`b')

}


foreach i in lead_3{
 areg  $outcome  $saturated $controls  i.year [fweight=weight] if weight!=., a(inegi) vce(cluster estado)
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
*ssc install coefplot

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
ytitle("estimate: Pr(sign. security cooperation agreement)") 

			  
*name(psm_clustering_loghomicides)
graph export "../Figures/event_study_psm_clustering.png", as(png) replace
graph export "../Figures/event_study_psm_clustering.pdf", as(pdf) replace
graph export "../Figures/event_study_psm_clustering.tif", as(tif) replace
graph save "../Figures/event_study_psm_clustering.gph", replace


	
*Table
	

/*EXTRA FIGURES:

******************************
*Figure. Goodman-Bacon decomposition
******************************
use "../Data/ConstructionDatabase/data_wleads&lags.dta", clear

xtset inegi year
set matsize 11000

*Controls 
gen l_logdefunciones=logdefunciones[_n-1]
global controls  winning_margin_governor governor_alignment

*Figure: 
est clear
eststo: bacondecomp logdefuncionespc reform l_logdefunciones  winning_margin_governor governor_alignment, stub(Bacon_) cluster(estado) // we have 6 timing groups
 graph export "../Figures/bacon_decomposition_clusterSEs.png", as(png) replace

 ****************************************************
* Figure: parallel trend 
****************************************************

graph drop _all 

use "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v2.dta", clear

collapse logdefuncionespc, by(year reform)

bytwoway (line logdefuncionespc year), by(reform) xline(2015) scheme(s1color) ///
 ytitle("log(Homicides per capita)") /// 
 legend(label(1 "Treated") label(2 "Control")) ///
 name(reform)
 
graph export "../Figures/parallel_trend.png", as(png) replace



****************************************************
* 2) Parallel trend  
****************************************************
use "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v2.dta", clear

*****DATA PREP******
**FIRST CUT
tab year,gen(year_)
gen treatmentgroup=0
replace treatmentgroup=1 if estado==3 | estado==4 | estado==6 | estado==7 | estado==11 | estado==12 | estado==14 | estado==15 | estado==16 | estado==17 | estado==19 | estado==22 | estado==24 | estado==27 | estado==31 
gen post=0
replace post=1 if year>2014
eststo: reg logdefuncionespc treatmentgroup##post, robust //effect is positive
eststo: reg logdefuncionespc treatmentgroup##year, robust //paralle trend seems to hold 
eststo: reg logdefuncionespc treatmentgroup##year, cluster(estado) 

eststo: reg logdefuncionespc treatmentgroup##year, robust //paralle trend seems to hold 
esttab est2, keep(treatmentgroup#year_1)

eststo: reg mariguana_kghec treatmentgroup##year, robust //paralle trend seems to hold 

coefplot est2

collapse logdefuncionespc, by(year treatmentgroup)

bytwoway (line logdefuncionespc year), by(treatmentgroup) xline(2015) scheme(s1color) ///
 ytitle("log(Homicides per capita)") /// 
 legend(label(1 "Treated") label(2 "Control"))
 graph export "../Figures/parallel_trend_firstcut.png", as(png) replace



****************************************************
use "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v2.dta", clear

*****DATA PREP******
**SECOND CUT
tab year,gen(year_)
drop if estado==3 | estado==4 | estado==6 | estado==7 | estado==11 | estado==12 | estado==14 | estado==15 | estado==16 | estado==17 | estado==19 | estado==22 | estado==24 | estado==27 | estado==31
gen treatmentgroup=0
replace treatmentgroup=1 if estado==1 | estado==2 | estado==8 | estado==10 | estado==20 | estado==23 | estado==25 | estado==28 | estado==32
gen post=0
replace post=1 if year>2015
eststo: reg logdefuncionespc treatmentgroup##year, robust //paralle trend seems to hold 
eststo: reg logdefuncionespc treatmentgroup##year, cluster(estado) //paralle trend seems to hold 
eststo: reg mariguana_kghec treatmentgroup##year, robust //paralle trend seems to hold 

collapse logdefuncionespc, by(year treatmentgroup)

bytwoway (line logdefuncionespc year), by(treatmentgroup) xline(2016) scheme(s1color) ///
 ytitle("log(Homicides per capita)") /// 
 legend(label(1 "Treated") label(2 "Control"))
 graph export "../Figures/parallel_trend_secondcut.png", as(png) replace

*should I condition on incumbency?

*/


