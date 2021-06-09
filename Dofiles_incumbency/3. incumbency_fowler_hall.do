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
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles_incumbency"

*========================================================================
*LOAD DATA
*use "../../Data/ConstructionDatabase/data_final_incumbency.dta", clear
use "../../Data/municipal_elections_incumbent_mexico_1989_present_v2.dta", clear

*Merge with Dube, Garcia-Ponce and Thoms (2016): data from 1990 to 2010
gen muncode=inegi

merge 1:1 muncode year using "../../Mexico/Data/Dube, Garcia-Ponce and Thom (2016)/jeea12172-sup-0002-replication-data/Dube_Garcia_ Thom_ReplicationData/MaizeToHaze_JEEA_ReplicationData.dta"
drop if _merge==2
drop _merge

merge m:m inegi year using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011_2018.dta"
drop if _merge==2
drop _merge
*========================================================================
*SET PANEL
*xtset inegi year 
*========================================================================
*SET GLOBALS
*1) controls
	global controls_naive logdefuncionespc_mean alignment_executive_strong_mean alignment_governor_strong_mean winning_margin_mean winning_margin_governor_mean pan_mayor2_mean pri_mayor2_mean hayCarteles_mean logpop_mean

*2) treatment

*3) outcomes
   	global outcome2 incumbent_yesterday_w_tomorrow2
	global outcome5 mv_incpartyfor1
*========================================================================
*Run .ado 
do "wild_areg_incumbency.do"
do "macros_tables_incumbency.do"
*========================================================================
*Probabilities of who ran for office
gen prob_running=0 if raceafter=="Out-p-lost"  | raceafter=="Out-p-won" 
replace prob_running=1 if raceafter=="Reelected" |  raceafter=="Beaten" 

label variable prob_running "Pr(Winner runs again)"
*average is: 35.30%. So instead of dividing by 2 we divide by 1.29
sum prob_running
glo denominator: di  2*(1-r(mean))
*========================================================================
*Drop years prior to 2000
drop if year<2000
*========================================================================
*FOWLER AND HALL (2014)	
*A) LINEAR POLYNOMIAL
est clear
*1) PROBABILITY OF WINNING 
eststo: rdrobust $outcome2  mv_incparty if reform==1 & year<2018, c(0) p(1) kernel(tri) bwselect(CCT)
	estadd local postreform \checkmark

	glo beta_reform: di %5.4f e(tau_cl)/${denominator}
	glo se_reform: di %5.4f  e(se_cl)/${denominator}
	glo grados_reform: di r(N)-1
	glo t_reform: di e(tau_cl)/e(se_cl)
	glo p_reform: di 2*ttail(${grados_reform},abs(${t_reform})) 	
	glo est_reform= "" 
			if (${p_reform}<=0.1) global est_reform = "*"
			if (${p_reform}<=0.05) global est_reform = "**"
			if (${p_reform}<=0.01) global est_reform = "***"
	glo N: di  r(N) 

eststo: rdrobust $outcome2  mv_incparty if reform==0 & year<2018, c(0) p(1) kernel(tri) bwselect(CCT)
	estadd local postreform 

	glo beta_noreform: di %5.4f e(tau_cl)/${denominator}
	glo se_noreform: di %5.4f  e(se_cl)/${denominator}
	glo grados_noreform: di r(N)-1
	glo t_noreform: di e(tau_cl)/e(se_cl)
	glo p_noreform: di 2*ttail(${grados_noreform},abs(${t_noreform})) 	
	glo est_noreform= "" 
			if (${p_noreform}<=0.1) global est_noreform = "*"
			if (${p_noreform}<=0.05) global est_noreform = "**"
			if (${p_noreform}<=0.01) global est_noreform = "***"
	glo N2: di   r(N) 
	
*estimate difference: 
	glo beta_diff: di %5.4f (${beta_reform} -(${beta_noreform}))/${denominator}
	glo p_diff: di 0.000001
	glo est_diff= "" 
			if (${p_diff}<=0.1) global est_diff = "*"
			if (${p_diff}<=0.05) global est_diff = "**"
			if (${p_diff}<=0.01) global est_diff = "***"
	glo se_diff: di %5.4f  ((sqrt(((${N}-1)*${se_reform} + (${N2}-1)*${se_noreform})/(${N}+${N2}-2)))*sqrt((1/ ${N})+(1/${N2})))/${denominator}
			
*2) WINNING MARGIN
eststo: rdrobust $outcome5  mv_incparty if reform==1 & year<2018, c(0) p(1) kernel(tri) bwselect(CCT)
	estadd local postreform \checkmark

	glo beta_reform2: di %5.4f e(tau_cl)/${denominator}
	glo se_reform2: di %5.4f  e(se_cl)/${denominator}
	glo grados_reform2: di r(N)-1
	glo t_reform2: di e(tau_cl)/e(se_cl)
	glo p_reform2: di 2*ttail(${grados_reform2},abs(${t_reform2})) 	
	glo est_reform2= "" 
			if (${p_reform2}<=0.1) global est_reform2 = "*"
			if (${p_reform2}<=0.05) global est_reform2 = "**"
			if (${p_reform2}<=0.01) global est_reform2 = "***"
	glo N_5: di  r(N) 

eststo: rdrobust $outcome5  mv_incparty if reform==0 & year<2018, c(0) p(1) kernel(tri) bwselect(CCT)
	estadd local postreform 

	glo beta_noreform2: di %5.4f e(tau_cl)/${denominator}
	glo se_noreform2: di %5.4f  e(se_cl)/${denominator}
	glo grados_noreform2: di r(N)-1
	glo t_noreform2: di e(tau_cl)/e(se_cl)
	glo p_noreform2: di 2*ttail(${grados_noreform2},abs(${t_noreform2})) 	
	glo est_noreform2= "" 
			if (${p_noreform2}<=0.1) global est_noreform2 = "*"
			if (${p_noreform2}<=0.05) global est_noreform2 = "**"
			if (${p_noreform2}<=0.01) global est_noreform2 = "***"
	glo N2_5: di   r(N) 
	
*estimate difference: 
	glo beta_diff2: di %5.4f (${beta_reform2}  -(${beta_noreform2} ))/${denominator}
	glo p_diff2: di 0.000001
	glo est_diff2= "" 
			if (${p_diff2}<=0.1) global est_diff2 = "*"
			if (${p_diff2}<=0.05) global est_diff2 = "**"
			if (${p_diff2}<=0.01) global est_diff2 = "***"
	glo se_diff2: di %5.4f  ((sqrt(((${N_5}-1)*${se_reform2}  + (${N2_5}-1)*${se_noreform2} )/(${N_5}+${N2_5}-2)))*sqrt((1/ ${N_5})+(1/${N2_5})))/${denominator}

*B)QUADRATIC POLYNOMIAL
est clear
*1) PROBABILITY OF WINNING 
eststo: rdrobust $outcome2  mv_incparty if reform==1 & year<2018, c(0) p(2) kernel(tri) bwselect(CCT)
	estadd local postreform \checkmark

	glo beta_reform_quad: di %5.4f e(tau_cl)/${denominator}
	glo se_reform_quad: di %5.4f  e(se_cl)/${denominator}
	glo grados_reform_quad: di r(N)-1
	glo t_reform_quad: di e(tau_cl)/e(se_cl)
	glo p_reform_quad: di 2*ttail(${grados_reform_quad},abs(${t_reform_quad})) 	
	glo est_reform_quad= "" 
			if (${p_reform_quad}<=0.1) global est_reform_quad = "*"
			if (${p_reform_quad}<=0.05) global est_reform_quad = "**"
			if (${p_reform_quad}<=0.01) global est_reform_quad = "***"
	glo N_quad: di  r(N) 

eststo: rdrobust $outcome2  mv_incparty if reform==0 & year<2018, c(0) p(2) kernel(tri) bwselect(CCT)
	estadd local postreform 

	glo beta_noreform_quad: di %5.4f e(tau_cl)/${denominator}
	glo se_noreform_quad: di %5.4f  e(se_cl)/${denominator}
	glo grados_noreform_quad: di r(N)-1
	glo t_noreform_quad: di e(tau_cl)/e(se_cl)
	glo p_noreform_quad: di 2*ttail(${grados_noreform_quad},abs(${t_noreform_quad})) 	
	glo est_noreform_quad= "" 
			if (${p_noreform_quad}<=0.1) global est_noreform_quad = "*"
			if (${p_noreform_quad}<=0.05) global est_noreform_quad = "**"
			if (${p_noreform_quad}<=0.01) global est_noreform_quad = "***"
	glo N2_quad: di   r(N) 
	
*estimate difference: 
	glo beta_diff_quad: di %5.4f (${beta_reform_quad} -(${beta_noreform_quad}))/${denominator}
	glo p_diff_quad: di 0.000001
	glo est_diff_quad= "" 
			if (${p_diff}<=0.1) global est_diff_quad = "*"
			if (${p_diff}<=0.05) global est_diff_quad = "**"
			if (${p_diff}<=0.01) global est_diff_quad = "***"
	glo se_diff_quad: di %5.4f  ((sqrt(((${N}-1)*${se_reform_quad} + (${N2}-1)*${se_noreform_quad})/(${N}+${N2}-2)))*sqrt((1/ ${N})+(1/${N2})))/${denominator}
			
*2) WINNING MARGIN
eststo: rdrobust $outcome5  mv_incparty if reform==1 & year<2018, c(0) p(2) kernel(tri) bwselect(CCT)
	estadd local postreform \checkmark

	glo beta_reform2_quad: di %5.4f e(tau_cl)/${denominator}
	glo se_reform2_quad: di %5.4f  e(se_cl)/${denominator}
	glo grados_reform2_quad: di r(N)-1
	glo t_reform2_quad: di e(tau_cl)/e(se_cl)
	glo p_reform2_quad: di 2*ttail(${grados_reform2_quad},abs(${t_reform2_quad})) 	
	glo est_reform_quad= "" 
			if (${p_reform2_quad}<=0.1) global est_reform_quad = "*"
			if (${p_reform2_quad}<=0.05) global est_reform_quad = "**"
			if (${p_reform2_quad}<=0.01) global est_reform_quad = "***"
	glo N_5_quad: di  r(N) 

eststo: rdrobust $outcome5  mv_incparty if reform==0 & year<2018, c(0) p(2) kernel(tri) bwselect(CCT)
	estadd local postreform 

	glo beta_noreform2_quad: di %5.4f e(tau_cl)/${denominator}
	glo se_noreform2_quad: di %5.4f  e(se_cl)/${denominator}
	glo grados_noreform2_quad: di r(N)-1
	glo t_noreform2_quad: di e(tau_cl)/e(se_cl)
	glo p_noreform2_quad: di 2*ttail(${grados_noreform2_quad},abs(${t_noreform2_quad})) 	
	glo est_noreform2_quad= "" 
			if (${p_noreform2_quad}<=0.1) global est_noreform2_quad = "*"
			if (${p_noreform2_quad}<=0.05) global est_noreform2_quad = "**"
			if (${p_noreform2_quad}<=0.01) global est_noreform2_quad = "***"
	glo N2_5_quad: di   r(N) 
	
*estimate difference: 
	glo beta_diff2_quad: di %5.4f (${beta_reform2_quad}  -(${beta_noreform2_quad} ))/${denominator}
	glo p_diff2_quad: di 0.000001
	glo est_diff2_quad= "" 
			if (${p_diff2_quad}<=0.1) global est_diff2_quad = "*"
			if (${p_diff2_quad}<=0.05) global est_diff2_quad = "**"
			if (${p_diff2_quad}<=0.01) global est_diff2_quad = "***"
	glo se_diff2_quad: di %5.4f  ((sqrt(((${N_5}-1)*${se_reform2_quad}  + (${N2_5}-1)*${se_noreform2_quad} )/(${N_5}+${N2_5}-2)))*sqrt((1/ ${N_5})+(1/${N2_5})))/${denominator}

*Table
texdoc init  "../Tables_incumbency/fowler_hall.tex", replace force
tex \begin{table}[htbp]\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
tex \centering
tex \caption{Personal and Partisan Incumbency Advantage following \citet{fowler_hall_2014}}
tex \label{tab:fowler_hall}
tex \scalebox{1}{    
tex \begin{tabular}{lccc}  
tex \hline \hline       
tex & \multicolumn{3}{c}{\textbf{Panel A: linear polynomial}}\\
tex & \multicolumn{1}{c}{No term limits:} & \multicolumn{1}{c}{term limits:} & \multicolumn{1}{c}{difference:}\\
tex Advantage: & \multicolumn{1}{c}{personal + partisan} & \multicolumn{1}{c}{partisan} & \multicolumn{1}{c}{personal} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} \\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4}\\
tex \addlinespace
tex RD estimate: prob(victory in t+1) &      $ ${beta_reform}^{${est_reform}} $ &  $ ${beta_noreform}^{${est_noreform}} $  &  $ ${beta_diff}^{${est_diff}} $  \\
tex  & ($ ${se_reform}$) & ($ ${se_noreform} $)  & ($ ${se_diff} $)\\
tex RD estimate: vote share in t+1 &      $ ${beta_reform2}^{${est_reform2}} $ &  $ ${beta_noreform2}^{${est_noreform2}} $  &  $ ${beta_diff2}^{${est_diff2}} $  \\
tex  & ($ ${se_reform2}$) & ($ ${se_noreform2} $)  & ($ ${se_diff2} $)\\

tex \addlinespace
tex Observations: prob(victory in t+1)      &            ${N}        &     ${N2}  \\
tex Observations: vote share in t+1      &            ${N_5}        &     ${N2_5}  \\
*tex Controls$^b$   &             &          \\
tex \\
tex & \multicolumn{3}{c}{\textbf{Panel B: quadratic polynomial}}\\
tex & \multicolumn{1}{c}{No term limits:} & \multicolumn{1}{c}{term limits:} & \multicolumn{1}{c}{difference:}\\
tex Advantage: & \multicolumn{1}{c}{personal + partisan} & \multicolumn{1}{c}{partisan} & \multicolumn{1}{c}{personal} \\
tex & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} \\ 

tex \cmidrule(lrr){2-2}  \cmidrule(lrr){3-3} \cmidrule(lrr){4-4}\\
tex \addlinespace
tex RD estimate: prob(victory in t+1) &      $ ${beta_reform_quad}^{${est_reform_quad}} $ &  $ ${beta_noreform_quad}^{${est_noreform_quad}} $  &  $ ${beta_diff_quad}^{${est_diff_quad}} $  \\
tex  & ($ ${se_reform_quad}$) & ($ ${se_noreform_quad} $)  & ($ ${se_diff_quad} $)\\
tex RD estimate: vote share in t+1 &      $ ${beta_reform2_quad}^{${est_reform2_quad}} $ &  $ ${beta_noreform2_quad}^{${est_noreform2_quad}} $  &  $ ${beta_diff2_quad}^{${est_diff2_quad}} $  \\
tex  & ($ ${se_reform2_quad}$) & ($ ${se_noreform2_quad} $)  & ($ ${se_diff2_quad} $)\\

tex \addlinespace
tex Observations: prob(victory in t+1)      &            ${N_quad}        &     ${N2_quad}  \\
tex Observations: vote share in t+1      &            ${N_5_quad}        &     ${N2_5_quad}  \\
*tex Controls$^b$   &             &          \\


tex \hline \hline      
tex \multicolumn{4}{p{1\textwidth}}{\footnotesize{Notes: Standard errors in parentheses are clustered at the state level, with the following significance-level: $^{***}$ 1\%; $^{**}$ 5\%; and $^*$ 10\%, that refer to two-sided t-test with the null hypothesis equal to 0 for each relative time period. Table estimated using all elections since the year 2000.}}
tex \end{tabular}
tex } 
tex \end{table}
texdoc close


*manual t-test to get personal incumbency advantage: 
di (.07667 -(-.04606))
*.12273 divided by two
**get pvalue
di (.07667 -(-.04606))/sqrt((.06159^2)+(.04253^2))
*1.6397369
*in z table=.94845, so significant to the .05155 level . This is the personal effect divided by two. 
**https://www.math.arizona.edu/~rsims/ma464/standardnormaltable.pdf 
*confidence intervals
*difference in coefficients +/- z sqrt((variance1^2)/n1+(variance2^2)/n2)

*get pvalue pooling variances:
di (.07667 -(-.04606))/((sqrt(((1341-1)*.06159 + (3215-1)*.04253)/(1341+3215-2)))*sqrt((1/ 1341)+(1/3215)))
*17.207516

*1) Do I pool variances?
*https://calcworkshop.com/confidence-interval/difference-in-means/

di F(.06159,.04253, 4554)
*.50885328
*since our t (z) is 1.960 we pool the variances (F<t)

**2) when variance is unknown and I don't pool variances (so this is wrong)
*High CI
di (.07667 -(-.04606))+(1.96*sqrt(((.06159)/ 1341)+((.04253)/3215)))
*0.13780507
*Low CI
di (.07667 -(-.04606))-(1.96*sqrt(((.06159)/ 1341)+((.04253)/3215)))
*0.10765493

**3) when variance is unknown and I pool variances (this is right)
*sp=(sqrt(((1341-1)*.06159 + (3215-1)*.04253)/(1341+3215-2)))
*t=1.960 with 1341+3215-2 degrees of freedom and 95% confidence, so its a Z 
*High CI:
di (.07667 -(-.04606))+(1.960*(sqrt(((1341-1)*.06159 + (3215-1)*.04253)/(1341+3215-2)))*sqrt((1/ 1341)+(1/3215)))
*.1367094
*Low CI:
di (.07667 -(-.04606))-(1.960*(sqrt(((1341-1)*.06159 + (3215-1)*.04253)/(1341+3215-2)))*sqrt((1/ 1341)+(1/3215)))
*.1087506

******************************
*Figure. RDD plot
******************************

*1. Probability of winning 
graph drop _all
foreach pol in 1 2 {
*pre reform 
rdbwselect  $outcome2 mv_incparty if reform==0 & year<2018, c(0) p(`pol') kernel(tri) bwselect(CCT)
global optimal = e(h_CCT)

rdplot $outcome2 mv_incparty if reform==0 & year<2018, c(0) p(`pol') lowerend(-${optimal}) upperend(${optimal}) ///
name(prereform) ///
graph_options(legend(off) title("(a) Prior Term-Limit Reform")  scheme(s1color))
graph export "../Figures_incumbency/prior_reform_pol`pol'.png", as(png) replace
graph save "../Figures_incumbency/prior_reform`var'_pol`pol'.gph", replace

*post reform
rdbwselect  $outcome2 mv_incparty if reform==1 & year<2018, c(0) p(`pol') kernel(tri) bwselect(CCT)
global optimal = e(h_CCT)

rdplot $outcome2 mv_incparty if reform==1 & year<2018, c(0) p(`pol') lowerend(-${optimal}) upperend(${optimal}) ///
name(postreform) ///
graph_options(legend(off) title("(b) Post Term-Limit Reform") scheme(s1color))
graph export "../Figures_incumbency/post_reform_pol`pol'.png", as(png) replace
graph save "../Figures_incumbency/post_reform`var'_pol`pol'.gph", replace

*All
graph combine  "../Figures_incumbency/prior_reform`var'_pol`pol'.gph" "../Figures_incumbency/post_reform`var'_pol`pol'.gph", ///
 subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) xcommon col(1) l1(Probability of victory at t+1) b1(Incumbent party vote margin at t (municipal elections))
graph export "../Figures_incumbency/RDD_incumbency_pol`pol'.png", as(png) replace
graph export "../Figures_incumbency/RDD_incumbency_pol`pol'.pdf", as(pdf) replace
graph export "../Figures_incumbency/RDD_incumbency_pol`pol'.tif", as(tif) replace

}

*2. Winning margin 
graph drop _all
foreach pol in 1 2 {
*pre reform 
rdbwselect  $outcome5 mv_incparty if reform==0 & year<2018, c(0) p(`pol') kernel(tri) bwselect(CCT)
global optimal = e(h_CCT)

rdplot $outcome5 mv_incparty if reform==0 & year<2018, c(0) p(`pol') lowerend(-${optimal}) upperend(${optimal}) ///
name(prereform) ///
graph_options(legend(off) title("(a) Prior Term-Limit Reform")  scheme(s1color))
graph export "../Figures_incumbency/prior_reform_pol`pol'.png", as(png) replace
graph save "../Figures_incumbency/prior_reform`var'_pol`pol'.gph", replace

*post reform
rdbwselect  $outcome5 mv_incparty if reform==1 & year<2018, c(0) p(`pol') kernel(tri) bwselect(CCT)
global optimal = e(h_CCT)

rdplot $outcome5 mv_incparty if reform==1 & year<2018, c(0) p(`pol') lowerend(-${optimal}) upperend(${optimal}) ///
name(postreform) ///
graph_options(legend(off) title("(b) Post Term-Limit Reform") scheme(s1color))
graph export "../Figures_incumbency/post_reform_pol`pol'.png", as(png) replace
graph save "../Figures_incumbency/post_reform`var'_pol`pol'.gph", replace

*All
graph combine  "../Figures_incumbency/prior_reform`var'_pol`pol'.gph" "../Figures_incumbency/post_reform`var'_pol`pol'.gph", ///
 subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) xcommon col(1) l1(Winning margin at t+1) b1(Incumbent party vote margin at t (municipal elections))
graph export "../Figures_incumbency/RDD_incumbency_margin_pol`pol'.png", as(png) replace
graph export "../Figures_incumbency/RDD_incumbency_margin_pol`pol'.pdf", as(pdf) replace
graph export "../Figures_incumbency/RDD_incumbency_margin_pol`pol'.tif", as(tif) replace

}


*========================================================================
*Appendix Table
est clear
*1) PROBABILITY OF WINNING 
eststo: rdrobust $outcome2  mv_incparty if reform==1 & year<2018, c(0) p(1) kernel(tri) bwselect(CCT)
	estadd local postreform 


eststo: rdrobust $outcome2  mv_incparty if reform==0 & year<2018, c(0) p(1) kernel(tri) bwselect(CCT)
	estadd local postreform \checkmark

eststo: rdrobust $outcome2  mv_incparty if reform==1 & year<2018, c(0) p(2) kernel(tri) bwselect(CCT)
	estadd local postreform 


eststo: rdrobust $outcome2  mv_incparty if reform==0 & year<2018, c(0) p(2) kernel(tri) bwselect(CCT)
	estadd local postreform \checkmark

*Appendix table

esttab est*, keep(RD_Estimate) star(* 0.1 ** 0.05 *** 0.01) se

esttab est1 est2 est3 est4 using "../Tables_incumbency/rdd_estimates_conditional.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N  postreform, fmt(%11.2gc 3) label("Observations" "Term Limit")) ///
keep(RD_Estimate) ///
mgroups("linear polynomial" "quadratic polynomial", ///
 pattern(1 0 1 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(RD_Estimate "Probability of victory at t+1") ///
collabels(none) nonotes booktabs nomtitles  nolines
	

*2) WINNING MARGIN
eststo: rdrobust $outcome5  mv_incparty if reform==1 & year<2018, c(0) p(1) kernel(tri) bwselect(CCT)
	estadd local postreform 

eststo: rdrobust $outcome5  mv_incparty if reform==0 & year<2018, c(0) p(1) kernel(tri) bwselect(CCT)
	estadd local postreform \checkmark

eststo: rdrobust $outcome5  mv_incparty if reform==1 & year<2018, c(0) p(2) kernel(tri) bwselect(CCT)
	estadd local postreform 

eststo: rdrobust $outcome5  mv_incparty if reform==0 & year<2018, c(0) p(2) kernel(tri) bwselect(CCT)
	estadd local postreform \checkmark

esttab est5 est6 est7 est8 using "../Tables_incumbency/rdd_estimates_conditional2.tex", replace f b(%9.4f) se(%9.4f) se  star(* 0.10 ** 0.05 *** 0.01) ///
s(N  postreform, fmt(%11.2gc 3) label("Observations" "Term Limit")) ///
keep(RD_Estimate) ///
mgroups("linear polynomial" "quadratic polynomial", ///
 pattern(1 0 1 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
coeflabel(RD_Estimate "Winning margin at t+1") ///
collabels(none) nonotes booktabs nomtitles  nolines
	
	
