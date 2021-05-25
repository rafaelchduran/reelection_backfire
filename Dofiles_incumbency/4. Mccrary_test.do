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
use "../../Data/ConstructionDatabase/data_wleads&lags_incumbency_weights.dta", clear
*========================================================================
*SET GLOBALS
*1) controls
	global controls_naive logdefuncionespc_mean alignment_executive_strong_mean alignment_governor_strong_mean winning_margin_mean winning_margin_governor_mean pan_mayor2_mean pri_mayor2_mean hayCarteles_mean logpop_mean

*2) treatment

*3) outcomes
   	global outcome2 incumbent_yesterday_w_tomorrow2
	global outcome5 mv_incpartyfor1
*========================================================================
*McCrary Test

est clear
foreach j in $outcome2{
foreach pol in  1 {

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT)
global optimal = e(h_CCT)

rddensity mv_incparty, c(0) p(`pol') h(${optimal}) plot vce(jackknife) ///
graph_options(legend(off) title("Panel A: linear polynomial") xtitle("Incumbent party vote margin at t (municipal elections)") ytitle("Density") scheme(s1color))
graph export "../Figures_incumbency/mccrary_test_pol`pol'.png", as(png) replace
graph save "../Figures_incumbency/mccrary_test_pol`pol'.gph", replace
}
}

foreach j in $outcome2{
foreach pol in  2{

rdbwselect  `j' mv_incparty, c(0) p(`pol') kernel(tri) bwselect(CCT)
global optimal = e(h_CCT)

rddensity mv_incparty, c(0) p(`pol') h(${optimal}) plot vce(jackknife) ///
graph_options(legend(off) title("Panel B: quadratic polynomial") xtitle("Incumbent party vote margin at t (municipal elections)") ytitle("Density") scheme(s1color))
graph export "../Figures_incumbency/mccrary_test_pol`pol'.png", as(png) replace
graph save "../Figures_incumbency/mccrary_test_pol`pol'.gph", replace
}
}

*All
graph combine  "../Figures_incumbency/mccrary_test_pol1.gph" "../Figures_incumbency/mccrary_test_pol2.gph", ///
 subtitle(" ")  ///
scheme(s1color)  imargin(vsmall) xcommon col(1) l1( ) b1( )
graph export "../Figures_incumbency/mccrary_pol1_2.png", as(png) replace
graph export "../Figures_incumbency/mccrary_pol1_2.pdf", as(pdf) replace
graph export "../Figures_incumbency/mccrary_pol1_2.tif", as(tif) replace

