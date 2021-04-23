*Regressions: Reform & Sec. Cooperation Agreements
*Paper: Reelection Backfire
*Author: Rafael Ch (rafael.ch@nyu.edu)
*Descriptive statistics and balance table
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
/*Things to control for:
0. homicides
1. heterogeneity in citizens security preferences 
2. alignment 
3. winning margin mayor
4. winning margin governor
4. parties
*/


*========================================================================

*1) Balance table of treatment and control group  

global  controls 
eststo clear 
foreach i in $controls{
quietly estpost ttest $controls, by(treatment)
est store table
}
*export
esttab using "balance_table.csv",  label replace nolegend ///
cells("mu_1(label(Mean Treatment)) mu_2(label(Mean Control))b(label(Diff. in Means)) se(label(SE Diff. in Means)) p(label(p-value))") 
