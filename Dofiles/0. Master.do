*Master do file
*Paper: Reelection Backfire
*Author: Rafael Ch (rafael.ch@nyu.edu)

*========================================================================
*Environment
clear all
set more off  
set varabbrev off 
*========================================================================
/*Notes
1. check that the main tables file is the last one I worked on. Basically, 
it should not have a lag DV since we have a twfe model, and it should control for pretreatment homicides by cohort. 
2. check the difference between acuerdo and acuerdo2 
3. check why do I have half of the observations in my regressions. tab year reform
4. check incumbency advantage: put it on fire
*/

*========================================================================
*Install packages 
*ssc install texdoc, replace
*ssc install texify, replace
*ssc install estout, replace 
*ssc install center, replace
*ssc install unique, replace
*ssc install clustse, replace

*========================================================================
*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Dofiles"

*========================================================================
*Run do files
*do "reelection_July19_2020_vargeneration_electoral_2.do" /*I don't need this one*/
do "1. electoral_Feb_27_2021.do"
do "1. incumbent_characteristics_Feb_27_2021.do"
do "1. envipe_Feb_27_2021.do"
do "1. dtos_Feb_27_2021.do"
do "1. mando_unico_Feb_27_2021.do"
do "2. database_Feb_27_2021.do"
do "3. cooperation_agreements_Feb_27_2021.do"
do "3. main_tables_Feb_27_2021.do"
do "4. additional_tables_Feb_27_2021.do"
do "3. cooperation_agreements_Feb_27_2021.do"
do "4. matching_Feb_27_2021.do"
do "5. figures_Feb_27_2021"

