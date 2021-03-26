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
*ssc install fillmissing, replace
*ssc install did_multiplegt, replace


*========================================================================
*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles"

*========================================================================
*Run do files
*do "reelection_July19_2020_vargeneration_electoral_2.do" /*I don't need this one*/
do "1. electoral.do"
do "1. incumbent_characteristics.do"
do "1. envipe.do"
do "1. dtos.do"
do "1. mando_unico.do"
do "2. database.do"
do "3. cooperation_agreements.do"
do "3. main_tables.do"
do "4. additional_tables.do"
do "3. cooperation_agreements.do"
do "4. matching.do"
do "5. figures"

