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
*net install did, from("https://raw.githubusercontent.com/NickCH-K/did/master/") replace
* didsetup (and type Yes and Yes)

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
*do "1. mando_unico.do" // use other file; contains new variables and corrected estimates. 
do "1. mando_unico_newvarnames.do"
do "2. database.do"
do "2. database_vargeneration.do"
do "3. descriptive_statistics&balance"
do "3. incumbency_event_study.do"



