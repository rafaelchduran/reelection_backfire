*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*Budget and used budget
*****************************************************

clear all
set more off  
set varabbrev off 

*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles"
*Clean datasets
*========================================================================
*PORCENTAJE EGRESOS 
******
**2010
******
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2011/REC_PRES.csv", clear
rename ubi_geo inegi
collapse (mean)por_egre, by (inegi) 
gen year=2010
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/porcentaje_egresos_2010.dta", replace

******
**2012
******
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2013/REC_PRES.csv", clear
rename ubic_geo inegi
rename porc_egr por_egre
collapse (mean)por_egre, by (inegi) 
gen year=2012
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/porcentaje_egresos_2012.dta", replace


******
**2014
******
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2015/REC_PRES.csv", clear
rename ubic_geo inegi
rename porc_egr por_egre
collapse (mean)por_egre, by (inegi) 
gen year=2014
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/porcentaje_egresos_2014.dta", replace


******
**2016
******
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2017/INTITPOR.csv", clear
rename ubic_geo inegi
sort inegi
destring totalpd1, replace force
rename totalpd1 por_egre 
collapse (mean)por_egre, by (inegi) 
gen year=2016
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/porcentaje_egresos_2016.dta", replace

/******
**2018
******
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2019/PRESUSOL.csv", clear
rename ubic_geo inegi
sort inegi
destring totalca1, replace force
keep if tip_pres==3
rename totalca1 ejercido
preserve 
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2019/PRESUSOL.csv", clear
rename ubic_geo inegi
sort inegi
destring totalca1, replace force
keep if tip_pres==2
rename totalca1 autorizado
destring autorizado, replace force
collapse (mean)autorizado, by (inegi) 
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/ejercido_2018.dta", replace
restore 
merge 1:1 inegi using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/ejercido_2018.dta"
gen por_egre=ejercido/autorizado
collapse (mean)por_egre, by (inegi) 
gen year=2018
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/porcentaje_egresos_2016.dta", replace
*/

*merge all:
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/porcentaje_egresos_2010.dta", clear
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/porcentaje_egresos_2012.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/porcentaje_egresos_2014.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/porcentaje_egresos_2016.dta"
sort inegi year
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/porcentaje_egresos_2010_2016.dta", replace

