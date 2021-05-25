*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*Sesiones and Iniciativas
*****************************************************

clear all
set more off  
set varabbrev off 

*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles"
*Clean datasets
*========================================================================
*HUMAN RESOURCES 
******
**2010
******
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2011/REC_HUMA.csv", clear
rename ubi_geo inegi
collapse (sum)t_person, by (inegi) 
gen year=2010
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/rec_humanos_2010.dta", replace

******
**2012
******
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2013/REC_HUMA.csv", clear
rename ubic_geo inegi
keep if cla_admi==1
rename tt_perso t_person 
collapse (sum)t_person, by (inegi) 
gen year=2012
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/rec_humanos_2012.dta", replace


******
**2014
******
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2015/REC_HUMA.csv", clear
rename ubic_geo inegi
keep if cla_admi==1
sort inegi
rename tt_perso t_person 
collapse (sum)t_person, by (inegi) 
gen year=2014
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/rec_humanos_2014.dta", replace


******
**2016
******
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2017/PERSO_AC.csv", clear
rename ubic_geo inegi
sort inegi
destring totalca1, replace force
rename totalca1 t_person 
collapse (sum)t_person, by (inegi) 
gen year=2016
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/rec_humanos_2016.dta", replace

******
**2018
******
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2019/PERSO_AC.csv", clear
rename ubic_geo inegi
sort inegi
destring totalca1, replace force
rename totalca1 t_person 
collapse (sum)t_person, by (inegi) 
gen year=2018
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/rec_humanos_2018.dta", replace

*merge all:
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/rec_humanos_2010.dta", clear
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/rec_humanos_2012.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/rec_humanos_2014.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/rec_humanos_2016.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/rec_humanos_2018.dta"
sort inegi year
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/rec_humanos_2010_2018.dta", replace

