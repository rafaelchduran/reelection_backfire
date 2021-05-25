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
*ACTIVIDADES AYUNTAMIENTOS 
******
**2010
******

*Sesiones
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2011/ACT_AYUN.csv", clear
rename ubi_geo inegi
sort inegi
replace ses_cab=2009 if ses_cab==1 | ses_cab==2
replace ses_cab=2010 if ses_cab==3 | ses_cab==4

collapse (mean)tot_ses, by (inegi ses_cab) 
rename ses_cab year
keep if year==2010
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2010.dta", replace
*Comisiones
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2011/COM_AYUN.csv", clear
rename ubi_geo inegi
collapse (mean)tot_com, by (inegi) 
gen year=2010
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/comisiones_2010.dta", replace
*Iniciativas comision
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2011/IN_TEASU.csv", clear
rename ubi_geo inegi
collapse (mean)tot_init, by (inegi) 
gen year=2010
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_2010.dta", replace
*Iniciativas
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2011/INI_AYUN.csv", clear
rename ubi_geo inegi
collapse (mean)tot_inic, by (inegi) 
gen year=2010
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_ayun_2010.dta", replace


******
**2012
******

*Sesiones
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2013/ACT_AYUN.csv", clear
rename ubic_geo inegi
sort inegi
rename sesi_cab ses_cab
replace ses_cab=2011 if ses_cab==1 | ses_cab==2
replace ses_cab=2012 if ses_cab==3 | ses_cab==4
rename tt_sesio tot_ses
collapse (mean)tot_ses, by (inegi ses_cab) 
rename ses_cab year
preserve
keep if year==2011
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2011.dta", replace
restore
preserve
keep if year==2012
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2012.dta", replace
restore

*Comisiones
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2013/COM_AYUN.csv", clear
rename ubic_geo inegi
rename tt_comis tot_com
collapse (mean)tot_com, by (inegi) 
gen year=2012
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/comisiones_2012.dta", replace
*Iniciativas 
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2013/IN_TEASU.csv", clear
rename ubic_geo inegi
rename tt_inici tot_inic
collapse (mean)tot_inic, by (inegi) 
gen year=2012
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_ayun_2012.dta", replace


******
**2014
******

*Sesiones
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2015/ACT_AYUN.csv", clear
rename ubic_geo inegi
sort inegi
rename sesi_cab ses_cab
replace ses_cab=2013 if ses_cab==1 | ses_cab==2
replace ses_cab=2014 if ses_cab==3 | ses_cab==4
rename tt_sesio tot_ses
collapse (mean)tot_ses, by (inegi ses_cab) 
rename ses_cab year
preserve
keep if year==2013
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2013.dta", replace
restore
preserve
keep if year==2014
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2014.dta", replace
restore
*Comisiones
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2015/COM_AYUN.csv", clear
rename ubic_geo inegi
rename tt_comis tot_com
collapse (mean)tot_com, by (inegi) 
gen year=2014
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/comisiones_2014.dta", replace
*Iniciativas 
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2015/IN_TEASU.csv", clear
rename ubic_geo inegi
rename tt_inici tot_inic
collapse (mean)tot_inic, by (inegi) 
gen year=2014
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_ayun_2014.dta", replace


******
**2016
******

*Sesiones
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2017/CABILDOS.csv", clear
rename ubic_geo inegi
sort inegi
rename sescab16 ses_cab
replace ses_cab=2015 if ses_cab==1 
replace ses_cab=2016 if ses_cab==2
rename totalca1 tot_ses
destring tot_ses, replace force 
collapse (mean)tot_ses, by (inegi ses_cab) 
rename ses_cab year
preserve
keep if year==2015
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2015.dta", replace
restore
preserve
keep if year==2016
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2016.dta", replace
restore

/*Sesiones abiertas
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2017/CABIASIS.csv", clear
rename ubic_geo inegi
sort inegi
rename cabild16 ses_cab
replace ses_cab=2015 if ses_cab==1 | ses_cab==2
replace ses_cab=2016 if ses_cab==3 | ses_cab==4
rename totalca1 tot_ses
destring tot_ses, replace force 
collapse (mean)tot_ses, by (inegi ses_cab) 
rename tot_ses tot_ses_b
rename ses_cab year
preserve
keep if year==2015
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesionesb_2015.dta", replace
restore
preserve
keep if year==2016
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesionesb_2016.dta", replace
restore

*merge a and b
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesionesa_2015.dta", clear
merge 1:1 inegi year using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesionesb_2015.dta"
replace tot_ses=tot_ses+tot_ses_b
drop tot_ses_b
*/

*Iniciativas 
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2017/INICIATI.csv", clear
rename ubic_geo inegi
rename totalca1 tot_inic
keep if inicicab==3
destring tot_inic, replace force
collapse (mean)tot_inic, by (inegi) 
gen year=2016
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_ayun_2016.dta", replace


******
**2018
******

*Sesiones
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2019/CABILDOS.csv", clear
rename ubic_geo inegi
sort inegi
rename sescab18 ses_cab
replace ses_cab=2017 if ses_cab==1 
replace ses_cab=2018 if ses_cab==2
rename totalca1 tot_ses
destring tot_ses, replace force 
collapse (mean)tot_ses, by (inegi ses_cab) 
rename ses_cab year
preserve
keep if year==2017
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2017.dta", replace
restore
preserve
keep if year==2018
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2018.dta", replace
restore

*Iniciativas 
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/ForIncumbency/2019/INICIATI.csv", clear
rename ubic_geo inegi
rename totalca1 tot_inic
keep if inicicab==3
destring tot_inic, replace force
collapse (mean)tot_inic, by (inegi) 
gen year=2018
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_ayun_2018.dta", replace

*merge all sesiones:
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2010.dta", clear
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2011.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2012.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2013.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2014.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2015.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2016.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2017.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2018.dta"
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/sesiones_2010_2018.dta", replace

*merge all iniciativas:
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_ayun_2010.dta", clear
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_ayun_2012.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_ayun_2014.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_ayun_2016.dta"
append using  "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_ayun_2018.dta"
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/StataIncumbency/iniciativas_ayun_2010_2018.dta", replace

