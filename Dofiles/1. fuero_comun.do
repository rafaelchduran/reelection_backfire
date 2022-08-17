*Adding petty crime
*Paper: Reelection Backfire
*Author: Rafael Ch (rafael.ch@nyu.edu)
*========================================================================
/*NOTES
Merge numbers changed because I added years from 2004 to 2009, and 2019
*/

*========================================================================
*Environment
clear all
set more off  
set varabbrev off 

*========================================================================
*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles"

insheet using "../../Data/ConstructionDatabase/DenunciasSNSP/Municipal-Delitos-2015-2019_dic19/Municipal-Delitos - diciembre 2019.csv", clear
*A. patrimonio
preserve
keep if bienjurdicoafectado=="El patrimonio" 
foreach crime in casa{
egen `crime'=rowtotal(enero febrero marzo abril mayo junio julio agosto septiembre octubre noviembre diciembre)
collapse (sum)`crime' (firstnm)clave_ent entidad  municipio, by(cvemunicipio ao)
order ao clave_ent entidad  municipio cvemunicipio `crime'
keep ao cvemunicipio `crime'
rename ao year
label variable year "year"
rename cvemunicipio inegi
label variable `crime' "`crime'"
save "../../Data/ConstructionDatabase/DenunciasSNSP/Municipal-Delitos-2015-2019_dic19/`crime'_2015_2019.dta", replace
}
restore

*B. La familia
preserve
keep if bienjurdicoafectado=="La familia" 
foreach crime in familia{
egen `crime'=rowtotal(enero febrero marzo abril mayo junio julio agosto septiembre octubre noviembre diciembre)
collapse (sum)`crime' (firstnm)clave_ent entidad  municipio, by(cvemunicipio ao)
order ao clave_ent entidad  municipio cvemunicipio `crime'
keep ao cvemunicipio `crime'
rename ao year
label variable year "year"
rename cvemunicipio inegi
label variable `crime' "`crime'"
save "../../Data/ConstructionDatabase/DenunciasSNSP/Municipal-Delitos-2015-2019_dic19/`crime'_2015_2019.dta", replace
}
restore

*C. La sociedad
preserve
keep if bienjurdicoafectado=="La sociedad" 
foreach crime in sociedad{
egen `crime'=rowtotal(enero febrero marzo abril mayo junio julio agosto septiembre octubre noviembre diciembre)
collapse (sum)`crime' (firstnm)clave_ent entidad  municipio, by(cvemunicipio ao)
order ao clave_ent entidad  municipio cvemunicipio `crime'
keep ao cvemunicipio `crime'
rename ao year
label variable year "year"
rename cvemunicipio inegi
label variable `crime' "`crime'"
save "../../Data/ConstructionDatabase/DenunciasSNSP/Municipal-Delitos-2015-2019_dic19/`crime'_2015_2019.dta", replace
}
restore

*D. Libertad personal
preserve
keep if bienjurdicoafectado=="Libertad personal" 
foreach crime in libertad{
egen `crime'=rowtotal(enero febrero marzo abril mayo junio julio agosto septiembre octubre noviembre diciembre)
collapse (sum)`crime' (firstnm)clave_ent entidad  municipio, by(cvemunicipio ao)
order ao clave_ent entidad  municipio cvemunicipio `crime'
keep ao cvemunicipio `crime'
rename ao year
label variable year "year"
rename cvemunicipio inegi
label variable `crime' "`crime'"
save "../../Data/ConstructionDatabase/DenunciasSNSP/Municipal-Delitos-2015-2019_dic19/`crime'_2015_2019.dta", replace
}
restore

/*E. Otros bienes juridicos
preserve
keep if bienjurdicoafectado=="Otros bienes jur�dicos afectados (del fuero com�n)" 
foreach crime in otros{
egen `crime'=rowtotal(enero febrero marzo abril mayo junio julio agosto septiembre octubre noviembre diciembre)
collapse (sum)`crime' (firstnm)clave_ent entidad  municipio, by(cvemunicipio ao)
order ao clave_ent entidad  municipio cvemunicipio `crime'
keep ao cvemunicipio `crime'
rename ao year
label variable year "year"
rename cvemunicipio inegi
label variable `crime' "`crime'"
save "../../Data/ConstructionDatabase/DenunciasSNSP/Municipal-Delitos-2015-2019_dic19/`crime'_2015_2019.dta", replace
}
restore
*/



*MERGE WITH FINAL DATASET: 
use "../../Data/ConstructionDatabase/data_final.dta", clear
drop _merge
foreach crime in casa familia sociedad libertad{
merge 1:1 inegi year using "../../Data/ConstructionDatabase/DenunciasSNSP/Municipal-Delitos-2015-2019_dic19/`crime'_2015_2019.dta"
drop if _merge==2
drop _merge
}

*Transform variables: 
foreach i in casa familia sociedad libertad{
**1) crime per 100,000 inhabitants
gen `i'pc=(`i'/pop)*100000

**2) logged(crime)
gen log`i'=log(`i'+1)

**3)logged crime per 100,000 inhabitants (using log((count + 1)/pop))
gen log`i'pc=log((`i'+1)/pop)

**4) inverse hyperbolic sine homicides 
gen ihs_`i'=asinh(`i')
}

**5) inverse hyperbolic sine homicides per capita
foreach i in casapc familiapc sociedadpc libertadpc{
gen ihs_`i'=asinh(`i')
}

*!!! correct this  
foreach i in casa sociedad libertad casapc casapc logcasapc logcasa familiapc logfamilia logfamiliapc sociedadpc logsociedad logsociedadpc libertadpc loglibertad loglibertadpc{
replace `i'=. if `i'==0
}



*SAVE FILE
save "../../Data/ConstructionDatabase/data_final_fuerocomun.dta", replace
