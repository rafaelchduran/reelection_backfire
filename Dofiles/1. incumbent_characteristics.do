*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*Incumbents characteristics   
*****************************************************

clear all
set more off  
set varabbrev off 

*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Dofiles"

*Merge all .csv files
foreach i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32{
insheet using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_`i'.csv", names clear
save "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_`i'.dta", replace
}

use "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_1.dta", clear
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_2.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_3.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_4.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_5.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_6.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_7.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_8.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_9.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_10.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_11.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_12.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_13.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_14.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_15.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_16.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_17.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_18.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_19.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_20.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_21.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_22.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_23.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_24.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_25.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_26.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_27.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_28.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_29.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_30.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_31.dta"
append using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_32.dta"


save "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico.dta", replace
export delimited using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico.csv", replace

*insheet final dataset to export and then extract a single year with municipalities
insheet using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_final.csv", clear
collapse (mean) ano_inicial, by(estado municipio)
drop ano_inicial
export delimited using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_forinegi.csv", replace

*collapse with inegi
insheet using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_with_inegi.csv", clear
save "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_with_inegi.dta", replace

*final merge:
insheet using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_final.csv", clear
replace estado="DISTRITO FEDERAL" if estado=="CDMX"
merge m:m estado municipio using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_with_inegi.dta"

drop if estado==""
drop _merge
sort inegi ano_inicial ano_final
/*bysort inegi ano_inicial: replace ano_final=ano_inicial[_n+1] if ano_final==.
sort inegi ano_inicial ano_final */

split presidentemunicipal, p(" ")
rename presidentemunicipal1 title
drop presidentemunicipal2-presidentemunicipal9
rename partido partido_from_snim
rename ano_inicial year
keep if year>2009 & year<2020

*by cases modify the year to state the election year rather than the year in office:
replace year=year-1 if estado=="Aguascalientes" & year<2019
replace year=year-1 if estado=="Chiapas"  & year==2011
replace year=year-1 if estado=="Coahuila de Zaragoza" & year==2010 
replace year=year-1 if estado=="Coahuila de Zaragoza" & year==2014 
replace year=year-1 if estado=="Coahuila de Zaragoza" & year==2018 
replace year=year-1 if estado=="Coahuila de Zaragoza" & year==2019 
replace year=year-1 if estado=="Hidalgo" & year==2012
replace year=year-1 if estado=="México" & year==2013 
replace year=year-1 if estado=="México" & year==2016 
replace year=year-1 if estado=="México" & year==2018
replace year=year-1 if estado=="México" & year==2019
replace year=year-1 if estado=="Michoacán de Ocampo" & year==2012
replace year=year-1 if estado=="Morelos" & year==2013 
replace year=year-1 if estado=="Morelos" & year==2016 
replace year=year-1 if estado=="Morelos" & year==2019 
replace year=year-1 if estado=="Oaxaca" & year==2011 
replace year=year-1 if estado=="Oaxaca" & year==2012 
replace year=year-1 if estado=="Oaxaca" & year==2014 
replace year=year-1 if estado=="Oaxaca" & year==2015 
replace year=year-1 if estado=="Oaxaca" & year==2017 
replace year=year-1 if estado=="Oaxaca" & year==2019 
replace year=year-1 if estado=="Puebla" & year==2011 
replace year=year-1 if estado=="Puebla" & year==2014 
replace year=year-1 if estado=="Quintana Roo" & year==2010 
replace year=year-1 if estado=="Sinaloa" & year==2014
replace year=year-1 if estado=="Tabasco" & year==2010 
replace year=year-1 if estado=="Tabasco" & year==2013 
replace year=year-1 if estado=="Tabasco" & year==2016 
replace year=year-1 if estado=="Tamaulipas" & year==2011 
*replace year=year-1 if estado=="Tamaulipas" & year==2016
replace year=year-1 if estado=="Tlaxcala" & year==2011 
replace year=year-1 if estado=="Tlaxcala" & year==2014 
replace year=year-1 if estado=="Tlaxcala" & year==2017 
replace year=year-1 if estado=="Veracruz de Ignacio de la Llave" & year==2011 
replace year=year-1 if estado=="Veracruz de Ignacio de la Llave" & year==2014 
replace year=year-1 if estado=="Veracruz de Ignacio de la Llave" & year==2018 
*Don´t have info of Yucatan 2010 election

**variables to keep to merge to main dataset:
keep sexo partido_from_snim year ano_final inegi title presidentemunicipal
order inegi year ano_final sexo partido_from_snim presidentemunicipal title

save "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_winegi_final.dta", replace

