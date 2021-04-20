*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*ENVIPE processing 
*****************************************************

clear all
set more off  
set varabbrev off 

*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles"

*RUN conversion in R

*Import ENVIPE 2011
**PERCEPCION DE INSEGURIDAD
foreach i in  2012 2013 2014 2015 2016 2017 2018 2019{
*foreach i in 2011 2012 2013 2014 2015 2016 2017 2018 2019{
*foreach i in 2019 {
insheet using "../../Data/ConstructionDatabase/EncuestaVictimizacion/CSVs/envipe_`i'.csv", clear
capture rename control id_viv
foreach var of varlist _all {
capture replace `var' = "." if `var' == "NA"
capture destring `var', replace
}
capture rename control id_viv
capture rename ent cve_ent
capture rename mun cve_mun
capture rename dom dominio
capture rename resul_v result_v
gen envipe=`i'

*variables of interest change:
capture rename ap4_2_01 ap4_2_1
capture rename ap4_2_02 ap4_2_2
capture rename ap4_2_03 ap4_2_3
capture rename ap4_2_04 ap4_2_4
capture rename ap4_2_05 ap4_2_5
capture rename ap4_2_06 ap4_2_6
capture rename ap4_2_07 ap4_2_7
capture rename ap4_2_08 ap4_2_8
capture rename ap4_2_09 ap4_2_9

capture rename ap5_1_06 ap5_1_6 
capture rename ap5_1_07 ap5_1_7 
capture rename ap5_1_08 ap5_1_8 

*identifica
capture rename ap5_3_01 ap5_3_1 
capture rename ap5_3_02 ap5_3_2 
capture rename ap5_3_03 ap5_3_3 
capture rename ap5_3_04 ap5_3_4 
capture rename ap5_3_05 ap5_3_5 
capture rename ap5_3_06 ap5_3_6 
capture rename ap5_3_07 ap5_3_7 
capture rename ap5_3_08 ap5_3_8 
capture rename ap5_3_09 ap5_3_9
*confianza
capture rename ap5_4_01 ap5_4_1 
capture rename ap5_4_02 ap5_4_2 
capture rename ap5_4_03 ap5_4_3 
capture rename ap5_4_04 ap5_4_4 
capture rename ap5_4_05 ap5_4_5 
capture rename ap5_4_06 ap5_4_6 
capture rename ap5_4_07 ap5_4_7 
capture rename ap5_4_08 ap5_4_8 
capture rename ap5_4_09 ap5_4_9
*corrupta
capture rename ap5_5_01 ap5_5_1 
capture rename ap5_5_02 ap5_5_2 
capture rename ap5_5_03 ap5_5_3 
capture rename ap5_5_04 ap5_5_4 
capture rename ap5_5_05 ap5_5_5 
capture rename ap5_5_06 ap5_5_6 
capture rename ap5_5_07 ap5_5_7 
capture rename ap5_5_08 ap5_5_8 
capture rename ap5_5_09 ap5_5_9 
*desempeno
capture rename ap5_6_01 ap5_6_1 
capture rename ap5_6_02 ap5_6_2 
capture rename ap5_6_03 ap5_6_3 
capture rename ap5_6_04 ap5_6_4 
capture rename ap5_6_05 ap5_6_5 
capture rename ap5_6_06 ap5_6_6 
capture rename ap5_6_07 ap5_6_7 
capture rename ap5_6_08 ap5_6_8 
capture rename ap5_6_09 ap5_6_9 

*gasto
capture gen  ap4_12b=ap4_11 if envipe==2011
capture gen  ap4_12b=ap4_12 if envipe>=2012

save "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_`i'_pervic.dta", replace

}

**append
*use "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2011_pervic.dta", clear
use "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2012_pervic.dta", clear
*append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2012_pervic.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2013_pervic.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2014_pervic.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2015_pervic.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2016_pervic.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2017_pervic.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2018_pervic.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2019_pervic.dta"

keep envipe v1 id_viv upm viv_sel hogar cve_ent cve_mun dominio ///
ap4_2_1 ap4_2_2 ap4_2_3 ap4_2_4 ap4_2_5 ap4_2_6 ap4_2_7 ap4_2_8 ap4_2_8  ap4_2_9 ap4_2_10 ap4_2_11 ///
ap4_3_1 ap4_3_2 ap4_3_3 ap4_7_1 ap4_7_2 ap4_7_3 ap4_12b ///
ap5_1_6 ap5_1_7 ap5_1_8 ap5_1_10 ap5_1_12 ///
ap5_3_1 ap5_3_2 ap5_3_3 ap5_3_4 ap5_3_5 ap5_3_6 ap5_3_7 ap5_3_8 ap5_3_8 ap5_3_9 ///
ap5_4_1 ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_8 ap5_4_9 ///
ap5_5_1 ap5_5_2 ap5_5_3 ap5_5_4 ap5_5_5 ap5_5_6 ap5_5_7 ap5_5_8 ap5_5_8 ap5_5_9 ///
ap5_6_1 ap5_6_2 ap5_6_3 ap5_6_4 ap5_6_5 ap5_6_6 ap5_6_7 ap5_6_8 ap5_6_8 ap5_6_9

order envipe v1 id_viv upm viv_sel hogar cve_ent cve_mun dominio  ///
ap4_2_1 ap4_2_2 ap4_2_3 ap4_2_4 ap4_2_5 ap4_2_6 ap4_2_7 ap4_2_8 ap4_2_8  ap4_2_9 ap4_2_10 ap4_2_11 ///
ap4_3_1 ap4_3_2 ap4_3_3 ap4_7_1 ap4_7_2 ap4_7_3 ap4_12b ///
ap5_1_6 ap5_1_7 ap5_1_8 ap5_1_10 ap5_1_12 ///
ap5_3_1 ap5_3_2 ap5_3_3 ap5_3_4 ap5_3_5 ap5_3_6 ap5_3_7 ap5_3_8 ap5_3_8 ap5_3_9 ///
ap5_4_1 ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_8 ap5_4_9 ///
ap5_5_1 ap5_5_2 ap5_5_3 ap5_5_4 ap5_5_5 ap5_5_6 ap5_5_7 ap5_5_8 ap5_5_8 ap5_5_9 ///
ap5_6_1 ap5_6_2 ap5_6_3 ap5_6_4 ap5_6_5 ap5_6_6 ap5_6_7 ap5_6_8 ap5_6_8 ap5_6_9

sort v1 envipe

*save "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2011_2019_pervic.dta", replace
save "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2012_2019_pervic.dta", replace


**DATOS DE VIVIENDA
foreach i in  2012 2013 2014 2015 2016 2017 2018 2019{
*foreach i in 2011 2012 2013 2014 2015 2016 2017 2018 2019{
insheet using "../../Data/ConstructionDatabase/EncuestaVictimizacion/CSVs/envipe_`i'_viv.csv", clear
*capture keep v1 control ent mun  fac_viv dom ap2_1
*capture keep v1 id_viv ent mun  fac_viv dom ap2_1
rename ap2_1 personas_hogar
capture rename control id_viv
capture rename ent cve_ent
capture rename mun cve_mun
capture rename dom dominio
capture rename resul_v result_v
gen envipe=`i'
order envipe
save "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_`i'_viv.dta", replace
}

**append all datasets:
*use "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2011_viv.dta", clear
use "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2012_viv.dta", clear
*append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2012_viv.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2013_viv.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2014_viv.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2015_viv.dta", force
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2016_viv.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2017_viv.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2018_viv.dta"
append using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2019_viv.dta"

keep envipe v1 id_viv viv_sel cve_ent cve_mun fac_viv  
*save "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2011_2019_viv.dta", replace
save "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2012_2019_viv.dta", replace

merge 1:1 envipe v1  using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2012_2019_pervic.dta", force
*merge 1:1 envipe v1  using "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2011_2019_pervic.dta", force
drop if _merge!=3
drop _merge 

gen year=envipe-1
rename cve_ent estado 
order envipe year v1

**** Drop year 2011 and before. Variables are different as well as the methodology
drop if year<2012

*Variable cleaning
**coded =1 if si, 0=no
foreach i in ap4_2_1 ap4_2_2 ap4_2_3  ap4_2_4 ap4_2_5 ap4_2_6 ap4_2_7 ap4_2_8 ap4_2_9 ap4_2_10 ap4_2_11{
replace `i'=. if `i'==3
replace `i'=0 if `i'==2
replace `i'=. if `i'==8
replace `i'=. if `i'==9
}

**coded =1 if inseguro, 0=seguro
foreach i in ap4_3_1 ap4_3_2 ap4_3_3{
replace `i'=. if `i'==8
replace `i'=. if `i'==9
replace `i'=. if `i'==0
replace `i'=0 if `i'==1
replace `i'=1 if `i'==2
}

**ap_4_7 asks about the collection year, no the year prior
foreach i in ap4_7_1 ap4_7_2 ap4_7_3{
replace `i'=. if `i'==8
replace `i'=. if `i'==9
}

foreach i in ap4_7_1 ap4_7_2 ap4_7_3{
gen `i'_b=0 if `i'>=3
replace `i'_b=1 if `i'<3
}

**private security expenses in pesos
foreach i in ap4_12b{
replace `i'=. if `i'==8888888
replace `i'=. if `i'==9999999
}
**change to thousand pesos
replace ap4_12b=ap4_12b/1000


**no se realizo and no are given the value of 0
foreach i in ap5_1_6 ap5_1_7 ap5_1_8{
replace `i'=. if `i'==7
replace `i'=. if `i'==9
replace `i'=1 if `i'==1
replace `i'=0 if `i'==2
replace `i'=0 if `i'==3
/*check this one since 2 and 3 might not be right and it should be 1 and 2*/
}

**organizaciones que el ciudadano identifica:
foreach i in ap5_3_1 ap5_3_2 ap5_3_3 ap5_3_4 ap5_3_5 ap5_3_6 ap5_3_7 ap5_3_8 ap5_3_9{
replace `i'=. if `i'==3
replace `i'=. if `i'==8
replace `i'=. if `i'==9
}

**1=si, 0=no
foreach i in ap5_3_1 ap5_3_2 ap5_3_3 ap5_3_4 ap5_3_5 ap5_3_6 ap5_3_7 ap5_3_8 ap5_3_9{
replace `i'=0 if `i'==2
replace `i'=1 if `i'==1
}

**confianza en la autoridad
foreach i in ap5_4_1 ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_9{
replace `i'=. if `i'==8
replace `i'=. if `i'==9
}
**1=mucha o alguna, 0=poco o nada
foreach i in ap5_4_1 ap5_4_2 ap5_4_3 ap5_4_4 ap5_4_5 ap5_4_6 ap5_4_7 ap5_4_8 ap5_4_9{
gen `i'_b=1 if `i'<3
replace `i'_b=0 if `i'>2
}

**corrupta: 1=si, 0=no
foreach i in ap5_5_1 ap5_5_2 ap5_5_3 ap5_5_4 ap5_5_5 ap5_5_6 ap5_5_7 ap5_5_8 ap5_5_9{
replace `i'=0 if `i'==2
replace `i'=1 if `i'==1
}

**desempeno de la autoridad
foreach i in ap5_6_1 ap5_6_2 ap5_6_3 ap5_6_4 ap5_6_5 ap5_6_6 ap5_6_7 ap5_6_8 ap5_6_9{
replace `i'=. if `i'==8
replace `i'=. if `i'==9
}
**1=mucha o alguna, 0=poco o nada
foreach i in ap5_6_1 ap5_6_2 ap5_6_3 ap5_6_4 ap5_6_5 ap5_6_6 ap5_6_7 ap5_6_8 ap5_6_9{
gen `i'_b=1 if `i'<3
replace `i'_b=0 if `i'>2
}





collapse (mean) ap4_2_1-ap5_6_9_b   [aweight=fac_viv], by(estado year)
 
*save "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2011_2019_estado.dta", replace
save "../../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2012_2019_estado.dta", replace


/*Questions of interest
PERCEPCION DE INSEGURIDAD
Temas que mas preocupan
ap4_2_1 pobreza
ap4_2_2 desempleo
ap4_2_3 narcotr'afico
ap4_2_4 aumento de precios
ap4_2_5 inseguridad
ap4_2_6 desastres naturales
ap4_2_7 escasez de agua
ap4_2_8 corrupción
ap4_2_9 educación
ap4_2_10 salud
ap4_2_11 falta castigo a delincuentes

¿En términos de delincuencia, considera que vivir en ... es… [use this to measure distance]
ap4_3_1 colonia
ap4_3_2 municipio
ap4_3_3 estado

4.7 De acuerdo con su experiencia, ¿considera que en lo que resta de 2019 la seguridad pública en (ÁMBITO GEOGRÁFICO) …
ap4_7_1 colonia
ap4_7_2 municipio
ap4_7_3 estado


For 2011, 
4.11 ¿Cuánto gastaron en total por esas medidas durante 2010? (en pesos)
ap4_11

For 2012-2019
4.12 ¿Cuánto gastaron en total por esas medidas durante 2018? (en pesos)
ap4_12

DESEMPENO INSTITUCIONAL
¿Sabe usted si alguna de las siguientes acciones se realizaron en el 2018 en su (MUNICIPIO/LOCALIDAD), como… 
ap5_1_6 contratar seguridad privada?
ap5_1_7 policias barriales
ap5_1_8 operativo contra delincuencia
ap5_1_10 patrullaje
ap5_1_12 combatir narco

5.3 De las autoridades que le mencionaré, dígame a cuáles identifica. Sí o No.
ap5_3_1 policia tránsito
ap5_3_2 policia preventiva municipal
ap5_3_3 policia estatal
ap5_3_4 policia federal
ap5_3_5 policia ministerial o judicial
ap5_3_6 MP, procuradurias estatales
ap5_3_7 PGR
ap5_3_8 ejercito
ap5_3_9 marina

5.4 ¿Cuánta confianza le inspira la (el) (AUTORIDAD)? Goes from 1 (mucha), 2(alguna), 3 (poca) to 4 (nada). The dummy indicator goes from negative to positive.
ap5_4_1 policia tránsito
ap5_4_2 policia preventiva municipal
ap5_4_3 policia estatal
ap5_4_4 policia federal
ap5_4_5 policia ministerial o judicial
ap5_4_6 MP, procuradurias estatales
ap5_4_7 PGR
ap5_4_8 ejercito
ap5_4_9 marina

*/


