*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*Mando unico
*****************************************************

clear all
set more off  
set varabbrev off 

*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles"

/*
Difference between acuerdo and acuerdo2:
-acuerdo asks if "Clasificacióndelosmunicipiosdeacuerdoconlaexistenciadealgúnconveniooacuerdoosimilarparaquela
AdministraciónPúblicaEstatalatravésdesusinstitucionessehicierancargodemaneratemporalopermanentedelaprestación
delserviciodeseguridadpúblicaensumunicipio,almomentodelaaplicacióndelcenso(mayo- octubre year)."
-acuerdo2 comes from the noaplica option of motive behind having a security cooperation agreement. 

Censos Gobiernos Municipales
2011:
The two files with mando unico information:
EFUN_SPU.dbf under Ejercicio de la Funcion folder [question on cuenta con acuerdo in 2011]
MOTIVCON.dbf under Ejercicio de la Funcion folder [motivo acuerdo in 2011]

The two files that contain these questions are: REC_HUSP.dbf under Recursos Humanos [I call this mando_unico_2011c]: 
-p.15: cantidad total de personal en seguridad pública municipal del 2010

and INFRA_SP.dbf under Infraestructura [I call this mando_unico_2011d]:
-p.23: cantidad de comandancia y/o etaciones y/o módulo y/o caetas para seguridad pública a nivel municipal
-p.25: hay Centros de evaluación y confianza estatal
-p.26: seguridad pública evaluada por el centro de evaluación y confianza estatal?
-p.27: % evaluado
-p.28: % aprobo

2013:
SPRE_HUM.dbf under SP_Recursos_cngmd2013_dbf
-TT_PERSO: cantidad total de personal en seguridad pública municipal del 2010
**no response to the evaluation question

**exploit the wage of police men.

Note:
-with Mando unico (acuerdogobierno3) I cannot check pretrends as they didn´t exit prior to that.67


Acuerdos by year:
acuerdo_gobestatal (seg publica): 2011, 2013, 2014, 2016, 2018
acuerdo_gobestatal2 (seg publica, based on needs): 2011, 2013, 2014, 2016, 2018
acuerdo_gobestatal3 (mando unico): 2014, 2015, 2017, 2018
acuerdo_gobestatal4 (seg publica, based on services): 2014, 2016, 2018

Motivos: 2011, 2013, 2014, 2016, 2018

Gobiernos in agreement: 2014, 2016, 2018

Servicios: 2014, 2016 (expanded), 2018 (expanded)

Tipoacuerdo: 2014

So choices:
1. no aplica option // its there right now
2. what agreements are used to create acuerdo and acuerdo2 // I fixed this
3. how to separate Mando Unico from others // on the works
4. assumption that same thing holds for the years that I'm missing 
*/

*Clean datasets
******
**2011
******

*Acuerdos
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2011a.csv", clear
rename ubi_geo inegi
sort inegi
foreach i in cuen_con tipo_int sol_ipol tipo_sis prob_rvi acio_pol delitoid rvic_fco rvic_ffe vict_fue t_interv t_solici t_victim est_tint est_t_so est_tvic{
replace `i'="." if `i'=="NA"
destring `i', replace
}

collapse (mean)cuen_con, by (inegi) // cuen_con: Gobierno del Estado se haga cargo de manera temporal o permanente de la prestación de servicios de seguridad pública
replace cuen_con=. if cuen_con==0 // no aplica option
replace cuen_con=0 if cuen_con==2
replace cuen_con=. if cuen_con==9
replace cuen_con=. if cuen_con==3
bysort inegi:  gen dup = cond(_N==1,0,_n)
drop dup
rename cuen_con acuerdo_gobestatal
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011a.dta", replace

*Motivos:
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2011b.csv", clear
rename ubi_geo inegi
sort inegi
replace mot_con=. if mot_con==8 // mot_con: Gobierno del Estado se haga cargo de manera temporal o permanente de la prestación de servicios de seguridad pública
replace mot_con=. if mot_con==9
tab mot_con, gen(motivo_)
collapse (mean)motivo_*, by (inegi) 
// motivo_1 no aplica
foreach i in 1 2 3 4 5 6 7 8{
replace motivo_`i'=1 if motivo_`i'>0
}
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011b.dta", replace

*Intervenciones and solicitudes de emergencia, y victimas (from 2010 and registered in 2011)
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2011a.csv", clear
rename ubi_geo inegi
sort inegi
foreach i in t_interv t_solici t_victim{
replace `i'="." if `i'=="NA"
destring `i', replace
replace `i'=0 if `i'==.
}

collapse (sum)t_interv t_solici t_victim, by (inegi) 
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011c.dta", replace


***merge 2011
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011a.dta", clear
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011b.dta"
drop _merge
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011c.dta"
drop _merge
gen acuerdo_gobestatal2=0 if motivo_1==1 /*no aplica option*/
egen summotivos=rowtotal( motivo_2 motivo_3 motivo_4 motivo_5 motivo_6 motivo_7 motivo_8)
replace acuerdo_gobestatal2=1 if summotivos>0
gen year=2011
rename motivo_1 motivo_noaplica
rename motivo_2 motivo_reformacons
rename motivo_3 motivo_reformaley
rename motivo_4 motivo_faltarecursos
rename motivo_5 motivo_profesioalizacion
rename motivo_6 motivo_coordinacion
rename motivo_7 motivo_crimen
rename motivo_8 motivo_otros
order inegi year
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011.dta", replace

******
**2013
******
*Acuerdos Gob. Estatal:
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2013a.csv", clear
rename ubic_geo inegi
sort inegi
foreach i in cuen_con{
replace `i'="." if `i'=="NA"
destring `i', replace
}

drop if cuen_con==.

collapse (mean)cuen_con, by (inegi) // cuen_con: AdministraciónPúblicaEstatalatravésdesusinstitucionessehicierancargodemaneratemporalopermanentedelaprestacióndelserviciodeseguridad
replace cuen_con=. if cuen_con==0 // no aplica option
replace cuen_con=0 if cuen_con==2
replace cuen_con=. if cuen_con==9
replace cuen_con=. if cuen_con==10
bysort inegi:  gen dup = cond(_N==1,0,_n)
drop dup
rename cuen_con acuerdo_gobestatal
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013a.dta", replace

*Motivos:
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2013a.csv", clear
rename ubic_geo inegi
sort inegi
foreach i in mot_conv{
replace `i'="." if `i'=="NA"
destring `i', replace
}

drop if mot_conv==.
sort inegi
replace mot_conv=. if mot_conv==9 // mot_conv:motivo que AdministraciónPúblicaEstatalatravésdesusinstitucionessehicieracargodemaneratemporalopermanentedelaprestacióndeserviciosdeseguridadpúblicaensumunicipio
replace mot_conv=. if mot_conv==10
replace mot_conv=. if mot_conv==11
tab mot_conv, gen(motivo_)
collapse (mean)motivo_*, by (inegi)
foreach i in 1 2 3 4 5 6 7 8{
replace motivo_`i'=1 if motivo_`i'>0
}
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013b.dta", replace

*Intervenciones and solicitudes de emergencia, y victimas (from 2012 and registered in 2013)
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2013a.csv", clear
rename ubic_geo inegi
sort inegi
rename tt_inter t_interv // Lacantidaddeintervencionesrealizadasporlapolicíamunicipal,durante el año 2012.
rename tt_prede t_prede // Lacantidaddepresuntosdelitosregistradosenlasintervencionesrealizadas por la policía municipal, durante el año 2012.
rename tt_victi t_victim // Lacantidaddeprobablesvíctimasregistradasenlasintervencionesde la policía municipal, durante el año 2012.
rename tt_probr t_probr // Lacantidaddeprobablesresponsablesregistradosenlasintervenciones de la policía municipal, durante el año 2012.
foreach i in t_interv t_prede t_victim t_probr{
replace `i'="." if `i'=="NA"
destring `i', replace
replace `i'=0 if `i'==.
}

collapse (sum)t_interv t_prede t_victim t_probr, by (inegi) 
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013c.dta", replace


***merge 2013
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013a.dta", clear
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013b.dta"
drop _merge
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013c.dta"
drop _merge
gen acuerdo_gobestatal2=0 if motivo_1==1 /*no aplica option*/
egen summotivos=rowtotal( motivo_2 motivo_3 motivo_4 motivo_5 motivo_6 motivo_7 motivo_8)
replace acuerdo_gobestatal2=1 if summotivos>0
gen year=2013
rename motivo_1 motivo_noaplica
rename motivo_2 motivo_reformacons
rename motivo_3 motivo_reformaley
rename motivo_4 motivo_faltarecursos
rename motivo_5 motivo_profesioalizacion
rename motivo_6 motivo_coordinacion
rename motivo_7 motivo_crimen
rename motivo_8 motivo_otros
order inegi year
tab acuerdo_gobestatal acuerdo_gobestatal2
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013.dta", replace

******
**2014 & 2015
******
*SPEJFUES.dbf in 
**conv_mup has data for 2015 while aso_gfdm has data for 2014
/**conv_mup: "Clasificacióndelosmunicipiosdeacuerdoconlaexistenciadeconveniooacuerdoosimilarparaquela
AdministraciónPúblicadesuEntidadFederativaoperebajoelsistemadeMandoÚnicoPolicialysehagacargodemaneratemporal
opermanentedelaprestacióndelserviciodeseguridadpública,tránsitoyvialidadenelmunicipio,almomento de la aplicación
 del censo (mayo - junio 2015)."
 
 *aso_gfdm: "Clasificacióndelosmunicipiosdeacuerdoconlaasociacióndelgobiernomunicipalconelgobiernofederalode
 otrasentidadesfederativas,municipiosodelegacionespararealizarfuncionesdeseguridad pública, durante el año 2014."
 
 --so technically, conv_mup is the one tied to the questions done before on treaty with governor and not president. 
 --my doubt i if conv_mup should be named acuerdo_gobestatal instead of acuerdo_gobestatal3, and aso_gfdm should be a 
 new measure of agreement in general. 
 --I cannot separate whether the agreement was signed with president or governor, only when there is difference 
 between signing Mando 'Unico and not. I could use the difference between conv_mup and aso_gfdm to get an agreeement that 
 is not with the governor. 
 
 *serv_pub: "Clasificacióndelosmunicipiosdeacuerdoconlosserviciospúblicosy/ofuncionesobjetodelaasociacióninter
 gubernamentalparaoperar bajo el Mando Único Policial, durante el año 2014."
 */
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2015.csv", clear
rename ubic_geo inegi
sort inegi
foreach i in conv_mup aso_gfdm serv_pub inst_reg tipo_gob raz_conv tt_gobas est_goba{
replace `i'="." if `i'=="NA"
destring `i', replace
}
* aso_gfdm es el total de acuerdos, incluidos el mando unico 
* el resto debe tener el mismo numero de 0s. La diferencia es el mando unico. 
collapse (mean) aso_gfdm raz_conv serv_pub inst_reg, by(inegi)
foreach i in aso_gfdm {
replace `i'=. if `i'==8
replace `i'=. if `i'==9
replace `i'=0 if `i'==2
}
foreach i in raz_conv serv_pub inst_reg{
replace `i'=. if `i'==8
replace `i'=. if `i'==9
replace `i'=0 if `i'==0
replace `i'=1 if `i'>0
}

tab aso_gfdm raz_conv

*2015
preserve

drop if conv_mup==. // AdministraciónPúblicadesuEntidadFederativaoperebajoelsistemadeMandoÚnicoPolicialysehagacargodemaneratemporalopermanentedelaprestacióndelserviciodeseguridadpública
collapse (mean)conv_mup, by (inegi)
replace conv_mup=. if conv_mup==0 // no aplica option
replace conv_mup=0 if conv_mup==2
replace conv_mup=. if conv_mup==8
replace conv_mup=. if conv_mup==9
bysort inegi:  gen dup = cond(_N==1,0,_n)
tab dup
drop dup
rename conv_mup acuerdo_gobestatal3
gen year=2015
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2015.dta", replace
restore


*2014

preserve
drop if aso_gfdm==. // Clasificacióndelosmunicipiosdeacuerdoconlaasociacióndelgobiernomunicipalconelgobiernofederalodeotrasentidadesfederativas,municipiosodelegacionespararealizarfuncionesdeseguridad pública, durante el año 2014.
collapse (mean)aso_gfdm, by (inegi)
replace aso_gfdm=. if aso_gfdm==0 // no aplica option
replace aso_gfdm=0 if aso_gfdm==2
replace aso_gfdm=. if aso_gfdm==8
replace aso_gfdm=. if aso_gfdm==9
bysort inegi:  gen dup = cond(_N==1,0,_n)
tab dup
drop dup
rename aso_gfdm acuerdo_gobestatal gen year=2014
order inegi year
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014a.dta", replace
restore

preserve
drop if raz_conv==.
sort inegi
replace raz_conv=. if raz_conv==8
replace raz_conv=. if raz_conv==9
tab raz_conv, gen(motivo_)
collapse (mean)motivo_*, by (inegi)
foreach i in 1 2 3 4 5 6 7 8{
replace motivo_`i'=1 if motivo_`i'>0
}
gen year=2014
order inegi year
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014b.dta", replace
restore


preserve
drop if serv_pub==.
sort inegi
replace serv_pub=. if serv_pub==8
replace serv_pub=. if serv_pub==9
tab serv_pub, gen(funciones_)
collapse (mean)funciones_*, by (inegi)
foreach i in 1 2 3 4 {
replace funciones_`i'=1 if funciones_`i'>0
}
gen year=2014
gen acuerdo_gobestatal4=0 if funciones_1==1
replace acuerdo_gobestatal4=1 if funciones_2==1 | funciones_3==1 | funciones_4==1 // may change this to only funciones_4
rename funciones_1 servicio_noaplica
rename funciones_2 servicio_segpublica
rename funciones_3 servicio_transito
rename funciones_4 acuerdo_gobestatal3

order inegi year
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014c.dta", replace
restore

preserve
drop if inst_reg==.
sort inegi
replace inst_reg=. if inst_reg==8
replace inst_reg=. if inst_reg==9
tab inst_reg, gen(tipoacuerdo_)
collapse (mean)tipoacuerdo_*, by (inegi)
foreach i in 1 2 3 4 5 {
replace tipoacuerdo_`i'=1 if tipoacuerdo_`i'>0
}
gen year=2014
rename tipoacuerdo_1 tipoacuerdo_noaplica
rename tipoacuerdo_2 tipoacuerdo_convenio
rename tipoacuerdo_3 tipoacuerdo_contrato
rename tipoacuerdo_4 tipoacuerdo_acuerdo
rename tipoacuerdo_5 tipoacuerdo_otro
order inegi year
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014d.dta", replace
restore


preserve
drop if tipo_gob==.
sort inegi
replace tipo_gob=. if tipo_gob==8
replace tipo_gob=. if tipo_gob==98
replace tipo_gob=. if tipo_gob==99
tab tipo_gob, gen(tipogob_)
collapse (mean)tipogob_*, by (inegi)
foreach i in 1 2 3 4 5 6 7 8{
replace tipogob_`i'=1 if tipogob_`i'>0
}
gen year=2014
rename tipogob_1 gobier_noaplica
rename tipogob_2 gobier1_4
rename tipogob_3 gobier5_7
rename tipogob_4 gobier8
rename tipogob_5 gobier9_12
rename tipogob_6 tipogob_fedymun
rename tipogob_7 tipogob_estymun
rename tipogob_8 tipogob_fedestymun
order inegi year
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014e.dta", replace
restore

preserve
drop if tt_gobas==.
sort inegi
collapse (mean)tt_gobas, by (inegi)
replace tt_gobas=round(tt_gobas)
gen year=2014
rename tt_gobas num_gob_convenio
order inegi year
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014f.dta", replace
restore

***merge 2014
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014a.dta", clear
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014b.dta"
drop if _merge==2
drop _merge
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014c.dta"
drop if _merge==2
drop _merge
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014d.dta"
drop if _merge==2
drop _merge
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014e.dta"
drop if _merge==2
drop _merge
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014f.dta"
drop if _merge==2
drop _merge


gen acuerdo_gobestatal2=0 if motivo_1==1 /*no aplica option*/
egen summotivos=rowtotal( motivo_2 motivo_3 motivo_4 motivo_5 motivo_6 motivo_7 motivo_8)
replace acuerdo_gobestatal2=1 if summotivos>0
rename motivo_1 motivo_noaplica
rename motivo_2 motivo_reformacons
rename motivo_3 motivo_reformaley
rename motivo_4 motivo_faltarecursos
rename motivo_5 motivo_profesioalizacion
rename motivo_6 motivo_coordinacion
rename motivo_7 motivo_crimen
rename motivo_8 motivo_otros
order inegi year
tab acuerdo_gobestatal acuerdo_gobestatal2

label variable acuerdo_gobestatal "Acuerdo realizar funciones de seguridad publica"
label variable acuerdo_gobestatal2 "Acuerdo realizar funciones de seguridad publica, por alguna necesidad (NA option)"
label variable acuerdo_gobestatal3 "Mando unico"
label variable acuerdo_gobestatal4 "Acuerdo realizar funciones de seguridad publica (2)"

order inegi year  acuerdo_gobestatal acuerdo_gobestatal2 acuerdo_gobestatal3  acuerdo_gobestatal4


save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014.dta", replace

**till here is identical to old do file, with the exception of some variable names and adding acuerdo_gobestatal4
******
**2016 & 2017
******
/*
nd_nsnn1 = ""Clasificacióndelosmunicipiosdeacuerdoconlaasociacióndelgobiernomunicipalconelgobiernofederalode
 otrasentidadesfederativas,municipiosodelegacionespararealizarfuncionesdeseguridad pública distintas al Mando Unico
 Policial, durante el año 2016." [same question as aso_gfdm=acuerdo_gobestatal]
 --they specified the question to differentiate it from nd_nsnn2

 nd_nsnn2 = "Clasificacióndelosmunicipiosdeacuerdoconlaexistenciadeconveniooacuerdoosimilarparaquela
AdministraciónPúblicadesuEntidadFederativaoperebajoelsistemadeMandoÚnicoPolicialysehagacargodemaneratemporal
opermanentedelaprestacióndelserviciodeseguridadpública,tránsitoyvialidadenelmunicipio,almomento de la aplicación
 del censo (mayo - junio 2017)." [same question as conv_mup= acuerdo_gobestatal3]

*/

insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2017c.csv", clear
rename ubic_geo inegi
sort inegi
foreach i in nd_nsnn1 nd_nsnn2{
**BIG CHANGES HERE:
replace `i'=. if `i'==0 // no aplica option
replace `i'=. if `i'==8
replace `i'=. if `i'==9
replace `i'=0 if `i'==2 // used to be flipped with 1
replace `i'=1 if `i'==1 // used to be flipped with 2
}
/*
           |       ND_NSNN2
  ND_NSNN1 |         0          1 |     Total
-----------+----------------------+----------
         0 |     1,563        290 |     1,853 
         1 |       191         93 |       284 
-----------+----------------------+----------
     Total |     1,754        383 |     2,137 


*/


preserve
drop if nd_nsnn2==.
collapse (mean)nd_nsnn2, by (inegi)
bysort inegi:  gen dup = cond(_N==1,0,_n)
tab dup
drop dup
rename nd_nsnn2 acuerdo_gobestatal3
label variable acuerdo_gobestatal3 "Mando unico"
gen year=2017
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2017.dta", replace
restore

preserve
drop if nd_nsnn1==.
collapse (mean)nd_nsnn1, by (inegi)
bysort inegi:  gen dup = cond(_N==1,0,_n)
tab dup
drop dup
rename nd_nsnn1 acuerdo_gobestatal
gen year=2016
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2016a.dta", replace
restore

*create motive and acuerdo_gobestatal2:
/*nd_segpu=Clasificación de los municipios de acuerdo con los servicios públicos y/o funciones objeto de la
asociación intergubernamental, durante el año 2016.
gobie1...gobier15 = Clasificación de los municipios de acuerdo con los gobiernos con los que se estableció la 
asociación intergubernamental durante el año 2016.

gobier1 = poder ejecutivo federal

*/
*GOBIERNO.dbf file:
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2017a.csv", clear
rename ubic_geo inegi
sort inegi


****CHECK THE NAs and other things. So what are missings and zeros. 
foreach i in gobier1 gobier2 gobier3 gobier4 gobier5 gobier6 gobier7 gobier8 gobier9 gobier10 gobier11 gobier12 gobier13 gobier15 totalca1{
replace `i'="0" if `i'=="NA"
replace `i'="." if `i'=="ND"
replace `i'="." if `i'=="NSS"
destring `i', replace
*replace `i'=0 if `i'==.
}

**motives behind signature
foreach i in racoac1 racoac2 racoac3 racoac4 racoac5 racoac6 racoac7 racoac8 {
*replace `i'="." if `i'=="NA"
replace `i'="." if `i'=="ND"
replace `i'="." if `i'=="NSS"
*destring `i', replace
}

gen racoac0=.
replace racoac0=0 if racoac1=="NA" & racoac2=="NA" & racoac3=="NA" & racoac4=="NA" & racoac5=="NA" & racoac6=="NA" ///
& racoac7=="NA" & racoac8=="NA"
replace racoac0=1 if racoac1=="1" | racoac2=="1" | racoac3=="1" | racoac4=="1" | racoac5=="1" | racoac6=="1" ///
| racoac7=="1" | racoac8=="1"
rename racoac0 acuerdo_gobestatal2

foreach i in racoac1 racoac2 racoac3 racoac4 racoac5 racoac6 racoac7 racoac8 {
replace `i'="0" if `i'=="NA"
destring `i', replace
}

foreach i in 1 2 3 4 5 6 7 8{
replace racoac`i'=1 if racoac`i'>0
}

rename racoac1 motivo_reformacons
rename racoac2 motivo_reformaley
rename racoac3 motivo_faltarecursos
rename racoac4 motivo_profesioalizacion
rename racoac5 motivo_coordinacion
rename racoac6 motivo_crimen
rename racoac7 motivo_otros
rename racoac8 motivo_nosabe

**type of agreement services
replace nd_segpu=. if nd_segpu==98
replace nd_segpu=. if nd_segpu==99
tab nd_segpu, gen(servicio_)
rename servicio_1 servicio_noaplica
rename servicio_2 servicio_segpublica
rename servicio_3 servicio_transito
rename servicio_4 servicio_prevencion
rename servicio_5 servicio_capacitacion
rename servicio_6 servicio_equiptec
rename servicio_7 servicio_investigacion
rename servicio_8 servicio_inteligencia
rename servicio_9 servicio_unificacion

collapse (mean)gobier* motivo_* nd_segpu acuerdo_gobestatal2 servicio_*, by (inegi)

replace nd_segpu=1 if nd_segpu>0
gen year=2016

foreach i in motivo_reformacons motivo_reformaley motivo_faltarecursos motivo_profesioalizacion motivo_coordinacion motivo_crimen motivo_otros motivo_nosabe{
replace `i'=1 if `i'>0
}


foreach i in servicio_noaplica servicio_segpublica servicio_transito servicio_prevencion servicio_capacitacion servicio_equiptec servicio_investigacion servicio_inteligencia servicio_unificacion{
replace `i'=1 if `i'>0
}


order inegi year
rename nd_segpu acuerdo_gobestatal4 // so acuerdo_gobestatal4 determined by no aplica but this is wrong
tab acuerdo_gobestatal4
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2016b.dta", replace

***merge 2016
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2016a.dta", clear
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2016b.dta"
drop _merge
order inegi year  acuerdo_gobestatal acuerdo_gobestatal2  acuerdo_gobestatal4

label variable acuerdo_gobestatal "Acuerdo realizar funciones de seguridad publica"
label variable acuerdo_gobestatal2 "Acuerdo realizar funciones de seguridad publica, por alguna necesidad"
label variable acuerdo_gobestatal4 "Acuerdo realizar funciones de seguridad publica (2)"

gen gobier1_4=0 if gobier1==0 & gobier2==0 & gobier3==0 & gobier4==0
replace gobier1_4=1 if gobier1==1 | gobier2==1 | gobier3==1 | gobier4==1

gen gobier5_7=0 if gobier5==0 & gobier6==0 & gobier7==0 
replace gobier5_7=1 if gobier5==1 | gobier6==1 | gobier7==1

gen gobier9_12=0 if gobier9==0 & gobier10==0 & gobier11==0  & gobier12==0 
replace gobier9_12=1 if gobier9==0 | gobier10==0 | gobier11==0  | gobier12==0 


save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2016.dta", replace


******
**2018
******
*MANDOUNI.dbf
/*
re_nsnn1 = "Clasificación de los municipios de acuerdo con la asociación del municipio con el gobierno federal,
de la entidad federativa o de otras entidades federativas, municipios o demarcaciones para realizar funciones de seguridad 
pública distintas al Mando Único Policial, durante el año 2018." [same question as aso_gfdm=acuerdo_gobestatal]
 --they specified the question to differentiate it from re_nsnn2

 re_nsnn2 = "Clasificación de los municipios de acuerdo con la existencia de un convenio o similar vigente para
que la prestación del servicio de seguridad pública, tránsito o vialidad municipal se realizara,
de manera temporal o permanente, bajo el sistema de Mando Único Policial, durante el año 2018." 
[same question as conv_mup= acuerdo_gobestatal3]

*/

insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2019c.csv", clear
rename ubic_geo inegi
sort inegi
rename re_nsnn1 nd_nsnn1
rename re_nsnn2 nd_nsnn2
foreach i in nd_nsnn1 nd_nsnn2{
**BIG CHANGES HERE:
replace `i'=. if `i'==0 // no aplica
replace `i'=. if `i'==98
replace `i'=. if `i'==97
replace `i'=. if `i'==9
replace `i'=0 if `i'==2 // this was wrong, I change it, it used to have a 1 
replace `i'=1 if `i'==1 // this was wrong, I change it, it used to have a 2 
}

preserve
collapse (mean)nd_nsnn1, by (inegi)
bysort inegi:  gen dup = cond(_N==1,0,_n)
tab dup
drop dup
rename nd_nsnn1 acuerdo_gobestatal
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2018a.dta", replace
restore

preserve
collapse (mean)nd_nsnn2, by (inegi)
bysort inegi:  gen dup = cond(_N==1,0,_n)
tab dup
drop dup
rename nd_nsnn2 acuerdo_gobestatal3
label variable acuerdo_gobestatal3 "Mando unico"
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2018b.dta", replace
restore

*create acuerdo_gobestatal2 from reasons to sign an agreement: 
**found in GOBIERNO.dbf
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2019b.csv", clear
rename ubic_geo inegi
sort inegi


****CHECK THE NAs and other things. So what are missings and zeros. 
foreach i in gobier1 gobier2 gobier3 gobier4 gobier5 gobier6 gobier7 gobier8 gobier9 gobier10 gobier11 gobier12 gobier13 gobier15 totalca1{
replace `i'="0" if `i'=="NA"
replace `i'="." if `i'=="ND"
replace `i'="." if `i'=="NSS"
destring `i', replace
*replace `i'=0 if `i'==.
}

**motives behind signature
foreach i in racoac1 racoac2 racoac3 racoac4 racoac5 racoac6 racoac7 racoac8 {
*replace `i'="." if `i'=="NA"
replace `i'="." if `i'=="ND"
replace `i'="." if `i'=="NSS"
*destring `i', replace
}

gen racoac0=.
replace racoac0=0 if racoac1=="NA" & racoac2=="NA" & racoac3=="NA" & racoac4=="NA" & racoac5=="NA" & racoac6=="NA" ///
& racoac7=="NA" & racoac8=="NA"
replace racoac0=1 if racoac1=="1" | racoac2=="1" | racoac3=="1" | racoac4=="1" | racoac5=="1" | racoac6=="1" ///
| racoac7=="1" | racoac8=="1"
rename racoac0 acuerdo_gobestatal2

foreach i in racoac1 racoac2 racoac3 racoac4 racoac5 racoac6 racoac7 racoac8 {
replace `i'="0" if `i'=="NA"
destring `i', replace
}

foreach i in 1 2 3 4 5 6 7 8{
replace racoac`i'=1 if racoac`i'>0
}

rename racoac1 motivo_reformacons
rename racoac2 motivo_reformaley
rename racoac3 motivo_faltarecursos
rename racoac4 motivo_profesioalizacion
rename racoac5 motivo_coordinacion
rename racoac6 motivo_crimen
rename racoac7 motivo_otros
rename racoac8 motivo_nosabe

**type of agreement services
rename segplfun nd_segpu
replace nd_segpu=. if nd_segpu==97
replace nd_segpu=. if nd_segpu==98
tab nd_segpu, gen(servicio_)
rename servicio_1 servicio_noaplica
rename servicio_2 servicio_segpublica
rename servicio_3 servicio_transito
rename servicio_4 servicio_prevencion
rename servicio_5 servicio_capacitacion
rename servicio_6 servicio_equiptec
rename servicio_7 servicio_investigacion
rename servicio_8 servicio_inteligencia
rename servicio_9 servicio_unificacion
rename servicio_10 servicio_nosesabe

collapse (mean)gobier* motivo_* nd_segpu acuerdo_gobestatal2 servicio_*, by (inegi)

replace nd_segpu=1 if nd_segpu>0

foreach i in gobier1 gobier2 gobier3 gobier4 gobier5 gobier6 gobier7 gobier8 gobier9 gobier10 gobier11 gobier12 gobier13 gobier15{
replace `i'=1 if `i'>0
}


foreach i in motivo_reformacons motivo_reformaley motivo_faltarecursos motivo_profesioalizacion motivo_coordinacion motivo_crimen motivo_otros motivo_nosabe{
replace `i'=1 if `i'>0
}


foreach i in servicio_noaplica servicio_segpublica servicio_transito servicio_prevencion servicio_capacitacion servicio_equiptec servicio_investigacion servicio_inteligencia servicio_unificacion{
replace `i'=1 if `i'>0
}


order inegi 
rename nd_segpu acuerdo_gobestatal4 // so acuerdo_gobestatal4 determined by no aplica but this is wrong
tab acuerdo_gobestatal4

save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2018c.dta", replace

***merge 2018
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2018a.dta", clear
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2018b.dta"
drop _merge

merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2018c.dta"
drop _merge

order inegi  acuerdo_gobestatal acuerdo_gobestatal2 acuerdo_gobestatal3 acuerdo_gobestatal4

label variable acuerdo_gobestatal "Acuerdo realizar funciones de seguridad publica"
label variable acuerdo_gobestatal2 "Acuerdo realizar funciones de seguridad publica, por alguna necesidad"
label variable acuerdo_gobestatal3 "Mando Unico"
label variable acuerdo_gobestatal4 "Acuerdo realizar funciones de seguridad publica (2)"

gen year=2018

gen gobier1_4=0 if gobier1==0 & gobier2==0 & gobier3==0 & gobier4==0
replace gobier1_4=1 if gobier1==1 | gobier2==1 | gobier3==1 | gobier4==1

gen gobier5_7=0 if gobier5==0 & gobier6==0 & gobier7==0 
replace gobier5_7=1 if gobier5==1 | gobier6==1 | gobier7==1

gen gobier9_12=0 if gobier9==0 & gobier10==0 & gobier11==0  & gobier12==0 
replace gobier9_12=1 if gobier9==0 | gobier10==0 | gobier11==0  | gobier12==0 



save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2018.dta", replace

*********************
*Merge all .dta files
*********************
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011.dta", clear
preserve
replace year=2012 if year==2011
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2012.dta", replace
restore
append using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2012.dta"
append using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013.dta"
append using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014.dta"
append using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2015.dta"
append using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2016.dta"
append using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2017.dta"
append using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2018.dta"

order inegi year acuerdo_gobestatal acuerdo_gobestatal2 acuerdo_gobestatal3 acuerdo_gobestatal4

rename gobier1_4 ac_gob_fed
rename gobier5_7 ac_gob_est
rename gobier8 ac_gob_mun
rename gobier9_12 ac_gob_otroest

order inegi year acuerdo_gobestatal acuerdo_gobestatal2 acuerdo_gobestatal3 acuerdo_gobestatal4 ac_gob_fed ac_gob_est ac_gob_mun ac_gob_otroest
 

/*
Acuerdos by year:
acuerdo_gobestatal (seg publica): 2011, 2013, 2014, 2016, 2018
acuerdo_gobestatal2 (seg publica, based on needs): 2011, 2013, 2014, 2016, 2018
acuerdo_gobestatal3 (mando unico): 2014, 2015, 2017, 2018
acuerdo_gobestatal4 (seg publica, based on services): 2014, 2016, 2018
*/

sort inegi year 
xtset inegi year 

*Sign security agreement
gen acuerdo=.
replace acuerdo=acuerdo_gobestatal if year==2011
replace acuerdo=acuerdo_gobestatal if year==2012
replace acuerdo=acuerdo_gobestatal if year==2013
replace acuerdo=acuerdo_gobestatal if year==2014
*by inegi: replace acuerdo=acuerdo_gobestatal[_n-1] if year==2015
*replace acuerdo=acuerdo_gobestatal3 if year==2015 // check this
replace acuerdo=acuerdo_gobestatal if year==2016
*by inegi: replace acuerdo=acuerdo_gobestatal[_n-1] if year==2017
*replace acuerdo=acuerdo_gobestatal3 if year==2017 // check this
replace acuerdo=acuerdo_gobestatal if year==2018 // check this

*Necesidad no aplica:
gen acuerdo2=.
replace acuerdo2=acuerdo_gobestatal2 if year==2011
replace acuerdo2=acuerdo_gobestatal2 if year==2012
replace acuerdo2=acuerdo_gobestatal2 if year==2013
replace acuerdo2=acuerdo_gobestatal2 if year==2014
*by inegi: replace acuerdo2=acuerdo_gobestatal2[_n-1] if year==2015
*replace acuerdo2=acuerdo_gobestatal3 if year==2015 // check this
replace acuerdo2=acuerdo_gobestatal2 if year==2016
*by inegi: replace acuerdo2=acuerdo_gobestatal2[_n-1] if year==2017
*replace acuerdo2=acuerdo_gobestatal3 if year==2017 // check this
replace acuerdo2=acuerdo_gobestatal2 if year==2018

*Mando Unico.
gen acuerdo3=.
by inegi: replace acuerdo3=acuerdo_gobestatal3[_n+1] if year==2013
replace acuerdo3=acuerdo_gobestatal3 if year==2014
replace acuerdo3=acuerdo_gobestatal3 if year==2015
by inegi: replace acuerdo3=acuerdo_gobestatal3[_n-1] if year==2016
replace acuerdo3=acuerdo_gobestatal3 if year==2017
replace acuerdo3=acuerdo_gobestatal3 if year==2018

*servicio no aplica
gen acuerdo4=.
by inegi: replace acuerdo4=acuerdo_gobestatal4[_n+1] if year==2013
replace acuerdo4=acuerdo_gobestatal4 if year==2014
by inegi: replace acuerdo4=acuerdo_gobestatal4[_n-1] if year==2015
replace acuerdo4=acuerdo_gobestatal4 if year==2016
by inegi: replace acuerdo4=acuerdo_gobestatal4[_n-1] if year==2017
replace acuerdo4=acuerdo_gobestatal4 if year==2018

*combined security agreement and Mando Unico:
gen acuerdo5=.
replace acuerdo5=acuerdo_gobestatal if year==2011
replace acuerdo5=acuerdo_gobestatal if year==2012
replace acuerdo5=acuerdo_gobestatal if year==2013
replace acuerdo5=acuerdo_gobestatal if year==2014
replace acuerdo5=acuerdo_gobestatal3 if year==2015
replace acuerdo5=acuerdo_gobestatal if year==2016
replace acuerdo5=acuerdo_gobestatal3 if year==2017
replace acuerdo5=acuerdo_gobestatal3 if year==2018

*Agreements - Mando Unico
gen acuerdo_federal=.
*replace acuerdo_federal=acuerdo_gobestatal-acuerdo_gobestatal3[_n+1] if year==2013
replace acuerdo_federal=acuerdo_gobestatal-acuerdo_gobestatal3 if year==2014
*replace acuerdo_federal=acuerdo_gobestatal-acuerdo_gobestatal3[_n-1] if year==2016
replace acuerdo_federal=acuerdo_gobestatal-acuerdo_gobestatal3 if year==2018

replace acuerdo_federal=1 if acuerdo_federal==1
replace acuerdo_federal=0 if acuerdo_federal==-1

*Agreements + Mando Unico
gen acuerdo_total=.
replace acuerdo_total=acuerdo_gobestatal if year==2011
replace acuerdo_total=acuerdo_gobestatal if year==2012
replace acuerdo_total=acuerdo_gobestatal if year==2013
replace acuerdo_total=acuerdo_gobestatal3+acuerdo_gobestatal if year==2014
replace acuerdo_total=acuerdo_gobestatal3 if year==2015
replace acuerdo_total=acuerdo_gobestatal+ acuerdo_gobestatal3 if year==2016
replace acuerdo_total=acuerdo_gobestatal3 if year==2017
replace acuerdo_total=acuerdo_gobestatal+acuerdo_gobestatal3 if year==2018

replace acuerdo_total=1 if acuerdo_total==2


save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011_2018.dta", replace

 
