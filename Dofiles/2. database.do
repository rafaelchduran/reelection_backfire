*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*Var Generation & Data set construction      
*****************************************************

clear all
set more off  
set varabbrev off 

*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Dofiles"

*Create main file: 
foreach y in 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019{
use "../Data/ConstructionDatabase/Municipality_Codes_UniqueID.dta", clear
gen year=`y'
save "../Data/ConstructionDatabase/municipalities_id_`y'.dta", replace
}

use "../Data/ConstructionDatabase/municipalities_id_2010.dta", clear
append using "../Data/ConstructionDatabase/municipalities_id_2011.dta"
append using "../Data/ConstructionDatabase/municipalities_id_2012.dta"
append using "../Data/ConstructionDatabase/municipalities_id_2013.dta"
append using "../Data/ConstructionDatabase/municipalities_id_2014.dta"
append using "../Data/ConstructionDatabase/municipalities_id_2015.dta"
append using "../Data/ConstructionDatabase/municipalities_id_2016.dta"
append using "../Data/ConstructionDatabase/municipalities_id_2017.dta"
append using "../Data/ConstructionDatabase/municipalities_id_2018.dta"
*append using "../Data/ConstructionDatabase/municipalities_id_2019.dta"

rename ENTIDAD estado
rename NOMBRE_ENTIDAD nombre_estado
rename MUNICIPIO municipio
rename NOMBRE_MUNICIPIO nombre_municipio
rename UNIQUE_MUNICIPALITY mun_id

*Main file to append all other files:
save "../Data/ConstructionDatabase/municipalities_id_2010_2019.dta", replace

*1) ADD TREATMENT using Magar's election's database
/*
**the id here is inegi not 
**need to collapse to the mun-year level
*/
rename mun_id inegi
merge 1:1 inegi year using  "../Data/municipal_elections_incumbent_mexico_1989_present_v2.dta"
drop if _merge==2
rename _merge hadelection
label variable hadelection "Dummy=1 if an election that year; 0 otherwise"
label variable year "year"
label variable inegi "INEGI identifying code"
label variable estado "State"
label variable nombre_estado "State name"
label variable municipio "municipality (number)"
label variable nombre_municipio "Municipality name"

/*
    Result                           # of obs.
    -----------------------------------------
    not matched                        42,530
        from master                    17,855  (_merge==1) - there are no elections
        from using                     24,675  (_merge==2) - 24,639 of years prior to 2010
														   - 36 municipalities that the mun_id changed
    matched                             6,615  (_merge==3)
    -----------------------------------------

Missing info: 
                                    mun |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
                                 Aldama |          4       11.11       11.11
                                Bacalar |          2        5.56       16.67
             Benemérito de las Américas |          4       11.11       27.78
                               Calakmul |          3        8.33       36.11
                     Maravilla Tenejapa |          4       11.11       47.22
                    Marqués de Comillas |          4       11.11       58.33
                            Montecristo |          3        8.33       66.67
                         Puerto Morelos |          1        2.78       69.44
                    San Andrés Duraznal |          4       11.11       80.56
                      Santiago el Pinar |          3        8.33       88.89
                                  Tulum |          4       11.11      100.00
----------------------------------------+-----------------------------------
                                  Total |         36      100.00

*/

*first treatment year
foreach i in 3 4 6 7 11 12 14 15 16 17 19 22 24 27 31{
replace reform=1 if year==2015 & estado==`i'
replace reform=1 if year==2016 & estado==`i'
replace reform=1 if year==2017 & estado==`i'
replace reform=1 if year==2018 & estado==`i'
replace reform=1 if year==2019 & estado==`i'
}
label variable reform "Dummy=1 if treated Electoral Reform; o otherwise"

*second treatment year
foreach i in 1 2 8 10 20 23 25 28 32{
replace reform=1 if year==2016 & estado==`i'
replace reform=1 if year==2017 & estado==`i'
replace reform=1 if year==2018 & estado==`i'
replace reform=1 if year==2019 & estado==`i'
}

*third treatment year 
foreach i in 5{
replace reform=1 if year==2017 & estado==`i'
replace reform=1 if year==2018 & estado==`i'
replace reform=1 if year==2019 & estado==`i'
}

*fourth treatment year
foreach i in 9 21 26{
replace reform=1 if year==2018 & estado==`i'
replace reform=1 if year==2019 & estado==`i'
}

replace reform=0 if reform==.

/*Fix treatment
foreach i in incumbent_yesterday_w_today incumbent_today_w_tomorrow incumbent_yesterday_w_tomorrow incumbent_yesterday_w_tomorrow2 coalition_v01 coalition_v02 coalition_v03 coalition_v04 coalition_v05 coalition_v06 coalition_v07 coalition_v08 coalition_v09 coalition_v10 coalition_v11 coalition_v12 coalition_v13 coalition_v14 coalition_v15 coalition_v16 coalition_v17 coalition_v18 reform alignment_executive_strong{
replace `i'=`i'[_n-1] if `i'==.
}
*/

save "../Data/ConstructionDatabase/municipalities_id_2010_2019_wtreatment.dta", replace

*2) ADD OUTCOME
**2.1) Homicides from SNSP
insheet using "../Data/ConstructionDatabase/DenunciasSNSP/Municipal-Delitos-2015-2019_dic19/Municipal-Delitos - diciembre 2019.csv", clear
keep if subtipodedelito=="Homicidio doloso" 
egen homicidio=rowtotal(enero febrero marzo abril mayo junio julio agosto septiembre octubre noviembre diciembre)
collapse (sum)homicidio (firstnm)clave_ent entidad  municipio, by(cvemunicipio ao)
order ao clave_ent entidad  municipio cvemunicipio homicidio
keep ao cvemunicipio homicidio
rename ao year
label variable year "year"
rename cvemunicipio inegi
rename homicidio homicide
label variable homicide "Homicide"
save "../Data/ConstructionDatabase/DenunciasSNSP/Municipal-Delitos-2015-2019_dic19/homicides_2015_2019.dta", replace

use "../Data/ConstructionDatabase/municipalities_id_2010_2019_wtreatment.dta", clear
merge 1:1 inegi year using "../Data/ConstructionDatabase/DenunciasSNSP/Municipal-Delitos-2015-2019_dic19/homicides_2015_2019.dta"
/*
    Result                           # of obs.
    -----------------------------------------
    not matched                        13,410
        from master                    13,305  (_merge==1) - lost because of other years
        from using                        105  (_merge==2) - some municipality codes not found

    matched                            11,165  (_merge==3)
    -----------------------------------------

*/
drop if _merge==2
rename _merge missinghomicidedata

save "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew.dta", replace

*merge old homicide methodology
insheet using "../Data/ConstructionDatabase/DenunciasSNSP/Incidencia municipal 2011 - 2017 oct19.csv", clear
keep if modalidad=="HOMICIDIOS" & tipo=="DOLOSOS"
egen homicidio_old=rowtotal(enero febrero marzo abril mayo junio julio agosto septiembre octubre noviembre diciembre)
collapse (sum)homicidio_old, by(inegi AÑO)
keep AÑO inegi homicidio_old

rename AÑO year
label variable year "year"
rename homicidio_old homicide_old
label variable homicide_old "Homicide (old measure)"
save "../Data/ConstructionDatabase/DenunciasSNSP/homicides_2011_2017.dta", replace

use "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew.dta", clear
merge 1:1 inegi year using "../Data/ConstructionDatabase/DenunciasSNSP/homicides_2011_2017.dta"
/*
    Result                           # of obs.
    -----------------------------------------
    not matched                        11,738
        from master                    11,636  (_merge==1) - years missing
        from using                        102  (_merge==2) - muns missing

    matched                            12,834  (_merge==3)
    -----------------------------------------
*/
drop if _merge==2
rename _merge missinghomicideolddata

save "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew&old.dta", replace

**2.2) Defunciones from INEGI
preserve
insheet using "../Data/ConstructionDatabase/DefuncionPorHomicidioINEGI/INEGI_ags_zac_reshape_final3.csv", clear
drop if inegi==.
reshape long defunciones, i(inegi) j(year)
label variable defunciones "defunciones por homicidio (INEGI)"
save "../Data/ConstructionDatabase/DefuncionPorHomicidioINEGI/defunciones_1990_2018.dta", replace
restore

merge 1:1 inegi year using "../Data/ConstructionDatabase/DefuncionPorHomicidioINEGI/defunciones_1990_2018.dta"
drop if _merge==2
drop _merge

**2.3) Effort from detenciones from SNSP (infomex)
**get municipalities to match with inegi's ids
preserve
insheet using "../Data/ConstructionDatabase/Effort/Detenciones/detenidos.csv", clear
*remove accents:
gen municipio2 = ustrlower( ustrregexra( ustrnormalize( municipio, "nfd" ) , "\p{Mark}", "" )  )
gen estado2 = ustrlower( ustrregexra( ustrnormalize( estado, "nfd" ) , "\p{Mark}", "" )  )
*upper case
gen municipio3 = upper(municipio2)
gen estado3 = upper(estado2)
drop municipio municipio2
drop estado estado2
rename municipio3 municipio
rename estado3 estado

collapse (sum) detenidos, by(estado municipio)
order estado municipio detenidos
replace estado="MEXICO" if estado=="ESTADO DE MEXICO"
replace estado="DISTRITO FEDERAL" if estado=="CIUDAD DE MEXICO"
drop if municipio=="SIN INFORMACION"
save "../Data/ConstructionDatabase/Effort/Detenciones/effort_policia_ids.dta", replace
export delimited using "../Data/ConstructionDatabase/Effort/Detenciones/effort_policia_ids.csv", replace
restore

**get municipal ids
preserve
use "../Data/ConstructionDatabase/municipalities_id_2010_2019_wtreatment.dta", clear
collapse (mean)estado inegi ife, by(nombre_estado nombre_municipio)
drop estado
export delimited using "../Data/ConstructionDatabase/Effort/Detenciones/muns_ids.csv", replace
restore

**upload match between effort policia ids and inegi
preserve
insheet using "../Data/ConstructionDatabase/Effort/Detenciones/effort_policia_muns_ids.csv", clear
save "../Data/ConstructionDatabase/Effort/Detenciones/effort_policia_muns_ids.dta", replace
restore

*merge
preserve
insheet using "../Data/ConstructionDatabase/Effort/Detenciones/detenidos.csv", clear
*remove accents:
gen municipio2 = ustrlower( ustrregexra( ustrnormalize( municipio, "nfd" ) , "\p{Mark}", "" )  )
gen estado2 = ustrlower( ustrregexra( ustrnormalize( estado, "nfd" ) , "\p{Mark}", "" )  )
*upper case
gen municipio3 = upper(municipio2)
gen estado3 = upper(estado2)
drop municipio municipio2
drop estado estado2
rename municipio3 municipio
rename estado3 estado

order estado municipio year detenidos
replace estado="MEXICO" if estado=="ESTADO DE MEXICO"
replace estado="DISTRITO FEDERAL" if estado=="CIUDAD DE MEXICO"
drop if municipio=="SIN INFORMACION"

merge m:1 estado municipio using "../Data/ConstructionDatabase/Effort/Detenciones/effort_policia_muns_ids.dta"
drop _merge
collapse (sum) detenidos, by(inegi year)
save "../Data/ConstructionDatabase/Effort/Detenciones/effort_policia_winegi.dta", replace
restore

*final merge with 
merge 1:1 inegi year using "../Data/ConstructionDatabase/Effort/Detenciones/effort_policia_winegi.dta"
drop if _merge==2
rename _merge missingpoliceffort
label variable detenidos "Detained by local police (in flagrancy, SNSP)"
label variable missingpoliceffort "Dummy missing local police effort"

**2.4) Illegal activities from SEDENA (infomex)
preserve
foreach i in armas cartuchos drogas  granadas laboratorios pistas vehiculos{
insheet using  "../Data/ConstructionDatabase/Effort/`i'_asegurados.csv", clear
save  "../Data/ConstructionDatabase/Effort/`i'_asegurados.dta", replace
}

foreach i in hectareas_amapola_mariguana{
insheet using  "../Data/ConstructionDatabase/Effort/`i'.csv", clear
save  "../Data/ConstructionDatabase/Effort/`i'.dta", replace
}

use "../Data/ConstructionDatabase/Effort/armas_asegurados.dta", clear
append using "../Data/ConstructionDatabase/Effort/cartuchos_asegurados.dta"
append using "../Data/ConstructionDatabase/Effort/drogas_asegurados.dta"
append using "../Data/ConstructionDatabase/Effort/hectareas_amapola_mariguana.dta"
append using "../Data/ConstructionDatabase/Effort/granadas_asegurados.dta"
append using "../Data/ConstructionDatabase/Effort/laboratorios_asegurados.dta"
append using "../Data/ConstructionDatabase/Effort/pistas_asegurados.dta"
append using "../Data/ConstructionDatabase/Effort/vehiculos_asegurados.dta"
*remove accents:
gen municipio2 = ustrlower( ustrregexra( ustrnormalize( municipio, "nfd" ) , "\p{Mark}", "" )  )
gen estado2 = ustrlower( ustrregexra( ustrnormalize( estado, "nfd" ) , "\p{Mark}", "" )  )
*upper case
gen municipio3 = upper(municipio2)
gen estado3 = upper(estado2)
drop municipio municipio2
drop estado estado2
rename municipio3 municipio
rename estado3 estado
order ano mes estado municipio
drop num
gen lab_eradicated_date = date(fecha, "DMY")
format lab_eradicated_date %td
gen lab_eradicated_day=day(lab_eradicated_date)
gen lab_eradicated_month=month(lab_eradicated_date)
gen lab_eradicated_year=year(lab_eradicated_date)

rename heroína_kg heroina_kg

label variable arma_corta "Secured short arms (SEDENA)"
label variable arma_larga "Secured long arms (SEDENA)"
label variable cartuchos "Secured cartridges (SEDENA)"
label variable cocaina_kg "Secured cocaine (kg, SEDENA)"
label variable heroina_kg "Secured heroine (kg, SEDENA)"
label variable mariguana_kg "Secured mariguana (kg, SEDENA)"
label variable metanfetamina_kg "Secured methamphetamine (kg, SEDENA)"
label variable amapola_kghec "Eradicated amapola (kg per hectare, SEDENA)"
label variable mariguana_kghec "Eradicated mariguana (kg per hectare, SEDENA)"
label variable granadas "Secured grenades (SEDENA)"
label variable laboratorio "Laboratories Eradicated (SEDENA)"
label variable pistas "Runways Eradicated (SEDENA)"
label variable vehiculo_aereo "Secured airplanes (SEDENA)"
label variable vehiculo_lacustre "Secured water vehicle (SEDENA)"
label variable lab_eradicated_date "Laboratories date eradication (SEDENA)"
label variable lab_eradicated_day "Laboratories day eradication (SEDENA)"
label variable lab_eradicated_month "Laboratories month eradication (SEDENA)"
label variable lab_eradicated_year "Laboratories year eradication (SEDENA)"

save  "../Data/ConstructionDatabase/Effort/effort_sedena.dta", replace
restore

preserve
*create dataset to export and merge municipal ids
use  "../Data/ConstructionDatabase/Effort/effort_sedena.dta", clear
gen var=1
collapse (mean)var, by(estado municipio)
drop if municipio=="SIN ESPECIFICAR"
drop if municipio=="NO ESPECIFICADO"
drop if municipio=="NO ESPECIFICADO BC"
drop if municipio=="NO ESPECIFICADO CHIH."
drop if municipio=="NO ESPECIFICADO CHIS."
drop if municipio=="NO ESPECIFICADO COL."
drop if municipio=="NO ESPECIFICADO DGO."
drop if municipio=="NO ESPECIFICADO GRO."
drop if municipio=="NO ESPECIFICADO JAL."
drop if municipio=="NO ESPECIFICADO MICH"
drop if municipio=="NO ESPECIFICADO N.L."
drop if municipio=="NO ESPECIFICADO OAX."
drop if municipio=="NO ESPECIFICADO SIN"
drop if municipio=="NO ESPECIFICADO SON."
drop if municipio=="NO ESPECIFICADO TAB."
drop if municipio=="NO ESPECIFICADO VER."
drop if municipio=="NO ESPECIFICADO ZAC"
export delimited using "../Data/ConstructionDatabase/Effort/effort_sedena.csv", replace
restore

*get municipal ids
preserve
use "../Data/ConstructionDatabase/municipalities_id_2010_2019_wtreatment.dta", clear
collapse (mean)estado inegi ife, by(nombre_estado nombre_municipio)
drop estado
export delimited using "../Data/ConstructionDatabase/Effort/muns_ids.csv", replace
restore

preserve
insheet using  "../Data/ConstructionDatabase/Effort/matchid_sedena_inegi.csv", clear
save "../Data/ConstructionDatabase/Effort/matchid_sedena_inegi.dta", replace
restore

preserve 
use  "../Data/ConstructionDatabase/Effort/effort_sedena.dta", clear
drop if municipio=="SIN ESPECIFICAR"
drop if municipio=="NO ESPECIFICADO"
drop if municipio=="NO ESPECIFICADO BC"
drop if municipio=="NO ESPECIFICADO CHIH."
drop if municipio=="NO ESPECIFICADO CHIS."
drop if municipio=="NO ESPECIFICADO COL."
drop if municipio=="NO ESPECIFICADO DGO."
drop if municipio=="NO ESPECIFICADO GRO."
drop if municipio=="NO ESPECIFICADO JAL."
drop if municipio=="NO ESPECIFICADO MICH"
drop if municipio=="NO ESPECIFICADO N.L."
drop if municipio=="NO ESPECIFICADO OAX."
drop if municipio=="NO ESPECIFICADO SIN"
drop if municipio=="NO ESPECIFICADO SON."
drop if municipio=="NO ESPECIFICADO TAB."
drop if municipio=="NO ESPECIFICADO VER."
drop if municipio=="NO ESPECIFICADO ZAC"
replace estado="COAHUILA" if estado=="COAHUILA DE ZARAGOZA"

merge m:m estado municipio using "../Data/ConstructionDatabase/Effort/matchid_sedena_inegi.dta"
drop if _merge!=3
drop _merge

drop fecha
rename ano year
rename mes month
rename estado nombre_estado
rename municipio nombre_municipio
label variable year "year"
label variable month "month"
label variable nombre_estado "State name"
label variable nombre_municipio "Municipality name"
label variable inegi "INEGI identifying code"
label variable ife "IFE identifying code"

replace month="ENERO" if month=="enero"
gen month2=.
replace month2=1 if month=="ENERO"
replace month2=2 if month=="FEBRERO"
replace month2=3 if month=="MARZO"
replace month2=4 if month=="ABRIL"
replace month2=5 if month=="MAYO"
replace month2=6 if month=="JUNIO"
replace month2=7 if month=="JULIO"
replace month2=8 if month=="AGOSTO"
replace month2=9 if month=="SEPTIEMBRE"
replace month2=10 if month=="OCTUBRE"
replace month2=11 if month=="NOVIEMBRE"
replace month2=12 if month=="DICIEMBRE"
drop month
rename month2 month_effort
label variable month_effort "Month of effort made (SEDENA)"

*collapse data
quietly bysort year month_effort inegi:  gen dup = cond(_N==1,0,_n)
drop dup
collapse (sum) arma_corta arma_larga cartuchos cocaina_kg heroina_kg mariguana_kg  metanfetamina_kg amapola_kghec  mariguana_kghec granadas  laboratorio pistas  vehiculo_aereo vehiculo_lacustre ///
(mean) ife (firstnm)nombre_estado (firstnm)nombre_municipio, ///
by(inegi year)
order year nombre_estado nombre_municipio inegi ife year
quietly bysort year  inegi:  gen dup = cond(_N==1,0,_n)
drop dup

label variable year "year"
label variable nombre_estado "State name"
label variable nombre_municipio "Municipality name"
label variable inegi "INEGI identifying code"
label variable ife "IFE identifying code"
label variable arma_corta "Secured short arms (SEDENA)"
label variable arma_larga "Secured long arms (SEDENA)"
label variable cartuchos "Secured cartridges (SEDENA)"
label variable cocaina_kg "Secured cocaine (kg, SEDENA)"
label variable heroina_kg "Secured heroine (kg, SEDENA)"
label variable mariguana_kg "Secured mariguana (kg, SEDENA)"
label variable metanfetamina_kg "Secured methamphetamine (kg, SEDENA)"
label variable amapola_kghec "Eradicated amapola (kg per hectare, SEDENA)"
label variable mariguana_kghec "Eradicated mariguana (kg per hectare, SEDENA)"
label variable granadas "Secured grenades (SEDENA)"
label variable laboratorio "Laboratories Eradicated (SEDENA)"
label variable pistas "Runways Eradicated (SEDENA)"
label variable vehiculo_aereo "Secured airplanes (SEDENA)"
label variable vehiculo_lacustre "Secured water vehicle (SEDENA)"
save  "../Data/ConstructionDatabase/Effort/effort_sedena_winegi.dta", replace
restore

merge 1:1 inegi year using "../Data/ConstructionDatabase/Effort/effort_sedena_winegi.dta"
drop if _merge==2
rename _merge missingarmyeffort
label variable missingarmyeffort "Dummy missing army effort"

*3) ADD COVARIATES
**3.1) Population from MAIZE TO HAZE 
preserve
use "../Data/ConstructionDatabase/Poblacion/MaizeToHaze_JEEA_ReplicationData.dta", clear
keep year state muncode munname pop
rename muncode inegi
rename pop pop_mazetohaze
save "../Data/ConstructionDatabase/Poblacion/pop_1990_2010.dta", replace
restore

merge 1:1 inegi year using "../Data/ConstructionDatabase/Poblacion/pop_1990_2010.dta"
/*

    Result                           # of obs.
    -----------------------------------------
    not matched                        19,188
        from master                    15,870  (_merge==1) *no effort placed by SEDENA
        from using                      3,318  (_merge==2) *missing values and years prior to 2010

    matched                             8,600  (_merge==3)
    -----------------------------------------

*/
drop if _merge==2
gen noeffort=1 if _merge==1
replace noeffort=0 if _merge==3
*ttest  defunciones, by(noeffort)
**there are far less defunciones in places were there is no effort
drop _merge

**3.2) Population from CONAPO
preserve 
insheet using "../Data/ConstructionDatabase/Poblacion/CONAPO/base_municipios_final_datos_01.csv", clear
collapse (sum)population, by(inegi year)
rename population pop_conapo
label variable pop_conapo "Population (CONAPO)"
save "../Data/ConstructionDatabase/Poblacion/CONAPO/base_municipios_final_datos_01.dta", replace

insheet using "../Data/ConstructionDatabase/Poblacion/CONAPO/base_municipios_final_datos_02.csv", clear
collapse (sum)population, by(inegi year)
rename population pop_conapo
label variable pop_conapo "Population (CONAPO)"
save "../Data/ConstructionDatabase/Poblacion/CONAPO/base_municipios_final_datos_02.dta", replace

append using "../Data/ConstructionDatabase/Poblacion/CONAPO/base_municipios_final_datos_01.dta"
drop if year>2019
save "../Data/ConstructionDatabase/Poblacion/CONAPO/pop_conapo_2015_2019.dta", replace
restore

merge 1:1 inegi year using "../Data/ConstructionDatabase/Poblacion/CONAPO/pop_conapo_2015_2019.dta"
drop if _merge==2 
drop _merge

**3.3) Population from INEGI
preserve 
insheet using "../Data/ConstructionDatabase/Poblacion/poblacion_muns_inegi_censo_2010.csv", clear
gen year=2010
keep pobl_total pobl_hombres pobl_mujeres inegi year 
rename pobl_total pop_inegi
rename pobl_hombres pop_men_inegi
rename pobl_mujeres pop_women_inegi
label variable pop_inegi "Population (CENSO 2010)"
label variable pop_men_inegi "Men Population (CENSO 2010)"
label variable pop_women_inegi "Women Population (CENSO 2010)"

save "../Data/ConstructionDatabase/Poblacion/pop_inegi_2010.dta", replace
restore

merge 1:1 inegi year using "../Data/ConstructionDatabase/Poblacion/pop_inegi_2010.dta"
drop if _merge==2 
drop _merge

**3.4) Population from SALUD (FROM CONAPO, from 2011 to 2014)
preserve 
insheet using "../Data/ConstructionDatabase/Poblacion/Salud/poblacion_municipal_salud_conapo_2010_2014_final.csv", clear
rename population_salud pop_salud
label variable pop_salud "Population (Secretaria de Salud from CONAPO proyections, 2011-2014)"
drop if year==2010
save "../Data/ConstructionDatabase/Poblacion/pop_salud_2011_2014.dta", replace
restore

merge 1:1 inegi year using "../Data/ConstructionDatabase/Poblacion/pop_salud_2011_2014.dta"
drop if _merge==2 
drop _merge

*save "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew&old_wpop.dta", replace


**3.5) Other covariates from MAIZE to HAZE
preserve
use "../Data/ConstructionDatabase/Poblacion/MaizeToHaze_JEEA_ReplicationData.dta", clear
keep year state muncode munname natlcornp-pcforest area_cultivated-other_crops_cultivated pop_male pop_female marijuana_value-tot4drugs_value_mex ///
tot4drugs_value_mex armed_personnel_per milexp military_exp_per riosstate-otros rainm6m7_80s rainm6m7_9093 tempm6m7_80s tempm6m7_9093 tempm4m5_80s tempm4m5_9093 rainm tempm
rename muncode inegi
save "../Data/ConstructionDatabase/Covariates/cov_1990_2010.dta", replace
restore 

merge 1:1 inegi year using "../Data/ConstructionDatabase/Covariates/cov_1990_2010.dta"
drop if _merge==2
drop _merge

*Fix covariates
foreach i in areakm2{
replace `i'=`i'[_n-1] if `i'==.
}

*save "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v1.dta", replace

**3.6) State-level electoral dynamics from Magar

merge m:m  nombre_estado using "../Data/state_elections_mexico_winning_margin.dta"
drop if _merge==2
drop _merge

**3.7) Local state capacity

**3.8) Income 

**3.9) Mayor characteristics
merge m:m inegi year using "../Data/ConstructionDatabase/SNIM/Transformations/presidentes_municipales_historico_winegi_final.dta"
drop if _merge==2 /*Loose some observations from Coahuila and Tabasco of 2009 */
tab emm if _merge==1 /*don't have info of yucatan 2010, and the others are spurious municipalities. 172 observations lost */
drop _merge

quietly bysort inegi year:  gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup

**3.10) ENVIPE Citizens demands
merge m:m estado year using "../Data/ConstructionDatabase/EncuestaVictimizacion/Stata/envipe_2011_2019_estado.dta"
drop if _merge==2
drop _merge

**3.11) Carteles presence, Castillo et. al (2018)
merge m:m inegi using "../Data/ConstructionDatabase/Trafficking Networks/Camilo, Mejia and Restrepo (2020). Cocaine Supply Shortages in Mexico/replication/Leviathan_Restat/dta/carteles.dta"
drop if _merge==2
drop _merge

**3.12) Mando unico by municipality
merge m:m inegi year using "../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011_2018.dta"
drop if _merge==2
drop _merge

**SAVE DATABASE
save "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v1.dta", replace

 
******************
*TRANSFORMATIONS
******************
use "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v1.dta", clear

*A.1) COVARIATES
*generate population for all years
*ssc install carryforward
foreach i in pop_inegi{
*bysort countryname: egen mean`i'=mean(`i') if year<1990 & year>=1975
bysort inegi: carryforward `i', replace 
}

gen pop=.

replace pop=pop_inegi if year==2010
replace pop=pop_salud if year>2010
replace pop=pop_conapo if year>=2015

*Crop eradication adjust for municipality area:
foreach i in cocaina_kg heroina_kg mariguana_kg  metanfetamina_kg{
gen `i'perkm2=`i'/areakm2
}

*add zeros to places that had no army intervention: compare municipalities with and without intervention
foreach i in arma_corta arma_larga cartuchos cocaina_kg heroina_kg mariguana_kg  metanfetamina_kg amapola_kghec  mariguana_kghec granadas  laboratorio pistas  vehiculo_aereo vehiculo_lacustre cocaina_kgperkm2 heroina_kgperkm2 mariguana_kgperkm2  metanfetamina_kgperkm2{
gen `i'_2=0 if `i'==.
replace `i'_2=`i' if `i'!=.
}


*create variable with accumulated kgs of drugs:
egen drugs=rowtotal(mariguana_kg cocaina_kg heroina_kg metanfetamina_kg amapola_kghec)
egen drugs_2=rowtotal(mariguana_kg_2 cocaina_kg_2 heroina_kg_2 metanfetamina_kg_2 amapola_kghec_2)

*log transformations due to outliers:
foreach i in cocaina_kg cocaina_kg_2 heroina_kg heroina_kg_2 metanfetamina_kg  metanfetamina_kg_2 amapola_kghec  amapola_kghec_2 ///
pistas vehiculo_aereo vehiculo_lacustre arma_corta arma_larga cartuchos arma_corta_2 arma_larga_2 cartuchos_2 granadas granadas_2 laboratorio laboratorio_2  ///
drugs  drugs_2{
gen log`i'=log(`i'+1)
}

*A.2) ALTERNATIVE OUTCOMES
***a) adverse selection

gen incumbent_quality=1 if title!="-" | title!="C."
replace incumbent_quality=0 if title=="-" | title=="C."
replace incumbent_quality=. if title==""

*B) OUTCOME 
gen homicidecombined=.
replace homicidecombined=homicide_old if year<2015
replace homicidecombined=homicide if year>=2015

foreach i in detenidos{
gen `i'_2=0 if `i'==.
replace `i'_2=`i' if `i'!=.
}

foreach i in homicide homicide_old homicidecombined defunciones detenidos detenidos_2{
**1) homicides per 100,000 inhabitants
gen `i'pc=(`i'/pop)*100000

**2) logged(homicides)
gen log`i'=log(`i'+1)

**3)logged homicides per 100,000 inhabitants (using log((count + 1)/pop))
gen log`i'pc=log((`i'+1)/pop)

**4) inverse hyperbolic sine homicides 
gen ihs_`i'=asinh(`i')
  }
  
foreach i in homicidepc homicide_oldpc homicidecombinedpc defuncionespc detenidospc detenidos_2pc{
**5) inverse hyperbolic sine homicides per capita
gen ihs_`i'=asinh(`i')
}
  
save "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v2.dta", replace

*C) EVENT-STUDY LEADS, LAGS AND CONTROLS

*C.1) lead and lags:
*Create adoption year variable:
preserve
collapse (mean)reform (firstnm)nombre_estado, by(estado year)
xtset estado year
gen adopt=.
replace adopt=1 if reform>0 & l.reform==0
replace adopt=0 if adopt==.
gen adopt_year=year if adopt==1 //the non-treated states do not have leads or lags. 
save "../Data/ConstructionDatabase/adopt_year.dta", replace
restore

merge m:m estado year using "../Data/ConstructionDatabase/adopt_year.dta"
xtset inegi year
xfill adopt_year, i(inegi)

*Create lead/lag indicators
order year adopt_year
gen rel_year=year-adopt_year
order year adopt_year rel_year

*turn lead/lags to indicator variables
tab rel_year, gen(rel_year_) // recall rel_year_9 is the year_zero
rename rel_year_1 lag_8
rename rel_year_2 lag_7
rename rel_year_3 lag_6
rename rel_year_4 lag_5
rename rel_year_5 lag_4
rename rel_year_6 lag_3
rename rel_year_7 lag_2
rename rel_year_8 lag_1
rename rel_year_9 date_0
rename rel_year_10 lead_1
rename rel_year_11 lead_2
rename rel_year_12 lead_3
*rename rel_year_13 lead_4

gen pre=0
replace pre=1 if lag_8==1 | lag_7==1 

gen pre2=0
replace pre2=1 if lag_8==1 | lag_7==1 | lag_6==1 | lag_5==1 


*C.2) Time-varying controls

*Benchmark model:
tab year, gen(year_)
foreach var in winning_margin_governor HHI effectiveparties  NP golosov dcoal{
foreach y in 1 2 3 4 5 6 7 8 9 10{
capture gen `var'_year_`y'=year_`y'*`var'
}
}

*Event case regression: 
**set controls
foreach i in 1 2 3 4 5 6 7 8{
capture gen margin_lag_`i'=lag_`i'*winning_margin_governor
capture gen margin_lead_`i'=lead_`i'*winning_margin_governor
capture gen margin_date0=date_0*winning_margin_governor
}


foreach var in ncand effectiveparties HHI num_parties NP golosov dcoal governor_alignment pop logdefuncionespc ihs_defuncionespc{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

foreach var in areakm2{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}

foreach var in logdefuncionespc{
foreach i in 1 2 3 4 5 6 7 8{
capture gen `var'_lag_`i'=lag_`i'*`var'
capture gen `var'_lead_`i'=lead_`i'*`var'
capture gen `var'_date0=date_0*`var'
}
}


save "../Data/ConstructionDatabase/data_wleads&lags.dta", replace

*D) EVENT-STUDY LEADS, ABRAHAM AND SUN (2020) FULL SATURATED MODEL
**FOR INCUMBENCY ESTIMATES:
preserve
gen whichlead="" 
replace whichlead="lag_8" if lag_8==1
replace whichlead="lag_7" if lag_7==1
replace whichlead="lag_6" if lag_6==1
replace whichlead="lag_5" if lag_5==1
replace whichlead="lag_4" if lag_4==1
replace whichlead="lag_3" if lag_3==1
replace whichlead="lag_2" if lag_2==1
*replace whichlead="lag_1" if lag_1==1
replace whichlead="date_0" if date_0==1
replace whichlead="lead_1" if lead_1==1
replace whichlead="lead_2" if lead_2==1
replace whichlead="lead_3" if lead_3==1
*replace whichlead="lead_4" if lead_4==1
encode whichlead, gen(whichlead_num)

drop if incumbent_yesterday_w_tomorrow==.
save "../Data/ConstructionDatabase/data_wleads&lags_incumbency.dta", replace

restore


gen whichlead="" 
replace whichlead="lag_8" if lag_8==1
replace whichlead="lag_7" if lag_7==1
replace whichlead="lag_6" if lag_6==1
replace whichlead="lag_5" if lag_5==1
replace whichlead="lag_4" if lag_4==1
replace whichlead="lag_3" if lag_3==1
replace whichlead="lag_2" if lag_2==1
*replace whichlead="lag_1" if lag_1==1
replace whichlead="date_0" if date_0==1
replace whichlead="lead_1" if lead_1==1
replace whichlead="lead_2" if lead_2==1
replace whichlead="lead_3" if lead_3==1
*replace whichlead="lead_4" if lead_4==1
encode whichlead, gen(whichlead_num)



drop if logdefuncionespc==.

*E) INDICATORS FOR HET. TREATMENT EFFECTS:

gen pri_mayor=0
replace pri_mayor=1 if firstword=="pri"

gen morena_mayor=0
replace morena_mayor=1 if firstword=="morena"

gen pan_mayor=0
replace pan_mayor=1 if firstword=="pan"


gen pri_president=0
replace pri_president=1 if year<=2018 & year>=2013


save "../Data/ConstructionDatabase/data_wleads&lags2.dta", replace


****************************************************
* Weights for Abraham and Sun (2020) specification
**************************************************** 
use "../Data/ConstructionDatabase/data_wleads&lags2.dta", replace

preserve
**c) get counts; recall that four states don't have lead and lags (the non-treated)
foreach i in adopt_year{
***c.1) Get n: n is the count of observations by adoption year and lead/lag:
****group observations by adoption year and type of lead/lag
order `i' whichlead
egen group_`i'_leadlag=group(`i' whichlead)
****count the number of times each group appears
bysort group_`i'_leadlag: egen n=count(group_`i'_leadlag)
***c.2) Get percentage: n / total
****group observations by type of lead/lag
egen group_leadlag=group(whichlead)
****
bysort group_leadlag: egen total=count(n) //total is the total number of leads/lags in the effective sample 
bysort group_leadlag: gen perc= n/total
keep whichlead `i' perc
drop if whichlead==""
sort whichlead `i'
collapse (mean)perc, by(`i' whichlead)
order whichlead `i' perc
sort whichlead `i' perc

**d) make variable name to merge in for indicators; we want only the effective indicators that we need for estimation
 tostring `i', generate(`i'_s)
gen indic=whichlead+"_"+`i'_s

save "../Data/ConstructionDatabase/weights.dta", replace
restore

rename _merge _mergeold
merge m:m whichlead `i' using "../Data/ConstructionDatabase/weights.dta" //we do not merge the lag_8 and lag_1
gen indic_name = strtoname(indic)
}

levelsof indic_name, local(names)
foreach n of local names {
    gen byte `n' = (indic_name == "`n'")
}



save "../Data/ConstructionDatabase/data_wleads&lags2_weights.dta", replace


****************************************************
* Weights for Abraham and Sun (2020) specification, FOR INCUMBENCY ADVANTAGE ESTIMATES;
**************************************************** 
use "../Data/ConstructionDatabase/data_wleads&lags_incumbency.dta", replace

preserve
**c) get counts; recall that four states don't have lead and lags (the non-treated)
foreach i in adopt_year{
***c.1) Get n: n is the count of observations by adoption year and lead/lag:
****group observations by adoption year and type of lead/lag
order `i' whichlead
egen group_`i'_leadlag=group(`i' whichlead)
****count the number of times each group appears
bysort group_`i'_leadlag: egen n=count(group_`i'_leadlag)
***c.2) Get percentage: n / total
****group observations by type of lead/lag
egen group_leadlag=group(whichlead)
****
bysort group_leadlag: egen total=count(n) //total is the total number of leads/lags in the effective sample 
bysort group_leadlag: gen perc= n/total
keep whichlead `i' perc
drop if whichlead==""
sort whichlead `i'
collapse (mean)perc, by(`i' whichlead)
order whichlead `i' perc
sort whichlead `i' perc

**d) make variable name to merge in for indicators; we want only the effective indicators that we need for estimation
 tostring `i', generate(`i'_s)
gen indic=whichlead+"_"+`i'_s

save "../Data/ConstructionDatabase/weights_incumbency.dta", replace
restore

rename _merge _mergeold
merge m:m whichlead `i' using "../Data/ConstructionDatabase/weights_incumbency.dta" //we do not merge the lag_8 and lag_1
gen indic_name = strtoname(indic)
}

levelsof indic_name, local(names)
foreach n of local names {
    gen byte `n' = (indic_name == "`n'")
}

save "../Data/ConstructionDatabase/data_wleads&lags_incumbency_weights.dta", replace


****************************************************
* For R
****************************************************
clear all
use "../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v2.dta", clear

collapse (sum) defunciones homicide homicide_old detenidos  pop (mean)reform (firstnm)nombre_estado, by(estado year)
drop if year>2018
foreach i in defunciones homicide homicide_old detenidos{
**1) homicides per 100,000 inhabitants
gen `i'pc=(`i'/pop)*100000

**2) logged(homicides)
gen log`i'=log(`i'+1)

**3)logged homicides per 100,000 inhabitants (using log((count + 1)/pop))
gen log`i'pc=log((`i'+1)/pop)

**4) inverse hyperbolic sine homicides 
gen ihs_`i'=asinh(`i')
  
  }
  
foreach i in homicidepc homicide_oldpc defuncionespc detenidospc{
**5) inverse hyperbolic sine homicides per capita
gen ihs_`i'=asinh(`i')
}
  
foreach i in homicidepc loghomicide loghomicidepc homicide_oldpc loghomicide_old loghomicide_oldpc{
replace `i'=. if `i'==0
}
   
rename estado estado_num
rename nombre_estado state
  
save "../Data/ConstructionDatabase/collapseddata_forR.dta", replace
 




