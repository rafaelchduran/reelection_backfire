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


*Clean datasets
******
**2011
******

insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2011a.csv", clear
rename ubi_geo inegi
sort inegi
foreach i in cuen_con tipo_int sol_ipol tipo_sis prob_rvi acio_pol delitoid rvic_fco rvic_ffe vict_fue t_interv t_solici t_victim est_tint est_t_so est_tvic{
replace `i'="." if `i'=="NA"
destring `i', replace
}

collapse (mean)cuen_con, by (inegi)
replace cuen_con=0 if cuen_con==2
replace cuen_con=. if cuen_con==9
replace cuen_con=. if cuen_con==3
bysort inegi:  gen dup = cond(_N==1,0,_n)
drop dup
rename cuen_con acuerdo_gobestatal
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011a.dta", replace

insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2011b.csv", clear
rename ubi_geo inegi
sort inegi
replace mot_con=. if mot_con==8
replace mot_con=. if mot_con==9
tab mot_con, gen(motivo_)
collapse (mean)motivo_*, by (inegi)
foreach i in 1 2 3 4 5 6 7 8{
replace motivo_`i'=1 if motivo_`i'>0
}
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011b.dta", replace

***merge 2011
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011a.dta", clear
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011b.dta"
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
insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2013a.csv", clear
rename ubic_geo inegi
sort inegi
foreach i in cuen_con{
replace `i'="." if `i'=="NA"
destring `i', replace
}

drop if cuen_con==.

collapse (mean)cuen_con, by (inegi)
replace cuen_con=0 if cuen_con==2
replace cuen_con=. if cuen_con==9
replace cuen_con=. if cuen_con==10
bysort inegi:  gen dup = cond(_N==1,0,_n)
drop dup
rename cuen_con acuerdo_gobestatal
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013a.dta", replace


insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2013a.csv", clear
rename ubic_geo inegi
sort inegi
foreach i in mot_conv{
replace `i'="." if `i'=="NA"
destring `i', replace
}

drop if mot_conv==.
sort inegi
replace mot_conv=. if mot_conv==9
replace mot_conv=. if mot_conv==10
replace mot_conv=. if mot_conv==11
tab mot_conv, gen(motivo_)
collapse (mean)motivo_*, by (inegi)
foreach i in 1 2 3 4 5 6 7 8{
replace motivo_`i'=1 if motivo_`i'>0
}
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013b.dta", replace

***merge 2013
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013a.dta", clear
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2013b.dta"
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

insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2015.csv", clear
rename ubic_geo inegi
sort inegi
foreach i in conv_mup aso_gfdm serv_pub inst_reg tipo_gob raz_conv tt_gobas est_goba{
replace `i'="." if `i'=="NA"
destring `i', replace
}
preserve
drop if conv_mup==.

collapse (mean)conv_mup, by (inegi)
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


preserve
drop if aso_gfdm==.

collapse (mean)aso_gfdm, by (inegi)
replace aso_gfdm=0 if aso_gfdm==2
replace aso_gfdm=. if aso_gfdm==8
replace aso_gfdm=. if aso_gfdm==9
bysort inegi:  gen dup = cond(_N==1,0,_n)
tab dup
drop dup
rename aso_gfdm acuerdo_gobestatal
gen year=2014
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
rename funciones_1 funciones_noaplica
rename funciones_2 funciones_segpublica
rename funciones_3 funciones_transito
rename funciones_4 funciones_mandounico
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
rename tipogob_1 tipogob_noaplica
rename tipogob_2 tipogob_fed
rename tipogob_3 tipogob_est
rename tipogob_4 tipogob_mun
rename tipogob_5 tipogob_estyotra
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


gen acuerdo_gobestatal3=funciones_mandounico

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
label variable acuerdo_gobestatal2 "Acuerdo realizar funciones de seguridad publica, por alguna necesidad"
label variable acuerdo_gobestatal3 "Mando unico"

save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2014.dta", replace


******
**2016 & 2017
******

insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2017c.csv", clear
rename ubic_geo inegi
sort inegi
foreach i in nd_nsnn1 nd_nsnn2{
*replace `i'=. if `i'==0
replace `i'=. if `i'==8
replace `i'=. if `i'==9
replace `i'=0 if `i'==1
replace `i'=1 if `i'==2
}


preserve
drop if nd_nsnn2==.
collapse (mean)nd_nsnn2, by (inegi)
bysort inegi:  gen dup = cond(_N==1,0,_n)
tab dup
drop dup
rename nd_nsnn2 acuerdo_gobestatal3
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


insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2017a.csv", clear
rename ubic_geo inegi
sort inegi
foreach i in gobier1 gobier2 gobier3 gobier4 gobier5 gobier6 gobier7 gobier8 gobier9 gobier10 gobier11 gobier12 gobier13 gobier15 racoac1 racoac2 racoac3 racoac4 racoac5 racoac6 racoac7 racoac8 totalca1{
replace `i'="." if `i'=="NA"
replace `i'="." if `i'=="ND"
replace `i'="." if `i'=="NSS"
destring `i', replace
}

replace nd_segpu=. if nd_segpu==98
replace nd_segpu=. if nd_segpu==99

collapse (mean)nd_segpu racoac*, by (inegi)
foreach i in 1 2 3 4 5 6 7 8{
replace racoac`i'=1 if racoac`i'>0
}
replace nd_segpu=1 if nd_segpu>0
gen year=2016
rename racoac1 motivo_reformacons
rename racoac2 motivo_reformaley
rename racoac3 motivo_faltarecursos
rename racoac4 motivo_profesioalizacion
rename racoac5 motivo_coordinacion
rename racoac6 motivo_crimen
rename racoac7 motivo_otros
drop racoac8
order inegi year
*rename nd_segpu acuerdo_gobestatal2
*tab acuerdo_gobestatal2
save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2016b.dta", replace

***merge 2016
use "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2016a.dta", clear
merge 1:1 inegi using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2016b.dta"
gen acuerdo_gobestatal2=0 if _merge==2
replace acuerdo_gobestatal2=1 if acuerdo_gobestatal==1
replace acuerdo_gobestatal2=0 if acuerdo_gobestatal==0
drop _merge

label variable acuerdo_gobestatal "Acuerdo realizar funciones de seguridad publica"
label variable acuerdo_gobestatal2 "Acuerdo realizar funciones de seguridad publica, por alguna necesidad"

save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2016.dta", replace


******
**2018
******

insheet using "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/CSVs/mando_unico_2019c.csv", clear
rename ubic_geo inegi
sort inegi
rename re_nsnn1 nd_nsnn1
rename re_nsnn2 nd_nsnn2
foreach i in nd_nsnn1 nd_nsnn2{
*replace `i'=. if `i'==0
replace `i'=. if `i'==98
replace `i'=. if `i'==97
replace `i'=. if `i'==9
replace `i'=0 if `i'==1 
replace `i'=1 if `i'==2
}

*drop if nd_nsnn2==.
collapse (mean)nd_nsnn2, by (inegi)
bysort inegi:  gen dup = cond(_N==1,0,_n)
tab dup
drop dup
rename nd_nsnn2 acuerdo_gobestatal3
gen acuerdo_gobestatal2=acuerdo_gobestatal3
replace acuerdo_gobestatal2=0 if acuerdo_gobestatal3==.
gen year=2018
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

order inegi year acuerdo_gobestatal acuerdo_gobestatal2 acuerdo_gobestatal3

gen acuerdo=.
replace acuerdo=acuerdo_gobestatal if year==2011
replace acuerdo=acuerdo_gobestatal if year==2012
replace acuerdo=acuerdo_gobestatal if year==2013
replace acuerdo=acuerdo_gobestatal if year==2014
replace acuerdo=acuerdo_gobestatal3 if year==2015
replace acuerdo=acuerdo_gobestatal if year==2016
replace acuerdo=acuerdo_gobestatal3 if year==2017
replace acuerdo=acuerdo_gobestatal3 if year==2018



gen acuerdo2=.
replace acuerdo2=acuerdo_gobestatal2 if year==2011
replace acuerdo2=acuerdo_gobestatal2 if year==2012
replace acuerdo2=acuerdo_gobestatal2 if year==2013
replace acuerdo2=acuerdo_gobestatal2 if year==2014
replace acuerdo2=acuerdo_gobestatal3 if year==2015
replace acuerdo2=acuerdo_gobestatal2 if year==2016
replace acuerdo2=acuerdo_gobestatal3 if year==2017
replace acuerdo2=acuerdo_gobestatal2 if year==2018

foreach y in 2011 2012 2013 2014 2015 2016 2017 2018{
sum acuerdo if year==`y'
}

foreach y in 2011 2012 2013 2014 2015 2016 2017 2018{
sum acuerdo2 if year==`y'
}

save "../../Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/Stata/mando_unico_2011_2018.dta", replace
