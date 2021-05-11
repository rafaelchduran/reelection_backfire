*Regressions: Reform & Sec. Cooperation Agreements
*Paper: Reelection Backfire
*Author: Rafael Ch (rafael.ch@nyu.edu)
*========================================================================
/*NOTES
*/
*========================================================================
*Environment
clear all
set more off  
set varabbrev off 
set maxvar 30000
set matsize 11000 
*========================================================================
*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles"

*========================================================================
*LOAD DATA
use "../../Data/ConstructionDatabase/data_final.dta", clear

*========================================================================
*SET PANEL
xtset inegi year 

**********************************************
*Table: Descriptive statistics
**********************************************

*--------------------*
*Agreements:
*--------------------*
label variable acuerdo_estcom "Security Coop. Agreement with governor"
label variable acuerdo_fednoest "Security Coop. Agreement with other actors besides the governor"
label variable servicio_segpublica "Delegate Public Security Provision"
label variable servicio_transito "Delegate Transit Activities"
label variable servicio_capacitacion "Delegate Training of Police Forces"
label variable servicio_equiptec "Delegate Equipment and Technology"
label variable servicio_investigacion "Delegate Research Activities"
label variable servicio_inteligencia "Delegate Intelligence Activities"
label variable servicio_unificacion "Delegate the Unification of Laws and Procedures"
label variable servicio_prevencion "Delegate Public Security Prevention"
label variable motivo_reformacons "Reason to delegate: Constitutional Change"
label variable motivo_reformaley "Reason to delegate: Change in Local Laws"
label variable motivo_faltarecursos "Reason to delegate: Constitutional Change"
label variable motivo_profesioalizacion "Reason to delegate: Need of Professionalization"
label variable motivo_coordinacion "Reason to delegate: Need of Coordination"
label variable motivo_crimen "Reason to delegate: Prevalence of Crime"
label variable motivo_otros "Reason to delegate: Other"

global services servicio_segpublica servicio_transito servicio_capacitacion servicio_equiptec servicio_investigacion servicio_inteligencia servicio_unificacion servicio_prevencion
global motivos motivo_reformacons motivo_reformaley motivo_faltarecursos motivo_profesioalizacion motivo_coordinacion motivo_crimen motivo_otros
global acuerdos acuerdo_estcom acuerdo_fednoest

estpost sum  $acuerdos $services $motivos
est store table1

esttab using "../Tables/desc_agreements.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

*--------------------*
*Violence:
*--------------------*
label variable defunciones "deaths by homicide (INEGI)"
label variable defuncionespc "deaths by homicide per capita (INEGI)"
label variable logdefuncionespc "log(deaths by homicide per capita) (INEGI)"
label variable ihs_defuncionespc "asinh(deaths by homicide per capita) (INEGI)"
global violence defuncionespc logdefuncionespc ihs_defuncionespc
estpost sum  $violence
est store table1

esttab using "../Tables/desc_violence.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)


*--------------------*
*Controls:
*--------------------*
label variable pan_mayor2 "PAN mayor=1; 0 otherwise"
label variable pri_mayor2 "PRI mayor=1; 0 otherwise"
label variable hayCarteles "Incidence of Cartel Presence"

global controls winning_margin_governor alignment_executive_strong alignment_governor_strong winning_margin pan_mayor2 pri_mayor2 hayCarteles 
estpost sum  $controls
est store table_controls

esttab using "../Tables/covariates_statelevel.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(0)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

label variable pop "Population (INEGI and CONAPO projections)"

estpost sum pop 
est store table5

esttab using "../Tables/population.tex", replace  cells("mean(fmt(%11.2gc)) sd(fmt(%11.2gc)) min(fmt(0)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)


*--------------------*
*Mechanisms:
*--------------------*
label variable ap4_2_3_mean "Topic that worries most: narcotraffic (average pre-treatment)"
label variable ap4_2_5_mean "Topic that worries most: insecurity (average pretreatment)"
label variable ap4_2_11_mean "Topic that worries most: punishment of criminals (average pretreatment)"
label variable ap4_2_1_mean "Topic that worries most: poverty (average pretreatment)"
label variable ap4_2_2_mean "Topic that worries most: unemployment (average pretreatment)"
label variable ap4_2_4_mean "Topic that worries most: inflation (average pretreatment)"
label variable ap4_2_6_mean "Topic that worries most: natural disaster (average pretreatment)"
label variable ap4_2_7_mean "Topic that worries most: water scarcity (average pretreatment)"
label variable ap4_2_8_mean "Topic that worries most: corruption (average pretreatment)"
label variable ap4_2_9_mean "Topic that worries most: health (average pretreatment)"
label variable ap4_2_10_mean "Topic that worries most: education (average pretreatment)"


global tastes ap4_2_3_mean ap4_2_5_mean ap4_2_11_mean ap4_2_1_mean ap4_2_2_mean ap4_2_4_mean ap4_2_6_mean ap4_2_7_mean ap4_2_8_mean ap4_2_9_mean ap4_2_10_mean

egen municipal_police=rowmean(ap5_4_1_b_mean ap5_4_2_b_mean)
egen state_police=rowmean(ap5_4_3_b_mean ap5_4_6_b_mean)
egen federal_police=rowmean(ap5_4_4_b_mean ap5_4_5_b_mean ap5_4_8 ap5_4_9_b_mean)

label variable municipal_police "Trust in Municipal Security Forces"
label variable state_police "Trust in State Security Forces"
label variable federal_police "Trust in Federal Security Forces, including the military"

global trust municipal_police state_police federal_police


egen municipal_police2=rowmean(ap5_3_1_mean ap5_3_2_mean)
egen state_police2=rowmean(ap5_3_3_mean ap5_3_6_mean)
egen federal_police2=rowmean(ap5_3_4_mean ap5_3_5_mean ap5_3_8 ap5_3_9_mean)

label variable municipal_police2 "Identify Municipal Security Forces"
label variable state_police2 "Identify State Security Forces"
label variable federal_police2 "Identify Federal Security Forces, including the military"


global identify municipal_police2 state_police2 federal_police2


egen municipal_police3=rowmean(ap5_5_1_mean ap5_5_2_mean)
egen state_police3=rowmean(ap5_5_3_mean ap5_5_6_mean)
egen federal_police3=rowmean(ap5_5_4_mean ap5_5_5_mean ap5_5_8 ap5_5_9_mean)

label variable municipal_police3 "Corruption of Municipal Security Forces"
label variable state_police3 "Corruption of State Security Forces"
label variable federal_police3 "Corruption of Federal Security Forces, including the military"


global corruption municipal_police3 state_police3 federal_police3

egen municipal_police4=rowmean(ap5_6_1_b_mean ap5_6_2_b_mean)
egen state_police4=rowmean(ap5_6_3_b_mean ap5_6_6_b_mean)
egen federal_police4=rowmean(ap5_6_4_b_mean ap5_6_5_b_mean ap5_6_8 ap5_6_9_b_mean)


label variable municipal_police4 "Efficiency of Municipal Security Forces"
label variable state_police4 "Efficiency of State Security Forces"
label variable federal_police4 "Efficiency of Federal Security Forces, including the military"


global efficiency municipal_police4 state_police4 federal_police4


label variable detenidos_2pc "Detained by local police per capita (in flagrancy, SNSP)"
label variable logdetenidos_2pc "log(Detained by local police per capita) (in flagrancy, SNSP)"
label variable logheroina_kg_2 "log(heroine kg), SEDENA"
label variable logcocaina_kg_2 "log(cocaine kg), SEDENA"
label variable logamapola_kghec_2 "log(poppy kg), SEDENA"
label variable logmetanfetamina_kg_2 "log(meth kg), SEDENA"
label variable loglaboratorio_2 "log(laboratories erradicated), SEDENA"

global effort detenidos_2pc logdetenidos_2pc  logheroina_kg_2  logmetanfetamina_kg_2  logcocaina_kg_2 logamapola_kghec_2  loglaboratorio_2  

estpost sum $tastes $trust $identify $corruption $efficiency $effort

est store table3

esttab using "../Tables/desc_mechanisms.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)


*--------------------*
*Preferences:
*--------------------*
label variable ap4_2_3 "Topic that worries most: narcotraffic (for all years)"
label variable ap4_2_5 "Topic that worries most: insecurity (for all years)"
label variable ap4_2_11 "Topic that worries most: punishment of criminals (for all years)"
label variable ap4_2_1 "Topic that worries most: poverty (for all years)"
label variable ap4_2_2 "Topic that worries most: unemployment (for all years)"
label variable ap4_2_4 "Topic that worries most: inflation (for all years)"
label variable ap4_2_6 "Topic that worries most: natural disaster (for all years)"
label variable ap4_2_7 "Topic that worries most: water scarcity (for all years)"
label variable ap4_2_8 "Topic that worries most: corruption (for all years)"
label variable ap4_2_9 "Topic that worries most: health (for all years)"
label variable ap4_2_10 "Topic that worries most: education (for all years)"

global preferences ap4_2_3 ap4_2_5  ap4_2_11 ap4_2_8 ap4_2_1  ap4_2_2  ap4_2_4 ap4_2_6  ap4_2_7  ap4_2_9  ap4_2_10   

estpost sum $preferences

est store table3

esttab using "../Tables/desc_preferences.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)


*--------------------*
*Alternative mechanisms:
*--------------------*
label variable incumbent_quality "Incumbent undergraduate or graduate title (indicator)"

global quality incumbent_quality
estpost sum  $quality
est store table4

esttab using "../Tables/desc_quality.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)

  

*========================================================================
*========================================================================
*========================================================================
*========================================================================
*========================================================================
/*========================================================================

*1) Balance table of treatment and control group  

global  controls $tastes $trust $identify $corruption $efficiency $effort $preferences $quality
eststo clear 
foreach i in $controls{
quietly estpost ttest $controls, by(treatment)
est store table
}
*export
esttab using "balance_table.csv",  label replace nolegend ///
cells("mu_1(label(Mean Treatment)) mu_2(label(Mean Control))b(label(Diff. in Means)) se(label(SE Diff. in Means)) p(label(p-value))") 


*--------------------*
*Treatment:
*--------------------*
/*estpost sum lag_8 lag_7 lag_6 lag_5 lag_4 lag_3 lag_2 lag_1 date_0 lead_1 lead_2 lead_3
est store table2

esttab using "../Tables/treatment.tex", replace  cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(%11.2gc)) count(fmt(%11.2gc))") ///
label nonumber f  noobs alignment(S) booktabs nomtitles plain collabels(none)
*/
