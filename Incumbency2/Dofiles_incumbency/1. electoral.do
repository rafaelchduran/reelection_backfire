*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*Electoral variables generation      
*****************************************************


clear all
set more off   
set varabbrev off 

*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Incumbency2/Dofiles_incumbency"

******************
*STATE LEVEL VARIABLES
******************

insheet using "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Mexico/Data/ElectionsMagar/elecRetrns-master_rafa/data/goed1985-present.incumbents.csv", clear
drop yr_pos mo_pos dy_pos
save "../../../Data/state_elections_mexico_1985_present.dta", replace


insheet using "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Mexico/Data/ElectionsMagar/elecRetrns-master_rafa/data/goed1961-on_rafa.csv", clear
save "../../../Data/state_elections_mexico_1961_2012.dta", replace


*a) winning margin of incumbent
use "../../../Data/state_elections_mexico_1961_2012.dta", clear
rename yr year
rename vtot tot

foreach i in l01 l02 l03 l04 l05 l06 l07 l08 l09 l10 l11 l12{
label variable `i' "label of candidate `i''s party or coalition "
}

label variable efec "effective votes, equal the total raw votes minus votes for write-in candidates and invalid ballots."
label variable nulos "invalid ballots"
label variable nr "votes for write-in candidates"
label variable tot "total raw votes"
label variable lisnom "eligible voters (*lista nominal*)"
*label variable notas "notes"
label variable fuente "source"

*drop if status=="new"

*vote share by party:
foreach i in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 nulos tot{
destring `i',replace force
}

foreach i in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12{
label variable `i' "raw vote for candidate `i'"
}

egen efec2=rowtotal(v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12)
label variable efec2 "Total effective votes"

foreach i in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12{
gen vote_share_`i'=`i'/efec2
label variable vote_share_`i' "Vote share of candidate `i'"
}
*max vote share and second max
egen max=rowmax(vote_share_v01-vote_share_v12)
drop if max>1 /*3 missing deleted*/
label variable max "Votes share first runner"

foreach v in vote_share_v01 vote_share_v02 vote_share_v03 vote_share_v04 vote_share_v05 vote_share_v06 vote_share_v07 vote_share_v08 vote_share_v09 vote_share_v10 vote_share_v11 vote_share_v12 {
replace `v'=. if `v'==max
}
 
egen max2=rowmax(vote_share_v01-vote_share_v12)
label variable max2 "Votes share second runner"

gen winning_margin_governor=max-max2
label variable winning_margin_governor "Winning margin: first - second runner (governor)"

*Effective number of parties:
**get votes share sq:
foreach i in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12{
gen vote_share_sq_`i'=vote_share_`i'^2
}

**estimate Laasko-Taagepera (1/sum(vote share per party^2)):
egen sum_vote_share_party_n_2=rowtotal(vote_share_sq_v01 vote_share_sq_v02 vote_share_sq_v03 vote_share_sq_v04 vote_share_sq_v05 vote_share_sq_v06 vote_share_sq_v07 vote_share_sq_v08 vote_share_sq_v09 vote_share_sq_v10 vote_share_sq_v11 vote_share_sq_v12)
gen effectiveparties=1/sum_vote_share_party_n_2
label variable effectiveparties "Laasko-Taagepera Effective Number of Parties Index (governor)"

**estimate Herfindahl-Hirschman Index:
egen HHI=rowtotal(vote_share_sq_v01 vote_share_sq_v02 vote_share_sq_v03 vote_share_sq_v04 vote_share_sq_v05 vote_share_sq_v06 vote_share_sq_v07 vote_share_sq_v08 vote_share_sq_v09 vote_share_sq_v10 vote_share_sq_v11 vote_share_sq_v12)

**estimate Molinar Index (NP)
egen N=rownonmiss(vote_share_sq_v01 vote_share_sq_v02 vote_share_sq_v03 vote_share_sq_v04 vote_share_sq_v05 vote_share_sq_v06 vote_share_sq_v07 vote_share_sq_v08 vote_share_sq_v09 vote_share_sq_v10 vote_share_sq_v11 vote_share_sq_v12)
gen N_sq=N^2
gen NP=1+N_sq*(sum_vote_share_party_n_2)
rename N num_parties
label variable num_parties "Number of parties (governor)"
label variable NP "Molinar Effective Number of Parties Index (governor)"

**estimate Golosov (2010) index: sum[voteshare/(vote share + voteshare_leadingparty^2-vote share^2)]
gen max_sq=max^2
foreach i in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12{
gen division_`i'=vote_share_`i'/[vote_share_`i' + max_sq - vote_share_sq_`i']
}

egen golosov=rowtotal(division_v01 division_v02 division_v03 division_v04 division_v05 division_v06 division_v07 division_v08 division_v09 division_v10 division_v11 division_v12)
label variable golosov "Golosov Effective Number of Parties Index (2015) (governor)"

save "../../../Data/state_elections_winning_margin.dta", replace


*Merge data and generate winning margin of incumbent
use "../../../Data/state_elections_mexico_1961_2012.dta", clear
rename yr year
*drop if year<1989
merge 1:1 ord edon year using  "../../../Data/state_elections_winning_margin.dta"
sort edon year
label variable edon "state number 1:32"
label variable year "year of election"

keep edon year edo ncand dcoal win winning_margin_governor num_parties HHI effectiveparties NP golosov
rename win win_governor
drop if year<2008
gen preref_gov_el_yr=year
gen gov_year_atref=2015-preref_gov_el_yr
gen postref_gov_el_yr=.

foreach edo in bcs cam col  gue mic nl que san son {
replace postref_gov_el_yr=2015 if edo=="`edo'"
}
*colima changed them to 2016, but this was not known in 2014

foreach edo in ags  cua dgo hgo oax qui sin tam tla zac{
replace postref_gov_el_yr=2016 if edo=="`edo'"
}

foreach edo in coa mex nay{
replace postref_gov_el_yr=2017 if edo=="`edo'"
}

foreach edo in cps df gua jal mor tab ver yuc{
replace postref_gov_el_yr=2018 if edo=="`edo'"
}

foreach edo in bc pue{
replace postref_gov_el_yr=2019 if edo=="`edo'"
}

gen nombre_estado=""
replace nombre_estado="AGUASCALIENTES" if edo=="ags"
replace nombre_estado="BAJA CALIFORNIA" if edo=="bc"
replace nombre_estado="BAJA CALIFORNIA SUR" if edo=="bcs"
replace nombre_estado="CAMPECHE" if edo=="cam"
replace nombre_estado="CHIAPAS" if edo=="cps"
replace nombre_estado="CHIHUAHUA" if edo=="cua"
replace nombre_estado="COAHUILA" if edo=="coa"
replace nombre_estado="COLIMA" if edo=="col"
replace nombre_estado="DISTRITO FEDERAL" if edo=="df"
replace nombre_estado="DURANGO" if edo=="dgo"
replace nombre_estado="GUANAJUATO" if edo=="gua"
replace nombre_estado="GUERRERO" if edo=="gue"
replace nombre_estado="HIDALGO" if edo=="hgo"
replace nombre_estado="JALISCO" if edo=="jal"
replace nombre_estado="MEXICO" if edo=="mex"
replace nombre_estado="MICHOACAN" if edo=="mic"
replace nombre_estado="MORELOS" if edo=="mor"
replace nombre_estado="NAYARIT" if edo=="nay"
replace nombre_estado="NUEVO LEON" if edo=="nl"
replace nombre_estado="OAXACA" if edo=="oax"
replace nombre_estado="PUEBLA" if edo=="pue"
replace nombre_estado="QUERETARO" if edo=="que"
replace nombre_estado="QUINTANA ROO" if edo=="qui"
replace nombre_estado="SAN LUIS POTOSI" if edo=="san"
replace nombre_estado="SINALOA" if edo=="sin"
replace nombre_estado="SONORA" if edo=="son"
replace nombre_estado="TABASCO" if edo=="tab"
replace nombre_estado="TAMAULIPAS" if edo=="tam"
replace nombre_estado="TLAXCALA" if edo=="tla"
replace nombre_estado="VERACRUZ" if edo=="ver"
replace nombre_estado="YUCATAN" if edo=="yuc"
replace nombre_estado="ZACATECAS" if edo=="zac"

gen governor_alignment=0
replace governor_alignment=1 if win_governor=="pri" 

drop year


save "../../../Data/state_elections_mexico_winning_margin.dta", replace


******************************************************
******************
*MUNICIPAL LEVEL VARIABLES
******************

/*How dataset should look like
year
mv_incumbent (winning margin of the incumbent party in t)
post_winning_incumbent_unc (outcome variable: measures whether the incumbent party won in the following election t+1)
country
term_lim
iid
*/

*Import Magar (2017) database on municipal elections results and incumbents:
insheet using "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Mexico/Data/ElectionsMagar/elecRetrns-master_rafa/data/aymu1977-present.csv", clear
save "../../../Data/municipal_elections_mexico_1997_present.dta", replace


insheet using "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Mexico/Data/ElectionsMagar/elecRetrns-master_rafa/data/aymu1989-present.incumbents.csv", clear
save "../../../Data/municipal_elections_incumbent_mexico_1989_present.dta", replace

/*

# Magar's Codebook

Most variables are included in every file, some appear in selected files only.  

-   *edon* = state number 1:32.
-   *edo* = state abbreviation (may differ from the 'official' abbreviations so that sorting them alphabetically preserves the order set by *edon*).
-   *disn* = district number.
-   *emm* = municipal indentifying code (*edo*-electionCycle./munn/).
-   *mun* = municipality.
-   *munn*, *inegi*, *ife* = municipal identifier, reporting the number and the codes used by INEGI and IFE, respectively.
-   *yr*, *mo*, *dy* = year, month, day of the election.
-   *cab* = cabecera, district's administrative center.
-   *circ* = PR district (circunscripcion electoral, 2nd tier).
-   *v01*, *v02*, &#x2026; = raw vote for candidate 1, 2, etc.
-   *l01*, *l02*, &#x2026; = label of candidate 1's, 2's, &#x2026; party or coalition.
-   *c01*, *c02*, &#x2026; = candidate 1's, 2's, &#x2026; name.
-   *s01*, *s02*, &#x2026; = suplente (substitute) for candidate 1, 2, etc.
-   *efec* = effective votes, equal the total raw votes minus votes for write-in candidates and invalid ballots.
-   *nr* = votes for write-in candidates.
-   *nul* = invalid ballots.
-   *tot* = total raw votes.
-   *lisnom* = eligible voters (*lista nominal*).
-   *nota* = notes.
-   *fuente* = source.
-   *ncand* = number of candidates running.
-   *dcoal* = dummy equal 1 if at least one major party candidate ran on a multi-party pre-electoral coalition, 0 otherwise.
-   *coalpan*, *coalpri*, *coalprd* = members of major-party coalitions ('no' indidates no coalition).
-   *imputacion*, *distpan*, *distpri*, *distprd* = when some parties coelesced in such way that only their pooled vote was reported, an attempt is made to infer how many votes each coalition member contributed to team. Variable *imputacion* lists what earlier election was used for this purpose ('no' if none carried); *dist* variables report the share of the coalition total attributable to PAN, PRI, and PRD, respectively. See [this](https://github.com/emagar/replicationMaterial/blob/master/gubCoat/onlineAppendix.pdf) for details.
-   *seyr*, *semo* = year of the previous/concurrent senatorial election.
-   *sepan*, *sepri*, *seprd* = votes won by major parties in previous/concurrent senatorial election.
-   *seefec* = effective votes in previous/concurrent senatorial election.
-   *fake* = indicates fake data for hegemonic era elections, made up of best guesses about what happened in the state's race for the purpose of computing vote lags. Will normally be dropped from analysis.
-   *win* = winner's party or coalition.
-   *incumbent* = winning candidate's name.
-   *race.after* = incumbent's status in the subsequent race. See [this](#org7482930) for categories and coding procedure ([aquí](#org6efb291) la versión en español del procedimiento codificador).


<a id="org7f6c9e2"></a>

# Coding procedure for the incumbent's status<a id="org7482930"></a>

In file `data/aymu1985-present.incumbents.csv`, variable *race.after* equals one of the following categories: 

1.  'Beaten' if the incumbent re-ran and lost;
2.  'Reelected' if the incumbent re-ran and won;
3.  'Renom-killed' if the incumbent re-ran and was killed in the campaign;
4.  'Hi-office' if the incumbent ran for higher office;
5.  'Out' if the incumbent withdrew or was not renominated;
6.  'Term-limited' if the incumbent was ineligible for reelection due to a term limit;
7.  A year indicates that it is too early to know the incumbent's status (and the year of the next race).

In categories other than the first two above, a suffix may be present. 

-   Suffix '-p-lost' indicates that the party lost the subsequent race (or, in case of incumbents elected by a multi-party coalition, that none of them won or was part of the winning coalition).
-   Suffix '-p-won' indicates that the party won the subsequent race (or, in case of incumbents elected by a multi-party coalition, that one of them won or at least one of them was in the winning coalition).

# Procedimiento para codificar el estatus del ocupante<a id="org6efb291"></a>

En el archivo `data/aymu1985-present.incumbents.csv`, la variable *race.after* indica el estatus del ocupante en la elección subsecuente. El estatus puede ser una de las categorías siguientes: 

1.  'Beaten' si el ocupante volvió a contender y perdió;
2.  'Reelected' si el ocupante volvió a contender y ganó;
3.  'Renom-killed' si el ocupante volvió a contender y fue asesinado en la campaña;
4.  'Hi-office' si el ocupante contendió por otro cargo de elección (p.ej. gobernador o senador);
5.  'Out' si el ocupante se retiró o no fue repostulado por el partido;
6.  'Term-limited' si el ocupante estaba constitucionalmente impedido para aspirar a reelegirse;
7.  Un año indica que aún es temprano para conocer el estatus (y el año de la próxima elección).

En las categorías 3 en adelante, un sufijo puede estar presente. 

-   El sufijo '-p-lost' indica que el partido perdió la elección subsecuente (o, para ocupantes electos por una coalición multi-partidista, que ninguno de esos partidos ganó o fue parte de la coalición ganadora).
-   El sufijo '-p-won' indica que el partido ganó la elección subsecuente (o, para ocupantes electos por una coalición multi-partidista, que uno de esos partidos ganó o que por lo menos uno fue parte de la coalición ganadora).

*/


*Variable generation 

*Example titiunik
use "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Literature/PolCompetition/IncumbencyDisadvantage/KlasnjaTitiunik2017-APSR-replication-files copy/KlasnjaTitiunik-Brazil-data.dta", clear

**Titiunik database

*a) winning margin of incumbent
**
use "../../../Data/municipal_elections_mexico_1997_present.dta", clear
rename yr year
*drop if year<1989

foreach i in l01 l02 l03 l04 l05 l06 l07 l08 l09 l10 l11 l12 l13 l14 l15 l16 l17 l18{
label variable `i' "label of candidate `i''s party or coalition "
}

label variable efec "effective votes, equal the total raw votes minus votes for write-in candidates and invalid ballots."
label variable nulos "invalid ballots"
label variable nr "votes for write-in candidates"
label variable tot "total raw votes"
label variable lisnom "eligible voters (*lista nominal*)"
label variable notas "notes"
label variable fuente "source"

drop if status=="new"

*vote share by party:
foreach i in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 nulos tot{
destring `i',replace force
}

foreach i in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18{
label variable `i' "raw vote for candidate `i'"
}

egen efec2=rowtotal(v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16)
label variable efec2 "Total effective votes"

foreach i in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18{
gen vote_share_`i'=`i'/efec2
label variable vote_share_`i' "Vote share of candidate `i'"
}
*max vote share and second max
egen max=rowmax(vote_share_v01-vote_share_v18)
drop if max>1 /*1134 missing or elecciones anuladas deleted*/
label variable max "Votes share first runner"

******************************
*Party wins or looses
foreach i in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18{
gen won_`i'=0
replace won_`i'=1 if vote_share_`i'==max
label variable won_`i' "Party `i' won election"
}

foreach i in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18{
gen won_next_`i'=0
replace won_next_`i'=1 if vote_share_`i'[_n+1]==max[_n+1]
label variable won_next_`i' "Party `i' won next election"
}


*******************************

foreach v in vote_share_v01 vote_share_v02 vote_share_v03 vote_share_v04 vote_share_v05 vote_share_v06 vote_share_v07 vote_share_v08 vote_share_v09 vote_share_v10 vote_share_v11 vote_share_v12 vote_share_v13 vote_share_v14 vote_share_v15 vote_share_v16 vote_share_v17 vote_share_v18{
replace `v'=. if `v'==max
}
 
egen max2=rowmax(vote_share_v01-vote_share_v18)
label variable max2 "Votes share second runner"

gen winning_margin=max-max2
label variable winning_margin "Winning margin: first - second runner"

save "../../../Data/municipal_elections_winning_margin.dta", replace


*Merge data and generate winning margin of incumbent
use "../../../Data/municipal_elections_incumbent_mexico_1989_present.dta", clear
rename yr year
*drop if year<1989
merge 1:1 year emm inegi using  "../../../Data/municipal_elections_winning_margin.dta"
sort inegi year
label variable edon "state number 1:32"
label variable emm "municipal indentifying code (*edo*-electionCycle./munn/)."
label variable mun "Municipality name"
label variable munn "municipal identifying code"
label variable inegi "INEGI identifying code"
label variable ife "IFE identifying code"
label variable status "historical status of variable"
label variable year "year of election"
label variable dy "day of election"
label variable mo "month of election"
label variable win "winner's party or coalition"
label variable incumbent "winning candidate's name"
label variable raceafter "incumbent's status in the subsequent race."
label variable note "notes"
label variable fuente "source"
******************************
gen inc_won_next=0
replace inc_won_next=1 if raceafter=="Reelected" | raceafter=="Out-p-won" | raceafter=="Term-limited-p-won"
label variable inc_won_next "Incumbent won next election"

*******************************
*Create all variables of Titiunik

*1) effective number of parties Laasko-Taagepera and Molinar
/*
Laakso-Taagepera (1979)
N=1/Sum_i^n p_i^2
n=number of parties with at least one vote
pi=vote share per party

Molinar Index = NP = 1+N^2(Sumi=2 to n)Pi^2, where the last expression stands
for the sum of all the minoriy parties.
N=number of parties; the thing is that candidates ran for the same party in 
some municipalities
Pi=vote share per party 

*/
foreach v in vote_share_v01 vote_share_v02 vote_share_v03 vote_share_v04 vote_share_v05 vote_share_v06 vote_share_v07 vote_share_v08 vote_share_v09 vote_share_v10 vote_share_v11 vote_share_v12 vote_share_v13 vote_share_v14 vote_share_v15 vote_share_v16 vote_share_v17 vote_share_v18{
replace `v'=max if `v'==.
}

foreach v in vote_share_v01 vote_share_v02 vote_share_v03 vote_share_v04 vote_share_v05 vote_share_v06 vote_share_v07 vote_share_v08 vote_share_v09 vote_share_v10 vote_share_v11 vote_share_v12 vote_share_v13 vote_share_v14 vote_share_v15 vote_share_v16 vote_share_v17 vote_share_v18{
replace `v'=. if `v'==0
}

egen N=rownonmiss(vote_share_v01 vote_share_v02 vote_share_v03 vote_share_v04 vote_share_v05 vote_share_v06 vote_share_v07 vote_share_v08 vote_share_v09 vote_share_v10 vote_share_v11 vote_share_v12 vote_share_v13 vote_share_v14 vote_share_v15 vote_share_v16 vote_share_v17 vote_share_v18)
gen N_sq=N^2


*Vote share per party squares
foreach v in vote_share_v01 vote_share_v02 vote_share_v03 vote_share_v04 vote_share_v05 vote_share_v06 vote_share_v07 vote_share_v08 vote_share_v09 vote_share_v10 vote_share_v11 vote_share_v12 vote_share_v13 vote_share_v14 vote_share_v15 vote_share_v16 vote_share_v17 vote_share_v18{
gen `v'_sq=`v'^2
}


*1.a.) Laasko-Taagepera (ENP)
egen sum_voteshare_sq=rowtotal(vote_share_v01_sq vote_share_v02_sq vote_share_v03_sq vote_share_v04_sq vote_share_v05_sq vote_share_v06_sq vote_share_v07_sq vote_share_v08_sq vote_share_v09_sq vote_share_v10_sq vote_share_v11_sq vote_share_v12_sq vote_share_v13_sq vote_share_v14_sq vote_share_v15_sq vote_share_v16_sq vote_share_v17_sq vote_share_v18_sq)
gen numparties_eff=1/sum_voteshare_sq
label variable numparties_eff "Effective num. of parties (Laasko-Taagepera)"

foreach i in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18{
replace `i'=. if `i'==0
}


egen numparties=rownonmiss(v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12 v13 v14 v15 v16 v17 v18)
label variable numparties "Number of parties"


*1.b.) Molinar Index (MNP)
gen numparties_eff_molinar=(1+N_sq*(sum_voteshare_sq))/10
label variable numparties_eff_molinar "Effective num. of parties (Molinar Index)"

*Create groups
egen mun_election=group(inegi year)
label variable mun_election "municipality-election year indicator"

*2.a.b.c.) Parties running, with one lag and one forwad
quietly by inegi year:  gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
xtset inegi year

foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18{
bysort inegi: gen party_`i'_runs=0
bysort inegi: replace party_`i'_runs=1 if vote_share_v`i'!=.
label variable party_`i'_runs "party `i' runs in election"

bysort inegi: gen party_`i'_runsfor1=0
bysort inegi: replace party_`i'_runsfor1=1 if vote_share_v`i'[_n+1]!=.
label variable party_`i'_runsfor1 "party `i' runs in following election"

bysort inegi: gen party_`i'_runslag1=0
bysort inegi: replace party_`i'_runslag1=1 if vote_share_v`i'[_n-1]!=.
label variable party_`i'_runslag1 "party `i' ran in last election"
**generate municipality-year id and then erase the first observation in party_`i'_runslag1
}


*3.) winning margin by main parties
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18{
gen mv_party_`i'=vote_share_v`i'-max
replace mv_party_`i'=winning_margin if mv_party_`i'==0
label variable mv_party_`i' "winning margin of party `i'"
}


*4. and 5.) vote share and winning margin at t+1 and at t-1
drop if efec2==.
*drop if win==""

xtset inegi year
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18{
bysort inegi: gen vote_share_v`i'for1=vote_share_v`i'[_n+1] if party_`i'_runs[_n+1]==1
bysort inegi: gen mv_party_`i'for1=mv_party_`i'[_n+1] if party_`i'_runs[_n+1]==1
label variable vote_share_v`i'for1 "vote share of party `i' at t+1 election"
label variable mv_party_`i'for1 "winning margin of party `i' at t+1 election"

bysort inegi: gen vote_share_v`i'lag1=vote_share_v`i'[_n-1] if party_`i'_runs[_n-1]==1
bysort inegi: gen mv_party_`i'lag1=mv_party_`i'[_n-1] if party_`i'_runs[_n-1]==1
label variable vote_share_v`i'lag1 "vote share of party `i' at t-1 election"
label variable mv_party_`i'lag1 "winning margin of party `i' at t-1 election"
}


*6.) Incumbent party (from t-1) won at t
** expect first line with missings, then 0 or 1s 
/** example first mun of aguascalientes: 
-first line: pri is incumbent and should be filled with missings
-second line: should be 1 because pri won
**/
**keep if inegi==1001 | inegi==1002
gen inc_party_won=.
replace inc_party_won=0 if raceafter[_n-1]=="Term-limited-p-lost" | raceafter[_n-1]=="Out-p-lost" | raceafter[_n-1]=="Beaten" 
replace inc_party_won=1 if raceafter[_n-1]=="Term-limited-p-won" | raceafter[_n-1]=="Out-p-won" | raceafter[_n-1]=="Reelected"

**Incumbent party at t won at t+1: RDD: effect of inc. winning at t on incumbent winning at t+1 vs inc. loosing on incumbent loosing at t+1.
gen inc_party_won_tplus1=.
replace inc_party_won_tplus1=0 if raceafter=="Term-limited-p-lost" | raceafter=="Out-p-lost" | raceafter=="Beaten" 
replace inc_party_won_tplus1=1 if raceafter=="Term-limited-p-won" | raceafter=="Out-p-won" | raceafter=="Reelected"
label variable inc_party_won_tplus1 "Incumbent party at t won at t+1"


*7.a) Incumbent's party (from t-1) margin of victory at t (this should be negative sometimes)
/** example first mun of aguascalientes: 
-first line: pri is incumbent and should be filled with missings
-second line: should be winning margin of pri in that election
-third line: should be winning margin of pan in that election
**/
gen mv_incparty=.
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18{
by inegi: replace mv_incparty=mv_party_`i' if winning_margin[_n-1]==mv_party_`i'[_n-1]
label variable mv_incparty "Incumbent's party (from t-1) margin of victory at t"

}

*7.b) Party margin of victory at t (this should be negative sometimes)
gen mv_party=.
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18{
by inegi: replace mv_party=mv_party_`i' if winning_margin[_n-1]==mv_party_`i'[_n-1]
label variable mv_party "Incumbent's party (from t-1) margin of victory at t"

}

*8.) Incumbent's party (from t-1) margin of victory at t+1 (this should be negative sometimes)
/** example first mun of aguascalientes: 
-first line: pri is incumbent and should be filled with missings
-second line: should be winning margin of pri in that election
-third line: should be winning margin of pan in that election
-last line: missing value
**/
gen mv_incpartyfor1=.
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18{
by inegi: replace mv_incpartyfor1=mv_party_`i'[_n+1] if winning_margin[_n-1]==mv_party_`i'[_n-1]
label variable mv_incpartyfor1 "Incumbent's party (from t-1) margin of victory at t+1"
}

*9.) Incumbent's party (from t-1) runs at t+1
gen inc_party_runsfor1=.
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18{
bysort inegi: replace inc_party_runsfor1=1 if party_`i'_runsfor1==1 & winning_margin[_n-1]==mv_party_`i'[_n-1]
replace inc_party_runsfor1=. if mv_incpartyfor1==.
label variable inc_party_runsfor1 "Incumbent's party (from t-1) runs at t+1"
}

***Main outcome variable:
*10.) Incumbent party (from t-1) won at t+1, conditional on running at t
/** example first mun of aguascalientes: 
-first line: missings
-second line: 0 (pri incumbent at t ran and at t+1 lost, so inc_party_wonfor1=0)
-third line: (pri incumbent at t-1, ran at t and at t+1 lost, so inc_party_wonfor1=0)
-fourth line: (pan incumbent at t-1, ran at t and at t+1 won, so inc_party_wonfor1=1)
-fifth line: (pan incumbent at t-1, ran at t and at t+1 won, so inc_party_wonfor1=1)
-sixth line: (pan incumbent at t-1, ran at t and at t+1 lost, so inc_party_wonfor1=0)
-seventh line: (pan incumbent at t-1, ran at t and at t+1 lost, so inc_party_wonfor1=0)
-eight line: (pri incumbent at t-1, ran at t and at t+1 lost, so inc_party_wonfor1=0)
-last line: missing value
**/

**a) Measure relying on party names (we don't care about coalitions):
gen incumbent_yesterday=win[_n-1]
gen incumbent_today=win
gen incumbent_tomorrow=win[_n+1]
replace incumbent_tomorrow=win if raceafter=="Term-limited-p-won" | raceafter=="Out-p-won" | raceafter=="Reelected"

gen incumbent_yesterday_w_today=.
replace incumbent_yesterday_w_today=0 if incumbent_yesterday!=incumbent_today
replace incumbent_yesterday_w_today=1 if incumbent_yesterday==incumbent_today
replace incumbent_yesterday_w_today=. if incumbent_yesterday==""

gen incumbent_today_w_tomorrow=.
replace incumbent_today_w_tomorrow=0 if incumbent_today!=incumbent_tomorrow
replace incumbent_today_w_tomorrow=1 if incumbent_today==incumbent_tomorrow
replace incumbent_today_w_tomorrow=. if incumbent_today==""

gen incumbent_yesterday_w_tomorrow=.
replace incumbent_yesterday_w_tomorrow=0 if incumbent_yesterday!=incumbent_tomorrow
replace incumbent_yesterday_w_tomorrow=1 if incumbent_yesterday==incumbent_tomorrow
replace incumbent_yesterday_w_tomorrow=. if incumbent_yesterday==""

*b) measure not relying on party names (because of coalitions)
*b.2) unconditional on being an incumbent on t-1
gen incumbent_today_w_tomorrow2=.
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18{
by inegi: replace incumbent_today_w_tomorrow2=0 if winning_margin[_n+1]!=mv_party_`i'[_n+1] & winning_margin[_n]==mv_party_`i'[_n]
by inegi: replace incumbent_today_w_tomorrow2=1 if winning_margin[_n+1]==mv_party_`i'[_n+1] & winning_margin[_n]==mv_party_`i'[_n]
by inegi: replace incumbent_today_w_tomorrow2=. if incumbent_today==""
by inegi: replace incumbent_today_w_tomorrow2=. if winning_margin[_n+1]==.
}

replace incumbent_today_w_tomorrow2=. if mun!=mun[_n-1]

*b.2) conditional on being an incumbent on t-1
gen incumbent_yesterday_w_tomorrow2=.
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18{
by inegi: replace incumbent_yesterday_w_tomorrow2=0 if winning_margin[_n+1]!=mv_party_`i'[_n+1] & winning_margin[_n-1]==mv_party_`i'[_n-1]
by inegi: replace incumbent_yesterday_w_tomorrow2=1 if winning_margin[_n+1]==mv_party_`i'[_n+1] & winning_margin[_n-1]==mv_party_`i'[_n-1]
by inegi: replace incumbent_yesterday_w_tomorrow2=. if incumbent_yesterday==""
by inegi: replace incumbent_yesterday_w_tomorrow2=. if winning_margin[_n+1]==.
}

replace incumbent_yesterday_w_tomorrow2=. if mun!=mun[_n-1]


*11.) Coalition size by party
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18{
replace l`i'=subinstr(l`i',"-"," ",.)
gen coalition_v`i'=wordcount(l`i')
label variable coalition_v`i' "coalition `i' size"
}

save "../../../Data/municipal_elections_incumbent_mexico_1989_present_v1.dta", replace
export delimited using "../../../Data/municipal_elections_incumbent_mexico_1989_present_v1.csv", replace

*Brazil Titiunik database:
**use "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Literature/PolCompetition/IncumbencyDisadvantage/KlasnjaTitiunik2017-APSR-replication-files/KlasnjaTitiunik-Brazil-data.dta", clear

**Treatment Variables
use "../../../Data/municipal_elections_incumbent_mexico_1989_present_v1.dta", clear

*1) Reelection Reform ** use changes, and use the idea that a state knows there will be reelection but have a free one

*first treatment year
gen reform=.
foreach i in 3 4 6 7 11 12 14 15 16 17 19 22 24 27 31{
replace reform=1 if year==2015 & edon==`i'
replace reform=1 if year==2016 & edon==`i'
replace reform=1 if year==2017 & edon==`i'
replace reform=1 if year==2018 & edon==`i'
replace reform=1 if year==2019 & edon==`i'
}
label variable reform "Dummy=1 if treated Electoral Reform; o otherwise"

*second treatment year
foreach i in 1 2 8 10 20 23 25 28 32{
replace reform=1 if year==2016 & edon==`i'
replace reform=1 if year==2017 & edon==`i'
replace reform=1 if year==2018 & edon==`i'
replace reform=1 if year==2019 & edon==`i'
}

*third treatment year 
foreach i in 5{
replace reform=1 if year==2017 & edon==`i'
replace reform=1 if year==2018 & edon==`i'
replace reform=1 if year==2019 & edon==`i'
}

*fourth treatment year
foreach i in 9 21 26{
replace reform=1 if year==2018 & edon==`i'
replace reform=1 if year==2019 & edon==`i'
}

replace reform=0 if reform==.

*2) Alignment with federal executive
replace win=subinstr(win,"-"," ",.)
egen firstword = ends(win), head

gen alignment_executive_strong=0
replace alignment_executive_strong=1 if firstword=="pri" | firstword=="pvem" | firstword=="prd" | firstword=="parm"  & year<2018 & year>2011
replace alignment_executive_strong=1 if firstword=="pan" & year<2012 & year>1999
replace alignment_executive_strong=1 if firstword=="morena" | firstword=="pt1" | firstword=="mc" | firstword=="conve" & year>2017

label variable alignment_executive_strong "alignment with federal executive=1; 0 otherwise"

*3) Alignment with governor
preserve
use "../../../Data/state_elections_mexico_winning_margin.dta", clear
keep edon  win_governor
save "../../../Data/state_elections_mexico_winning_margin_formun_merge.dta", replace
restore
drop _merge

merge m:m edon using "../../../Data/state_elections_mexico_winning_margin_formun_merge.dta"
drop _merge
gen alignment_governor_strong=0
replace alignment_governor_strong=1 if firstword==win_governor

gen alignment_governor_weak=0
replace alignment_governor_weak=1 if firstword==win_governor
replace alignment_governor_weak=1 if firstword=="pri" | firstword=="pvem" | firstword=="prd" | firstword=="parm"  & win_governor=="pri"

label variable alignment_governor_strong "alignment with state executive=1; 0 otherwise"
label variable alignment_governor_weak "alignment with state executive=1; 0 otherwise (weak)"


*4) Alignment with executive and governor

gen double_alignment=0
replace double_alignment=1 if alignment_executive_strong==1 & alignment_governor_strong==1

*drop _merge
*save dataset
save "../../../Data/municipal_elections_incumbent_mexico_1989_present_v2.dta", replace

drop if year<2018
save "../../../Data/municipal_elections_incumbent_mexico_1989_present_v3.dta", replace




