*****************************************************
*Reelection Backfire
*Rafael Ch (2020)
*DTOs presence  
*****************************************************

clear all
set more off  
set varabbrev off 

*Working Directory
cd "/Users/rafach/Dropbox/Dissertation/GovernmentStrategies/Dofiles"

*Load data
use "../Data/ConstructionDatabase/Trafficking Networks/Camilo, Mejia and Restrepo (2020). Cocaine Supply Shortages in Mexico/replication/Leviathan_Restat/dta/CastilloMejiaRestrepo.dta", clear


*label variables
rename codmpio inegi

collapse (mean)distEntradasPrinc eneCarteles hayCarteles nCarteles vCarteles, by(inegi)

save "../Data/ConstructionDatabase/Trafficking Networks/Camilo, Mejia and Restrepo (2020). Cocaine Supply Shortages in Mexico/replication/Leviathan_Restat/dta/carteles.dta", replace
