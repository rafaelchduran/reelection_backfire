


cap prog drop cvmatrix
prog def cvmatrix, eclass
	syntax [anything],  cluster(varlist) vcov_first(string) [addvar(varname) weight(varname) dyadic wild]
	* Obtain regresison equation
	gettoken subcmd 0 : 0
	gettoken requation 0 : 0, parse(",")
	* Extract regression elements
	tokenize `requation'
	local requation=subinstr("`requation'","reg","",.)
	local indep "`3'"
	* Extract the information about clusters
	tokenize `cluster'
	local clust1 "`1'"
	local clust2 "`2'"
	local clust3 "`3'"
	* Define weights 
	if "`weight'"=="" {
		qui: tempvar weight
		qui: gen `weight'=1 
	}
	* Define temporary matrices and scalars
	tempname b_coef vcov db_coef d_b fsvcov cvcov
	
	* If wild option is selected we compute the wild bootstrap standard errors
	if "`wild'"!="" {
		tempname bounds vbeta 
		qui reg `requation' [aw=`weight']
		qui boottest `indep', bootcluster(`clust1' `clust2' `clust3') nograph 
		matrix `bounds'=r(CI)
		matrix `vbeta'=((`bounds'[1,2]-`bounds'[1,1])/(2*1.96))^2
		if "`addvar'"!="" {
			tempname bounds2 vbeta2 
			qui reg `requation' [aw=`weight']
			qui boottest `addvar', bootcluster(`clust1' `clust2' `clust3') nograph  
			matrix `bounds2'=r(CI)
			matrix `vbeta2'=((`bounds2'[1,2]-`bounds2'[1,1])/(2*1.96))^2
		}
	}
	* Second stage regression
	if "`dyadic'"=="" {
		qui reghdfe `requation' [aw=`weight'], noabsorb cluster(`clust1' `clust2' `clust3') 		
	}
	else {
		if "`clust2'"=="" {
			noi di as err "Dyadic designs need two cluster variables."
			exit
		}
		else {
			qui dyadclust: reg `requation', ego(`clust1') alter(`clust2') weights(`weight') 
		}	
	}
	* Obtain the b and the vcov
	scalar def `b_coef'=_b[`indep']
	matrix `vcov'=e(V)
	
	* Obtaining the derivative 
	cap matrix drop `d_b'
	cap qui des `indep'_*, varlist
	if _rc!=0 {
		di as err "You have not provided `indep' in the right format: `indep'_1,`indep'_2,..."
		exit
	}
	qui foreach vv of varlist `r(varlist)' {
		local srequation=subinstr("`requation'","`indep'","`vv'",.)
		reg `srequation' 		
		* Storing db/da
		scalar `db_coef'=_b[`vv']-`b_coef'
		matrix `d_b' = (nullmat(`d_b') \ `db_coef') 	
	}

	* Constructing the correction matrix
	preserve
		u "`vcov_first'", clear
		local ssz=18 // In Pascali this size is set to 18
		mkmat *, matrix(`fsvcov')
		mat `fsvcov'=`fsvcov'[1..`ssz',1..`ssz']
	restore
	matrix `d_b'=`d_b'[1..`ssz',.]
	matrix `cvcov'=`d_b''*`fsvcov'*`d_b'
	local cf=`cvcov'[1,1]
	local cf=`=sqrt(`cf')'
	noi disp "Standard error correction factor:"
	disp `cf'
	
	* Correcting the VCOV matrix; notice the distinction for the wild option
	if "`wild'"=="" {
		matrix `vcov'[1,1] = `vcov'[1,1] + `cvcov'[1,1]
		if "`addvar'"!="" {
			matrix `vcov'[2,2] = `vcov'[2,2] + `cvcov'[1,1]	
		}
	}
	else {
		matrix `vcov'[1,1] = `vbeta'[1,1] + `cvcov'[1,1]
		if "`addvar'"!="" {
			matrix `vcov'[2,2] = `vbeta2'[1,1] + `cvcov'[1,1]	
		}
	}
	qui reg `requation' [aw=`weight']
	ereturn repost V= `vcov' 
	ereturn scalar df_r = .
	regress
end
