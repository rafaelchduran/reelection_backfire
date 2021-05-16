*========================================================================
*2SLS
capture program drop getvcov_2sls
 program define getvcov_2sls, eclass 
  boottest {lpred}, ///
  bootcluster(estado) seed(5675) level(95) boottype(wild)   nograph 
		matrix bounds_1=r(CI)
		matrix vbeta_1=((bounds_1[1,2]-bounds_1[1,1])/(2*1.96))^2
			matrix list vbeta_1
			
end
*substitute matrix
capture program drop newcov_2sls
 program define newcov_2sls, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=vbeta_1[1,1]
 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: short2
capture program drop wildcorrection_2sls
 program define wildcorrection_2sls, eclass 
  args Dep_var
qui xi: reghdfe `Dep_var' lpred $controls_time_acuerdo i.year, a(inegi) 
getvcov_2sls
newcov_2sls
end


*========================================================================
*Variance-Covariance Matrix corrections:
/*to see matrices 
matrix list bounds_1
di bounds_1[1,1]
*/

*program to get vcov
capture program drop getvcov_short
 program define getvcov_short, eclass 
  boottest {lag_5} {lag_4} {lag_3} {lag_2} {date_0} {lead_1} {lead_2} {lead_3}, ///
  bootcluster(estado) seed(5675) level(95) boottype(wild)   nograph 
	*tempname bounds_1 vbeta_1 vcov_1 bounds_2 vbeta_2 vcov_2 bounds_3 vbeta_3 vcov_3 bounds_4 vbeta_4 vcov_4    
		matrix bounds_5=r(CI_1)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_2)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_3)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_4)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
		matrix bounds_0=r(CI_5)
		matrix vbeta_0=((bounds_0[1,2]-bounds_0[1,1])/(2*1.96))^2
			matrix list vbeta_0			
		matrix bounds_p1=r(CI_6)
		matrix vbeta_p1=((bounds_p1[1,2]-bounds_p1[1,1])/(2*1.96))^2
			matrix list vbeta_p1	
		matrix bounds_p2=r(CI_7)
		matrix vbeta_p2=((bounds_p2[1,2]-bounds_p2[1,1])/(2*1.96))^2
			matrix list vbeta_p2				
		matrix bounds_p3=r(CI_8)
		matrix vbeta_p3=((bounds_p3[1,2]-bounds_p3[1,1])/(2*1.96))^2
			matrix list vbeta_p3				
end

capture program drop getvcov_short2
 program define getvcov_short2, eclass 
  boottest{lag_4} {lag_3} {lag_2} {date_0} {lead_1} {lead_3}, ///
  bootcluster(estado) seed(5675) level(95) boottype(wild)   nograph 
		matrix bounds_5=r(CI_1)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_2)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_3)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_4)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
		matrix bounds_0=r(CI_5)
		matrix vbeta_0=((bounds_0[1,2]-bounds_0[1,1])/(2*1.96))^2
			matrix list vbeta_0			
		matrix bounds_p1=r(CI_6)
		matrix vbeta_p1=((bounds_p1[1,2]-bounds_p1[1,1])/(2*1.96))^2
			matrix list vbeta_p1					
end

capture program drop getvcov
 program define getvcov, eclass 
  boottest {lag_7}  {lag_6}  {lag_5} {lag_4} {lag_3} {lag_2} {date_0} {lead_1} {lead_2} {lead_3}, ///
  bootcluster(estado) seed(5675) level(95) boottype(wild)   nograph 
		matrix bounds_7=r(CI_1)
		matrix vbeta_7=((bounds_7[1,2]-bounds_7[1,1])/(2*1.96))^2
			matrix list vbeta_7
		matrix bounds_6=r(CI_2)
		matrix vbeta_6=((bounds_6[1,2]-bounds_6[1,1])/(2*1.96))^2
			matrix list vbeta_6
		matrix bounds_5=r(CI_3)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_4)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_5)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_6)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
		matrix bounds_0=r(CI_7)
		matrix vbeta_0=((bounds_0[1,2]-bounds_0[1,1])/(2*1.96))^2
			matrix list vbeta_0			
		matrix bounds_p1=r(CI_8)
		matrix vbeta_p1=((bounds_p1[1,2]-bounds_p1[1,1])/(2*1.96))^2
			matrix list vbeta_p1	
		matrix bounds_p2=r(CI_9)
		matrix vbeta_p2=((bounds_p2[1,2]-bounds_p2[1,1])/(2*1.96))^2
			matrix list vbeta_p2				
		matrix bounds_p3=r(CI_10)
		matrix vbeta_p3=((bounds_p3[1,2]-bounds_p3[1,1])/(2*1.96))^2
			matrix list vbeta_p3				
end

capture program drop getvcov_naive_incumbency
 program define getvcov_naive_incumbency, eclass 
  boottest {lag_7}  {lag_6}  {lag_5} {lag_4} {lag_3} {date_0} , ///
  bootcluster(estado) seed(5675) level(95) boottype(wild)   nograph 
		matrix bounds_7=r(CI_1)
		matrix vbeta_7=((bounds_7[1,2]-bounds_7[1,1])/(2*1.96))^2
			matrix list vbeta_7
		matrix bounds_6=r(CI_2)
		matrix vbeta_6=((bounds_6[1,2]-bounds_6[1,1])/(2*1.96))^2
			matrix list vbeta_6
		matrix bounds_5=r(CI_3)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_4)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_5)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_6)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
end


*substitute matrix
capture program drop newcov_short
 program define newcov_short, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=vbeta_5[1,1]
 matrix V_corrected[2,2]=vbeta_4[1,1]
 matrix V_corrected[3,3]=vbeta_3[1,1]
 matrix V_corrected[4,4]=vbeta_2[1,1]
 matrix V_corrected[5,5]=vbeta_0[1,1]
 matrix V_corrected[6,6]=vbeta_p1[1,1]
 matrix V_corrected[7,7]=vbeta_p2[1,1]
 matrix V_corrected[8,8]=vbeta_p3[1,1]
 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

capture program drop newcov_short2
 program define newcov_short2, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=vbeta_5[1,1]
 matrix V_corrected[2,2]=vbeta_4[1,1]
 matrix V_corrected[3,3]=vbeta_3[1,1]
 matrix V_corrected[4,4]=vbeta_2[1,1]
 matrix V_corrected[5,5]=vbeta_0[1,1]
 matrix V_corrected[6,6]=vbeta_p1[1,1]
 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

capture program drop newcov
 program define newcov, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=vbeta_7[1,1]
 matrix V_corrected[2,2]=vbeta_6[1,1]
 matrix V_corrected[3,3]=vbeta_5[1,1]
 matrix V_corrected[4,4]=vbeta_4[1,1]
 matrix V_corrected[5,5]=vbeta_3[1,1]
 matrix V_corrected[6,6]=vbeta_2[1,1]
 matrix V_corrected[7,7]=vbeta_0[1,1]
 matrix V_corrected[8,8]=vbeta_p1[1,1]
 matrix V_corrected[9,9]=vbeta_p2[1,1]
 matrix V_corrected[10,10]=vbeta_p3[1,1]
 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

capture program drop newcov_naive_incumbency
 program define newcov_naive_incumbency, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=vbeta_7[1,1]
 matrix V_corrected[2,2]=vbeta_6[1,1]
 matrix V_corrected[3,3]=vbeta_5[1,1]
 matrix V_corrected[4,4]=vbeta_4[1,1]
 matrix V_corrected[5,5]=vbeta_3[1,1]
 matrix V_corrected[6,6]=vbeta_2[1,1]
 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: short
capture program drop wildcorrection_short
 program define wildcorrection_short, eclass 
  args Dep_var
  quietly  xi: reghdfe `Dep_var' $lagsleads_short  $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
	getvcov_short
	newcov_short
end


*program to get results: short2
capture program drop wildcorrection_short2
 program define wildcorrection_short2, eclass 
  args Dep_var
  quietly  xi: reghdfe `Dep_var' $lagsleads2  $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
	getvcov_short2
	newcov_short2
end

*for services regressions:
capture program drop wildcorrection_short3
 program define wildcorrection_short3, eclass 
  args Dep_var
  quietly  xi: reghdfe `Dep_var' $lagsleads_services  $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
	getvcov_short2
	newcov_short2
end



*program to get results: long
capture program drop wildcorrection
 program define wildcorrection, eclass 
  args Dep_var
  quietly  xi: reghdfe `Dep_var' $lagsleads  $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
	getvcov
	newcov
end


*program to get results: long
capture program drop wildcorrection_incumbency_pol1
 program define wildcorrection_incumbency_pol1, eclass 
  args Dep_var
	qui xi: reghdfe  `Dep_var'  $lagsleads pol1 $int_lagsleads_pol1 $controls_time_acuerdo i.year if mv_incparty<${optimal_1}/2 & mv_incparty>-${optimal_1}/2, a(inegi) vce(cluster estado)
	getvcov_naive_incumbency
	newcov_naive_incumbency
end

capture program drop wildcorrection_incumbency_pol2
 program define wildcorrection_incumbency_pol2, eclass 
  args Dep_var
	qui xi: reghdfe  `Dep_var'  $lagsleads pol2 $int_lagsleads_pol2 $controls_time_acuerdo i.year if mv_incparty<${optimal_2}/2 & mv_incparty>-${optimal_2}2, a(inegi) vce(cluster estado)
	getvcov_naive_incumbency
	newcov_naive_incumbency
end
*================================================================================================
*ABRAHAM AND SUN REGRESSIONS CORRECTION: 
capture program drop getvcov_as_short
 program define getvcov_as_short, eclass 
 boottest {lag_5_2018}  {lag_4_2017}  {lag_4_2018 } {lag_3_2016} {lag_3_2017} ///
  {lag_3_2018} {lag_2_2016} {lag_2_2017} {lag_2_2018} {date_0_2015} {date_0_2016} {date_0_2017} ///
  {date_0_2018} {lead_1_2015} {lead_1_2016} {lead_1_2017} {lead_2_2015} {lead_2_2016} /// 
  {lead_3_2015}, bootcluster(estado year) seed(5675) level(95) boottype(wild)   nograph 
 
 		matrix bounds_19=r(CI_1)
		matrix vbeta_19=((bounds_19[1,2]-bounds_19[1,1])/(2*1.96))^2
			matrix list vbeta_19
		matrix bounds_18=r(CI_2)
		matrix vbeta_18=((bounds_18[1,2]-bounds_18[1,1])/(2*1.96))^2
			matrix list vbeta_18
		matrix bounds_17=r(CI_3)
		matrix vbeta_17=((bounds_17[1,2]-bounds_17[1,1])/(2*1.96))^2
			matrix list vbeta_17
		matrix bounds_16=r(CI_4)
		matrix vbeta_16=((bounds_16[1,2]-bounds_16[1,1])/(2*1.96))^2
			matrix list vbeta_16
		matrix bounds_15=r(CI_5)
		matrix vbeta_15=((bounds_15[1,2]-bounds_15[1,1])/(2*1.96))^2
			matrix list vbeta_15
		matrix bounds_14=r(CI_6)
		matrix vbeta_14=((bounds_14[1,2]-bounds_14[1,1])/(2*1.96))^2
			matrix list vbeta_14			
		matrix bounds_13=r(CI_7)
		matrix vbeta_13=((bounds_13[1,2]-bounds_13[1,1])/(2*1.96))^2
			matrix list vbeta_13			
		matrix bounds_12=r(CI_8)
		matrix vbeta_12=((bounds_12[1,2]-bounds_12[1,1])/(2*1.96))^2
			matrix list vbeta_12	
		matrix bounds_11=r(CI_9)
		matrix vbeta_11=((bounds_11[1,2]-bounds_11[1,1])/(2*1.96))^2
			matrix list vbeta_11			
		matrix bounds_10=r(CI_10)
		matrix vbeta_10=((bounds_10[1,2]-bounds_10[1,1])/(2*1.96))^2
			matrix list vbeta_10		
		matrix bounds_9=r(CI_11)
		matrix vbeta_9=((bounds_9[1,2]-bounds_9[1,1])/(2*1.96))^2
			matrix list vbeta_9			
		matrix bounds_8=r(CI_12)
		matrix vbeta_8=((bounds_8[1,2]-bounds_8[1,1])/(2*1.86))^2
			matrix list vbeta_8			
		matrix bounds_7=r(CI_13)
		matrix vbeta_7=((bounds_7[1,2]-bounds_7[1,1])/(2*1.96))^2
			matrix list vbeta_7
		matrix bounds_6=r(CI_14)
		matrix vbeta_6=((bounds_6[1,2]-bounds_6[1,1])/(2*1.96))^2
			matrix list vbeta_6
		matrix bounds_5=r(CI_15)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_16)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_17)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_18)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
		matrix bounds_1=r(CI_19)
		matrix vbeta_1=((bounds_1[1,2]-bounds_1[1,1])/(2*1.96))^2
			matrix list vbeta_1					
end

capture program drop newcov_as_short
 program define newcov_as_short, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=vbeta_19[1,1]
 matrix V_corrected[2,2]=vbeta_18[1,1]
 matrix V_corrected[3,3]=vbeta_17[1,1]
 matrix V_corrected[4,4]=vbeta_16[1,1]
 matrix V_corrected[5,5]=vbeta_15[1,1]
 matrix V_corrected[6,6]=vbeta_14[1,1]
 matrix V_corrected[7,7]=vbeta_13[1,1]
 matrix V_corrected[8,8]=vbeta_12[1,1]
 matrix V_corrected[9,9]=vbeta_11[1,1]
 matrix V_corrected[10,10]=vbeta_10[1,1]
 matrix V_corrected[11,11]=vbeta_9[1,1]
 matrix V_corrected[12,12]=vbeta_8[1,1]
 matrix V_corrected[13,13]=vbeta_7[1,1]
 matrix V_corrected[14,14]=vbeta_6[1,1]
 matrix V_corrected[15,15]=vbeta_5[1,1]
 matrix V_corrected[16,16]=vbeta_4[1,1]
 matrix V_corrected[17,17]=vbeta_3[1,1]
 matrix V_corrected[18,18]=vbeta_2[1,1]
 matrix V_corrected[19,19]=vbeta_1[1,1]

 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: AS (2021)
capture program drop wildcorrection_as_short
 program define wildcorrection_as_short, eclass 
  args Dep_var
  quietly  xi: areg `Dep_var' $sat  $controls  i.year, a(inegi) vce(cluster estado)
	getvcov_as_short
	newcov_as_short
end

***LONG
capture program drop getvcov_as_long
 program define getvcov_as_long, eclass 
  boottest {lag_7_2018} {lag_6_2017} {lag_6_2018}  {lag_5_2016} {lag_5_2017}  {lag_5_2018} ///
  {lag_4_2015} {lag_4_2016} {lag_4_2017} {lag_4_2018} ///
  {lag_3_2015} {lag_3_2016} {lag_3_2017} {lag_3_2018} ///
  {lag_2_2015} {lag_2_2016} {lag_2_2017} {lag_2_2018} ///
  {date_0_2015} {date_0_2016} {date_0_2017} {date_0_2018} ///
  {lead_1_2015} {lead_1_2016} {lead_1_2017} {lead_2_2015} {lead_2_2016} /// 
  {lead_3_2015}, bootcluster(estado year) seed(5675) level(95) boottype(wild)   nograph 
  		matrix bounds_28=r(CI_1)
		matrix vbeta_28=((bounds_28[1,2]-bounds_28[1,1])/(2*1.96))^2
			matrix list vbeta_28
   		matrix bounds_27=r(CI_2)
		matrix vbeta_27=((bounds_27[1,2]-bounds_27[1,1])/(2*1.96))^2
			matrix list vbeta_27
   		matrix bounds_26=r(CI_3)
		matrix vbeta_26=((bounds_26[1,2]-bounds_26[1,1])/(2*1.96))^2
			matrix list vbeta_26
   		matrix bounds_25=r(CI_4)
		matrix vbeta_25=((bounds_25[1,2]-bounds_25[1,1])/(2*1.96))^2
			matrix list vbeta_25
   		matrix bounds_24=r(CI_5)
		matrix vbeta_24=((bounds_24[1,2]-bounds_24[1,1])/(2*1.96))^2
			matrix list vbeta_24
   		matrix bounds_23=r(CI_6)
		matrix vbeta_23=((bounds_23[1,2]-bounds_23[1,1])/(2*1.96))^2
			matrix list vbeta_23
  		matrix bounds_22=r(CI_7)
		matrix vbeta_22=((bounds_22[1,2]-bounds_22[1,1])/(2*1.96))^2
			matrix list vbeta_22
   		matrix bounds_21=r(CI_8)
		matrix vbeta_21=((bounds_21[1,2]-bounds_21[1,1])/(2*1.96))^2
			matrix list vbeta_21
   		matrix bounds_20=r(CI_9)
		matrix vbeta_20=((bounds_20[1,2]-bounds_20[1,1])/(2*1.96))^2
			matrix list vbeta_20 
 		matrix bounds_19=r(CI_10)
		matrix vbeta_19=((bounds_19[1,2]-bounds_19[1,1])/(2*1.96))^2
			matrix list vbeta_19
		matrix bounds_18=r(CI_11)
		matrix vbeta_18=((bounds_18[1,2]-bounds_18[1,1])/(2*1.96))^2
			matrix list vbeta_18
		matrix bounds_17=r(CI_12)
		matrix vbeta_17=((bounds_17[1,2]-bounds_17[1,1])/(2*1.96))^2
			matrix list vbeta_17
		matrix bounds_16=r(CI_13)
		matrix vbeta_16=((bounds_16[1,2]-bounds_16[1,1])/(2*1.96))^2
			matrix list vbeta_16
		matrix bounds_15=r(CI_14)
		matrix vbeta_15=((bounds_15[1,2]-bounds_15[1,1])/(2*1.96))^2
			matrix list vbeta_15
		matrix bounds_14=r(CI_15)
		matrix vbeta_14=((bounds_14[1,2]-bounds_14[1,1])/(2*1.96))^2
			matrix list vbeta_14			
		matrix bounds_13=r(CI_16)
		matrix vbeta_13=((bounds_13[1,2]-bounds_13[1,1])/(2*1.96))^2
			matrix list vbeta_13			
		matrix bounds_12=r(CI_17)
		matrix vbeta_12=((bounds_12[1,2]-bounds_12[1,1])/(2*1.96))^2
			matrix list vbeta_12	
		matrix bounds_11=r(CI_18)
		matrix vbeta_11=((bounds_11[1,2]-bounds_11[1,1])/(2*1.96))^2
			matrix list vbeta_11			
		matrix bounds_10=r(CI_19)
		matrix vbeta_10=((bounds_10[1,2]-bounds_10[1,1])/(2*1.96))^2
			matrix list vbeta_10		
		matrix bounds_9=r(CI_20)
		matrix vbeta_9=((bounds_9[1,2]-bounds_9[1,1])/(2*1.96))^2
			matrix list vbeta_9			
		matrix bounds_8=r(CI_21)
		matrix vbeta_8=((bounds_8[1,2]-bounds_8[1,1])/(2*1.86))^2
			matrix list vbeta_8			
		matrix bounds_7=r(CI_22)
		matrix vbeta_7=((bounds_7[1,2]-bounds_7[1,1])/(2*1.96))^2
			matrix list vbeta_7
		matrix bounds_6=r(CI_23)
		matrix vbeta_6=((bounds_6[1,2]-bounds_6[1,1])/(2*1.96))^2
			matrix list vbeta_6
		matrix bounds_5=r(CI_24)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_25)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_26)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_27)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
		matrix bounds_1=r(CI_28)
		matrix vbeta_1=((bounds_1[1,2]-bounds_1[1,1])/(2*1.96))^2
			matrix list vbeta_1					
end

capture program drop newcov_as_long
 program define newcov_as_long, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=vbeta_28[1,1]
 matrix V_corrected[2,2]=vbeta_27[1,1]
 matrix V_corrected[3,3]=vbeta_26[1,1]
 matrix V_corrected[4,4]=vbeta_25[1,1]
 matrix V_corrected[5,5]=vbeta_24[1,1]
 matrix V_corrected[6,6]=vbeta_23[1,1]
 matrix V_corrected[7,7]=vbeta_22[1,1]
 matrix V_corrected[8,8]=vbeta_21[1,1]
 matrix V_corrected[9,9]=vbeta_20[1,1]
 matrix V_corrected[10,10]=vbeta_19[1,1]
 matrix V_corrected[11,11]=vbeta_18[1,1]
 matrix V_corrected[12,12]=vbeta_17[1,1]
 matrix V_corrected[13,13]=vbeta_16[1,1]
 matrix V_corrected[14,14]=vbeta_15[1,1]
 matrix V_corrected[15,15]=vbeta_14[1,1]
 matrix V_corrected[16,16]=vbeta_13[1,1]
 matrix V_corrected[17,17]=vbeta_12[1,1]
 matrix V_corrected[18,18]=vbeta_11[1,1]
 matrix V_corrected[19,19]=vbeta_10[1,1]
 matrix V_corrected[20,20]=vbeta_9[1,1]
 matrix V_corrected[21,21]=vbeta_8[1,1]
 matrix V_corrected[22,22]=vbeta_7[1,1]
 matrix V_corrected[23,23]=vbeta_6[1,1]
 matrix V_corrected[24,24]=vbeta_5[1,1]
 matrix V_corrected[25,25]=vbeta_4[1,1]
 matrix V_corrected[26,26]=vbeta_3[1,1]
 matrix V_corrected[27,27]=vbeta_2[1,1]
 matrix V_corrected[28,28]=vbeta_1[1,1]

 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: AS (2021)
capture program drop wildcorrection_as_long
 program define wildcorrection_as_long, eclass 
  args Dep_var
xi: reghdfe  `Dep_var'  $saturated $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
  *quietly  xi: areg `Dep_var' $saturated $controls  i.year, a(inegi) vce(cluster estado)
	getvcov_as_long
	newcov_as_long
end
 

***NEW ESTIMATES
capture program drop getvcov_as_new
 program define getvcov_as_new, eclass 
 boottest {lag_5_2018}  {lag_4_2017} {lag_3_2016} ///
  {lag_3_2018} {lag_2_2017} {date_0_2015} {date_0_2017} ///
  {date_0_2018} {lead_1_2016} {lead_1_2017} {lead_2_2015} {lead_2_2016} /// 
  {lead_3_2015}, bootcluster(estado year) seed(5675) level(95) boottype(wild)   nograph 
 
		
		matrix bounds_13=r(CI_1)
		matrix vbeta_13=((bounds_13[1,2]-bounds_13[1,1])/(2*1.96))^2
			matrix list vbeta_13			
		matrix bounds_12=r(CI_2)
		matrix vbeta_12=((bounds_12[1,2]-bounds_12[1,1])/(2*1.96))^2
			matrix list vbeta_12	
		matrix bounds_11=r(CI_3)
		matrix vbeta_11=((bounds_11[1,2]-bounds_11[1,1])/(2*1.96))^2
			matrix list vbeta_11			
		matrix bounds_10=r(CI_4)
		matrix vbeta_10=((bounds_10[1,2]-bounds_10[1,1])/(2*1.96))^2
			matrix list vbeta_10		
		matrix bounds_9=r(CI_5)
		matrix vbeta_9=((bounds_9[1,2]-bounds_9[1,1])/(2*1.96))^2
			matrix list vbeta_9			
		matrix bounds_8=r(CI_6)
		matrix vbeta_8=((bounds_8[1,2]-bounds_8[1,1])/(2*1.86))^2
			matrix list vbeta_8			
		matrix bounds_7=r(CI_7)
		matrix vbeta_7=((bounds_7[1,2]-bounds_7[1,1])/(2*1.96))^2
			matrix list vbeta_7
		matrix bounds_6=r(CI_8)
		matrix vbeta_6=((bounds_6[1,2]-bounds_6[1,1])/(2*1.96))^2
			matrix list vbeta_6
		matrix bounds_5=r(CI_9)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_10)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_11)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_12)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
		matrix bounds_1=r(CI_13)
		matrix vbeta_1=((bounds_1[1,2]-bounds_1[1,1])/(2*1.96))^2
			matrix list vbeta_1					
end

capture program drop newcov_as_new
 program define newcov_as_new, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=vbeta_13[1,1]
 matrix V_corrected[2,2]=vbeta_12[1,1]
 matrix V_corrected[3,3]=vbeta_11[1,1]
 matrix V_corrected[4,4]=vbeta_10[1,1]
 matrix V_corrected[5,5]=vbeta_9[1,1]
 matrix V_corrected[6,6]=vbeta_8[1,1]
 matrix V_corrected[7,7]=vbeta_7[1,1]
 matrix V_corrected[8,8]=vbeta_6[1,1]
 matrix V_corrected[9,9]=vbeta_5[1,1]
 matrix V_corrected[10,10]=vbeta_4[1,1]
 matrix V_corrected[11,11]=vbeta_3[1,1]
 matrix V_corrected[12,12]=vbeta_2[1,1]
 matrix V_corrected[13,13]=vbeta_1[1,1]


 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: AS (2021)
capture program drop wildcorrection_as_new
 program define wildcorrection_as_new, eclass 
  args Dep_var
  quietly  xi: areg `Dep_var' $sat $controls  i.year, a(inegi) vce(cluster estado)
	getvcov_as_new
	newcov_as_new
end


***NEW ESTIMATES 2
capture program drop getvcov_as_new2
 program define getvcov_as_new2, eclass 
 boottest {lag_6_2018} ///
 {lag_5_2017} {lag_5_2018} ///
 {lag_4_2016}  {lag_4_2017} {lag_4_2018} ///
 {lag_3_2015} {lag_3_2016} {lag_3_2017} {lag_3_2018} ///
 {lag_2_2015} {lag_2_2016} {lag_2_2017} {lag_2_2018} ///
 {date_0_2015} {date_0_2016} {date_0_2017} {date_0_2018} ///
 {lead_1_2015}  {lead_1_2016} {lead_1_2017} ///
 {lead_2_2015} {lead_2_2016} /// 
 {lead_3_2015}, bootcluster(estado year) seed(5675) level(95) boottype(wild)   nograph 
 
	  	matrix bounds_24=r(CI_1)
		matrix vbeta_24=((bounds_24[1,2]-bounds_24[1,1])/(2*1.96))^2
			matrix list vbeta_24
   		matrix bounds_23=r(CI_2)
		matrix vbeta_23=((bounds_23[1,2]-bounds_23[1,1])/(2*1.96))^2
			matrix list vbeta_23
  		matrix bounds_22=r(CI_3)
		matrix vbeta_22=((bounds_22[1,2]-bounds_22[1,1])/(2*1.96))^2
			matrix list vbeta_22
   		matrix bounds_21=r(CI_4)
		matrix vbeta_21=((bounds_21[1,2]-bounds_21[1,1])/(2*1.96))^2
			matrix list vbeta_21
   		matrix bounds_20=r(CI_5)
		matrix vbeta_20=((bounds_20[1,2]-bounds_20[1,1])/(2*1.96))^2
			matrix list vbeta_20 
 		matrix bounds_19=r(CI_6)
		matrix vbeta_19=((bounds_19[1,2]-bounds_19[1,1])/(2*1.96))^2
			matrix list vbeta_19
		matrix bounds_18=r(CI_7)
		matrix vbeta_18=((bounds_18[1,2]-bounds_18[1,1])/(2*1.96))^2
			matrix list vbeta_18
		matrix bounds_17=r(CI_8)
		matrix vbeta_17=((bounds_17[1,2]-bounds_17[1,1])/(2*1.96))^2
			matrix list vbeta_17
		matrix bounds_16=r(CI_9)
		matrix vbeta_16=((bounds_16[1,2]-bounds_16[1,1])/(2*1.96))^2
			matrix list vbeta_16
		matrix bounds_15=r(CI_10)
		matrix vbeta_15=((bounds_15[1,2]-bounds_15[1,1])/(2*1.96))^2
			matrix list vbeta_15
		matrix bounds_14=r(CI_11)
		matrix vbeta_14=((bounds_14[1,2]-bounds_14[1,1])/(2*1.96))^2
			matrix list vbeta_14			
		matrix bounds_13=r(CI_12)
		matrix vbeta_13=((bounds_13[1,2]-bounds_13[1,1])/(2*1.96))^2
			matrix list vbeta_13			
		matrix bounds_12=r(CI_13)
		matrix vbeta_12=((bounds_12[1,2]-bounds_12[1,1])/(2*1.96))^2
			matrix list vbeta_12	
		matrix bounds_11=r(CI_14)
		matrix vbeta_11=((bounds_11[1,2]-bounds_11[1,1])/(2*1.96))^2
			matrix list vbeta_11			
		matrix bounds_10=r(CI_15)
		matrix vbeta_10=((bounds_10[1,2]-bounds_10[1,1])/(2*1.96))^2
			matrix list vbeta_10		
		matrix bounds_9=r(CI_16)
		matrix vbeta_9=((bounds_9[1,2]-bounds_9[1,1])/(2*1.96))^2
			matrix list vbeta_9			
		matrix bounds_8=r(CI_17)
		matrix vbeta_8=((bounds_8[1,2]-bounds_8[1,1])/(2*1.86))^2
			matrix list vbeta_8			
		matrix bounds_7=r(CI_18)
		matrix vbeta_7=((bounds_7[1,2]-bounds_7[1,1])/(2*1.96))^2
			matrix list vbeta_7
		matrix bounds_6=r(CI_19)
		matrix vbeta_6=((bounds_6[1,2]-bounds_6[1,1])/(2*1.96))^2
			matrix list vbeta_6
		matrix bounds_5=r(CI_20)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_21)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_22)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_23)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
		matrix bounds_1=r(CI_24)
		matrix vbeta_1=((bounds_1[1,2]-bounds_1[1,1])/(2*1.96))^2
			matrix list vbeta_1					
			
end

capture program drop newcov_as_new2
 program define newcov_as_new2, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
	 matrix V_corrected[1,1]=vbeta_24[1,1]
	 matrix V_corrected[2,2]=vbeta_23[1,1]
	 matrix V_corrected[3,3]=vbeta_22[1,1]
	 matrix V_corrected[4,4]=vbeta_22[1,1]
	 matrix V_corrected[5,5]=vbeta_20[1,1]
	 matrix V_corrected[6,6]=vbeta_19[1,1]
	 matrix V_corrected[7,7]=vbeta_18[1,1]
	 matrix V_corrected[8,8]=vbeta_17[1,1]
	 matrix V_corrected[9,9]=vbeta_16[1,1]
	 matrix V_corrected[10,10]=vbeta_15[1,1]
	 matrix V_corrected[11,11]=vbeta_14[1,1]
	 matrix V_corrected[12,12]=vbeta_13[1,1]
	 matrix V_corrected[13,13]=vbeta_12[1,1]
	 matrix V_corrected[14,14]=vbeta_11[1,1]
	 matrix V_corrected[15,15]=vbeta_10[1,1]
	 matrix V_corrected[16,16]=vbeta_9[1,1]
	 matrix V_corrected[17,17]=vbeta_8[1,1]
	 matrix V_corrected[18,18]=vbeta_7[1,1]
	 matrix V_corrected[19,19]=vbeta_6[1,1]
	 matrix V_corrected[20,20]=vbeta_5[1,1]
	 matrix V_corrected[21,21]=vbeta_4[1,1]
	 matrix V_corrected[22,22]=vbeta_3[1,1]
	 matrix V_corrected[23,23]=vbeta_2[1,1]
	 matrix V_corrected[24,24]=vbeta_1[1,1]



 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: AS (2021)
capture program drop wildcorrection_as_new2
 program define wildcorrection_as_new2, eclass 
  args Dep_var
  quietly  xi: reghdfe `Dep_var' $saturated $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
	getvcov_as_new2
	newcov_as_new2
end



***LONG
capture program drop getvcov_as_long2
 program define getvcov_as_long2, eclass 
  boottest {lag_6_2018} ///
  {lag_5_2016} {lag_5_2017}  {lag_5_2018} ///
  {lag_4_2015} {lag_4_2016} {lag_4_2017} {lag_4_2018} ///
 {lag_3_2016} {lag_3_2017} {lag_3_2018} ///
  {lag_2_2015}  {lag_2_2017} {lag_2_2018} ///
  {lag_1_2015} {lag_1_2016} {lag_1_2017} {lag_1_2018} ///
  {lead_1_2015} {lead_1_2016} {lead_1_2017} ///
  {lead_2_2015} {lead_2_2016} /// 
  {lead_3_2015}, bootcluster(estado) seed(5675) level(95) boottype(wild)   nograph 
 	  	matrix bounds_24=r(CI_1)
		matrix vbeta_24=((bounds_24[1,2]-bounds_24[1,1])/(2*1.96))^2
			matrix list vbeta_24
   		matrix bounds_23=r(CI_2)
		matrix vbeta_23=((bounds_23[1,2]-bounds_23[1,1])/(2*1.96))^2
			matrix list vbeta_23
  		matrix bounds_22=r(CI_3)
		matrix vbeta_22=((bounds_22[1,2]-bounds_22[1,1])/(2*1.96))^2
			matrix list vbeta_22
   		matrix bounds_21=r(CI_4)
		matrix vbeta_21=((bounds_21[1,2]-bounds_21[1,1])/(2*1.96))^2
			matrix list vbeta_21
   		matrix bounds_20=r(CI_5)
		matrix vbeta_20=((bounds_20[1,2]-bounds_20[1,1])/(2*1.96))^2
			matrix list vbeta_20 
 		matrix bounds_19=r(CI_6)
		matrix vbeta_19=((bounds_19[1,2]-bounds_19[1,1])/(2*1.96))^2
			matrix list vbeta_19
		matrix bounds_18=r(CI_7)
		matrix vbeta_18=((bounds_18[1,2]-bounds_18[1,1])/(2*1.96))^2
			matrix list vbeta_18
		matrix bounds_17=r(CI_8)
		matrix vbeta_17=((bounds_17[1,2]-bounds_17[1,1])/(2*1.96))^2
			matrix list vbeta_17
		matrix bounds_16=r(CI_9)
		matrix vbeta_16=((bounds_16[1,2]-bounds_16[1,1])/(2*1.96))^2
			matrix list vbeta_16
		matrix bounds_15=r(CI_10)
		matrix vbeta_15=((bounds_15[1,2]-bounds_15[1,1])/(2*1.96))^2
			matrix list vbeta_15
		matrix bounds_14=r(CI_11)
		matrix vbeta_14=((bounds_14[1,2]-bounds_14[1,1])/(2*1.96))^2
			matrix list vbeta_14			
		matrix bounds_13=r(CI_12)
		matrix vbeta_13=((bounds_13[1,2]-bounds_13[1,1])/(2*1.96))^2
			matrix list vbeta_13			
		matrix bounds_12=r(CI_13)
		matrix vbeta_12=((bounds_12[1,2]-bounds_12[1,1])/(2*1.96))^2
			matrix list vbeta_12	
		matrix bounds_11=r(CI_14)
		matrix vbeta_11=((bounds_11[1,2]-bounds_11[1,1])/(2*1.96))^2
			matrix list vbeta_11			
		matrix bounds_10=r(CI_15)
		matrix vbeta_10=((bounds_10[1,2]-bounds_10[1,1])/(2*1.96))^2
			matrix list vbeta_10		
		matrix bounds_9=r(CI_16)
		matrix vbeta_9=((bounds_9[1,2]-bounds_9[1,1])/(2*1.96))^2
			matrix list vbeta_9			
		matrix bounds_8=r(CI_17)
		matrix vbeta_8=((bounds_8[1,2]-bounds_8[1,1])/(2*1.86))^2
			matrix list vbeta_8			
		matrix bounds_7=r(CI_18)
		matrix vbeta_7=((bounds_7[1,2]-bounds_7[1,1])/(2*1.96))^2
			matrix list vbeta_7
		matrix bounds_6=r(CI_19)
		matrix vbeta_6=((bounds_6[1,2]-bounds_6[1,1])/(2*1.96))^2
			matrix list vbeta_6
		matrix bounds_5=r(CI_20)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_21)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_22)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_23)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
		matrix bounds_1=r(CI_24)
		matrix vbeta_1=((bounds_1[1,2]-bounds_1[1,1])/(2*1.96))^2
			matrix list vbeta_1	
end

capture program drop newcov_as_long2
 program define newcov_as_long2, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
	 matrix V_corrected[1,1]=vbeta_24[1,1]
	 matrix V_corrected[2,2]=vbeta_23[1,1]
	 matrix V_corrected[3,3]=vbeta_22[1,1]
	 matrix V_corrected[4,4]=vbeta_22[1,1]
	 matrix V_corrected[5,5]=vbeta_20[1,1]
	 matrix V_corrected[6,6]=vbeta_19[1,1]
	 matrix V_corrected[7,7]=vbeta_18[1,1]
	 matrix V_corrected[8,8]=vbeta_17[1,1]
	 matrix V_corrected[9,9]=vbeta_16[1,1]
	 matrix V_corrected[10,10]=vbeta_15[1,1]
	 matrix V_corrected[11,11]=vbeta_14[1,1]
	 matrix V_corrected[12,12]=vbeta_13[1,1]
	 matrix V_corrected[13,13]=vbeta_12[1,1]
	 matrix V_corrected[14,14]=vbeta_11[1,1]
	 matrix V_corrected[15,15]=vbeta_10[1,1]
	 matrix V_corrected[16,16]=vbeta_9[1,1]
	 matrix V_corrected[17,17]=vbeta_8[1,1]
	 matrix V_corrected[18,18]=vbeta_7[1,1]
	 matrix V_corrected[19,19]=vbeta_6[1,1]
	 matrix V_corrected[20,20]=vbeta_5[1,1]
	 matrix V_corrected[21,21]=vbeta_4[1,1]
	 matrix V_corrected[22,22]=vbeta_3[1,1]
	 matrix V_corrected[23,23]=vbeta_2[1,1]
	 matrix V_corrected[24,24]=vbeta_1[1,1]

 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: AS (2021)
capture program drop wildcorrection_as_long2
 program define wildcorrection_as_long2, eclass 
  args Dep_var
  quietly xi: areg  `Dep_var'  $saturated $controls_time_acuerdo2 i.year, a(inegi) vce(cluster estado) 
	getvcov_as_long2
	newcov_as_long2
end


***Trim
capture program drop getvcov_as_trim
 program define getvcov_as_trim, eclass 
 boottest {lag_4_2015} {lag_4_2016}  {lag_4_2017} {lag_4_2018} ///
 {lag_3_2015} {lag_3_2016} {lag_3_2017} {lag_3_2018} ///
 {lag_2_2015} {lag_2_2016} {lag_2_2017} {lag_2_2018} ///
 {date_0_2015} {date_0_2016} {date_0_2017} {date_0_2018} ///
 {lead_1_2015}  {lead_1_2016} {lead_1_2017} ///
 {lead_2_2015} {lead_2_2016} /// 
 {lead_3_2015}, bootcluster(estado year) seed(5675) level(95) boottype(wild)   nograph 
 

  		matrix bounds_22=r(CI_1)
		matrix vbeta_22=((bounds_22[1,2]-bounds_22[1,1])/(2*1.96))^2
			matrix list vbeta_22
   		matrix bounds_21=r(CI_2)
		matrix vbeta_21=((bounds_21[1,2]-bounds_21[1,1])/(2*1.96))^2
			matrix list vbeta_21
   		matrix bounds_20=r(CI_3)
		matrix vbeta_20=((bounds_20[1,2]-bounds_20[1,1])/(2*1.96))^2
			matrix list vbeta_20 
 		matrix bounds_19=r(CI_4)
		matrix vbeta_19=((bounds_19[1,2]-bounds_19[1,1])/(2*1.96))^2
			matrix list vbeta_19
		matrix bounds_18=r(CI_5)
		matrix vbeta_18=((bounds_18[1,2]-bounds_18[1,1])/(2*1.96))^2
			matrix list vbeta_18
		matrix bounds_17=r(CI_6)
		matrix vbeta_17=((bounds_17[1,2]-bounds_17[1,1])/(2*1.96))^2
			matrix list vbeta_17
		matrix bounds_16=r(CI_7)
		matrix vbeta_16=((bounds_16[1,2]-bounds_16[1,1])/(2*1.96))^2
			matrix list vbeta_16
		matrix bounds_15=r(CI_8)
		matrix vbeta_15=((bounds_15[1,2]-bounds_15[1,1])/(2*1.96))^2
			matrix list vbeta_15
		matrix bounds_14=r(CI_9)
		matrix vbeta_14=((bounds_14[1,2]-bounds_14[1,1])/(2*1.96))^2
			matrix list vbeta_14			
		matrix bounds_13=r(CI_10)
		matrix vbeta_13=((bounds_13[1,2]-bounds_13[1,1])/(2*1.96))^2
			matrix list vbeta_13			
		matrix bounds_12=r(CI_11)
		matrix vbeta_12=((bounds_12[1,2]-bounds_12[1,1])/(2*1.96))^2
			matrix list vbeta_12	
		matrix bounds_11=r(CI_12)
		matrix vbeta_11=((bounds_11[1,2]-bounds_11[1,1])/(2*1.96))^2
			matrix list vbeta_11			
		matrix bounds_10=r(CI_13)
		matrix vbeta_10=((bounds_10[1,2]-bounds_10[1,1])/(2*1.96))^2
			matrix list vbeta_10		
		matrix bounds_9=r(CI_14)
		matrix vbeta_9=((bounds_9[1,2]-bounds_9[1,1])/(2*1.96))^2
			matrix list vbeta_9			
		matrix bounds_8=r(CI_15)
		matrix vbeta_8=((bounds_8[1,2]-bounds_8[1,1])/(2*1.86))^2
			matrix list vbeta_8			
		matrix bounds_7=r(CI_16)
		matrix vbeta_7=((bounds_7[1,2]-bounds_7[1,1])/(2*1.96))^2
			matrix list vbeta_7
		matrix bounds_6=r(CI_17)
		matrix vbeta_6=((bounds_6[1,2]-bounds_6[1,1])/(2*1.96))^2
			matrix list vbeta_6
		matrix bounds_5=r(CI_18)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_19)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_20)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_21)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
		matrix bounds_1=r(CI_22)
		matrix vbeta_1=((bounds_1[1,2]-bounds_1[1,1])/(2*1.96))^2
			matrix list vbeta_1					
			
end

capture program drop newcov_as_trim
 program define newcov_as_trim, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
	 matrix V_corrected[1,1]=vbeta_22[1,1]
	 matrix V_corrected[2,2]=vbeta_21[1,1]
	 matrix V_corrected[3,3]=vbeta_20[1,1]
	 matrix V_corrected[4,4]=vbeta_19[1,1]
	 matrix V_corrected[5,5]=vbeta_18[1,1]
	 matrix V_corrected[6,6]=vbeta_17[1,1]
	 matrix V_corrected[7,7]=vbeta_16[1,1]
	 matrix V_corrected[8,8]=vbeta_15[1,1]
	 matrix V_corrected[9,9]=vbeta_14[1,1]
	 matrix V_corrected[10,10]=vbeta_13[1,1]
	 matrix V_corrected[11,11]=vbeta_12[1,1]
	 matrix V_corrected[12,12]=vbeta_11[1,1]
	 matrix V_corrected[13,13]=vbeta_10[1,1]
	 matrix V_corrected[14,14]=vbeta_9[1,1]
	 matrix V_corrected[15,15]=vbeta_8[1,1]
	 matrix V_corrected[16,16]=vbeta_7[1,1]
	 matrix V_corrected[17,17]=vbeta_6[1,1]
	 matrix V_corrected[18,18]=vbeta_5[1,1]
	 matrix V_corrected[19,19]=vbeta_4[1,1]
	 matrix V_corrected[20,20]=vbeta_3[1,1]
	 matrix V_corrected[21,21]=vbeta_2[1,1]
	 matrix V_corrected[22,22]=vbeta_1[1,1]



 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: AS (2021)
capture program drop wildcorrection_as_trim
 program define wildcorrection_as_trim, eclass 
  args Dep_var
 quietly xi: reghdfe  `Dep_var'  $trim $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	getvcov_as_trim
	newcov_as_trim
end

***Trim for comparison with federal agreements 
capture program drop getvcov_as_trim2
 program define getvcov_as_trim2, eclass 
 boottest {lag_4_2018} ///
 {lag_3_2017} ///
  {lag_2_2016} {lag_2_2018} ///
 {date_0_2016}  ///
 {lead_1_2015}   {lead_1_2017} ///
 {lead_3_2015}, bootcluster(estado year) seed(5675) level(95) boottype(wild)   nograph 
 

  		matrix bounds_22=r(CI_1)
		matrix vbeta_22=((bounds_22[1,2]-bounds_22[1,1])/(2*1.96))^2
			matrix list vbeta_22
   		matrix bounds_21=r(CI_2)
		matrix vbeta_21=((bounds_21[1,2]-bounds_21[1,1])/(2*1.96))^2
			matrix list vbeta_21
   		matrix bounds_20=r(CI_3)
		matrix vbeta_20=((bounds_20[1,2]-bounds_20[1,1])/(2*1.96))^2
			matrix list vbeta_20 
 		matrix bounds_19=r(CI_4)
		matrix vbeta_19=((bounds_19[1,2]-bounds_19[1,1])/(2*1.96))^2
			matrix list vbeta_19
		matrix bounds_18=r(CI_5)
		matrix vbeta_18=((bounds_18[1,2]-bounds_18[1,1])/(2*1.96))^2
			matrix list vbeta_18
		matrix bounds_17=r(CI_6)
		matrix vbeta_17=((bounds_17[1,2]-bounds_17[1,1])/(2*1.96))^2
			matrix list vbeta_17
		matrix bounds_16=r(CI_7)
		matrix vbeta_16=((bounds_16[1,2]-bounds_16[1,1])/(2*1.96))^2
			matrix list vbeta_16
		matrix bounds_15=r(CI_8)
		matrix vbeta_15=((bounds_15[1,2]-bounds_15[1,1])/(2*1.96))^2
			matrix list vbeta_15

end

capture program drop newcov_as_trim2
 program define newcov_as_trim2, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
	 matrix V_corrected[1,1]=vbeta_22[1,1]
	 matrix V_corrected[2,2]=0.000001
	 matrix V_corrected[3,3]=vbeta_20[1,1]
	 matrix V_corrected[4,4]=vbeta_19[1,1]
	 matrix V_corrected[5,5]=vbeta_18[1,1]
	 matrix V_corrected[6,6]=vbeta_17[1,1]
	 matrix V_corrected[7,7]=0.0000001
	 matrix V_corrected[8,8]=vbeta_15[1,1]
 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

capture program drop newcov_as_trim3
 program define newcov_as_trim3, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
	 matrix V_corrected[1,1]=vbeta_22[1,1]
	 matrix V_corrected[2,2]=0.000001
	 matrix V_corrected[3,3]=vbeta_20[1,1]
	 matrix V_corrected[4,4]=vbeta_19[1,1]
	 matrix V_corrected[5,5]=vbeta_18[1,1]
	 matrix V_corrected[6,6]=vbeta_17[1,1]
	 matrix V_corrected[7,7]=0.0000001
	 matrix V_corrected[8,8]=vbeta_15[1,1]
 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: AS (2021)
capture program drop wildcorrection_as_trim2
 program define wildcorrection_as_trim2, eclass 
  args Dep_var
 quietly xi: reghdfe  `Dep_var'  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	getvcov_as_trim2
	newcov_as_trim2
end

capture program drop wildcorrection_as_trim3
 program define wildcorrection_as_trim3, eclass 
  args Dep_var
 quietly xi: reghdfe  `Dep_var'  $saturated3 $controls_time_acuerdo i.year, a(inegi) vce(cluster estado)
	getvcov_as_trim2
	newcov_as_trim3
end

***MOTIVES
capture program drop getvcov_as_motives
 program define getvcov_as_motives, eclass 
  boottest {lag_7_2018} ///
 {lag_6_2017} {lag_6_2018} ///
 {lag_5_2016} {lag_5_2017}  {lag_5_2018} ///
  {lag_4_2015} {lag_4_2016} {lag_4_2017} {lag_4_2018} ///
  {lag_3_2015} {lag_3_2016} {lag_3_2017}  ///
  {lag_2_2015} {lag_2_2016}  {lag_2_2018} ///
   {date_0_2016}  ///
  {lead_1_2015} {lead_1_2017} ///
  {lead_3_2015}, bootcluster(estado) seed(5675) level(95) boottype(wild)   nograph 
  		
		matrix bounds_28=r(CI_1)
		matrix vbeta_28=((bounds_28[1,2]-bounds_28[1,1])/(2*1.96))^2
			matrix list vbeta_28
   		matrix bounds_27=r(CI_2)
		matrix vbeta_27=((bounds_27[1,2]-bounds_27[1,1])/(2*1.96))^2
			matrix list vbeta_27
   		matrix bounds_26=r(CI_3)
		matrix vbeta_26=((bounds_26[1,2]-bounds_26[1,1])/(2*1.96))^2
			matrix list vbeta_26
   		matrix bounds_25=r(CI_4)
		matrix vbeta_25=((bounds_25[1,2]-bounds_25[1,1])/(2*1.96))^2
			matrix list vbeta_25
   		matrix bounds_24=r(CI_5)
		matrix vbeta_24=((bounds_24[1,2]-bounds_24[1,1])/(2*1.96))^2
			matrix list vbeta_24
   		matrix bounds_23=r(CI_6)
		matrix vbeta_23=((bounds_23[1,2]-bounds_23[1,1])/(2*1.96))^2
			matrix list vbeta_23
  		matrix bounds_22=r(CI_7)
		matrix vbeta_22=((bounds_22[1,2]-bounds_22[1,1])/(2*1.96))^2
			matrix list vbeta_22
   		matrix bounds_21=r(CI_8)
		matrix vbeta_21=((bounds_21[1,2]-bounds_21[1,1])/(2*1.96))^2
			matrix list vbeta_21
   		matrix bounds_20=r(CI_9)
		matrix vbeta_20=((bounds_20[1,2]-bounds_20[1,1])/(2*1.96))^2
			matrix list vbeta_20 
 		matrix bounds_19=r(CI_10)
		matrix vbeta_19=((bounds_19[1,2]-bounds_19[1,1])/(2*1.96))^2
			matrix list vbeta_19
		matrix bounds_18=r(CI_11)
		matrix vbeta_18=((bounds_18[1,2]-bounds_18[1,1])/(2*1.96))^2
			matrix list vbeta_18
		matrix bounds_17=r(CI_12)
		matrix vbeta_17=((bounds_17[1,2]-bounds_17[1,1])/(2*1.96))^2
			matrix list vbeta_17
		matrix bounds_16=r(CI_13)
		matrix vbeta_16=((bounds_16[1,2]-bounds_16[1,1])/(2*1.96))^2
			matrix list vbeta_16
		matrix bounds_15=r(CI_14)
		matrix vbeta_15=((bounds_15[1,2]-bounds_15[1,1])/(2*1.96))^2
			matrix list vbeta_15
		matrix bounds_14=r(CI_15)
		matrix vbeta_14=((bounds_14[1,2]-bounds_14[1,1])/(2*1.96))^2
			matrix list vbeta_14			
		matrix bounds_13=r(CI_16)
		matrix vbeta_13=((bounds_13[1,2]-bounds_13[1,1])/(2*1.96))^2
			matrix list vbeta_13			
		matrix bounds_12=r(CI_17)
		matrix vbeta_12=((bounds_12[1,2]-bounds_12[1,1])/(2*1.96))^2
			matrix list vbeta_12	
		matrix bounds_11=r(CI_18)
		matrix vbeta_11=((bounds_11[1,2]-bounds_11[1,1])/(2*1.96))^2
			matrix list vbeta_11			
		matrix bounds_10=r(CI_19)
		matrix vbeta_10=((bounds_10[1,2]-bounds_10[1,1])/(2*1.96))^2
			matrix list vbeta_10		
		matrix bounds_9=r(CI_20)
		matrix vbeta_9=((bounds_9[1,2]-bounds_9[1,1])/(2*1.96))^2
			matrix list vbeta_9			
end

capture program drop newcov_as_motives
 program define newcov_as_motives, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=vbeta_28[1,1]
 matrix V_corrected[2,2]=vbeta_27[1,1]
 matrix V_corrected[3,3]=vbeta_26[1,1]
 matrix V_corrected[4,4]=vbeta_25[1,1]
 matrix V_corrected[5,5]=vbeta_24[1,1]
 matrix V_corrected[6,6]=vbeta_23[1,1]
 matrix V_corrected[7,7]=vbeta_22[1,1]
 matrix V_corrected[8,8]=vbeta_21[1,1]
 matrix V_corrected[9,9]=vbeta_20[1,1]
 matrix V_corrected[10,10]=vbeta_19[1,1]
 matrix V_corrected[11,11]=vbeta_18[1,1]
 matrix V_corrected[12,12]=vbeta_17[1,1]
 matrix V_corrected[13,13]=vbeta_16[1,1]
 matrix V_corrected[14,14]=vbeta_15[1,1]
 matrix V_corrected[15,15]=vbeta_14[1,1]
 matrix V_corrected[16,16]=vbeta_13[1,1]
 matrix V_corrected[17,17]=vbeta_12[1,1]
 matrix V_corrected[18,18]=vbeta_11[1,1]
 matrix V_corrected[19,19]=vbeta_10[1,1]
 matrix V_corrected[20,20]=vbeta_9[1,1]

 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: AS (2021)
capture program drop wildcorrection_as_motives
 program define wildcorrection_as_motives, eclass 
  args Dep_var
  quietly  xi: areg `Dep_var' $sat_motives $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
	getvcov_as_motives
	newcov_as_motives
end
 

 **HOMICIDES REGRESSIONS
 ***LONG
capture program drop getvcov_as_homicides
 program define getvcov_as_homicides, eclass 
  boottest {lag_7_2017} {lag_7_2018} ///
  {lag_6_2016} {lag_6_2017} {lag_6_2018} ///
  {lag_5_2015} {lag_5_2016} {lag_5_2017} {lag_5_2018} ///
  {lag_4_2015} {lag_4_2016} {lag_4_2017} {lag_4_2018} ///
  {lag_3_2015} {lag_3_2016} {lag_3_2017} {lag_3_2018} ///
  {lag_2_2015} {lag_2_2016} {lag_2_2017} {lag_2_2018} ///
  {date_0_2015} {date_0_2016} {date_0_2017} {date_0_2018} ///
  {lead_1_2015} {lead_1_2016} {lead_1_2017} ///
  {lead_2_2015} {lead_2_2016} /// 
  {lead_3_2015}, bootcluster(estado ) seed(5675) level(95) boottype(wild)   nograph 
  
 
  		matrix bounds_28=r(CI_1)
		matrix vbeta_28=((bounds_28[1,2]-bounds_28[1,1])/(2*1.96))^2
			matrix list vbeta_28
   		matrix bounds_27=r(CI_2)
		matrix vbeta_27=((bounds_27[1,2]-bounds_27[1,1])/(2*1.96))^2
			matrix list vbeta_27
   		matrix bounds_26=r(CI_3)
		matrix vbeta_26=((bounds_26[1,2]-bounds_26[1,1])/(2*1.96))^2
			matrix list vbeta_26
   		matrix bounds_25=r(CI_4)
		matrix vbeta_25=((bounds_25[1,2]-bounds_25[1,1])/(2*1.96))^2
			matrix list vbeta_25
   		matrix bounds_24=r(CI_5)
		matrix vbeta_24=((bounds_24[1,2]-bounds_24[1,1])/(2*1.96))^2
			matrix list vbeta_24
   		matrix bounds_23=r(CI_6)
		matrix vbeta_23=((bounds_23[1,2]-bounds_23[1,1])/(2*1.96))^2
			matrix list vbeta_23
  		matrix bounds_22=r(CI_7)
		matrix vbeta_22=((bounds_22[1,2]-bounds_22[1,1])/(2*1.96))^2
			matrix list vbeta_22
   		matrix bounds_21=r(CI_8)
		matrix vbeta_21=((bounds_21[1,2]-bounds_21[1,1])/(2*1.96))^2
			matrix list vbeta_21
   		matrix bounds_20=r(CI_9)
		matrix vbeta_20=((bounds_20[1,2]-bounds_20[1,1])/(2*1.96))^2
			matrix list vbeta_20 
 		matrix bounds_19=r(CI_10)
		matrix vbeta_19=((bounds_19[1,2]-bounds_19[1,1])/(2*1.96))^2
			matrix list vbeta_19
		matrix bounds_18=r(CI_11)
		matrix vbeta_18=((bounds_18[1,2]-bounds_18[1,1])/(2*1.96))^2
			matrix list vbeta_18
		matrix bounds_17=r(CI_12)
		matrix vbeta_17=((bounds_17[1,2]-bounds_17[1,1])/(2*1.96))^2
			matrix list vbeta_17
		matrix bounds_16=r(CI_13)
		matrix vbeta_16=((bounds_16[1,2]-bounds_16[1,1])/(2*1.96))^2
			matrix list vbeta_16
		matrix bounds_15=r(CI_14)
		matrix vbeta_15=((bounds_15[1,2]-bounds_15[1,1])/(2*1.96))^2
			matrix list vbeta_15
		matrix bounds_14=r(CI_15)
		matrix vbeta_14=((bounds_14[1,2]-bounds_14[1,1])/(2*1.96))^2
			matrix list vbeta_14			
		matrix bounds_13=r(CI_16)
		matrix vbeta_13=((bounds_13[1,2]-bounds_13[1,1])/(2*1.96))^2
			matrix list vbeta_13			
		matrix bounds_12=r(CI_17)
		matrix vbeta_12=((bounds_12[1,2]-bounds_12[1,1])/(2*1.96))^2
			matrix list vbeta_12	
		matrix bounds_11=r(CI_18)
		matrix vbeta_11=((bounds_11[1,2]-bounds_11[1,1])/(2*1.96))^2
			matrix list vbeta_11			
		matrix bounds_10=r(CI_19)
		matrix vbeta_10=((bounds_10[1,2]-bounds_10[1,1])/(2*1.96))^2
			matrix list vbeta_10		
		matrix bounds_9=r(CI_20)
		matrix vbeta_9=((bounds_9[1,2]-bounds_9[1,1])/(2*1.96))^2
			matrix list vbeta_9			
		matrix bounds_8=r(CI_21)
		matrix vbeta_8=((bounds_8[1,2]-bounds_8[1,1])/(2*1.86))^2
			matrix list vbeta_8			
		matrix bounds_7=r(CI_22)
		matrix vbeta_7=((bounds_7[1,2]-bounds_7[1,1])/(2*1.96))^2
			matrix list vbeta_7
		matrix bounds_6=r(CI_23)
		matrix vbeta_6=((bounds_6[1,2]-bounds_6[1,1])/(2*1.96))^2
			matrix list vbeta_6
		matrix bounds_5=r(CI_24)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_25)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_26)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_27)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
		matrix bounds_1=r(CI_28)
		matrix vbeta_1=((bounds_1[1,2]-bounds_1[1,1])/(2*1.96))^2
			matrix list vbeta_1		
		matrix bounds_p1=r(CI_29)
		matrix vbeta_p1=((bounds_p1[1,2]-bounds_p1[1,1])/(2*1.96))^2
			matrix list vbeta_p1
		matrix bounds_p2=r(CI_30)
		matrix vbeta_p2=((bounds_p2[1,2]-bounds_p2[1,1])/(2*1.96))^2
			matrix list vbeta_p2
		matrix bounds_p3=r(CI_31)
		matrix vbeta_p3=((bounds_p3[1,2]-bounds_p3[1,1])/(2*1.96))^2
			matrix list vbeta_p3				
					
end

capture program drop newcov_as_homicides
 program define newcov_as_homicides, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=vbeta_28[1,1]
 matrix V_corrected[2,2]=vbeta_27[1,1]
 matrix V_corrected[3,3]=vbeta_26[1,1]
 matrix V_corrected[4,4]=vbeta_25[1,1]
 matrix V_corrected[5,5]=vbeta_24[1,1]
 matrix V_corrected[6,6]=vbeta_23[1,1]
 matrix V_corrected[7,7]=vbeta_22[1,1]
 matrix V_corrected[8,8]=vbeta_21[1,1]
 matrix V_corrected[9,9]=vbeta_20[1,1]
 matrix V_corrected[10,10]=vbeta_19[1,1]
 matrix V_corrected[11,11]=vbeta_18[1,1]
 matrix V_corrected[12,12]=vbeta_17[1,1]
 matrix V_corrected[13,13]=vbeta_16[1,1]
 matrix V_corrected[14,14]=vbeta_15[1,1]
 matrix V_corrected[15,15]=vbeta_14[1,1]
 matrix V_corrected[16,16]=vbeta_13[1,1]
 matrix V_corrected[17,17]=vbeta_12[1,1]
 matrix V_corrected[18,18]=vbeta_11[1,1]
 matrix V_corrected[19,19]=vbeta_10[1,1]
 matrix V_corrected[20,20]=vbeta_9[1,1]
 matrix V_corrected[21,21]=vbeta_8[1,1]
 matrix V_corrected[22,22]=vbeta_7[1,1]
 matrix V_corrected[23,23]=vbeta_6[1,1]
 *matrix V_corrected[24,24]=0.00001
 matrix V_corrected[24,24]=vbeta_5[1,1]
 matrix V_corrected[25,25]=vbeta_4[1,1]
 matrix V_corrected[26,26]=vbeta_3[1,1]
 matrix V_corrected[27,27]=vbeta_2[1,1]
 *matrix V_corrected[28,28]=vbeta_1[1,1]
 matrix V_corrected[28,28]=0.000001
 matrix V_corrected[29,29]=vbeta_p1[1,1]
 matrix V_corrected[30,30]=vbeta_p2[1,1]
 matrix V_corrected[31,31]=vbeta_p3[1,1]
 
 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: AS (2021)
capture program drop wildcorrection_as_homicides
 program define wildcorrection_as_homicides, eclass 
  args Dep_var
qui xi: reghdfe   `Dep_var'  $saturated $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
	getvcov_as_homicides
	newcov_as_homicides
end


 **HOMICIDES REGRESSIONS
 ***LONG
capture program drop getvcov_as_homicides2
 program define getvcov_as_homicides2, eclass 
  boottest  {lag_7_2018} ///
 {lag_6_2017} {lag_6_2018} ///
 {lag_5_2016} {lag_5_2017} {lag_5_2018} ///
  {lag_4_2015} {lag_4_2016} {lag_4_2017} {lag_4_2018} ///
  {lag_3_2015} {lag_3_2016} {lag_3_2017} {lag_3_2018} ///
  {lag_2_2015} {lag_2_2016} {lag_2_2017} {lag_2_2018} ///
  {date_0_2015} {date_0_2016} {date_0_2017} {date_0_2018} ///
  {lead_1_2015} {lead_1_2016} {lead_1_2017} ///
  {lead_2_2015} {lead_2_2016} /// 
  {lead_3_2015}, bootcluster(estado ) seed(5675) level(95) boottype(wild)   nograph 
  
 
  		matrix bounds_28=r(CI_1)
		matrix vbeta_28=((bounds_28[1,2]-bounds_28[1,1])/(2*1.96))^2
			matrix list vbeta_28
   		matrix bounds_27=r(CI_2)
		matrix vbeta_27=((bounds_27[1,2]-bounds_27[1,1])/(2*1.96))^2
			matrix list vbeta_27
   		matrix bounds_26=r(CI_3)
		matrix vbeta_26=((bounds_26[1,2]-bounds_26[1,1])/(2*1.96))^2
			matrix list vbeta_26
   		matrix bounds_25=r(CI_4)
		matrix vbeta_25=((bounds_25[1,2]-bounds_25[1,1])/(2*1.96))^2
			matrix list vbeta_25
   		matrix bounds_24=r(CI_5)
		matrix vbeta_24=((bounds_24[1,2]-bounds_24[1,1])/(2*1.96))^2
			matrix list vbeta_24
   		matrix bounds_23=r(CI_6)
		matrix vbeta_23=((bounds_23[1,2]-bounds_23[1,1])/(2*1.96))^2
			matrix list vbeta_23
  		matrix bounds_22=r(CI_7)
		matrix vbeta_22=((bounds_22[1,2]-bounds_22[1,1])/(2*1.96))^2
			matrix list vbeta_22
   		matrix bounds_21=r(CI_8)
		matrix vbeta_21=((bounds_21[1,2]-bounds_21[1,1])/(2*1.96))^2
			matrix list vbeta_21
   		matrix bounds_20=r(CI_9)
		matrix vbeta_20=((bounds_20[1,2]-bounds_20[1,1])/(2*1.96))^2
			matrix list vbeta_20 
 		matrix bounds_19=r(CI_10)
		matrix vbeta_19=((bounds_19[1,2]-bounds_19[1,1])/(2*1.96))^2
			matrix list vbeta_19
		matrix bounds_18=r(CI_11)
		matrix vbeta_18=((bounds_18[1,2]-bounds_18[1,1])/(2*1.96))^2
			matrix list vbeta_18
		matrix bounds_17=r(CI_12)
		matrix vbeta_17=((bounds_17[1,2]-bounds_17[1,1])/(2*1.96))^2
			matrix list vbeta_17
		matrix bounds_16=r(CI_13)
		matrix vbeta_16=((bounds_16[1,2]-bounds_16[1,1])/(2*1.96))^2
			matrix list vbeta_16
		matrix bounds_15=r(CI_14)
		matrix vbeta_15=((bounds_15[1,2]-bounds_15[1,1])/(2*1.96))^2
			matrix list vbeta_15
		matrix bounds_14=r(CI_15)
		matrix vbeta_14=((bounds_14[1,2]-bounds_14[1,1])/(2*1.96))^2
			matrix list vbeta_14			
		matrix bounds_13=r(CI_16)
		matrix vbeta_13=((bounds_13[1,2]-bounds_13[1,1])/(2*1.96))^2
			matrix list vbeta_13			
		matrix bounds_12=r(CI_17)
		matrix vbeta_12=((bounds_12[1,2]-bounds_12[1,1])/(2*1.96))^2
			matrix list vbeta_12	
		matrix bounds_11=r(CI_18)
		matrix vbeta_11=((bounds_11[1,2]-bounds_11[1,1])/(2*1.96))^2
			matrix list vbeta_11			
		matrix bounds_10=r(CI_19)
		matrix vbeta_10=((bounds_10[1,2]-bounds_10[1,1])/(2*1.96))^2
			matrix list vbeta_10		
		matrix bounds_9=r(CI_20)
		matrix vbeta_9=((bounds_9[1,2]-bounds_9[1,1])/(2*1.96))^2
			matrix list vbeta_9			
		matrix bounds_8=r(CI_21)
		matrix vbeta_8=((bounds_8[1,2]-bounds_8[1,1])/(2*1.86))^2
			matrix list vbeta_8			
		matrix bounds_7=r(CI_22)
		matrix vbeta_7=((bounds_7[1,2]-bounds_7[1,1])/(2*1.96))^2
			matrix list vbeta_7
		matrix bounds_6=r(CI_23)
		matrix vbeta_6=((bounds_6[1,2]-bounds_6[1,1])/(2*1.96))^2
			matrix list vbeta_6
		matrix bounds_5=r(CI_24)
		matrix vbeta_5=((bounds_5[1,2]-bounds_5[1,1])/(2*1.96))^2
			matrix list vbeta_5
		matrix bounds_4=r(CI_25)
		matrix vbeta_4=((bounds_4[1,2]-bounds_4[1,1])/(2*1.96))^2
			matrix list vbeta_4
		matrix bounds_3=r(CI_26)
		matrix vbeta_3=((bounds_3[1,2]-bounds_3[1,1])/(2*1.96))^2
			matrix list vbeta_3
		matrix bounds_2=r(CI_27)
		matrix vbeta_2=((bounds_2[1,2]-bounds_2[1,1])/(2*1.96))^2
			matrix list vbeta_2
		matrix bounds_1=r(CI_28)
		matrix vbeta_1=((bounds_1[1,2]-bounds_1[1,1])/(2*1.96))^2
			matrix list vbeta_1		

end

capture program drop newcov_as_homicides2
 program define newcov_as_homicides2, eclass   /*here create a program called newcov that affects e() variables*/
 matrix V_corrected=e(V)
 matrix V_corrected[1,1]=vbeta_28[1,1]
 matrix V_corrected[2,2]=vbeta_27[1,1]
 matrix V_corrected[3,3]=vbeta_26[1,1]
 matrix V_corrected[4,4]=vbeta_25[1,1]
 matrix V_corrected[5,5]=vbeta_24[1,1]
 matrix V_corrected[6,6]=vbeta_23[1,1]
 matrix V_corrected[7,7]=vbeta_22[1,1]
 matrix V_corrected[8,8]=vbeta_21[1,1]
 matrix V_corrected[9,9]=vbeta_20[1,1]
 matrix V_corrected[10,10]=vbeta_19[1,1]
 matrix V_corrected[11,11]=vbeta_18[1,1]
 matrix V_corrected[12,12]=vbeta_17[1,1]
 matrix V_corrected[13,13]=vbeta_16[1,1]
 matrix V_corrected[14,14]=vbeta_15[1,1]
 matrix V_corrected[15,15]=vbeta_14[1,1]
 matrix V_corrected[16,16]=vbeta_13[1,1]
 matrix V_corrected[17,17]=vbeta_12[1,1]
 matrix V_corrected[18,18]=vbeta_11[1,1]
 matrix V_corrected[19,19]=vbeta_10[1,1]
 matrix V_corrected[20,20]=vbeta_9[1,1]
 *matrix V_corrected[21,21]=vbeta_8[1,1]
 matrix V_corrected[22,22]=vbeta_9[1,1]
 matrix V_corrected[23,23]=vbeta_6[1,1]
 matrix V_corrected[24,24]=vbeta_5[1,1]
 matrix V_corrected[25,25]=vbeta_4[1,1]
 matrix V_corrected[26,26]=vbeta_3[1,1]
 matrix V_corrected[27,27]=vbeta_2[1,1]
 matrix V_corrected[28,28]=vbeta_1[1,1]
 ereturn repost V= V_corrected       /*this program newcov substitutes the variance-covariance matrix with new var-cov */ 
end

*program to get results: AS (2021)
capture program drop wildcorrection_as_homicides2
 program define wildcorrection_as_homicides2, eclass 
  args Dep_var
qui xi: reghdfe   `Dep_var'  $saturated $controls_time_acuerdo  i.year, a(inegi) vce(cluster estado)
	getvcov_as_homicides2
	newcov_as_homicides2
end

 
