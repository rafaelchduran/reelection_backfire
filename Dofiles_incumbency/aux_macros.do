*AUX for macros 

lincom 	(_b[date_0]*`a')+(_b[lead_1]*`b')+(_b[lead_2]*`c') + (_b[lead_3]*`d') 
	glo aggregate: di %5.3f r(estimate)
	estadd local aggregate $aggregate
	glo se_aggregate: di %5.3f r(se)
	estadd local se_aggregate $se_aggregate
test (_b[date_0]*`a')+(_b[lead_1]*`b')+(_b[lead_2]*`c') + (_b[lead_3]*`d') 	=0
	glo p_aggregate: di %5.3f r(p)
	estadd local p_aggregate $p_aggregate

boottest date_0, bootcluster(estado) nograph seed(5675) level(95)  boottype(wild)   // works for acuerdo3 5%
	glo p_date_0: di %5.3f r(p)
	estadd local p_date_0 $p_date_0
	matrix a = r(CI)
	glo cileft_date_0: di %5.3f a[1,1]
	glo ciright_date_0: di %5.3f a[1,2]
	estadd local ci_date_0 = trim("[$cileft_date_0, $ciright_date_0]")
boottest lead_1, bootcluster(estado) nograph seed(5675) level(95)  // works for acuerdo3 5%
	glo p_lead_1: di %5.3f r(p)
	estadd local p_lead_1 $p_lead_1
	matrix a = r(CI)
	glo cileft_lead_1: di %5.3f a[1,1]
	glo ciright_lead_1: di %5.3f a[1,2]
	estadd local ci_lead_1 = trim("[$cileft_lead_1, $ciright_lead_1]")
boottest lead_2, bootcluster(estado) nograph seed(5675) level(95)  // works for acuerdo3 10%
	glo p_lead_2: di %5.3f r(p)
	estadd local p_lead_2 $p_lead_2
	matrix a = r(CI)
	glo cileft_lead_2: di %5.3f a[1,1]
	glo ciright_lead_2: di %5.3f a[1,2]
	estadd local ci_lead_2 = trim("[$cileft_lead_2, $ciright_lead_2]")
boottest lead_3, bootcluster(estado) nograph seed(5675) level(95) // works for acuerdo3 5%
	glo p_lead_3: di %5.3f r(p)
	estadd local p_lead_3 $p_lead_3
	matrix a = r(CI)
	glo cileft_lead_3: di %5.3f a[1,1]
	glo ciright_lead_3: di %5.3f a[1,2]
	estadd local ci_lead_3 = trim("[$cileft_lead_3, $ciright_lead_3]")



			
