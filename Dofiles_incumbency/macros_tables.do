*Program macros for tables
capture program drop macros_tables
 program define macros_tables, eclass  
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	estadd local wildci 

**estimate aggregate effect:
	sum perc if date_0==1, meanonly
	local a = r(mean)
	sum perc if lead_1==1, meanonly
	local b = r(mean)
	sum perc if lead_2==1, meanonly
	local c = r(mean)
	sum perc if lead_3==1, meanonly
	local d = r(mean)
	
test [(_b[date_0]*`a')+(_b[lead_1]*`b')+(_b[lead_2]*`c') + (_b[lead_3]*`d')]/4 	=0
	glo p_aggregate: di %5.3f r(p)
	estadd local p_aggregate $p_aggregate
	glo est_aggregate= "" 
			if (${p_aggregate}<=0.11) global est_aggregate = "*"
			if (${p_aggregate}<=0.05) global est_aggregate = "**"
			if (${p_aggregate}<=0.01) global est_aggregate = "***"	
lincom 	[(_b[date_0]*`a')+(_b[lead_1]*`b')+(_b[lead_2]*`c') + (_b[lead_3]*`d')]/4 
	glo aggregate: di %5.3f r(estimate)
	estadd local aggregate $${aggregate}^{${est_aggregate}}$$	
	glo se_aggregate: di %5.3f r(se)
	estadd local se_aggregate $se_aggregate	
end

*Program macros for tables
capture program drop macros_tables2
 program define macros_tables2, eclass  
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	estadd local wildci \checkmark

**estimate aggregate effect:
	sum perc if date_0==1, meanonly
	local a = r(mean)
	sum perc if lead_1==1, meanonly
	local b = r(mean)
	sum perc if lead_2==1, meanonly
	local c = r(mean)
	sum perc if lead_3==1, meanonly
	local d = r(mean)
	
lincom 	[(_b[date_0]*`a')+(_b[lead_1]*`b')+(_b[lead_2]*`c') + (_b[lead_3]*`d')]/4 
	glo aggregate: di %5.3f r(estimate)
	estadd local aggregate $aggregate
	glo se_aggregate: di %5.3f r(se)
	estadd local se_aggregate $se_aggregate
test [(_b[date_0]*`a')+(_b[lead_1]*`b')+(_b[lead_2]*`c') + (_b[lead_3]*`d')]/4 	=0
	glo p_aggregate: di %5.3f r(p)
	estadd local p_aggregate $p_aggregate
	
end

*Program macros for tables
capture program drop macros_tables3
 program define macros_tables3, eclass  
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	estadd local wildci \checkmark
end


*Program macros for tables in sercies
capture program drop macros_tables4
 program define macros_tables4, eclass  
	estadd local controls \checkmark
	estadd local munfe \checkmark
	estadd local yearfe \checkmark
	estadd local clustermun \checkmark
	estadd local wildci \checkmark

**estimate aggregate effect:
	sum perc if date_0==1, meanonly
	local a = r(mean)
	sum perc if lead_1==1, meanonly
	local b = r(mean)
	sum perc if lead_3==1, meanonly
	local d = r(mean)
	
lincom 	[(_b[date_0]*`a')+(_b[lead_1]*`b') + (_b[lead_3]*`d')]/3 
	glo aggregate: di %5.3f r(estimate)
	estadd local aggregate $aggregate
	glo se_aggregate: di %5.3f r(se)
	estadd local se_aggregate $se_aggregate
test [(_b[date_0]*`a')+(_b[lead_1]*`b')+ (_b[lead_3]*`d')]/3 	=0
	glo p_aggregate: di %5.3f r(p)
	estadd local p_aggregate $p_aggregate
	
end

