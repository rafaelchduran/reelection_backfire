*===============================================================================================
*SET GLOBALS

global  controls logdefuncionespc ap4_2_3 ap4_2_11 ap4_12b ap5_4_2_b ap5_4_8_b ///
alignment_executive_strong alignment_governor_strong governor_pri winning_margin winning_margin_governor ///
pan_mayor2 pri_mayor2 morena_mayor2

global controls_t_2 logdefuncionespc_t_2 ap4_2_3_t_2 ap4_2_11_t_2 ap4_12b_t_2 ap5_4_2_b_t_2 ap5_4_8_b_t_2 ///
 alignment_executive_strong_t_2 alignment_governor_strong_t_2 governor_pri_t_2 winning_margin_t_2 ///
 winning_margin_governor_t_2 pan_mayor2_t_2 pri_mayor2_t_2 morena_mayor2_t_2
 
 global controls_t_2 logdefuncionespc_t_2  ap4_2_11_t_2  ap4_12b_t_2 ap5_4_2_b_t_2 ap5_4_8_b_t_2 ///
 alignment_executive_strong_t_2 alignment_governor_strong_t_2 ///
 winning_margin_governor_t_2 pri_mayor2_t_2  


global controls_t_3 logdefuncionespc_t_3 ap4_2_3_t_3 ap4_2_11_t_3 ap4_12b_t_3 ap5_4_2_b_t_3 ap5_4_8_b_t_3 ///
 alignment_executive_strong_t_3 alignment_governor_strong_t_3 governor_pri_t_3 winning_margin_t_3 ///
 winning_margin_governor_t_3 pan_mayor2_t_3 pri_mayor2_t_3 morena_mayor2_t_3
 

global controls_2014 logdefuncionespc_2014 ap4_2_3_2014 ap4_2_11_2014 ap4_12b_2014 ap5_4_2_b_2014 ///
 ap5_4_8_b_2014 alignment_executive_strong_2014 alignment_governor_strong_2014 winning_margin_2014 ///
 winning_margin_governor_2014 pan_mayor2_2014 pri_mayor2_2014  

 global controls_2014 logdefuncionespc_2014 ap4_2_3_2014 ap4_2_11_2014  ///
  alignment_executive_strong_2014 alignment_governor_strong_2014 winning_margin_2014 ///
 winning_margin_governor_2014 acuerdo_2014

 
  
global controls_mean logdefuncionespc_mean ap4_2_3_mean ap4_2_11_mean ap4_12b_mean ap5_4_2_b_mean ///
 ap5_4_8_b_mean alignment_executive_strong_mean alignment_governor_strong_mean winning_margin_mean ///
 winning_margin_governor_mean pan_mayor2_mean pri_mayor2_mean  logpop_
 
global controls2_mean logdefuncionespc_mean ap4_2_3_mean ap4_2_11_mean ap4_12b_mean ap5_4_2_b_mean ///
 ap5_4_8_b_mean alignment_executive_strong_mean alignment_governor_strong_mean winning_margin_mean ///
 winning_margin_governor_mean pan_mayor2_mean pri_mayor2_mean  acuerdo2_mean


global controls $controls_mean
global controls2 $controls2_mean
global controls $controls_2014


	global narco ap4_2_3_lag_8 ap4_2_3_lag_7 ap4_2_3_lag_6 ap4_2_3_lag_5 ap4_2_3_lag_4 ap4_2_3_lag_3 ap4_2_3_lag_2
	global punishment ap4_2_11_lag_8 ap4_2_11_lag_7 ap4_2_11_lag_6 ap4_2_11_lag_5 ap4_2_11_lag_4 ap4_2_11_lag_3 ap4_2_11_lag_2
	global money ap4_12b_lag_8 ap4_12b_lag_7 ap4_12b_lag_6 ap4_12b_lag_5 ap4_12b_lag_4 ap4_12b_lag_3 ap4_12b_lag_2
	global police ap5_4_2_b_lag_8 ap5_4_2_b_lag_7 ap5_4_2_b_lag_6 ap5_4_2_b_lag_5 ap5_4_2_b_lag_4 ap5_4_2_b_lag_3 ap5_4_2_b_lag_2
	global army ap5_4_8_b_lag_8 ap5_4_8_b_lag_7 ap5_4_8_b_lag_6 ap5_4_8_b_lag_5 ap5_4_8_b_lag_4 ap5_4_8_b_lag_3 ap5_4_8_b_lag_2
	global citizens $narco $punishment $money $police $army
	global citizens_  $punishment $money $police $army

	global narco2 ap4_2_3
	global punishment2 ap4_2_11
	global money2 ap4_12b
	global police2 ap5_4_2_b
	global army2 ap5_4_8_b
	global citizens2  $narco2 $punishment2 $money2 $police2 $army2  
	
	global citizens3  ap4_2_11_pre ap4_12b_pre ap5_4_2_b_pre ap5_4_8_b_pre // ap4_2_3_pre
	
	global incumbent_adv inc_lag_8 inc_lag_7 inc_lag_6 inc_lag_5 inc_lag_4 inc_lag_3 inc_lag_2
	global incumbent_adv2 inc_party_runsfor1_lag_8 inc_party_runsfor1_lag_7 inc_party_runsfor1_lag_6 inc_party_runsfor1_lag_5 inc_party_runsfor1_lag_4 inc_party_runsfor1_lag_3 inc_party_runsfor1_lag_2
	global incumbent_adv3 inc_party_won_lag_8 inc_party_won_lag_7 inc_party_won_lag_6 inc_party_won_lag_5 inc_party_won_lag_4 inc_party_won_lag_3 inc_party_won_lag_2
	global num_parties numparties_eff_lag_8 numparties_eff_lag_7 numparties_eff_lag_6 numparties_eff_lag_5 numparties_eff_lag_4 numparties_eff_lag_3 numparties_eff_lag_2
	global num_parties2 numparties_eff_molinar_lag_8 numparties_eff_molinar_lag_7 numparties_eff_molinar_lag_6 numparties_eff_molinar_lag_5 numparties_eff_molinar_lag_4 numparties_eff_molinar_lag_3 numparties_eff_molinar_lag_2
	
	global incumbency $incumbent_adv $num_parties
	
	global controls winning_margin_governor  governor_notaligned pri_mayor2 morena_mayor2 $citizens2 winning_margin alignment_executive_strong
	*global controls winning_margin_governor  governor_alignment pri_mayor2 morena_mayor2 
	global controls_pre winning_margin_governor_pre governor_alignment_pre logdefuncionespc_pre  winning_margin_pre alignment_executive_strong_pre $citizens3
	global controls_pre winning_margin_governor_pre governor_alignment_pre   winning_margin_pre alignment_executive_strong_pre $citizens3

	*global controls margin_lag_8 margin_lag_7 margin_lag_6 margin_lag_5 margin_lag_4 margin_lag_3 margin_lag_2 governor_alignment_lag_8 governor_alignment_lag_7 governor_alignment_lag_6 governor_alignment_lag_5 governor_alignment_lag_4 governor_alignment_lag_3 governor_alignment_lag_2 	
	global defunciones logdefuncionespc_lag_8 logdefuncionespc_lag_7 logdefuncionespc_lag_6 logdefuncionespc_lag_5 logdefuncionespc_lag_4 logdefuncionespc_lag_3 logdefuncionespc_lag_2	
*/

/*temporal globals
	global narco ap4_2_3_lag_8 ap4_2_3_lag_7 ap4_2_3_lag_6 ap4_2_3_lag_5 ap4_2_3_lag_4 ap4_2_3_lag_3 ap4_2_3_lag_2
	global punishment ap4_2_11_lag_8 ap4_2_11_lag_7 ap4_2_11_lag_6 ap4_2_11_lag_5 ap4_2_11_lag_4 ap4_2_11_lag_3 ap4_2_11_lag_2
	global money ap4_12b_lag_8 ap4_12b_lag_7 ap4_12b_lag_6 ap4_12b_lag_5 ap4_12b_lag_4 ap4_12b_lag_3 ap4_12b_lag_2
	global police ap5_4_2_b_lag_8 ap5_4_2_b_lag_7 ap5_4_2_b_lag_6 ap5_4_2_b_lag_5 ap5_4_2_b_lag_4 ap5_4_2_b_lag_3 ap5_4_2_b_lag_2
	global army ap5_4_8_b_lag_8 ap5_4_8_b_lag_7 ap5_4_8_b_lag_6 ap5_4_8_b_lag_5 ap5_4_8_b_lag_4 ap5_4_8_b_lag_3 ap5_4_8_b_lag_2
	global citizens $narco $punishment $money $police $army
	global homicides logdefuncionespc_lag_2 logdefuncionespc_lag_3 logdefuncionespc_lag_4 logdefuncionespc_lag_5 logdefuncionespc_lag_6 logdefuncionespc_lag_7 logdefuncionespc_lag_8
	global executive alignment_executive_strong_lag_2 alignment_executive_strong_lag_3 alignment_executive_strong_lag_4 alignment_executive_strong_lag_5 alignment_executive_strong_lag_6 alignment_executive_strong_lag_7 alignment_executive_strong_lag_8
	global governor alignment_governor_strong_lag_2 alignment_governor_strong_lag_3 alignment_governor_strong_lag_4 alignment_governor_strong_lag_5 alignment_governor_strong_lag_6 alignment_governor_strong_lag_7 alignment_governor_strong_lag_8
	global margin winning_margin_lag_2 winning_margin_lag_3 winning_margin_lag_4 winning_margin_lag_5 winning_margin_lag_6 winning_margin_lag_7 winning_margin_lag_8
	global margin_governor winning_margin_governor_lag_2 winning_margin_governor_lag_3 winning_margin_governor_lag_4 winning_margin_governor_lag_5 winning_margin_governor_lag_6 winning_margin_governor_lag_7 winning_margin_governor_lag_8
	global acuerdo acuerdo_lag_2 acuerdo_lag_3 acuerdo_lag_4 acuerdo_lag_5 acuerdo_lag_6 acuerdo_lag_7 acuerdo_lag_8
	global acuerdo2 acuerdo2_lag_2 acuerdo2_lag_3 acuerdo2_lag_4 acuerdo2_lag_5 acuerdo2_lag_6 acuerdo2_lag_7 acuerdo2_lag_8
	
	global controls_time_acuerdo $punishment $homicides $executive $governor $margin $margin_governor 
	global controls_time_acuerdo2 $punishment $homicides $executive $governor $margin $margin_governor 

	global controls $controls_time_acuerdo
	*/
	
